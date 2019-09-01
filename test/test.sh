testequationsfile=testeqs 
bin=../calc

cat ${testequationsfile} | \
   awk '{for (i=1;i<NF-1;i++){
            printf $i;
            printf " ";
         };
         printf "\n"
        }' | \
   ${bin} > tmprslts

cat ${testequationsfile} | \
   awk '{print $NF}' \
   > tmpref

idx=0
failed=0
succeded=0
for diff in $(paste tmprslts tmpref | awk '{print sqrt(($1 - $2)**2)}');
do
   ((idx++))
   str=$(cat ${testequationsfile} | head -n ${idx} | tail -n 1)
   if [[ $(echo "${diff} > 1*10^-15" | bc -l) -eq 0 ]];then
      str=$(cat ${testequationsfile} | head -n ${idx} | tail -n 1)
      echo "[  OK  ] ${str}"
      ((succeded++))
   else
      echo "[ FAIL ] ${str}"
      echo " result: $(cat tmprslts | head -n ${idx} | tail -n 1)"
      ((failed++))
   fi
done
echo ""
echo "${succeded} Tests succeded"
echo "${failed} Tests failed"


