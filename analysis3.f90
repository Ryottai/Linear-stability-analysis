program presentation3
implicit none
real trJ, detJ
write(*,*) 'input trJ'
read(*,*) trJ
write(*,*) 'input detJ'
read(*,*) detJ

!平衡点の種類、解の応答
if (detJ>0) then
    if (trJ>0) then
        if (detJ**2 < trJ**2/4) then
            write(*,*) 'Statible Node, Convergence' !不安定結節点、発散
        else if (detJ**2 > trJ**2/4) then
            write(*,*) 'Stable Focus, Damped Vibration' !不安定焦点、発散振動
        end if
    else if (trJ<0) then
        if (detJ**2 < trJ**2/4) then
            write(*,*) 'Statible Node, Convergence' !安定結節点、収束
        else if (detJ**2 > trJ**2/4) then
            write(*,*) 'Stable Focus, Damped Vibration' !安定焦点、減衰振動     
        end if
    else 
        write(*,*) 'Center Point(Whirlpool Point), Limit cycle' !中心点、リミットサイクル    
    end if
else if (detJ<0) then
    write(*,*) 'Saddle Point, Convergence' !鞍点、サドル型
else
    write(*,*) 'No value'
end if

stop
end program presentation3
