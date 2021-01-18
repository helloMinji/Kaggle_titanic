![image](https://user-images.githubusercontent.com/41939828/104958720-dcc97200-5a13-11eb-80b7-99214b44f7d8.png)

### Kaggle Competition | Titanic Machine Learning from Disaster
타이타닉호의 침몰은 역사상 가장 악명 높은 난파선 중 하나입니다.

1912년 4월 15일, RMS 타이타닉은 빙산과 충돌한 후 가라앉았습니다. 불행히도, 모든 탑승객을 위한 구명보트가 충분하지 않아서 승객과 승무원 2224명 중 1502명이 사망했습니다.

생존에 운의 요소가 포함되어 있기는 하지만, 일부 그룹의 사람들은 다른 그룹보다 살아남을 가능성이 더 높은 것 같습니다.

이 과제에서는 다음과 같은 질문에 대한 답을 얻을 수 있는 예측 모델을 구축할 것을 요청합니다. 승객 데이터(이름, 나이, 성별, 사회경제적 계층 등)를 이용해 "어떤 종류의 사람들이 더 생존할 가능성이 높았을까요?"

ㅡ[홈페이지](https://www.kaggle.com/c/titanic)에서 발췌
</br>     
</br>     
</br>     

-----------------------------------------------------------------------------------
## R

### 설치
1. 저장소 전체를 zip 파일로 다운로드하거나 오른쪽 코드를 터미널에서 실행 ```git clone https://github.com/agconti/kaggle-titanic.git```
2. R [설치](https://www.r-project.org/) (cran - 원하는 나라의 링크 아무거나 클릭 - 알맞은 os 버전 선택)
3. R studio [설치](https://www.rstudio.com/)
4. R studio에서 analysis_191015.R을 실행 (코드의 package 중 필요한 패키지는 코드 내에서 설치: install.packages("패키지명") )
</br>     

### 코드 정보
코드에 대한 자세한 내용은 [링크](https://hellominji.tistory.com/2)을 참고하세요.
</br>     
</br>     
</br>     

-----------------------------------------------------------------------------------
## Python

### 설치
1. 저장소 전체를 zip 파일로 다운로드하거나 오른쪽 코드를 터미널에서 실행 ```git clone https://github.com/agconti/kaggle-titanic.git```
2. 아나콘다 [설치](https://www.anaconda.com/products/individual#download-section)
3. cmd창에서 필요한 패키지 설치 ```conda install 패키지명```
4. 주피터 노트북에서 analysys_191109.ipynb를 실행
5. 완료되면 deactivate.

#### 패키지
- pandas
- numpy
- sklearn
- matplotlib
</br>     

### 코드 정보
#### 데이터 처리
- Pandas로 데이터 가져 오기
- 데이터 정리
#### 데이터 분석
- 로지스틱 회귀 모델(Logistic Regression)
- 랜덤포레스트(Random Forest)
- kNN
- Navie Bayes
- SVM
#### 분석의 평가
- 로컬에서 결과를 평가하기위한 train_and_test 함수 생성(정확도 확인)
- 결과를 제출용 파일로 출력
