{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Comprehend
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Comprehend where

import Data.Proxy
import Network.AWS.Comprehend
import Test.AWS.Comprehend.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDetectSentiment $
--             batchDetectSentiment
--
--         , requestDeleteEntityRecognizer $
--             deleteEntityRecognizer
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             describeKeyPhrasesDetectionJob
--
--         , requestListEntitiesDetectionJobs $
--             listEntitiesDetectionJobs
--
--         , requestStartSentimentDetectionJob $
--             startSentimentDetectionJob
--
--         , requestBatchDetectSyntax $
--             batchDetectSyntax
--
--         , requestStartTopicsDetectionJob $
--             startTopicsDetectionJob
--
--         , requestBatchDetectKeyPhrases $
--             batchDetectKeyPhrases
--
--         , requestDescribeSentimentDetectionJob $
--             describeSentimentDetectionJob
--
--         , requestStartEntitiesDetectionJob $
--             startEntitiesDetectionJob
--
--         , requestDescribeEntityRecognizer $
--             describeEntityRecognizer
--
--         , requestDetectSentiment $
--             detectSentiment
--
--         , requestStartDominantLanguageDetectionJob $
--             startDominantLanguageDetectionJob
--
--         , requestStopTrainingDocumentClassifier $
--             stopTrainingDocumentClassifier
--
--         , requestDescribeDocumentClassificationJob $
--             describeDocumentClassificationJob
--
--         , requestBatchDetectEntities $
--             batchDetectEntities
--
--         , requestCreateEntityRecognizer $
--             createEntityRecognizer
--
--         , requestStopKeyPhrasesDetectionJob $
--             stopKeyPhrasesDetectionJob
--
--         , requestCreateDocumentClassifier $
--             createDocumentClassifier
--
--         , requestListEntityRecognizers $
--             listEntityRecognizers
--
--         , requestStopSentimentDetectionJob $
--             stopSentimentDetectionJob
--
--         , requestDetectDominantLanguage $
--             detectDominantLanguage
--
--         , requestDescribeTopicsDetectionJob $
--             describeTopicsDetectionJob
--
--         , requestListDocumentClassificationJobs $
--             listDocumentClassificationJobs
--
--         , requestDetectEntities $
--             detectEntities
--
--         , requestDescribeDocumentClassifier $
--             describeDocumentClassifier
--
--         , requestDescribeDominantLanguageDetectionJob $
--             describeDominantLanguageDetectionJob
--
--         , requestStopEntitiesDetectionJob $
--             stopEntitiesDetectionJob
--
--         , requestStopTrainingEntityRecognizer $
--             stopTrainingEntityRecognizer
--
--         , requestListKeyPhrasesDetectionJobs $
--             listKeyPhrasesDetectionJobs
--
--         , requestDescribeEntitiesDetectionJob $
--             describeEntitiesDetectionJob
--
--         , requestStopDominantLanguageDetectionJob $
--             stopDominantLanguageDetectionJob
--
--         , requestListTopicsDetectionJobs $
--             listTopicsDetectionJobs
--
--         , requestBatchDetectDominantLanguage $
--             batchDetectDominantLanguage
--
--         , requestStartDocumentClassificationJob $
--             startDocumentClassificationJob
--
--         , requestDetectKeyPhrases $
--             detectKeyPhrases
--
--         , requestDetectSyntax $
--             detectSyntax
--
--         , requestListSentimentDetectionJobs $
--             listSentimentDetectionJobs
--
--         , requestDeleteDocumentClassifier $
--             deleteDocumentClassifier
--
--         , requestListDominantLanguageDetectionJobs $
--             listDominantLanguageDetectionJobs
--
--         , requestStartKeyPhrasesDetectionJob $
--             startKeyPhrasesDetectionJob
--
--         , requestListDocumentClassifiers $
--             listDocumentClassifiers
--
--           ]

--     , testGroup "response"
--         [ responseBatchDetectSentiment $
--             batchDetectSentimentResponse
--
--         , responseDeleteEntityRecognizer $
--             deleteEntityRecognizerResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             describeKeyPhrasesDetectionJobResponse
--
--         , responseListEntitiesDetectionJobs $
--             listEntitiesDetectionJobsResponse
--
--         , responseStartSentimentDetectionJob $
--             startSentimentDetectionJobResponse
--
--         , responseBatchDetectSyntax $
--             batchDetectSyntaxResponse
--
--         , responseStartTopicsDetectionJob $
--             startTopicsDetectionJobResponse
--
--         , responseBatchDetectKeyPhrases $
--             batchDetectKeyPhrasesResponse
--
--         , responseDescribeSentimentDetectionJob $
--             describeSentimentDetectionJobResponse
--
--         , responseStartEntitiesDetectionJob $
--             startEntitiesDetectionJobResponse
--
--         , responseDescribeEntityRecognizer $
--             describeEntityRecognizerResponse
--
--         , responseDetectSentiment $
--             detectSentimentResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             startDominantLanguageDetectionJobResponse
--
--         , responseStopTrainingDocumentClassifier $
--             stopTrainingDocumentClassifierResponse
--
--         , responseDescribeDocumentClassificationJob $
--             describeDocumentClassificationJobResponse
--
--         , responseBatchDetectEntities $
--             batchDetectEntitiesResponse
--
--         , responseCreateEntityRecognizer $
--             createEntityRecognizerResponse
--
--         , responseStopKeyPhrasesDetectionJob $
--             stopKeyPhrasesDetectionJobResponse
--
--         , responseCreateDocumentClassifier $
--             createDocumentClassifierResponse
--
--         , responseListEntityRecognizers $
--             listEntityRecognizersResponse
--
--         , responseStopSentimentDetectionJob $
--             stopSentimentDetectionJobResponse
--
--         , responseDetectDominantLanguage $
--             detectDominantLanguageResponse
--
--         , responseDescribeTopicsDetectionJob $
--             describeTopicsDetectionJobResponse
--
--         , responseListDocumentClassificationJobs $
--             listDocumentClassificationJobsResponse
--
--         , responseDetectEntities $
--             detectEntitiesResponse
--
--         , responseDescribeDocumentClassifier $
--             describeDocumentClassifierResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             describeDominantLanguageDetectionJobResponse
--
--         , responseStopEntitiesDetectionJob $
--             stopEntitiesDetectionJobResponse
--
--         , responseStopTrainingEntityRecognizer $
--             stopTrainingEntityRecognizerResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             listKeyPhrasesDetectionJobsResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             describeEntitiesDetectionJobResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             stopDominantLanguageDetectionJobResponse
--
--         , responseListTopicsDetectionJobs $
--             listTopicsDetectionJobsResponse
--
--         , responseBatchDetectDominantLanguage $
--             batchDetectDominantLanguageResponse
--
--         , responseStartDocumentClassificationJob $
--             startDocumentClassificationJobResponse
--
--         , responseDetectKeyPhrases $
--             detectKeyPhrasesResponse
--
--         , responseDetectSyntax $
--             detectSyntaxResponse
--
--         , responseListSentimentDetectionJobs $
--             listSentimentDetectionJobsResponse
--
--         , responseDeleteDocumentClassifier $
--             deleteDocumentClassifierResponse
--
--         , responseListDominantLanguageDetectionJobs $
--             listDominantLanguageDetectionJobsResponse
--
--         , responseStartKeyPhrasesDetectionJob $
--             startKeyPhrasesDetectionJobResponse
--
--         , responseListDocumentClassifiers $
--             listDocumentClassifiersResponse
--
--           ]
--     ]

-- Requests

requestBatchDetectSentiment :: BatchDetectSentiment -> TestTree
requestBatchDetectSentiment = req
    "BatchDetectSentiment"
    "fixture/BatchDetectSentiment.yaml"

requestDeleteEntityRecognizer :: DeleteEntityRecognizer -> TestTree
requestDeleteEntityRecognizer = req
    "DeleteEntityRecognizer"
    "fixture/DeleteEntityRecognizer.yaml"

requestDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJob -> TestTree
requestDescribeKeyPhrasesDetectionJob = req
    "DescribeKeyPhrasesDetectionJob"
    "fixture/DescribeKeyPhrasesDetectionJob.yaml"

requestListEntitiesDetectionJobs :: ListEntitiesDetectionJobs -> TestTree
requestListEntitiesDetectionJobs = req
    "ListEntitiesDetectionJobs"
    "fixture/ListEntitiesDetectionJobs.yaml"

requestStartSentimentDetectionJob :: StartSentimentDetectionJob -> TestTree
requestStartSentimentDetectionJob = req
    "StartSentimentDetectionJob"
    "fixture/StartSentimentDetectionJob.yaml"

requestBatchDetectSyntax :: BatchDetectSyntax -> TestTree
requestBatchDetectSyntax = req
    "BatchDetectSyntax"
    "fixture/BatchDetectSyntax.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob = req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases = req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestDescribeSentimentDetectionJob :: DescribeSentimentDetectionJob -> TestTree
requestDescribeSentimentDetectionJob = req
    "DescribeSentimentDetectionJob"
    "fixture/DescribeSentimentDetectionJob.yaml"

requestStartEntitiesDetectionJob :: StartEntitiesDetectionJob -> TestTree
requestStartEntitiesDetectionJob = req
    "StartEntitiesDetectionJob"
    "fixture/StartEntitiesDetectionJob.yaml"

requestDescribeEntityRecognizer :: DescribeEntityRecognizer -> TestTree
requestDescribeEntityRecognizer = req
    "DescribeEntityRecognizer"
    "fixture/DescribeEntityRecognizer.yaml"

requestDetectSentiment :: DetectSentiment -> TestTree
requestDetectSentiment = req
    "DetectSentiment"
    "fixture/DetectSentiment.yaml"

requestStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJob -> TestTree
requestStartDominantLanguageDetectionJob = req
    "StartDominantLanguageDetectionJob"
    "fixture/StartDominantLanguageDetectionJob.yaml"

requestStopTrainingDocumentClassifier :: StopTrainingDocumentClassifier -> TestTree
requestStopTrainingDocumentClassifier = req
    "StopTrainingDocumentClassifier"
    "fixture/StopTrainingDocumentClassifier.yaml"

requestDescribeDocumentClassificationJob :: DescribeDocumentClassificationJob -> TestTree
requestDescribeDocumentClassificationJob = req
    "DescribeDocumentClassificationJob"
    "fixture/DescribeDocumentClassificationJob.yaml"

requestBatchDetectEntities :: BatchDetectEntities -> TestTree
requestBatchDetectEntities = req
    "BatchDetectEntities"
    "fixture/BatchDetectEntities.yaml"

requestCreateEntityRecognizer :: CreateEntityRecognizer -> TestTree
requestCreateEntityRecognizer = req
    "CreateEntityRecognizer"
    "fixture/CreateEntityRecognizer.yaml"

requestStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJob -> TestTree
requestStopKeyPhrasesDetectionJob = req
    "StopKeyPhrasesDetectionJob"
    "fixture/StopKeyPhrasesDetectionJob.yaml"

requestCreateDocumentClassifier :: CreateDocumentClassifier -> TestTree
requestCreateDocumentClassifier = req
    "CreateDocumentClassifier"
    "fixture/CreateDocumentClassifier.yaml"

requestListEntityRecognizers :: ListEntityRecognizers -> TestTree
requestListEntityRecognizers = req
    "ListEntityRecognizers"
    "fixture/ListEntityRecognizers.yaml"

requestStopSentimentDetectionJob :: StopSentimentDetectionJob -> TestTree
requestStopSentimentDetectionJob = req
    "StopSentimentDetectionJob"
    "fixture/StopSentimentDetectionJob.yaml"

requestDetectDominantLanguage :: DetectDominantLanguage -> TestTree
requestDetectDominantLanguage = req
    "DetectDominantLanguage"
    "fixture/DetectDominantLanguage.yaml"

requestDescribeTopicsDetectionJob :: DescribeTopicsDetectionJob -> TestTree
requestDescribeTopicsDetectionJob = req
    "DescribeTopicsDetectionJob"
    "fixture/DescribeTopicsDetectionJob.yaml"

requestListDocumentClassificationJobs :: ListDocumentClassificationJobs -> TestTree
requestListDocumentClassificationJobs = req
    "ListDocumentClassificationJobs"
    "fixture/ListDocumentClassificationJobs.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities = req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestDescribeDocumentClassifier :: DescribeDocumentClassifier -> TestTree
requestDescribeDocumentClassifier = req
    "DescribeDocumentClassifier"
    "fixture/DescribeDocumentClassifier.yaml"

requestDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJob -> TestTree
requestDescribeDominantLanguageDetectionJob = req
    "DescribeDominantLanguageDetectionJob"
    "fixture/DescribeDominantLanguageDetectionJob.yaml"

requestStopEntitiesDetectionJob :: StopEntitiesDetectionJob -> TestTree
requestStopEntitiesDetectionJob = req
    "StopEntitiesDetectionJob"
    "fixture/StopEntitiesDetectionJob.yaml"

requestStopTrainingEntityRecognizer :: StopTrainingEntityRecognizer -> TestTree
requestStopTrainingEntityRecognizer = req
    "StopTrainingEntityRecognizer"
    "fixture/StopTrainingEntityRecognizer.yaml"

requestListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobs -> TestTree
requestListKeyPhrasesDetectionJobs = req
    "ListKeyPhrasesDetectionJobs"
    "fixture/ListKeyPhrasesDetectionJobs.yaml"

requestDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJob -> TestTree
requestDescribeEntitiesDetectionJob = req
    "DescribeEntitiesDetectionJob"
    "fixture/DescribeEntitiesDetectionJob.yaml"

requestStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJob -> TestTree
requestStopDominantLanguageDetectionJob = req
    "StopDominantLanguageDetectionJob"
    "fixture/StopDominantLanguageDetectionJob.yaml"

requestListTopicsDetectionJobs :: ListTopicsDetectionJobs -> TestTree
requestListTopicsDetectionJobs = req
    "ListTopicsDetectionJobs"
    "fixture/ListTopicsDetectionJobs.yaml"

requestBatchDetectDominantLanguage :: BatchDetectDominantLanguage -> TestTree
requestBatchDetectDominantLanguage = req
    "BatchDetectDominantLanguage"
    "fixture/BatchDetectDominantLanguage.yaml"

requestStartDocumentClassificationJob :: StartDocumentClassificationJob -> TestTree
requestStartDocumentClassificationJob = req
    "StartDocumentClassificationJob"
    "fixture/StartDocumentClassificationJob.yaml"

requestDetectKeyPhrases :: DetectKeyPhrases -> TestTree
requestDetectKeyPhrases = req
    "DetectKeyPhrases"
    "fixture/DetectKeyPhrases.yaml"

requestDetectSyntax :: DetectSyntax -> TestTree
requestDetectSyntax = req
    "DetectSyntax"
    "fixture/DetectSyntax.yaml"

requestListSentimentDetectionJobs :: ListSentimentDetectionJobs -> TestTree
requestListSentimentDetectionJobs = req
    "ListSentimentDetectionJobs"
    "fixture/ListSentimentDetectionJobs.yaml"

requestDeleteDocumentClassifier :: DeleteDocumentClassifier -> TestTree
requestDeleteDocumentClassifier = req
    "DeleteDocumentClassifier"
    "fixture/DeleteDocumentClassifier.yaml"

requestListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobs -> TestTree
requestListDominantLanguageDetectionJobs = req
    "ListDominantLanguageDetectionJobs"
    "fixture/ListDominantLanguageDetectionJobs.yaml"

requestStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJob -> TestTree
requestStartKeyPhrasesDetectionJob = req
    "StartKeyPhrasesDetectionJob"
    "fixture/StartKeyPhrasesDetectionJob.yaml"

requestListDocumentClassifiers :: ListDocumentClassifiers -> TestTree
requestListDocumentClassifiers = req
    "ListDocumentClassifiers"
    "fixture/ListDocumentClassifiers.yaml"

-- Responses

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment = res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectSentiment)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer = res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy DeleteEntityRecognizer)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob = res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeKeyPhrasesDetectionJob)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs = res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListEntitiesDetectionJobs)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob = res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartSentimentDetectionJob)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax = res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectSyntax)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob = res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartTopicsDetectionJob)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases = res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob = res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeSentimentDetectionJob)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob = res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartEntitiesDetectionJob)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer = res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEntityRecognizer)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment = res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy DetectSentiment)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob = res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartDominantLanguageDetectionJob)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier = res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy StopTrainingDocumentClassifier)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob = res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDocumentClassificationJob)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities = res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectEntities)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer = res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy CreateEntityRecognizer)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob = res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopKeyPhrasesDetectionJob)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier = res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy CreateDocumentClassifier)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers = res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    comprehend
    (Proxy :: Proxy ListEntityRecognizers)

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob = res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopSentimentDetectionJob)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage = res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy DetectDominantLanguage)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob = res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeTopicsDetectionJob)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs = res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListDocumentClassificationJobs)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities = res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectEntities)

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier = res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDocumentClassifier)

responseDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJobResponse -> TestTree
responseDescribeDominantLanguageDetectionJob = res
    "DescribeDominantLanguageDetectionJobResponse"
    "fixture/DescribeDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDominantLanguageDetectionJob)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob = res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopEntitiesDetectionJob)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer = res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy StopTrainingEntityRecognizer)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs = res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListKeyPhrasesDetectionJobs)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob = res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEntitiesDetectionJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob = res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopDominantLanguageDetectionJob)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs = res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage = res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob = res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartDocumentClassificationJob)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases = res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectKeyPhrases)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax = res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    comprehend
    (Proxy :: Proxy DetectSyntax)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs = res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListSentimentDetectionJobs)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier = res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy DeleteDocumentClassifier)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs = res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListDominantLanguageDetectionJobs)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob = res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartKeyPhrasesDetectionJob)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers = res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    comprehend
    (Proxy :: Proxy ListDocumentClassifiers)
