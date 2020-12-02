{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Comprehend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestCreateEndpoint $
--             createEndpoint
--
--         , requestStopEventsDetectionJob $
--             stopEventsDetectionJob
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
--         , requestDescribeEventsDetectionJob $
--             describeEventsDetectionJob
--
--         , requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestUpdateEndpoint $
--             updateEndpoint
--
--         , requestListTagsForResource $
--             listTagsForResource
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
--         , requestStopPiiEntitiesDetectionJob $
--             stopPiiEntitiesDetectionJob
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
--         , requestListEventsDetectionJobs $
--             listEventsDetectionJobs
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
--         , requestListPiiEntitiesDetectionJobs $
--             listPiiEntitiesDetectionJobs
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
--         , requestClassifyDocument $
--             classifyDocument
--
--         , requestStartEventsDetectionJob $
--             startEventsDetectionJob
--
--         , requestDescribeTopicsDetectionJob $
--             describeTopicsDetectionJob
--
--         , requestListDocumentClassificationJobs $
--             listDocumentClassificationJobs
--
--         , requestDetectPiiEntities $
--             detectPiiEntities
--
--         , requestListEndpoints $
--             listEndpoints
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
--         , requestStartPiiEntitiesDetectionJob $
--             startPiiEntitiesDetectionJob
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
--         , requestTagResource $
--             tagResource
--
--         , requestDescribePiiEntitiesDetectionJob $
--             describePiiEntitiesDetectionJob
--
--         , requestListTopicsDetectionJobs $
--             listTopicsDetectionJobs
--
--         , requestUntagResource $
--             untagResource
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
--         , requestDescribeEndpoint $
--             describeEndpoint
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
--         , responseCreateEndpoint $
--             createEndpointResponse
--
--         , responseStopEventsDetectionJob $
--             stopEventsDetectionJobResponse
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
--         , responseDescribeEventsDetectionJob $
--             describeEventsDetectionJobResponse
--
--         , responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             updateEndpointResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
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
--         , responseStopPiiEntitiesDetectionJob $
--             stopPiiEntitiesDetectionJobResponse
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
--         , responseListEventsDetectionJobs $
--             listEventsDetectionJobsResponse
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
--         , responseListPiiEntitiesDetectionJobs $
--             listPiiEntitiesDetectionJobsResponse
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
--         , responseClassifyDocument $
--             classifyDocumentResponse
--
--         , responseStartEventsDetectionJob $
--             startEventsDetectionJobResponse
--
--         , responseDescribeTopicsDetectionJob $
--             describeTopicsDetectionJobResponse
--
--         , responseListDocumentClassificationJobs $
--             listDocumentClassificationJobsResponse
--
--         , responseDetectPiiEntities $
--             detectPiiEntitiesResponse
--
--         , responseListEndpoints $
--             listEndpointsResponse
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
--         , responseStartPiiEntitiesDetectionJob $
--             startPiiEntitiesDetectionJobResponse
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
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             describePiiEntitiesDetectionJobResponse
--
--         , responseListTopicsDetectionJobs $
--             listTopicsDetectionJobsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
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
--         , responseDescribeEndpoint $
--             describeEndpointResponse
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
requestBatchDetectSentiment =
  req
    "BatchDetectSentiment"
    "fixture/BatchDetectSentiment.yaml"

requestDeleteEntityRecognizer :: DeleteEntityRecognizer -> TestTree
requestDeleteEntityRecognizer =
  req
    "DeleteEntityRecognizer"
    "fixture/DeleteEntityRecognizer.yaml"

requestDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJob -> TestTree
requestDescribeKeyPhrasesDetectionJob =
  req
    "DescribeKeyPhrasesDetectionJob"
    "fixture/DescribeKeyPhrasesDetectionJob.yaml"

requestListEntitiesDetectionJobs :: ListEntitiesDetectionJobs -> TestTree
requestListEntitiesDetectionJobs =
  req
    "ListEntitiesDetectionJobs"
    "fixture/ListEntitiesDetectionJobs.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestStopEventsDetectionJob :: StopEventsDetectionJob -> TestTree
requestStopEventsDetectionJob =
  req
    "StopEventsDetectionJob"
    "fixture/StopEventsDetectionJob.yaml"

requestStartSentimentDetectionJob :: StartSentimentDetectionJob -> TestTree
requestStartSentimentDetectionJob =
  req
    "StartSentimentDetectionJob"
    "fixture/StartSentimentDetectionJob.yaml"

requestBatchDetectSyntax :: BatchDetectSyntax -> TestTree
requestBatchDetectSyntax =
  req
    "BatchDetectSyntax"
    "fixture/BatchDetectSyntax.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob =
  req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

requestDescribeEventsDetectionJob :: DescribeEventsDetectionJob -> TestTree
requestDescribeEventsDetectionJob =
  req
    "DescribeEventsDetectionJob"
    "fixture/DescribeEventsDetectionJob.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases =
  req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestDescribeSentimentDetectionJob :: DescribeSentimentDetectionJob -> TestTree
requestDescribeSentimentDetectionJob =
  req
    "DescribeSentimentDetectionJob"
    "fixture/DescribeSentimentDetectionJob.yaml"

requestStartEntitiesDetectionJob :: StartEntitiesDetectionJob -> TestTree
requestStartEntitiesDetectionJob =
  req
    "StartEntitiesDetectionJob"
    "fixture/StartEntitiesDetectionJob.yaml"

requestStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJob -> TestTree
requestStopPiiEntitiesDetectionJob =
  req
    "StopPiiEntitiesDetectionJob"
    "fixture/StopPiiEntitiesDetectionJob.yaml"

requestDescribeEntityRecognizer :: DescribeEntityRecognizer -> TestTree
requestDescribeEntityRecognizer =
  req
    "DescribeEntityRecognizer"
    "fixture/DescribeEntityRecognizer.yaml"

requestDetectSentiment :: DetectSentiment -> TestTree
requestDetectSentiment =
  req
    "DetectSentiment"
    "fixture/DetectSentiment.yaml"

requestStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJob -> TestTree
requestStartDominantLanguageDetectionJob =
  req
    "StartDominantLanguageDetectionJob"
    "fixture/StartDominantLanguageDetectionJob.yaml"

requestStopTrainingDocumentClassifier :: StopTrainingDocumentClassifier -> TestTree
requestStopTrainingDocumentClassifier =
  req
    "StopTrainingDocumentClassifier"
    "fixture/StopTrainingDocumentClassifier.yaml"

requestDescribeDocumentClassificationJob :: DescribeDocumentClassificationJob -> TestTree
requestDescribeDocumentClassificationJob =
  req
    "DescribeDocumentClassificationJob"
    "fixture/DescribeDocumentClassificationJob.yaml"

requestListEventsDetectionJobs :: ListEventsDetectionJobs -> TestTree
requestListEventsDetectionJobs =
  req
    "ListEventsDetectionJobs"
    "fixture/ListEventsDetectionJobs.yaml"

requestBatchDetectEntities :: BatchDetectEntities -> TestTree
requestBatchDetectEntities =
  req
    "BatchDetectEntities"
    "fixture/BatchDetectEntities.yaml"

requestCreateEntityRecognizer :: CreateEntityRecognizer -> TestTree
requestCreateEntityRecognizer =
  req
    "CreateEntityRecognizer"
    "fixture/CreateEntityRecognizer.yaml"

requestStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJob -> TestTree
requestStopKeyPhrasesDetectionJob =
  req
    "StopKeyPhrasesDetectionJob"
    "fixture/StopKeyPhrasesDetectionJob.yaml"

requestCreateDocumentClassifier :: CreateDocumentClassifier -> TestTree
requestCreateDocumentClassifier =
  req
    "CreateDocumentClassifier"
    "fixture/CreateDocumentClassifier.yaml"

requestListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobs -> TestTree
requestListPiiEntitiesDetectionJobs =
  req
    "ListPiiEntitiesDetectionJobs"
    "fixture/ListPiiEntitiesDetectionJobs.yaml"

requestListEntityRecognizers :: ListEntityRecognizers -> TestTree
requestListEntityRecognizers =
  req
    "ListEntityRecognizers"
    "fixture/ListEntityRecognizers.yaml"

requestStopSentimentDetectionJob :: StopSentimentDetectionJob -> TestTree
requestStopSentimentDetectionJob =
  req
    "StopSentimentDetectionJob"
    "fixture/StopSentimentDetectionJob.yaml"

requestDetectDominantLanguage :: DetectDominantLanguage -> TestTree
requestDetectDominantLanguage =
  req
    "DetectDominantLanguage"
    "fixture/DetectDominantLanguage.yaml"

requestClassifyDocument :: ClassifyDocument -> TestTree
requestClassifyDocument =
  req
    "ClassifyDocument"
    "fixture/ClassifyDocument.yaml"

requestStartEventsDetectionJob :: StartEventsDetectionJob -> TestTree
requestStartEventsDetectionJob =
  req
    "StartEventsDetectionJob"
    "fixture/StartEventsDetectionJob.yaml"

requestDescribeTopicsDetectionJob :: DescribeTopicsDetectionJob -> TestTree
requestDescribeTopicsDetectionJob =
  req
    "DescribeTopicsDetectionJob"
    "fixture/DescribeTopicsDetectionJob.yaml"

requestListDocumentClassificationJobs :: ListDocumentClassificationJobs -> TestTree
requestListDocumentClassificationJobs =
  req
    "ListDocumentClassificationJobs"
    "fixture/ListDocumentClassificationJobs.yaml"

requestDetectPiiEntities :: DetectPiiEntities -> TestTree
requestDetectPiiEntities =
  req
    "DetectPiiEntities"
    "fixture/DetectPiiEntities.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities =
  req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestDescribeDocumentClassifier :: DescribeDocumentClassifier -> TestTree
requestDescribeDocumentClassifier =
  req
    "DescribeDocumentClassifier"
    "fixture/DescribeDocumentClassifier.yaml"

requestDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJob -> TestTree
requestDescribeDominantLanguageDetectionJob =
  req
    "DescribeDominantLanguageDetectionJob"
    "fixture/DescribeDominantLanguageDetectionJob.yaml"

requestStopEntitiesDetectionJob :: StopEntitiesDetectionJob -> TestTree
requestStopEntitiesDetectionJob =
  req
    "StopEntitiesDetectionJob"
    "fixture/StopEntitiesDetectionJob.yaml"

requestStopTrainingEntityRecognizer :: StopTrainingEntityRecognizer -> TestTree
requestStopTrainingEntityRecognizer =
  req
    "StopTrainingEntityRecognizer"
    "fixture/StopTrainingEntityRecognizer.yaml"

requestStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJob -> TestTree
requestStartPiiEntitiesDetectionJob =
  req
    "StartPiiEntitiesDetectionJob"
    "fixture/StartPiiEntitiesDetectionJob.yaml"

requestListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobs -> TestTree
requestListKeyPhrasesDetectionJobs =
  req
    "ListKeyPhrasesDetectionJobs"
    "fixture/ListKeyPhrasesDetectionJobs.yaml"

requestDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJob -> TestTree
requestDescribeEntitiesDetectionJob =
  req
    "DescribeEntitiesDetectionJob"
    "fixture/DescribeEntitiesDetectionJob.yaml"

requestStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJob -> TestTree
requestStopDominantLanguageDetectionJob =
  req
    "StopDominantLanguageDetectionJob"
    "fixture/StopDominantLanguageDetectionJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJob -> TestTree
requestDescribePiiEntitiesDetectionJob =
  req
    "DescribePiiEntitiesDetectionJob"
    "fixture/DescribePiiEntitiesDetectionJob.yaml"

requestListTopicsDetectionJobs :: ListTopicsDetectionJobs -> TestTree
requestListTopicsDetectionJobs =
  req
    "ListTopicsDetectionJobs"
    "fixture/ListTopicsDetectionJobs.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestBatchDetectDominantLanguage :: BatchDetectDominantLanguage -> TestTree
requestBatchDetectDominantLanguage =
  req
    "BatchDetectDominantLanguage"
    "fixture/BatchDetectDominantLanguage.yaml"

requestStartDocumentClassificationJob :: StartDocumentClassificationJob -> TestTree
requestStartDocumentClassificationJob =
  req
    "StartDocumentClassificationJob"
    "fixture/StartDocumentClassificationJob.yaml"

requestDetectKeyPhrases :: DetectKeyPhrases -> TestTree
requestDetectKeyPhrases =
  req
    "DetectKeyPhrases"
    "fixture/DetectKeyPhrases.yaml"

requestDetectSyntax :: DetectSyntax -> TestTree
requestDetectSyntax =
  req
    "DetectSyntax"
    "fixture/DetectSyntax.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListSentimentDetectionJobs :: ListSentimentDetectionJobs -> TestTree
requestListSentimentDetectionJobs =
  req
    "ListSentimentDetectionJobs"
    "fixture/ListSentimentDetectionJobs.yaml"

requestDeleteDocumentClassifier :: DeleteDocumentClassifier -> TestTree
requestDeleteDocumentClassifier =
  req
    "DeleteDocumentClassifier"
    "fixture/DeleteDocumentClassifier.yaml"

requestListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobs -> TestTree
requestListDominantLanguageDetectionJobs =
  req
    "ListDominantLanguageDetectionJobs"
    "fixture/ListDominantLanguageDetectionJobs.yaml"

requestStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJob -> TestTree
requestStartKeyPhrasesDetectionJob =
  req
    "StartKeyPhrasesDetectionJob"
    "fixture/StartKeyPhrasesDetectionJob.yaml"

requestListDocumentClassifiers :: ListDocumentClassifiers -> TestTree
requestListDocumentClassifiers =
  req
    "ListDocumentClassifiers"
    "fixture/ListDocumentClassifiers.yaml"

-- Responses

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment =
  res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectSentiment)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy DeleteEntityRecognizer)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeKeyPhrasesDetectionJob)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs =
  res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListEntitiesDetectionJobs)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    comprehend
    (Proxy :: Proxy CreateEndpoint)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopEventsDetectionJob)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartSentimentDetectionJob)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectSyntax)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartTopicsDetectionJob)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEventsDetectionJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    comprehend
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    comprehend
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    comprehend
    (Proxy :: Proxy ListTagsForResource)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeSentimentDetectionJob)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob =
  res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartEntitiesDetectionJob)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopPiiEntitiesDetectionJob)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEntityRecognizer)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy DetectSentiment)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartDominantLanguageDetectionJob)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy StopTrainingDocumentClassifier)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob =
  res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDocumentClassificationJob)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListEventsDetectionJobs)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectEntities)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer =
  res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy CreateEntityRecognizer)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob =
  res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopKeyPhrasesDetectionJob)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier =
  res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy CreateDocumentClassifier)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListPiiEntitiesDetectionJobs)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    comprehend
    (Proxy :: Proxy ListEntityRecognizers)

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob =
  res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopSentimentDetectionJob)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage =
  res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy DetectDominantLanguage)

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    comprehend
    (Proxy :: Proxy ClassifyDocument)

responseStartEventsDetectionJob :: StartEventsDetectionJobResponse -> TestTree
responseStartEventsDetectionJob =
  res
    "StartEventsDetectionJobResponse"
    "fixture/StartEventsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartEventsDetectionJob)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob =
  res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeTopicsDetectionJob)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs =
  res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListDocumentClassificationJobs)

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectPiiEntities)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    comprehend
    (Proxy :: Proxy ListEndpoints)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectEntities)

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier =
  res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDocumentClassifier)

responseDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJobResponse -> TestTree
responseDescribeDominantLanguageDetectionJob =
  res
    "DescribeDominantLanguageDetectionJobResponse"
    "fixture/DescribeDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeDominantLanguageDetectionJob)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob =
  res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopEntitiesDetectionJob)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer =
  res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    comprehend
    (Proxy :: Proxy StopTrainingEntityRecognizer)

responseStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJobResponse -> TestTree
responseStartPiiEntitiesDetectionJob =
  res
    "StartPiiEntitiesDetectionJobResponse"
    "fixture/StartPiiEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartPiiEntitiesDetectionJob)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListKeyPhrasesDetectionJobs)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEntitiesDetectionJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StopDominantLanguageDetectionJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    comprehend
    (Proxy :: Proxy TagResource)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribePiiEntitiesDetectionJob)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    comprehend
    (Proxy :: Proxy UntagResource)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob =
  res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartDocumentClassificationJob)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases =
  res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectKeyPhrases)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    comprehend
    (Proxy :: Proxy DetectSyntax)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeEndpoint)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListSentimentDetectionJobs)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier =
  res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    comprehend
    (Proxy :: Proxy DeleteDocumentClassifier)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListDominantLanguageDetectionJobs)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob =
  res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartKeyPhrasesDetectionJob)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers =
  res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    comprehend
    (Proxy :: Proxy ListDocumentClassifiers)
