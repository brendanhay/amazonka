{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Comprehend
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestStopSentimentDetectionJob $
--             newStopSentimentDetectionJob
--
--         , requestStartEventsDetectionJob $
--             newStartEventsDetectionJob
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestStartSentimentDetectionJob $
--             newStartSentimentDetectionJob
--
--         , requestStopEventsDetectionJob $
--             newStopEventsDetectionJob
--
--         , requestListEntityRecognizers $
--             newListEntityRecognizers
--
--         , requestBatchDetectSentiment $
--             newBatchDetectSentiment
--
--         , requestDeleteEntityRecognizer $
--             newDeleteEntityRecognizer
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJob
--
--         , requestListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobs
--
--         , requestStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJob
--
--         , requestListDocumentClassifiers $
--             newListDocumentClassifiers
--
--         , requestCreateEntityRecognizer $
--             newCreateEntityRecognizer
--
--         , requestStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJob
--
--         , requestListEventsDetectionJobs $
--             newListEventsDetectionJobs
--
--         , requestListSentimentDetectionJobs $
--             newListSentimentDetectionJobs
--
--         , requestDetectSyntax $
--             newDetectSyntax
--
--         , requestStartDocumentClassificationJob $
--             newStartDocumentClassificationJob
--
--         , requestDetectKeyPhrases $
--             newDetectKeyPhrases
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestBatchDetectEntities $
--             newBatchDetectEntities
--
--         , requestListTopicsDetectionJobs $
--             newListTopicsDetectionJobs
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguage
--
--         , requestStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifier
--
--         , requestDescribeEntityRecognizer $
--             newDescribeEntityRecognizer
--
--         , requestDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobs
--
--         , requestDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJob
--
--         , requestStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJob
--
--         , requestStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJob
--
--         , requestDescribeDocumentClassifier $
--             newDescribeDocumentClassifier
--
--         , requestStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizer
--
--         , requestStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJob
--
--         , requestStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJob
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJob
--
--         , requestDetectPiiEntities $
--             newDetectPiiEntities
--
--         , requestClassifyDocument $
--             newClassifyDocument
--
--         , requestDetectDominantLanguage $
--             newDetectDominantLanguage
--
--         , requestListDocumentClassificationJobs $
--             newListDocumentClassificationJobs
--
--         , requestDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJob
--
--         , requestListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobs
--
--         , requestCreateDocumentClassifier $
--             newCreateDocumentClassifier
--
--         , requestListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobs
--
--         , requestDeleteDocumentClassifier $
--             newDeleteDocumentClassifier
--
--         , requestDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJob
--
--         , requestStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJob
--
--         , requestDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJob
--
--         , requestStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJob
--
--         , requestDetectSentiment $
--             newDetectSentiment
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDetectEntities $
--             newDetectEntities
--
--         , requestDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJob
--
--         , requestBatchDetectSyntax $
--             newBatchDetectSyntax
--
--         , requestBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrases
--
--         , requestStartTopicsDetectionJob $
--             newStartTopicsDetectionJob
--
--           ]

--     , testGroup "response"
--         [ responseStopSentimentDetectionJob $
--             newStopSentimentDetectionJobResponse
--
--         , responseStartEventsDetectionJob $
--             newStartEventsDetectionJobResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseStartSentimentDetectionJob $
--             newStartSentimentDetectionJobResponse
--
--         , responseStopEventsDetectionJob $
--             newStopEventsDetectionJobResponse
--
--         , responseListEntityRecognizers $
--             newListEntityRecognizersResponse
--
--         , responseBatchDetectSentiment $
--             newBatchDetectSentimentResponse
--
--         , responseDeleteEntityRecognizer $
--             newDeleteEntityRecognizerResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJobResponse
--
--         , responseListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobsResponse
--
--         , responseStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJobResponse
--
--         , responseListDocumentClassifiers $
--             newListDocumentClassifiersResponse
--
--         , responseCreateEntityRecognizer $
--             newCreateEntityRecognizerResponse
--
--         , responseStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJobResponse
--
--         , responseListEventsDetectionJobs $
--             newListEventsDetectionJobsResponse
--
--         , responseListSentimentDetectionJobs $
--             newListSentimentDetectionJobsResponse
--
--         , responseDetectSyntax $
--             newDetectSyntaxResponse
--
--         , responseStartDocumentClassificationJob $
--             newStartDocumentClassificationJobResponse
--
--         , responseDetectKeyPhrases $
--             newDetectKeyPhrasesResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseBatchDetectEntities $
--             newBatchDetectEntitiesResponse
--
--         , responseListTopicsDetectionJobs $
--             newListTopicsDetectionJobsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguageResponse
--
--         , responseStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifierResponse
--
--         , responseDescribeEntityRecognizer $
--             newDescribeEntityRecognizerResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobsResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJobResponse
--
--         , responseStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJobResponse
--
--         , responseStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJobResponse
--
--         , responseDescribeDocumentClassifier $
--             newDescribeDocumentClassifierResponse
--
--         , responseStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizerResponse
--
--         , responseStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJobResponse
--
--         , responseStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJobResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJobResponse
--
--         , responseDetectPiiEntities $
--             newDetectPiiEntitiesResponse
--
--         , responseClassifyDocument $
--             newClassifyDocumentResponse
--
--         , responseDetectDominantLanguage $
--             newDetectDominantLanguageResponse
--
--         , responseListDocumentClassificationJobs $
--             newListDocumentClassificationJobsResponse
--
--         , responseDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJobResponse
--
--         , responseListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobsResponse
--
--         , responseCreateDocumentClassifier $
--             newCreateDocumentClassifierResponse
--
--         , responseListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobsResponse
--
--         , responseDeleteDocumentClassifier $
--             newDeleteDocumentClassifierResponse
--
--         , responseDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJobResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJobResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJobResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJobResponse
--
--         , responseDetectSentiment $
--             newDetectSentimentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDetectEntities $
--             newDetectEntitiesResponse
--
--         , responseDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJobResponse
--
--         , responseBatchDetectSyntax $
--             newBatchDetectSyntaxResponse
--
--         , responseBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrasesResponse
--
--         , responseStartTopicsDetectionJob $
--             newStartTopicsDetectionJobResponse
--
--           ]
--     ]

-- Requests

requestStopSentimentDetectionJob :: StopSentimentDetectionJob -> TestTree
requestStopSentimentDetectionJob =
  req
    "StopSentimentDetectionJob"
    "fixture/StopSentimentDetectionJob.yaml"

requestStartEventsDetectionJob :: StartEventsDetectionJob -> TestTree
requestStartEventsDetectionJob =
  req
    "StartEventsDetectionJob"
    "fixture/StartEventsDetectionJob.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestStartSentimentDetectionJob :: StartSentimentDetectionJob -> TestTree
requestStartSentimentDetectionJob =
  req
    "StartSentimentDetectionJob"
    "fixture/StartSentimentDetectionJob.yaml"

requestStopEventsDetectionJob :: StopEventsDetectionJob -> TestTree
requestStopEventsDetectionJob =
  req
    "StopEventsDetectionJob"
    "fixture/StopEventsDetectionJob.yaml"

requestListEntityRecognizers :: ListEntityRecognizers -> TestTree
requestListEntityRecognizers =
  req
    "ListEntityRecognizers"
    "fixture/ListEntityRecognizers.yaml"

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

requestListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobs -> TestTree
requestListDominantLanguageDetectionJobs =
  req
    "ListDominantLanguageDetectionJobs"
    "fixture/ListDominantLanguageDetectionJobs.yaml"

requestStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJob -> TestTree
requestStopKeyPhrasesDetectionJob =
  req
    "StopKeyPhrasesDetectionJob"
    "fixture/StopKeyPhrasesDetectionJob.yaml"

requestListDocumentClassifiers :: ListDocumentClassifiers -> TestTree
requestListDocumentClassifiers =
  req
    "ListDocumentClassifiers"
    "fixture/ListDocumentClassifiers.yaml"

requestCreateEntityRecognizer :: CreateEntityRecognizer -> TestTree
requestCreateEntityRecognizer =
  req
    "CreateEntityRecognizer"
    "fixture/CreateEntityRecognizer.yaml"

requestStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJob -> TestTree
requestStartKeyPhrasesDetectionJob =
  req
    "StartKeyPhrasesDetectionJob"
    "fixture/StartKeyPhrasesDetectionJob.yaml"

requestListEventsDetectionJobs :: ListEventsDetectionJobs -> TestTree
requestListEventsDetectionJobs =
  req
    "ListEventsDetectionJobs"
    "fixture/ListEventsDetectionJobs.yaml"

requestListSentimentDetectionJobs :: ListSentimentDetectionJobs -> TestTree
requestListSentimentDetectionJobs =
  req
    "ListSentimentDetectionJobs"
    "fixture/ListSentimentDetectionJobs.yaml"

requestDetectSyntax :: DetectSyntax -> TestTree
requestDetectSyntax =
  req
    "DetectSyntax"
    "fixture/DetectSyntax.yaml"

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

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestBatchDetectEntities :: BatchDetectEntities -> TestTree
requestBatchDetectEntities =
  req
    "BatchDetectEntities"
    "fixture/BatchDetectEntities.yaml"

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

requestStopTrainingDocumentClassifier :: StopTrainingDocumentClassifier -> TestTree
requestStopTrainingDocumentClassifier =
  req
    "StopTrainingDocumentClassifier"
    "fixture/StopTrainingDocumentClassifier.yaml"

requestDescribeEntityRecognizer :: DescribeEntityRecognizer -> TestTree
requestDescribeEntityRecognizer =
  req
    "DescribeEntityRecognizer"
    "fixture/DescribeEntityRecognizer.yaml"

requestDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJob -> TestTree
requestDescribePiiEntitiesDetectionJob =
  req
    "DescribePiiEntitiesDetectionJob"
    "fixture/DescribePiiEntitiesDetectionJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobs -> TestTree
requestListKeyPhrasesDetectionJobs =
  req
    "ListKeyPhrasesDetectionJobs"
    "fixture/ListKeyPhrasesDetectionJobs.yaml"

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

requestStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJob -> TestTree
requestStopPiiEntitiesDetectionJob =
  req
    "StopPiiEntitiesDetectionJob"
    "fixture/StopPiiEntitiesDetectionJob.yaml"

requestDescribeDocumentClassifier :: DescribeDocumentClassifier -> TestTree
requestDescribeDocumentClassifier =
  req
    "DescribeDocumentClassifier"
    "fixture/DescribeDocumentClassifier.yaml"

requestStopTrainingEntityRecognizer :: StopTrainingEntityRecognizer -> TestTree
requestStopTrainingEntityRecognizer =
  req
    "StopTrainingEntityRecognizer"
    "fixture/StopTrainingEntityRecognizer.yaml"

requestStartEntitiesDetectionJob :: StartEntitiesDetectionJob -> TestTree
requestStartEntitiesDetectionJob =
  req
    "StartEntitiesDetectionJob"
    "fixture/StartEntitiesDetectionJob.yaml"

requestStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJob -> TestTree
requestStartPiiEntitiesDetectionJob =
  req
    "StartPiiEntitiesDetectionJob"
    "fixture/StartPiiEntitiesDetectionJob.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

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

requestDescribeEventsDetectionJob :: DescribeEventsDetectionJob -> TestTree
requestDescribeEventsDetectionJob =
  req
    "DescribeEventsDetectionJob"
    "fixture/DescribeEventsDetectionJob.yaml"

requestDetectPiiEntities :: DetectPiiEntities -> TestTree
requestDetectPiiEntities =
  req
    "DetectPiiEntities"
    "fixture/DetectPiiEntities.yaml"

requestClassifyDocument :: ClassifyDocument -> TestTree
requestClassifyDocument =
  req
    "ClassifyDocument"
    "fixture/ClassifyDocument.yaml"

requestDetectDominantLanguage :: DetectDominantLanguage -> TestTree
requestDetectDominantLanguage =
  req
    "DetectDominantLanguage"
    "fixture/DetectDominantLanguage.yaml"

requestListDocumentClassificationJobs :: ListDocumentClassificationJobs -> TestTree
requestListDocumentClassificationJobs =
  req
    "ListDocumentClassificationJobs"
    "fixture/ListDocumentClassificationJobs.yaml"

requestDescribeTopicsDetectionJob :: DescribeTopicsDetectionJob -> TestTree
requestDescribeTopicsDetectionJob =
  req
    "DescribeTopicsDetectionJob"
    "fixture/DescribeTopicsDetectionJob.yaml"

requestListEntitiesDetectionJobs :: ListEntitiesDetectionJobs -> TestTree
requestListEntitiesDetectionJobs =
  req
    "ListEntitiesDetectionJobs"
    "fixture/ListEntitiesDetectionJobs.yaml"

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

requestDeleteDocumentClassifier :: DeleteDocumentClassifier -> TestTree
requestDeleteDocumentClassifier =
  req
    "DeleteDocumentClassifier"
    "fixture/DeleteDocumentClassifier.yaml"

requestDescribeDocumentClassificationJob :: DescribeDocumentClassificationJob -> TestTree
requestDescribeDocumentClassificationJob =
  req
    "DescribeDocumentClassificationJob"
    "fixture/DescribeDocumentClassificationJob.yaml"

requestStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJob -> TestTree
requestStopDominantLanguageDetectionJob =
  req
    "StopDominantLanguageDetectionJob"
    "fixture/StopDominantLanguageDetectionJob.yaml"

requestDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJob -> TestTree
requestDescribeEntitiesDetectionJob =
  req
    "DescribeEntitiesDetectionJob"
    "fixture/DescribeEntitiesDetectionJob.yaml"

requestStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJob -> TestTree
requestStartDominantLanguageDetectionJob =
  req
    "StartDominantLanguageDetectionJob"
    "fixture/StartDominantLanguageDetectionJob.yaml"

requestDetectSentiment :: DetectSentiment -> TestTree
requestDetectSentiment =
  req
    "DetectSentiment"
    "fixture/DetectSentiment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities =
  req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestDescribeSentimentDetectionJob :: DescribeSentimentDetectionJob -> TestTree
requestDescribeSentimentDetectionJob =
  req
    "DescribeSentimentDetectionJob"
    "fixture/DescribeSentimentDetectionJob.yaml"

requestBatchDetectSyntax :: BatchDetectSyntax -> TestTree
requestBatchDetectSyntax =
  req
    "BatchDetectSyntax"
    "fixture/BatchDetectSyntax.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases =
  req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob =
  req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

-- Responses

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob =
  res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopSentimentDetectionJob)

responseStartEventsDetectionJob :: StartEventsDetectionJobResponse -> TestTree
responseStartEventsDetectionJob =
  res
    "StartEventsDetectionJobResponse"
    "fixture/StartEventsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartEventsDetectionJob)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpoint)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartSentimentDetectionJob)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopEventsDetectionJob)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntityRecognizers)

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment =
  res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectSentiment)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEntityRecognizer)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKeyPhrasesDetectionJob)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDominantLanguageDetectionJobs)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob =
  res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopKeyPhrasesDetectionJob)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers =
  res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentClassifiers)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer =
  res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEntityRecognizer)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob =
  res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartKeyPhrasesDetectionJob)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventsDetectionJobs)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSentimentDetectionJobs)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    defaultService
    (Proxy :: Proxy DetectSyntax)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob =
  res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartDocumentClassificationJob)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases =
  res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectKeyPhrases)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoint)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectEntities)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrainingDocumentClassifier)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEntityRecognizer)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePiiEntitiesDetectionJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListKeyPhrasesDetectionJobs)

responseDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJobResponse -> TestTree
responseDescribeDominantLanguageDetectionJob =
  res
    "DescribeDominantLanguageDetectionJobResponse"
    "fixture/DescribeDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDominantLanguageDetectionJob)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob =
  res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopEntitiesDetectionJob)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopPiiEntitiesDetectionJob)

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier =
  res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentClassifier)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer =
  res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrainingEntityRecognizer)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob =
  res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartEntitiesDetectionJob)

responseStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJobResponse -> TestTree
responseStartPiiEntitiesDetectionJob =
  res
    "StartPiiEntitiesDetectionJobResponse"
    "fixture/StartPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartPiiEntitiesDetectionJob)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpoints)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventsDetectionJob)

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectPiiEntities)

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy ClassifyDocument)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage =
  res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    defaultService
    (Proxy :: Proxy DetectDominantLanguage)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs =
  res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentClassificationJobs)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob =
  res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTopicsDetectionJob)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs =
  res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntitiesDetectionJobs)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier =
  res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocumentClassifier)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPiiEntitiesDetectionJobs)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier =
  res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocumentClassifier)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob =
  res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentClassificationJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopDominantLanguageDetectionJob)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEntitiesDetectionJob)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartDominantLanguageDetectionJob)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    defaultService
    (Proxy :: Proxy DetectSentiment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectEntities)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSentimentDetectionJob)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectSyntax)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartTopicsDetectionJob)
