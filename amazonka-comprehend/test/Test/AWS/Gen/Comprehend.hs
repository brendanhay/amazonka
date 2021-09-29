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
--         [ requestStartSentimentDetectionJob $
--             newStartSentimentDetectionJob
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestStopEventsDetectionJob $
--             newStopEventsDetectionJob
--
--         , requestStopSentimentDetectionJob $
--             newStopSentimentDetectionJob
--
--         , requestStartEventsDetectionJob $
--             newStartEventsDetectionJob
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJob
--
--         , requestListEntityRecognizers $
--             newListEntityRecognizers
--
--         , requestDeleteEntityRecognizer $
--             newDeleteEntityRecognizer
--
--         , requestBatchDetectSentiment $
--             newBatchDetectSentiment
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
--         , requestListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobs
--
--         , requestStartDocumentClassificationJob $
--             newStartDocumentClassificationJob
--
--         , requestDetectKeyPhrases $
--             newDetectKeyPhrases
--
--         , requestListSentimentDetectionJobs $
--             newListSentimentDetectionJobs
--
--         , requestListEventsDetectionJobs $
--             newListEventsDetectionJobs
--
--         , requestBatchDetectEntities $
--             newBatchDetectEntities
--
--         , requestDetectSyntax $
--             newDetectSyntax
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestContainsPiiEntities $
--             newContainsPiiEntities
--
--         , requestBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguage
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListTopicsDetectionJobs $
--             newListTopicsDetectionJobs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifier
--
--         , requestListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobs
--
--         , requestDescribeEntityRecognizer $
--             newDescribeEntityRecognizer
--
--         , requestListDocumentClassifierSummaries $
--             newListDocumentClassifierSummaries
--
--         , requestDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJob
--
--         , requestDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJob
--
--         , requestStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJob
--
--         , requestDescribeDocumentClassifier $
--             newDescribeDocumentClassifier
--
--         , requestStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJob
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
--         , requestDetectPiiEntities $
--             newDetectPiiEntities
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJob
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestListEndpoints $
--             newListEndpoints
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
--         , requestClassifyDocument $
--             newClassifyDocument
--
--         , requestListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobs
--
--         , requestListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobs
--
--         , requestCreateDocumentClassifier $
--             newCreateDocumentClassifier
--
--         , requestDeleteDocumentClassifier $
--             newDeleteDocumentClassifier
--
--         , requestDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJob
--
--         , requestDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJob
--
--         , requestStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJob
--
--         , requestDetectSentiment $
--             newDetectSentiment
--
--         , requestStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJob
--
--         , requestListEntityRecognizerSummaries $
--             newListEntityRecognizerSummaries
--
--         , requestBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrases
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestBatchDetectSyntax $
--             newBatchDetectSyntax
--
--         , requestDetectEntities $
--             newDetectEntities
--
--         , requestStartTopicsDetectionJob $
--             newStartTopicsDetectionJob
--
--         , requestDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJob
--
--           ]

--     , testGroup "response"
--         [ responseStartSentimentDetectionJob $
--             newStartSentimentDetectionJobResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseStopEventsDetectionJob $
--             newStopEventsDetectionJobResponse
--
--         , responseStopSentimentDetectionJob $
--             newStopSentimentDetectionJobResponse
--
--         , responseStartEventsDetectionJob $
--             newStartEventsDetectionJobResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJobResponse
--
--         , responseListEntityRecognizers $
--             newListEntityRecognizersResponse
--
--         , responseDeleteEntityRecognizer $
--             newDeleteEntityRecognizerResponse
--
--         , responseBatchDetectSentiment $
--             newBatchDetectSentimentResponse
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
--         , responseListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobsResponse
--
--         , responseStartDocumentClassificationJob $
--             newStartDocumentClassificationJobResponse
--
--         , responseDetectKeyPhrases $
--             newDetectKeyPhrasesResponse
--
--         , responseListSentimentDetectionJobs $
--             newListSentimentDetectionJobsResponse
--
--         , responseListEventsDetectionJobs $
--             newListEventsDetectionJobsResponse
--
--         , responseBatchDetectEntities $
--             newBatchDetectEntitiesResponse
--
--         , responseDetectSyntax $
--             newDetectSyntaxResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseContainsPiiEntities $
--             newContainsPiiEntitiesResponse
--
--         , responseBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguageResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListTopicsDetectionJobs $
--             newListTopicsDetectionJobsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifierResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobsResponse
--
--         , responseDescribeEntityRecognizer $
--             newDescribeEntityRecognizerResponse
--
--         , responseListDocumentClassifierSummaries $
--             newListDocumentClassifierSummariesResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJobResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJobResponse
--
--         , responseStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJobResponse
--
--         , responseDescribeDocumentClassifier $
--             newDescribeDocumentClassifierResponse
--
--         , responseStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJobResponse
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
--         , responseDetectPiiEntities $
--             newDetectPiiEntitiesResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJobResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
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
--         , responseClassifyDocument $
--             newClassifyDocumentResponse
--
--         , responseListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobsResponse
--
--         , responseListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobsResponse
--
--         , responseCreateDocumentClassifier $
--             newCreateDocumentClassifierResponse
--
--         , responseDeleteDocumentClassifier $
--             newDeleteDocumentClassifierResponse
--
--         , responseDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJobResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJobResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJobResponse
--
--         , responseDetectSentiment $
--             newDetectSentimentResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJobResponse
--
--         , responseListEntityRecognizerSummaries $
--             newListEntityRecognizerSummariesResponse
--
--         , responseBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrasesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseBatchDetectSyntax $
--             newBatchDetectSyntaxResponse
--
--         , responseDetectEntities $
--             newDetectEntitiesResponse
--
--         , responseStartTopicsDetectionJob $
--             newStartTopicsDetectionJobResponse
--
--         , responseDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJobResponse
--
--           ]
--     ]

-- Requests

requestStartSentimentDetectionJob :: StartSentimentDetectionJob -> TestTree
requestStartSentimentDetectionJob =
  req
    "StartSentimentDetectionJob"
    "fixture/StartSentimentDetectionJob.yaml"

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

requestDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJob -> TestTree
requestDescribeKeyPhrasesDetectionJob =
  req
    "DescribeKeyPhrasesDetectionJob"
    "fixture/DescribeKeyPhrasesDetectionJob.yaml"

requestListEntityRecognizers :: ListEntityRecognizers -> TestTree
requestListEntityRecognizers =
  req
    "ListEntityRecognizers"
    "fixture/ListEntityRecognizers.yaml"

requestDeleteEntityRecognizer :: DeleteEntityRecognizer -> TestTree
requestDeleteEntityRecognizer =
  req
    "DeleteEntityRecognizer"
    "fixture/DeleteEntityRecognizer.yaml"

requestBatchDetectSentiment :: BatchDetectSentiment -> TestTree
requestBatchDetectSentiment =
  req
    "BatchDetectSentiment"
    "fixture/BatchDetectSentiment.yaml"

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

requestListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobs -> TestTree
requestListDominantLanguageDetectionJobs =
  req
    "ListDominantLanguageDetectionJobs"
    "fixture/ListDominantLanguageDetectionJobs.yaml"

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

requestListSentimentDetectionJobs :: ListSentimentDetectionJobs -> TestTree
requestListSentimentDetectionJobs =
  req
    "ListSentimentDetectionJobs"
    "fixture/ListSentimentDetectionJobs.yaml"

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

requestContainsPiiEntities :: ContainsPiiEntities -> TestTree
requestContainsPiiEntities =
  req
    "ContainsPiiEntities"
    "fixture/ContainsPiiEntities.yaml"

requestBatchDetectDominantLanguage :: BatchDetectDominantLanguage -> TestTree
requestBatchDetectDominantLanguage =
  req
    "BatchDetectDominantLanguage"
    "fixture/BatchDetectDominantLanguage.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListTopicsDetectionJobs :: ListTopicsDetectionJobs -> TestTree
requestListTopicsDetectionJobs =
  req
    "ListTopicsDetectionJobs"
    "fixture/ListTopicsDetectionJobs.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStopTrainingDocumentClassifier :: StopTrainingDocumentClassifier -> TestTree
requestStopTrainingDocumentClassifier =
  req
    "StopTrainingDocumentClassifier"
    "fixture/StopTrainingDocumentClassifier.yaml"

requestListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobs -> TestTree
requestListKeyPhrasesDetectionJobs =
  req
    "ListKeyPhrasesDetectionJobs"
    "fixture/ListKeyPhrasesDetectionJobs.yaml"

requestDescribeEntityRecognizer :: DescribeEntityRecognizer -> TestTree
requestDescribeEntityRecognizer =
  req
    "DescribeEntityRecognizer"
    "fixture/DescribeEntityRecognizer.yaml"

requestListDocumentClassifierSummaries :: ListDocumentClassifierSummaries -> TestTree
requestListDocumentClassifierSummaries =
  req
    "ListDocumentClassifierSummaries"
    "fixture/ListDocumentClassifierSummaries.yaml"

requestDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJob -> TestTree
requestDescribePiiEntitiesDetectionJob =
  req
    "DescribePiiEntitiesDetectionJob"
    "fixture/DescribePiiEntitiesDetectionJob.yaml"

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

requestDescribeDocumentClassifier :: DescribeDocumentClassifier -> TestTree
requestDescribeDocumentClassifier =
  req
    "DescribeDocumentClassifier"
    "fixture/DescribeDocumentClassifier.yaml"

requestStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJob -> TestTree
requestStopPiiEntitiesDetectionJob =
  req
    "StopPiiEntitiesDetectionJob"
    "fixture/StopPiiEntitiesDetectionJob.yaml"

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

requestDetectPiiEntities :: DetectPiiEntities -> TestTree
requestDetectPiiEntities =
  req
    "DetectPiiEntities"
    "fixture/DetectPiiEntities.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDescribeEventsDetectionJob :: DescribeEventsDetectionJob -> TestTree
requestDescribeEventsDetectionJob =
  req
    "DescribeEventsDetectionJob"
    "fixture/DescribeEventsDetectionJob.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

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

requestClassifyDocument :: ClassifyDocument -> TestTree
requestClassifyDocument =
  req
    "ClassifyDocument"
    "fixture/ClassifyDocument.yaml"

requestListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobs -> TestTree
requestListPiiEntitiesDetectionJobs =
  req
    "ListPiiEntitiesDetectionJobs"
    "fixture/ListPiiEntitiesDetectionJobs.yaml"

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

requestListEntityRecognizerSummaries :: ListEntityRecognizerSummaries -> TestTree
requestListEntityRecognizerSummaries =
  req
    "ListEntityRecognizerSummaries"
    "fixture/ListEntityRecognizerSummaries.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases =
  req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestBatchDetectSyntax :: BatchDetectSyntax -> TestTree
requestBatchDetectSyntax =
  req
    "BatchDetectSyntax"
    "fixture/BatchDetectSyntax.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities =
  req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob =
  req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

requestDescribeSentimentDetectionJob :: DescribeSentimentDetectionJob -> TestTree
requestDescribeSentimentDetectionJob =
  req
    "DescribeSentimentDetectionJob"
    "fixture/DescribeSentimentDetectionJob.yaml"

-- Responses

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartSentimentDetectionJob)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpoint)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopEventsDetectionJob)

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

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKeyPhrasesDetectionJob)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntityRecognizers)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEntityRecognizer)

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment =
  res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectSentiment)

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

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDominantLanguageDetectionJobs)

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

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSentimentDetectionJobs)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventsDetectionJobs)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectEntities)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    defaultService
    (Proxy :: Proxy DetectSyntax)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoint)

responseContainsPiiEntities :: ContainsPiiEntitiesResponse -> TestTree
responseContainsPiiEntities =
  res
    "ContainsPiiEntitiesResponse"
    "fixture/ContainsPiiEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ContainsPiiEntities)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrainingDocumentClassifier)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListKeyPhrasesDetectionJobs)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEntityRecognizer)

responseListDocumentClassifierSummaries :: ListDocumentClassifierSummariesResponse -> TestTree
responseListDocumentClassifierSummaries =
  res
    "ListDocumentClassifierSummariesResponse"
    "fixture/ListDocumentClassifierSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentClassifierSummaries)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePiiEntitiesDetectionJob)

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

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier =
  res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentClassifier)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopPiiEntitiesDetectionJob)

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

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectPiiEntities)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventsDetectionJob)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpoints)

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

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy ClassifyDocument)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPiiEntitiesDetectionJobs)

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

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEntitiesDetectionJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopDominantLanguageDetectionJob)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    defaultService
    (Proxy :: Proxy DetectSentiment)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartDominantLanguageDetectionJob)

responseListEntityRecognizerSummaries :: ListEntityRecognizerSummariesResponse -> TestTree
responseListEntityRecognizerSummaries =
  res
    "ListEntityRecognizerSummariesResponse"
    "fixture/ListEntityRecognizerSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntityRecognizerSummaries)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDetectSyntax)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectEntities)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartTopicsDetectionJob)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSentimentDetectionJob)
