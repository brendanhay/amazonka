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

import Amazonka.Comprehend
import qualified Data.Proxy as Proxy
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
--             newBatchDetectSentiment
--
--         , requestDeleteEntityRecognizer $
--             newDeleteEntityRecognizer
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJob
--
--         , requestListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobs
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestStopEventsDetectionJob $
--             newStopEventsDetectionJob
--
--         , requestStartSentimentDetectionJob $
--             newStartSentimentDetectionJob
--
--         , requestBatchDetectSyntax $
--             newBatchDetectSyntax
--
--         , requestStartTopicsDetectionJob $
--             newStartTopicsDetectionJob
--
--         , requestDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJob
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrases
--
--         , requestDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJob
--
--         , requestStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJob
--
--         , requestStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJob
--
--         , requestDescribeEntityRecognizer $
--             newDescribeEntityRecognizer
--
--         , requestDetectSentiment $
--             newDetectSentiment
--
--         , requestStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJob
--
--         , requestStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifier
--
--         , requestDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJob
--
--         , requestContainsPiiEntities $
--             newContainsPiiEntities
--
--         , requestListEventsDetectionJobs $
--             newListEventsDetectionJobs
--
--         , requestBatchDetectEntities $
--             newBatchDetectEntities
--
--         , requestCreateEntityRecognizer $
--             newCreateEntityRecognizer
--
--         , requestStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJob
--
--         , requestCreateDocumentClassifier $
--             newCreateDocumentClassifier
--
--         , requestListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobs
--
--         , requestListEntityRecognizers $
--             newListEntityRecognizers
--
--         , requestStopSentimentDetectionJob $
--             newStopSentimentDetectionJob
--
--         , requestDetectDominantLanguage $
--             newDetectDominantLanguage
--
--         , requestClassifyDocument $
--             newClassifyDocument
--
--         , requestStartEventsDetectionJob $
--             newStartEventsDetectionJob
--
--         , requestDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJob
--
--         , requestListDocumentClassificationJobs $
--             newListDocumentClassificationJobs
--
--         , requestDetectPiiEntities $
--             newDetectPiiEntities
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestDetectEntities $
--             newDetectEntities
--
--         , requestDescribeDocumentClassifier $
--             newDescribeDocumentClassifier
--
--         , requestDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJob
--
--         , requestListEntityRecognizerSummaries $
--             newListEntityRecognizerSummaries
--
--         , requestStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJob
--
--         , requestStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizer
--
--         , requestStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJob
--
--         , requestListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobs
--
--         , requestDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJob
--
--         , requestListDocumentClassifierSummaries $
--             newListDocumentClassifierSummaries
--
--         , requestStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJob
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
--         , requestStartDocumentClassificationJob $
--             newStartDocumentClassificationJob
--
--         , requestDetectKeyPhrases $
--             newDetectKeyPhrases
--
--         , requestDetectSyntax $
--             newDetectSyntax
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestListSentimentDetectionJobs $
--             newListSentimentDetectionJobs
--
--         , requestDeleteDocumentClassifier $
--             newDeleteDocumentClassifier
--
--         , requestListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobs
--
--         , requestStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJob
--
--         , requestListDocumentClassifiers $
--             newListDocumentClassifiers
--
--           ]

--     , testGroup "response"
--         [ responseBatchDetectSentiment $
--             newBatchDetectSentimentResponse
--
--         , responseDeleteEntityRecognizer $
--             newDeleteEntityRecognizerResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJobResponse
--
--         , responseListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobsResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseStopEventsDetectionJob $
--             newStopEventsDetectionJobResponse
--
--         , responseStartSentimentDetectionJob $
--             newStartSentimentDetectionJobResponse
--
--         , responseBatchDetectSyntax $
--             newBatchDetectSyntaxResponse
--
--         , responseStartTopicsDetectionJob $
--             newStartTopicsDetectionJobResponse
--
--         , responseDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJobResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrasesResponse
--
--         , responseDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJobResponse
--
--         , responseStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJobResponse
--
--         , responseStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJobResponse
--
--         , responseDescribeEntityRecognizer $
--             newDescribeEntityRecognizerResponse
--
--         , responseDetectSentiment $
--             newDetectSentimentResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJobResponse
--
--         , responseStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifierResponse
--
--         , responseDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJobResponse
--
--         , responseContainsPiiEntities $
--             newContainsPiiEntitiesResponse
--
--         , responseListEventsDetectionJobs $
--             newListEventsDetectionJobsResponse
--
--         , responseBatchDetectEntities $
--             newBatchDetectEntitiesResponse
--
--         , responseCreateEntityRecognizer $
--             newCreateEntityRecognizerResponse
--
--         , responseStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJobResponse
--
--         , responseCreateDocumentClassifier $
--             newCreateDocumentClassifierResponse
--
--         , responseListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobsResponse
--
--         , responseListEntityRecognizers $
--             newListEntityRecognizersResponse
--
--         , responseStopSentimentDetectionJob $
--             newStopSentimentDetectionJobResponse
--
--         , responseDetectDominantLanguage $
--             newDetectDominantLanguageResponse
--
--         , responseClassifyDocument $
--             newClassifyDocumentResponse
--
--         , responseStartEventsDetectionJob $
--             newStartEventsDetectionJobResponse
--
--         , responseDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJobResponse
--
--         , responseListDocumentClassificationJobs $
--             newListDocumentClassificationJobsResponse
--
--         , responseDetectPiiEntities $
--             newDetectPiiEntitiesResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseDetectEntities $
--             newDetectEntitiesResponse
--
--         , responseDescribeDocumentClassifier $
--             newDescribeDocumentClassifierResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJobResponse
--
--         , responseListEntityRecognizerSummaries $
--             newListEntityRecognizerSummariesResponse
--
--         , responseStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJobResponse
--
--         , responseStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizerResponse
--
--         , responseStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJobResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobsResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJobResponse
--
--         , responseListDocumentClassifierSummaries $
--             newListDocumentClassifierSummariesResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJobResponse
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
--         , responseStartDocumentClassificationJob $
--             newStartDocumentClassificationJobResponse
--
--         , responseDetectKeyPhrases $
--             newDetectKeyPhrasesResponse
--
--         , responseDetectSyntax $
--             newDetectSyntaxResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseListSentimentDetectionJobs $
--             newListSentimentDetectionJobsResponse
--
--         , responseDeleteDocumentClassifier $
--             newDeleteDocumentClassifierResponse
--
--         , responseListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobsResponse
--
--         , responseStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJobResponse
--
--         , responseListDocumentClassifiers $
--             newListDocumentClassifiersResponse
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

requestContainsPiiEntities :: ContainsPiiEntities -> TestTree
requestContainsPiiEntities =
  req
    "ContainsPiiEntities"
    "fixture/ContainsPiiEntities.yaml"

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

requestListEntityRecognizerSummaries :: ListEntityRecognizerSummaries -> TestTree
requestListEntityRecognizerSummaries =
  req
    "ListEntityRecognizerSummaries"
    "fixture/ListEntityRecognizerSummaries.yaml"

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

requestListDocumentClassifierSummaries :: ListDocumentClassifierSummaries -> TestTree
requestListDocumentClassifierSummaries =
  req
    "ListDocumentClassifierSummaries"
    "fixture/ListDocumentClassifierSummaries.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectSentiment)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEntityRecognizer)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKeyPhrasesDetectionJob)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs =
  res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitiesDetectionJobs)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEventsDetectionJob)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSentimentDetectionJob)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectSyntax)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTopicsDetectionJob)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventsDetectionJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectKeyPhrases)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSentimentDetectionJob)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob =
  res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEntitiesDetectionJob)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPiiEntitiesDetectionJob)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntityRecognizer)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectSentiment)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDominantLanguageDetectionJob)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingDocumentClassifier)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob =
  res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentClassificationJob)

responseContainsPiiEntities :: ContainsPiiEntitiesResponse -> TestTree
responseContainsPiiEntities =
  res
    "ContainsPiiEntitiesResponse"
    "fixture/ContainsPiiEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContainsPiiEntities)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventsDetectionJobs)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectEntities)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer =
  res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEntityRecognizer)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob =
  res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopKeyPhrasesDetectionJob)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier =
  res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentClassifier)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPiiEntitiesDetectionJobs)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntityRecognizers)

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob =
  res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSentimentDetectionJob)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage =
  res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectDominantLanguage)

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClassifyDocument)

responseStartEventsDetectionJob :: StartEventsDetectionJobResponse -> TestTree
responseStartEventsDetectionJob =
  res
    "StartEventsDetectionJobResponse"
    "fixture/StartEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEventsDetectionJob)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob =
  res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopicsDetectionJob)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs =
  res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassificationJobs)

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectPiiEntities)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectEntities)

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier =
  res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentClassifier)

responseDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJobResponse -> TestTree
responseDescribeDominantLanguageDetectionJob =
  res
    "DescribeDominantLanguageDetectionJobResponse"
    "fixture/DescribeDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDominantLanguageDetectionJob)

responseListEntityRecognizerSummaries :: ListEntityRecognizerSummariesResponse -> TestTree
responseListEntityRecognizerSummaries =
  res
    "ListEntityRecognizerSummariesResponse"
    "fixture/ListEntityRecognizerSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntityRecognizerSummaries)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob =
  res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEntitiesDetectionJob)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer =
  res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingEntityRecognizer)

responseStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJobResponse -> TestTree
responseStartPiiEntitiesDetectionJob =
  res
    "StartPiiEntitiesDetectionJobResponse"
    "fixture/StartPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPiiEntitiesDetectionJob)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyPhrasesDetectionJobs)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntitiesDetectionJob)

responseListDocumentClassifierSummaries :: ListDocumentClassifierSummariesResponse -> TestTree
responseListDocumentClassifierSummaries =
  res
    "ListDocumentClassifierSummariesResponse"
    "fixture/ListDocumentClassifierSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassifierSummaries)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDominantLanguageDetectionJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePiiEntitiesDetectionJob)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicsDetectionJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectDominantLanguage)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob =
  res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentClassificationJob)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases =
  res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectKeyPhrases)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectSyntax)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSentimentDetectionJobs)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier =
  res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentClassifier)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDominantLanguageDetectionJobs)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob =
  res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartKeyPhrasesDetectionJob)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers =
  res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassifiers)
