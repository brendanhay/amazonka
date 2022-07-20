{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Comprehend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Comprehend where

import Amazonka.Comprehend
import qualified Data.Proxy as Proxy
import Test.Amazonka.Comprehend.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguage
--
--         , requestBatchDetectEntities $
--             newBatchDetectEntities
--
--         , requestBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrases
--
--         , requestBatchDetectSentiment $
--             newBatchDetectSentiment
--
--         , requestBatchDetectSyntax $
--             newBatchDetectSyntax
--
--         , requestClassifyDocument $
--             newClassifyDocument
--
--         , requestContainsPiiEntities $
--             newContainsPiiEntities
--
--         , requestCreateDocumentClassifier $
--             newCreateDocumentClassifier
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestCreateEntityRecognizer $
--             newCreateEntityRecognizer
--
--         , requestDeleteDocumentClassifier $
--             newDeleteDocumentClassifier
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteEntityRecognizer $
--             newDeleteEntityRecognizer
--
--         , requestDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJob
--
--         , requestDescribeDocumentClassifier $
--             newDescribeDocumentClassifier
--
--         , requestDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJob
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJob
--
--         , requestDescribeEntityRecognizer $
--             newDescribeEntityRecognizer
--
--         , requestDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJob
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJob
--
--         , requestDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJob
--
--         , requestDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJob
--
--         , requestDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJob
--
--         , requestDetectDominantLanguage $
--             newDetectDominantLanguage
--
--         , requestDetectEntities $
--             newDetectEntities
--
--         , requestDetectKeyPhrases $
--             newDetectKeyPhrases
--
--         , requestDetectPiiEntities $
--             newDetectPiiEntities
--
--         , requestDetectSentiment $
--             newDetectSentiment
--
--         , requestDetectSyntax $
--             newDetectSyntax
--
--         , requestListDocumentClassificationJobs $
--             newListDocumentClassificationJobs
--
--         , requestListDocumentClassifierSummaries $
--             newListDocumentClassifierSummaries
--
--         , requestListDocumentClassifiers $
--             newListDocumentClassifiers
--
--         , requestListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobs
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobs
--
--         , requestListEntityRecognizerSummaries $
--             newListEntityRecognizerSummaries
--
--         , requestListEntityRecognizers $
--             newListEntityRecognizers
--
--         , requestListEventsDetectionJobs $
--             newListEventsDetectionJobs
--
--         , requestListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobs
--
--         , requestListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobs
--
--         , requestListSentimentDetectionJobs $
--             newListSentimentDetectionJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTopicsDetectionJobs $
--             newListTopicsDetectionJobs
--
--         , requestStartDocumentClassificationJob $
--             newStartDocumentClassificationJob
--
--         , requestStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJob
--
--         , requestStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJob
--
--         , requestStartEventsDetectionJob $
--             newStartEventsDetectionJob
--
--         , requestStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJob
--
--         , requestStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJob
--
--         , requestStartSentimentDetectionJob $
--             newStartSentimentDetectionJob
--
--         , requestStartTopicsDetectionJob $
--             newStartTopicsDetectionJob
--
--         , requestStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJob
--
--         , requestStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJob
--
--         , requestStopEventsDetectionJob $
--             newStopEventsDetectionJob
--
--         , requestStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJob
--
--         , requestStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJob
--
--         , requestStopSentimentDetectionJob $
--             newStopSentimentDetectionJob
--
--         , requestStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifier
--
--         , requestStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizer
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseBatchDetectDominantLanguage $
--             newBatchDetectDominantLanguageResponse
--
--         , responseBatchDetectEntities $
--             newBatchDetectEntitiesResponse
--
--         , responseBatchDetectKeyPhrases $
--             newBatchDetectKeyPhrasesResponse
--
--         , responseBatchDetectSentiment $
--             newBatchDetectSentimentResponse
--
--         , responseBatchDetectSyntax $
--             newBatchDetectSyntaxResponse
--
--         , responseClassifyDocument $
--             newClassifyDocumentResponse
--
--         , responseContainsPiiEntities $
--             newContainsPiiEntitiesResponse
--
--         , responseCreateDocumentClassifier $
--             newCreateDocumentClassifierResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseCreateEntityRecognizer $
--             newCreateEntityRecognizerResponse
--
--         , responseDeleteDocumentClassifier $
--             newDeleteDocumentClassifierResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteEntityRecognizer $
--             newDeleteEntityRecognizerResponse
--
--         , responseDescribeDocumentClassificationJob $
--             newDescribeDocumentClassificationJobResponse
--
--         , responseDescribeDocumentClassifier $
--             newDescribeDocumentClassifierResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             newDescribeDominantLanguageDetectionJobResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             newDescribeEntitiesDetectionJobResponse
--
--         , responseDescribeEntityRecognizer $
--             newDescribeEntityRecognizerResponse
--
--         , responseDescribeEventsDetectionJob $
--             newDescribeEventsDetectionJobResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             newDescribeKeyPhrasesDetectionJobResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             newDescribePiiEntitiesDetectionJobResponse
--
--         , responseDescribeSentimentDetectionJob $
--             newDescribeSentimentDetectionJobResponse
--
--         , responseDescribeTopicsDetectionJob $
--             newDescribeTopicsDetectionJobResponse
--
--         , responseDetectDominantLanguage $
--             newDetectDominantLanguageResponse
--
--         , responseDetectEntities $
--             newDetectEntitiesResponse
--
--         , responseDetectKeyPhrases $
--             newDetectKeyPhrasesResponse
--
--         , responseDetectPiiEntities $
--             newDetectPiiEntitiesResponse
--
--         , responseDetectSentiment $
--             newDetectSentimentResponse
--
--         , responseDetectSyntax $
--             newDetectSyntaxResponse
--
--         , responseListDocumentClassificationJobs $
--             newListDocumentClassificationJobsResponse
--
--         , responseListDocumentClassifierSummaries $
--             newListDocumentClassifierSummariesResponse
--
--         , responseListDocumentClassifiers $
--             newListDocumentClassifiersResponse
--
--         , responseListDominantLanguageDetectionJobs $
--             newListDominantLanguageDetectionJobsResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseListEntitiesDetectionJobs $
--             newListEntitiesDetectionJobsResponse
--
--         , responseListEntityRecognizerSummaries $
--             newListEntityRecognizerSummariesResponse
--
--         , responseListEntityRecognizers $
--             newListEntityRecognizersResponse
--
--         , responseListEventsDetectionJobs $
--             newListEventsDetectionJobsResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             newListKeyPhrasesDetectionJobsResponse
--
--         , responseListPiiEntitiesDetectionJobs $
--             newListPiiEntitiesDetectionJobsResponse
--
--         , responseListSentimentDetectionJobs $
--             newListSentimentDetectionJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTopicsDetectionJobs $
--             newListTopicsDetectionJobsResponse
--
--         , responseStartDocumentClassificationJob $
--             newStartDocumentClassificationJobResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             newStartDominantLanguageDetectionJobResponse
--
--         , responseStartEntitiesDetectionJob $
--             newStartEntitiesDetectionJobResponse
--
--         , responseStartEventsDetectionJob $
--             newStartEventsDetectionJobResponse
--
--         , responseStartKeyPhrasesDetectionJob $
--             newStartKeyPhrasesDetectionJobResponse
--
--         , responseStartPiiEntitiesDetectionJob $
--             newStartPiiEntitiesDetectionJobResponse
--
--         , responseStartSentimentDetectionJob $
--             newStartSentimentDetectionJobResponse
--
--         , responseStartTopicsDetectionJob $
--             newStartTopicsDetectionJobResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             newStopDominantLanguageDetectionJobResponse
--
--         , responseStopEntitiesDetectionJob $
--             newStopEntitiesDetectionJobResponse
--
--         , responseStopEventsDetectionJob $
--             newStopEventsDetectionJobResponse
--
--         , responseStopKeyPhrasesDetectionJob $
--             newStopKeyPhrasesDetectionJobResponse
--
--         , responseStopPiiEntitiesDetectionJob $
--             newStopPiiEntitiesDetectionJobResponse
--
--         , responseStopSentimentDetectionJob $
--             newStopSentimentDetectionJobResponse
--
--         , responseStopTrainingDocumentClassifier $
--             newStopTrainingDocumentClassifierResponse
--
--         , responseStopTrainingEntityRecognizer $
--             newStopTrainingEntityRecognizerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--           ]
--     ]

-- Requests

requestBatchDetectDominantLanguage :: BatchDetectDominantLanguage -> TestTree
requestBatchDetectDominantLanguage =
  req
    "BatchDetectDominantLanguage"
    "fixture/BatchDetectDominantLanguage.yaml"

requestBatchDetectEntities :: BatchDetectEntities -> TestTree
requestBatchDetectEntities =
  req
    "BatchDetectEntities"
    "fixture/BatchDetectEntities.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases =
  req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestBatchDetectSentiment :: BatchDetectSentiment -> TestTree
requestBatchDetectSentiment =
  req
    "BatchDetectSentiment"
    "fixture/BatchDetectSentiment.yaml"

requestBatchDetectSyntax :: BatchDetectSyntax -> TestTree
requestBatchDetectSyntax =
  req
    "BatchDetectSyntax"
    "fixture/BatchDetectSyntax.yaml"

requestClassifyDocument :: ClassifyDocument -> TestTree
requestClassifyDocument =
  req
    "ClassifyDocument"
    "fixture/ClassifyDocument.yaml"

requestContainsPiiEntities :: ContainsPiiEntities -> TestTree
requestContainsPiiEntities =
  req
    "ContainsPiiEntities"
    "fixture/ContainsPiiEntities.yaml"

requestCreateDocumentClassifier :: CreateDocumentClassifier -> TestTree
requestCreateDocumentClassifier =
  req
    "CreateDocumentClassifier"
    "fixture/CreateDocumentClassifier.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestCreateEntityRecognizer :: CreateEntityRecognizer -> TestTree
requestCreateEntityRecognizer =
  req
    "CreateEntityRecognizer"
    "fixture/CreateEntityRecognizer.yaml"

requestDeleteDocumentClassifier :: DeleteDocumentClassifier -> TestTree
requestDeleteDocumentClassifier =
  req
    "DeleteDocumentClassifier"
    "fixture/DeleteDocumentClassifier.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeleteEntityRecognizer :: DeleteEntityRecognizer -> TestTree
requestDeleteEntityRecognizer =
  req
    "DeleteEntityRecognizer"
    "fixture/DeleteEntityRecognizer.yaml"

requestDescribeDocumentClassificationJob :: DescribeDocumentClassificationJob -> TestTree
requestDescribeDocumentClassificationJob =
  req
    "DescribeDocumentClassificationJob"
    "fixture/DescribeDocumentClassificationJob.yaml"

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

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJob -> TestTree
requestDescribeEntitiesDetectionJob =
  req
    "DescribeEntitiesDetectionJob"
    "fixture/DescribeEntitiesDetectionJob.yaml"

requestDescribeEntityRecognizer :: DescribeEntityRecognizer -> TestTree
requestDescribeEntityRecognizer =
  req
    "DescribeEntityRecognizer"
    "fixture/DescribeEntityRecognizer.yaml"

requestDescribeEventsDetectionJob :: DescribeEventsDetectionJob -> TestTree
requestDescribeEventsDetectionJob =
  req
    "DescribeEventsDetectionJob"
    "fixture/DescribeEventsDetectionJob.yaml"

requestDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJob -> TestTree
requestDescribeKeyPhrasesDetectionJob =
  req
    "DescribeKeyPhrasesDetectionJob"
    "fixture/DescribeKeyPhrasesDetectionJob.yaml"

requestDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJob -> TestTree
requestDescribePiiEntitiesDetectionJob =
  req
    "DescribePiiEntitiesDetectionJob"
    "fixture/DescribePiiEntitiesDetectionJob.yaml"

requestDescribeSentimentDetectionJob :: DescribeSentimentDetectionJob -> TestTree
requestDescribeSentimentDetectionJob =
  req
    "DescribeSentimentDetectionJob"
    "fixture/DescribeSentimentDetectionJob.yaml"

requestDescribeTopicsDetectionJob :: DescribeTopicsDetectionJob -> TestTree
requestDescribeTopicsDetectionJob =
  req
    "DescribeTopicsDetectionJob"
    "fixture/DescribeTopicsDetectionJob.yaml"

requestDetectDominantLanguage :: DetectDominantLanguage -> TestTree
requestDetectDominantLanguage =
  req
    "DetectDominantLanguage"
    "fixture/DetectDominantLanguage.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities =
  req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestDetectKeyPhrases :: DetectKeyPhrases -> TestTree
requestDetectKeyPhrases =
  req
    "DetectKeyPhrases"
    "fixture/DetectKeyPhrases.yaml"

requestDetectPiiEntities :: DetectPiiEntities -> TestTree
requestDetectPiiEntities =
  req
    "DetectPiiEntities"
    "fixture/DetectPiiEntities.yaml"

requestDetectSentiment :: DetectSentiment -> TestTree
requestDetectSentiment =
  req
    "DetectSentiment"
    "fixture/DetectSentiment.yaml"

requestDetectSyntax :: DetectSyntax -> TestTree
requestDetectSyntax =
  req
    "DetectSyntax"
    "fixture/DetectSyntax.yaml"

requestListDocumentClassificationJobs :: ListDocumentClassificationJobs -> TestTree
requestListDocumentClassificationJobs =
  req
    "ListDocumentClassificationJobs"
    "fixture/ListDocumentClassificationJobs.yaml"

requestListDocumentClassifierSummaries :: ListDocumentClassifierSummaries -> TestTree
requestListDocumentClassifierSummaries =
  req
    "ListDocumentClassifierSummaries"
    "fixture/ListDocumentClassifierSummaries.yaml"

requestListDocumentClassifiers :: ListDocumentClassifiers -> TestTree
requestListDocumentClassifiers =
  req
    "ListDocumentClassifiers"
    "fixture/ListDocumentClassifiers.yaml"

requestListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobs -> TestTree
requestListDominantLanguageDetectionJobs =
  req
    "ListDominantLanguageDetectionJobs"
    "fixture/ListDominantLanguageDetectionJobs.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestListEntitiesDetectionJobs :: ListEntitiesDetectionJobs -> TestTree
requestListEntitiesDetectionJobs =
  req
    "ListEntitiesDetectionJobs"
    "fixture/ListEntitiesDetectionJobs.yaml"

requestListEntityRecognizerSummaries :: ListEntityRecognizerSummaries -> TestTree
requestListEntityRecognizerSummaries =
  req
    "ListEntityRecognizerSummaries"
    "fixture/ListEntityRecognizerSummaries.yaml"

requestListEntityRecognizers :: ListEntityRecognizers -> TestTree
requestListEntityRecognizers =
  req
    "ListEntityRecognizers"
    "fixture/ListEntityRecognizers.yaml"

requestListEventsDetectionJobs :: ListEventsDetectionJobs -> TestTree
requestListEventsDetectionJobs =
  req
    "ListEventsDetectionJobs"
    "fixture/ListEventsDetectionJobs.yaml"

requestListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobs -> TestTree
requestListKeyPhrasesDetectionJobs =
  req
    "ListKeyPhrasesDetectionJobs"
    "fixture/ListKeyPhrasesDetectionJobs.yaml"

requestListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobs -> TestTree
requestListPiiEntitiesDetectionJobs =
  req
    "ListPiiEntitiesDetectionJobs"
    "fixture/ListPiiEntitiesDetectionJobs.yaml"

requestListSentimentDetectionJobs :: ListSentimentDetectionJobs -> TestTree
requestListSentimentDetectionJobs =
  req
    "ListSentimentDetectionJobs"
    "fixture/ListSentimentDetectionJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTopicsDetectionJobs :: ListTopicsDetectionJobs -> TestTree
requestListTopicsDetectionJobs =
  req
    "ListTopicsDetectionJobs"
    "fixture/ListTopicsDetectionJobs.yaml"

requestStartDocumentClassificationJob :: StartDocumentClassificationJob -> TestTree
requestStartDocumentClassificationJob =
  req
    "StartDocumentClassificationJob"
    "fixture/StartDocumentClassificationJob.yaml"

requestStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJob -> TestTree
requestStartDominantLanguageDetectionJob =
  req
    "StartDominantLanguageDetectionJob"
    "fixture/StartDominantLanguageDetectionJob.yaml"

requestStartEntitiesDetectionJob :: StartEntitiesDetectionJob -> TestTree
requestStartEntitiesDetectionJob =
  req
    "StartEntitiesDetectionJob"
    "fixture/StartEntitiesDetectionJob.yaml"

requestStartEventsDetectionJob :: StartEventsDetectionJob -> TestTree
requestStartEventsDetectionJob =
  req
    "StartEventsDetectionJob"
    "fixture/StartEventsDetectionJob.yaml"

requestStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJob -> TestTree
requestStartKeyPhrasesDetectionJob =
  req
    "StartKeyPhrasesDetectionJob"
    "fixture/StartKeyPhrasesDetectionJob.yaml"

requestStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJob -> TestTree
requestStartPiiEntitiesDetectionJob =
  req
    "StartPiiEntitiesDetectionJob"
    "fixture/StartPiiEntitiesDetectionJob.yaml"

requestStartSentimentDetectionJob :: StartSentimentDetectionJob -> TestTree
requestStartSentimentDetectionJob =
  req
    "StartSentimentDetectionJob"
    "fixture/StartSentimentDetectionJob.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob =
  req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

requestStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJob -> TestTree
requestStopDominantLanguageDetectionJob =
  req
    "StopDominantLanguageDetectionJob"
    "fixture/StopDominantLanguageDetectionJob.yaml"

requestStopEntitiesDetectionJob :: StopEntitiesDetectionJob -> TestTree
requestStopEntitiesDetectionJob =
  req
    "StopEntitiesDetectionJob"
    "fixture/StopEntitiesDetectionJob.yaml"

requestStopEventsDetectionJob :: StopEventsDetectionJob -> TestTree
requestStopEventsDetectionJob =
  req
    "StopEventsDetectionJob"
    "fixture/StopEventsDetectionJob.yaml"

requestStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJob -> TestTree
requestStopKeyPhrasesDetectionJob =
  req
    "StopKeyPhrasesDetectionJob"
    "fixture/StopKeyPhrasesDetectionJob.yaml"

requestStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJob -> TestTree
requestStopPiiEntitiesDetectionJob =
  req
    "StopPiiEntitiesDetectionJob"
    "fixture/StopPiiEntitiesDetectionJob.yaml"

requestStopSentimentDetectionJob :: StopSentimentDetectionJob -> TestTree
requestStopSentimentDetectionJob =
  req
    "StopSentimentDetectionJob"
    "fixture/StopSentimentDetectionJob.yaml"

requestStopTrainingDocumentClassifier :: StopTrainingDocumentClassifier -> TestTree
requestStopTrainingDocumentClassifier =
  req
    "StopTrainingDocumentClassifier"
    "fixture/StopTrainingDocumentClassifier.yaml"

requestStopTrainingEntityRecognizer :: StopTrainingEntityRecognizer -> TestTree
requestStopTrainingEntityRecognizer =
  req
    "StopTrainingEntityRecognizer"
    "fixture/StopTrainingEntityRecognizer.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

-- Responses

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectDominantLanguage)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectEntities)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectKeyPhrases)

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment =
  res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectSentiment)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDetectSyntax)

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClassifyDocument)

responseContainsPiiEntities :: ContainsPiiEntitiesResponse -> TestTree
responseContainsPiiEntities =
  res
    "ContainsPiiEntitiesResponse"
    "fixture/ContainsPiiEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContainsPiiEntities)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier =
  res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentClassifier)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer =
  res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEntityRecognizer)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier =
  res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentClassifier)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEntityRecognizer)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob =
  res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentClassificationJob)

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

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntitiesDetectionJob)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntityRecognizer)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventsDetectionJob)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKeyPhrasesDetectionJob)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePiiEntitiesDetectionJob)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSentimentDetectionJob)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob =
  res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopicsDetectionJob)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage =
  res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectDominantLanguage)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectEntities)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases =
  res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectKeyPhrases)

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectPiiEntities)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectSentiment)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectSyntax)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs =
  res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassificationJobs)

responseListDocumentClassifierSummaries :: ListDocumentClassifierSummariesResponse -> TestTree
responseListDocumentClassifierSummaries =
  res
    "ListDocumentClassifierSummariesResponse"
    "fixture/ListDocumentClassifierSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassifierSummaries)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers =
  res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentClassifiers)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDominantLanguageDetectionJobs)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs =
  res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitiesDetectionJobs)

responseListEntityRecognizerSummaries :: ListEntityRecognizerSummariesResponse -> TestTree
responseListEntityRecognizerSummaries =
  res
    "ListEntityRecognizerSummariesResponse"
    "fixture/ListEntityRecognizerSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntityRecognizerSummaries)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntityRecognizers)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventsDetectionJobs)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyPhrasesDetectionJobs)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPiiEntitiesDetectionJobs)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSentimentDetectionJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicsDetectionJobs)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob =
  res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentClassificationJob)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDominantLanguageDetectionJob)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob =
  res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEntitiesDetectionJob)

responseStartEventsDetectionJob :: StartEventsDetectionJobResponse -> TestTree
responseStartEventsDetectionJob =
  res
    "StartEventsDetectionJobResponse"
    "fixture/StartEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEventsDetectionJob)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob =
  res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartKeyPhrasesDetectionJob)

responseStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJobResponse -> TestTree
responseStartPiiEntitiesDetectionJob =
  res
    "StartPiiEntitiesDetectionJobResponse"
    "fixture/StartPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPiiEntitiesDetectionJob)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSentimentDetectionJob)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTopicsDetectionJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDominantLanguageDetectionJob)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob =
  res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEntitiesDetectionJob)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEventsDetectionJob)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob =
  res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopKeyPhrasesDetectionJob)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPiiEntitiesDetectionJob)

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob =
  res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSentimentDetectionJob)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingDocumentClassifier)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer =
  res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingEntityRecognizer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)
