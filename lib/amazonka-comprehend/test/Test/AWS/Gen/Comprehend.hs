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
--             mkBatchDetectSentiment
--
--         , requestDeleteEntityRecognizer $
--             mkDeleteEntityRecognizer
--
--         , requestDescribeKeyPhrasesDetectionJob $
--             mkDescribeKeyPhrasesDetectionJob
--
--         , requestListEntitiesDetectionJobs $
--             mkListEntitiesDetectionJobs
--
--         , requestCreateEndpoint $
--             mkCreateEndpoint
--
--         , requestStopEventsDetectionJob $
--             mkStopEventsDetectionJob
--
--         , requestStartSentimentDetectionJob $
--             mkStartSentimentDetectionJob
--
--         , requestBatchDetectSyntax $
--             mkBatchDetectSyntax
--
--         , requestStartTopicsDetectionJob $
--             mkStartTopicsDetectionJob
--
--         , requestDescribeEventsDetectionJob $
--             mkDescribeEventsDetectionJob
--
--         , requestDeleteEndpoint $
--             mkDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             mkUpdateEndpoint
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestBatchDetectKeyPhrases $
--             mkBatchDetectKeyPhrases
--
--         , requestDescribeSentimentDetectionJob $
--             mkDescribeSentimentDetectionJob
--
--         , requestStartEntitiesDetectionJob $
--             mkStartEntitiesDetectionJob
--
--         , requestStopPiiEntitiesDetectionJob $
--             mkStopPiiEntitiesDetectionJob
--
--         , requestDescribeEntityRecognizer $
--             mkDescribeEntityRecognizer
--
--         , requestDetectSentiment $
--             mkDetectSentiment
--
--         , requestStartDominantLanguageDetectionJob $
--             mkStartDominantLanguageDetectionJob
--
--         , requestStopTrainingDocumentClassifier $
--             mkStopTrainingDocumentClassifier
--
--         , requestDescribeDocumentClassificationJob $
--             mkDescribeDocumentClassificationJob
--
--         , requestListEventsDetectionJobs $
--             mkListEventsDetectionJobs
--
--         , requestBatchDetectEntities $
--             mkBatchDetectEntities
--
--         , requestCreateEntityRecognizer $
--             mkCreateEntityRecognizer
--
--         , requestStopKeyPhrasesDetectionJob $
--             mkStopKeyPhrasesDetectionJob
--
--         , requestCreateDocumentClassifier $
--             mkCreateDocumentClassifier
--
--         , requestListPiiEntitiesDetectionJobs $
--             mkListPiiEntitiesDetectionJobs
--
--         , requestListEntityRecognizers $
--             mkListEntityRecognizers
--
--         , requestStopSentimentDetectionJob $
--             mkStopSentimentDetectionJob
--
--         , requestDetectDominantLanguage $
--             mkDetectDominantLanguage
--
--         , requestClassifyDocument $
--             mkClassifyDocument
--
--         , requestStartEventsDetectionJob $
--             mkStartEventsDetectionJob
--
--         , requestDescribeTopicsDetectionJob $
--             mkDescribeTopicsDetectionJob
--
--         , requestListDocumentClassificationJobs $
--             mkListDocumentClassificationJobs
--
--         , requestDetectPiiEntities $
--             mkDetectPiiEntities
--
--         , requestListEndpoints $
--             mkListEndpoints
--
--         , requestDetectEntities $
--             mkDetectEntities
--
--         , requestDescribeDocumentClassifier $
--             mkDescribeDocumentClassifier
--
--         , requestDescribeDominantLanguageDetectionJob $
--             mkDescribeDominantLanguageDetectionJob
--
--         , requestStopEntitiesDetectionJob $
--             mkStopEntitiesDetectionJob
--
--         , requestStopTrainingEntityRecognizer $
--             mkStopTrainingEntityRecognizer
--
--         , requestStartPiiEntitiesDetectionJob $
--             mkStartPiiEntitiesDetectionJob
--
--         , requestListKeyPhrasesDetectionJobs $
--             mkListKeyPhrasesDetectionJobs
--
--         , requestDescribeEntitiesDetectionJob $
--             mkDescribeEntitiesDetectionJob
--
--         , requestStopDominantLanguageDetectionJob $
--             mkStopDominantLanguageDetectionJob
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDescribePiiEntitiesDetectionJob $
--             mkDescribePiiEntitiesDetectionJob
--
--         , requestListTopicsDetectionJobs $
--             mkListTopicsDetectionJobs
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestBatchDetectDominantLanguage $
--             mkBatchDetectDominantLanguage
--
--         , requestStartDocumentClassificationJob $
--             mkStartDocumentClassificationJob
--
--         , requestDetectKeyPhrases $
--             mkDetectKeyPhrases
--
--         , requestDetectSyntax $
--             mkDetectSyntax
--
--         , requestDescribeEndpoint $
--             mkDescribeEndpoint
--
--         , requestListSentimentDetectionJobs $
--             mkListSentimentDetectionJobs
--
--         , requestDeleteDocumentClassifier $
--             mkDeleteDocumentClassifier
--
--         , requestListDominantLanguageDetectionJobs $
--             mkListDominantLanguageDetectionJobs
--
--         , requestStartKeyPhrasesDetectionJob $
--             mkStartKeyPhrasesDetectionJob
--
--         , requestListDocumentClassifiers $
--             mkListDocumentClassifiers
--
--           ]

--     , testGroup "response"
--         [ responseBatchDetectSentiment $
--             mkBatchDetectSentimentResponse
--
--         , responseDeleteEntityRecognizer $
--             mkDeleteEntityRecognizerResponse
--
--         , responseDescribeKeyPhrasesDetectionJob $
--             mkDescribeKeyPhrasesDetectionJobResponse
--
--         , responseListEntitiesDetectionJobs $
--             mkListEntitiesDetectionJobsResponse
--
--         , responseCreateEndpoint $
--             mkCreateEndpointResponse
--
--         , responseStopEventsDetectionJob $
--             mkStopEventsDetectionJobResponse
--
--         , responseStartSentimentDetectionJob $
--             mkStartSentimentDetectionJobResponse
--
--         , responseBatchDetectSyntax $
--             mkBatchDetectSyntaxResponse
--
--         , responseStartTopicsDetectionJob $
--             mkStartTopicsDetectionJobResponse
--
--         , responseDescribeEventsDetectionJob $
--             mkDescribeEventsDetectionJobResponse
--
--         , responseDeleteEndpoint $
--             mkDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             mkUpdateEndpointResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseBatchDetectKeyPhrases $
--             mkBatchDetectKeyPhrasesResponse
--
--         , responseDescribeSentimentDetectionJob $
--             mkDescribeSentimentDetectionJobResponse
--
--         , responseStartEntitiesDetectionJob $
--             mkStartEntitiesDetectionJobResponse
--
--         , responseStopPiiEntitiesDetectionJob $
--             mkStopPiiEntitiesDetectionJobResponse
--
--         , responseDescribeEntityRecognizer $
--             mkDescribeEntityRecognizerResponse
--
--         , responseDetectSentiment $
--             mkDetectSentimentResponse
--
--         , responseStartDominantLanguageDetectionJob $
--             mkStartDominantLanguageDetectionJobResponse
--
--         , responseStopTrainingDocumentClassifier $
--             mkStopTrainingDocumentClassifierResponse
--
--         , responseDescribeDocumentClassificationJob $
--             mkDescribeDocumentClassificationJobResponse
--
--         , responseListEventsDetectionJobs $
--             mkListEventsDetectionJobsResponse
--
--         , responseBatchDetectEntities $
--             mkBatchDetectEntitiesResponse
--
--         , responseCreateEntityRecognizer $
--             mkCreateEntityRecognizerResponse
--
--         , responseStopKeyPhrasesDetectionJob $
--             mkStopKeyPhrasesDetectionJobResponse
--
--         , responseCreateDocumentClassifier $
--             mkCreateDocumentClassifierResponse
--
--         , responseListPiiEntitiesDetectionJobs $
--             mkListPiiEntitiesDetectionJobsResponse
--
--         , responseListEntityRecognizers $
--             mkListEntityRecognizersResponse
--
--         , responseStopSentimentDetectionJob $
--             mkStopSentimentDetectionJobResponse
--
--         , responseDetectDominantLanguage $
--             mkDetectDominantLanguageResponse
--
--         , responseClassifyDocument $
--             mkClassifyDocumentResponse
--
--         , responseStartEventsDetectionJob $
--             mkStartEventsDetectionJobResponse
--
--         , responseDescribeTopicsDetectionJob $
--             mkDescribeTopicsDetectionJobResponse
--
--         , responseListDocumentClassificationJobs $
--             mkListDocumentClassificationJobsResponse
--
--         , responseDetectPiiEntities $
--             mkDetectPiiEntitiesResponse
--
--         , responseListEndpoints $
--             mkListEndpointsResponse
--
--         , responseDetectEntities $
--             mkDetectEntitiesResponse
--
--         , responseDescribeDocumentClassifier $
--             mkDescribeDocumentClassifierResponse
--
--         , responseDescribeDominantLanguageDetectionJob $
--             mkDescribeDominantLanguageDetectionJobResponse
--
--         , responseStopEntitiesDetectionJob $
--             mkStopEntitiesDetectionJobResponse
--
--         , responseStopTrainingEntityRecognizer $
--             mkStopTrainingEntityRecognizerResponse
--
--         , responseStartPiiEntitiesDetectionJob $
--             mkStartPiiEntitiesDetectionJobResponse
--
--         , responseListKeyPhrasesDetectionJobs $
--             mkListKeyPhrasesDetectionJobsResponse
--
--         , responseDescribeEntitiesDetectionJob $
--             mkDescribeEntitiesDetectionJobResponse
--
--         , responseStopDominantLanguageDetectionJob $
--             mkStopDominantLanguageDetectionJobResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDescribePiiEntitiesDetectionJob $
--             mkDescribePiiEntitiesDetectionJobResponse
--
--         , responseListTopicsDetectionJobs $
--             mkListTopicsDetectionJobsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseBatchDetectDominantLanguage $
--             mkBatchDetectDominantLanguageResponse
--
--         , responseStartDocumentClassificationJob $
--             mkStartDocumentClassificationJobResponse
--
--         , responseDetectKeyPhrases $
--             mkDetectKeyPhrasesResponse
--
--         , responseDetectSyntax $
--             mkDetectSyntaxResponse
--
--         , responseDescribeEndpoint $
--             mkDescribeEndpointResponse
--
--         , responseListSentimentDetectionJobs $
--             mkListSentimentDetectionJobsResponse
--
--         , responseDeleteDocumentClassifier $
--             mkDeleteDocumentClassifierResponse
--
--         , responseListDominantLanguageDetectionJobs $
--             mkListDominantLanguageDetectionJobsResponse
--
--         , responseStartKeyPhrasesDetectionJob $
--             mkStartKeyPhrasesDetectionJobResponse
--
--         , responseListDocumentClassifiers $
--             mkListDocumentClassifiersResponse
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
    mkServiceConfig
    (Proxy :: Proxy BatchDetectSentiment)

responseDeleteEntityRecognizer :: DeleteEntityRecognizerResponse -> TestTree
responseDeleteEntityRecognizer =
  res
    "DeleteEntityRecognizerResponse"
    "fixture/DeleteEntityRecognizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEntityRecognizer)

responseDescribeKeyPhrasesDetectionJob :: DescribeKeyPhrasesDetectionJobResponse -> TestTree
responseDescribeKeyPhrasesDetectionJob =
  res
    "DescribeKeyPhrasesDetectionJobResponse"
    "fixture/DescribeKeyPhrasesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeKeyPhrasesDetectionJob)

responseListEntitiesDetectionJobs :: ListEntitiesDetectionJobsResponse -> TestTree
responseListEntitiesDetectionJobs =
  res
    "ListEntitiesDetectionJobsResponse"
    "fixture/ListEntitiesDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEntitiesDetectionJobs)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEndpoint)

responseStopEventsDetectionJob :: StopEventsDetectionJobResponse -> TestTree
responseStopEventsDetectionJob =
  res
    "StopEventsDetectionJobResponse"
    "fixture/StopEventsDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopEventsDetectionJob)

responseStartSentimentDetectionJob :: StartSentimentDetectionJobResponse -> TestTree
responseStartSentimentDetectionJob =
  res
    "StartSentimentDetectionJobResponse"
    "fixture/StartSentimentDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSentimentDetectionJob)

responseBatchDetectSyntax :: BatchDetectSyntaxResponse -> TestTree
responseBatchDetectSyntax =
  res
    "BatchDetectSyntaxResponse"
    "fixture/BatchDetectSyntaxResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDetectSyntax)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob =
  res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartTopicsDetectionJob)

responseDescribeEventsDetectionJob :: DescribeEventsDetectionJobResponse -> TestTree
responseDescribeEventsDetectionJob =
  res
    "DescribeEventsDetectionJobResponse"
    "fixture/DescribeEventsDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEventsDetectionJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases =
  res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseDescribeSentimentDetectionJob :: DescribeSentimentDetectionJobResponse -> TestTree
responseDescribeSentimentDetectionJob =
  res
    "DescribeSentimentDetectionJobResponse"
    "fixture/DescribeSentimentDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSentimentDetectionJob)

responseStartEntitiesDetectionJob :: StartEntitiesDetectionJobResponse -> TestTree
responseStartEntitiesDetectionJob =
  res
    "StartEntitiesDetectionJobResponse"
    "fixture/StartEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartEntitiesDetectionJob)

responseStopPiiEntitiesDetectionJob :: StopPiiEntitiesDetectionJobResponse -> TestTree
responseStopPiiEntitiesDetectionJob =
  res
    "StopPiiEntitiesDetectionJobResponse"
    "fixture/StopPiiEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopPiiEntitiesDetectionJob)

responseDescribeEntityRecognizer :: DescribeEntityRecognizerResponse -> TestTree
responseDescribeEntityRecognizer =
  res
    "DescribeEntityRecognizerResponse"
    "fixture/DescribeEntityRecognizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEntityRecognizer)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment =
  res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectSentiment)

responseStartDominantLanguageDetectionJob :: StartDominantLanguageDetectionJobResponse -> TestTree
responseStartDominantLanguageDetectionJob =
  res
    "StartDominantLanguageDetectionJobResponse"
    "fixture/StartDominantLanguageDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartDominantLanguageDetectionJob)

responseStopTrainingDocumentClassifier :: StopTrainingDocumentClassifierResponse -> TestTree
responseStopTrainingDocumentClassifier =
  res
    "StopTrainingDocumentClassifierResponse"
    "fixture/StopTrainingDocumentClassifierResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopTrainingDocumentClassifier)

responseDescribeDocumentClassificationJob :: DescribeDocumentClassificationJobResponse -> TestTree
responseDescribeDocumentClassificationJob =
  res
    "DescribeDocumentClassificationJobResponse"
    "fixture/DescribeDocumentClassificationJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDocumentClassificationJob)

responseListEventsDetectionJobs :: ListEventsDetectionJobsResponse -> TestTree
responseListEventsDetectionJobs =
  res
    "ListEventsDetectionJobsResponse"
    "fixture/ListEventsDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEventsDetectionJobs)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities =
  res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDetectEntities)

responseCreateEntityRecognizer :: CreateEntityRecognizerResponse -> TestTree
responseCreateEntityRecognizer =
  res
    "CreateEntityRecognizerResponse"
    "fixture/CreateEntityRecognizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEntityRecognizer)

responseStopKeyPhrasesDetectionJob :: StopKeyPhrasesDetectionJobResponse -> TestTree
responseStopKeyPhrasesDetectionJob =
  res
    "StopKeyPhrasesDetectionJobResponse"
    "fixture/StopKeyPhrasesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopKeyPhrasesDetectionJob)

responseCreateDocumentClassifier :: CreateDocumentClassifierResponse -> TestTree
responseCreateDocumentClassifier =
  res
    "CreateDocumentClassifierResponse"
    "fixture/CreateDocumentClassifierResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDocumentClassifier)

responseListPiiEntitiesDetectionJobs :: ListPiiEntitiesDetectionJobsResponse -> TestTree
responseListPiiEntitiesDetectionJobs =
  res
    "ListPiiEntitiesDetectionJobsResponse"
    "fixture/ListPiiEntitiesDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPiiEntitiesDetectionJobs)

responseListEntityRecognizers :: ListEntityRecognizersResponse -> TestTree
responseListEntityRecognizers =
  res
    "ListEntityRecognizersResponse"
    "fixture/ListEntityRecognizersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEntityRecognizers)

responseStopSentimentDetectionJob :: StopSentimentDetectionJobResponse -> TestTree
responseStopSentimentDetectionJob =
  res
    "StopSentimentDetectionJobResponse"
    "fixture/StopSentimentDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopSentimentDetectionJob)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage =
  res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectDominantLanguage)

responseClassifyDocument :: ClassifyDocumentResponse -> TestTree
responseClassifyDocument =
  res
    "ClassifyDocumentResponse"
    "fixture/ClassifyDocumentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ClassifyDocument)

responseStartEventsDetectionJob :: StartEventsDetectionJobResponse -> TestTree
responseStartEventsDetectionJob =
  res
    "StartEventsDetectionJobResponse"
    "fixture/StartEventsDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartEventsDetectionJob)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob =
  res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTopicsDetectionJob)

responseListDocumentClassificationJobs :: ListDocumentClassificationJobsResponse -> TestTree
responseListDocumentClassificationJobs =
  res
    "ListDocumentClassificationJobsResponse"
    "fixture/ListDocumentClassificationJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDocumentClassificationJobs)

responseDetectPiiEntities :: DetectPiiEntitiesResponse -> TestTree
responseDetectPiiEntities =
  res
    "DetectPiiEntitiesResponse"
    "fixture/DetectPiiEntitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectPiiEntities)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEndpoints)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities =
  res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectEntities)

responseDescribeDocumentClassifier :: DescribeDocumentClassifierResponse -> TestTree
responseDescribeDocumentClassifier =
  res
    "DescribeDocumentClassifierResponse"
    "fixture/DescribeDocumentClassifierResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDocumentClassifier)

responseDescribeDominantLanguageDetectionJob :: DescribeDominantLanguageDetectionJobResponse -> TestTree
responseDescribeDominantLanguageDetectionJob =
  res
    "DescribeDominantLanguageDetectionJobResponse"
    "fixture/DescribeDominantLanguageDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDominantLanguageDetectionJob)

responseStopEntitiesDetectionJob :: StopEntitiesDetectionJobResponse -> TestTree
responseStopEntitiesDetectionJob =
  res
    "StopEntitiesDetectionJobResponse"
    "fixture/StopEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopEntitiesDetectionJob)

responseStopTrainingEntityRecognizer :: StopTrainingEntityRecognizerResponse -> TestTree
responseStopTrainingEntityRecognizer =
  res
    "StopTrainingEntityRecognizerResponse"
    "fixture/StopTrainingEntityRecognizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopTrainingEntityRecognizer)

responseStartPiiEntitiesDetectionJob :: StartPiiEntitiesDetectionJobResponse -> TestTree
responseStartPiiEntitiesDetectionJob =
  res
    "StartPiiEntitiesDetectionJobResponse"
    "fixture/StartPiiEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartPiiEntitiesDetectionJob)

responseListKeyPhrasesDetectionJobs :: ListKeyPhrasesDetectionJobsResponse -> TestTree
responseListKeyPhrasesDetectionJobs =
  res
    "ListKeyPhrasesDetectionJobsResponse"
    "fixture/ListKeyPhrasesDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListKeyPhrasesDetectionJobs)

responseDescribeEntitiesDetectionJob :: DescribeEntitiesDetectionJobResponse -> TestTree
responseDescribeEntitiesDetectionJob =
  res
    "DescribeEntitiesDetectionJobResponse"
    "fixture/DescribeEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEntitiesDetectionJob)

responseStopDominantLanguageDetectionJob :: StopDominantLanguageDetectionJobResponse -> TestTree
responseStopDominantLanguageDetectionJob =
  res
    "StopDominantLanguageDetectionJobResponse"
    "fixture/StopDominantLanguageDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopDominantLanguageDetectionJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDescribePiiEntitiesDetectionJob :: DescribePiiEntitiesDetectionJobResponse -> TestTree
responseDescribePiiEntitiesDetectionJob =
  res
    "DescribePiiEntitiesDetectionJobResponse"
    "fixture/DescribePiiEntitiesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePiiEntitiesDetectionJob)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs =
  res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage =
  res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseStartDocumentClassificationJob :: StartDocumentClassificationJobResponse -> TestTree
responseStartDocumentClassificationJob =
  res
    "StartDocumentClassificationJobResponse"
    "fixture/StartDocumentClassificationJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartDocumentClassificationJob)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases =
  res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectKeyPhrases)

responseDetectSyntax :: DetectSyntaxResponse -> TestTree
responseDetectSyntax =
  res
    "DetectSyntaxResponse"
    "fixture/DetectSyntaxResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectSyntax)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEndpoint)

responseListSentimentDetectionJobs :: ListSentimentDetectionJobsResponse -> TestTree
responseListSentimentDetectionJobs =
  res
    "ListSentimentDetectionJobsResponse"
    "fixture/ListSentimentDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSentimentDetectionJobs)

responseDeleteDocumentClassifier :: DeleteDocumentClassifierResponse -> TestTree
responseDeleteDocumentClassifier =
  res
    "DeleteDocumentClassifierResponse"
    "fixture/DeleteDocumentClassifierResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDocumentClassifier)

responseListDominantLanguageDetectionJobs :: ListDominantLanguageDetectionJobsResponse -> TestTree
responseListDominantLanguageDetectionJobs =
  res
    "ListDominantLanguageDetectionJobsResponse"
    "fixture/ListDominantLanguageDetectionJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDominantLanguageDetectionJobs)

responseStartKeyPhrasesDetectionJob :: StartKeyPhrasesDetectionJobResponse -> TestTree
responseStartKeyPhrasesDetectionJob =
  res
    "StartKeyPhrasesDetectionJobResponse"
    "fixture/StartKeyPhrasesDetectionJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartKeyPhrasesDetectionJob)

responseListDocumentClassifiers :: ListDocumentClassifiersResponse -> TestTree
responseListDocumentClassifiers =
  res
    "ListDocumentClassifiersResponse"
    "fixture/ListDocumentClassifiersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDocumentClassifiers)
