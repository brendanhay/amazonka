{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeGuruReviewer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeGuruReviewer where

import qualified Data.Proxy as Proxy
import Network.AWS.CodeGuruReviewer
import Test.AWS.CodeGuruReviewer.Internal
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
--         [ requestListRecommendationFeedback $
--             newListRecommendationFeedback
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDisassociateRepository $
--             newDisassociateRepository
--
--         , requestDescribeRepositoryAssociation $
--             newDescribeRepositoryAssociation
--
--         , requestDescribeCodeReview $
--             newDescribeCodeReview
--
--         , requestListRepositoryAssociations $
--             newListRepositoryAssociations
--
--         , requestDescribeRecommendationFeedback $
--             newDescribeRecommendationFeedback
--
--         , requestListRecommendations $
--             newListRecommendations
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateCodeReview $
--             newCreateCodeReview
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListCodeReviews $
--             newListCodeReviews
--
--         , requestAssociateRepository $
--             newAssociateRepository
--
--         , requestPutRecommendationFeedback $
--             newPutRecommendationFeedback
--
--           ]

--     , testGroup "response"
--         [ responseListRecommendationFeedback $
--             newListRecommendationFeedbackResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDisassociateRepository $
--             newDisassociateRepositoryResponse
--
--         , responseDescribeRepositoryAssociation $
--             newDescribeRepositoryAssociationResponse
--
--         , responseDescribeCodeReview $
--             newDescribeCodeReviewResponse
--
--         , responseListRepositoryAssociations $
--             newListRepositoryAssociationsResponse
--
--         , responseDescribeRecommendationFeedback $
--             newDescribeRecommendationFeedbackResponse
--
--         , responseListRecommendations $
--             newListRecommendationsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateCodeReview $
--             newCreateCodeReviewResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListCodeReviews $
--             newListCodeReviewsResponse
--
--         , responseAssociateRepository $
--             newAssociateRepositoryResponse
--
--         , responsePutRecommendationFeedback $
--             newPutRecommendationFeedbackResponse
--
--           ]
--     ]

-- Requests

requestListRecommendationFeedback :: ListRecommendationFeedback -> TestTree
requestListRecommendationFeedback =
  req
    "ListRecommendationFeedback"
    "fixture/ListRecommendationFeedback.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDisassociateRepository :: DisassociateRepository -> TestTree
requestDisassociateRepository =
  req
    "DisassociateRepository"
    "fixture/DisassociateRepository.yaml"

requestDescribeRepositoryAssociation :: DescribeRepositoryAssociation -> TestTree
requestDescribeRepositoryAssociation =
  req
    "DescribeRepositoryAssociation"
    "fixture/DescribeRepositoryAssociation.yaml"

requestDescribeCodeReview :: DescribeCodeReview -> TestTree
requestDescribeCodeReview =
  req
    "DescribeCodeReview"
    "fixture/DescribeCodeReview.yaml"

requestListRepositoryAssociations :: ListRepositoryAssociations -> TestTree
requestListRepositoryAssociations =
  req
    "ListRepositoryAssociations"
    "fixture/ListRepositoryAssociations.yaml"

requestDescribeRecommendationFeedback :: DescribeRecommendationFeedback -> TestTree
requestDescribeRecommendationFeedback =
  req
    "DescribeRecommendationFeedback"
    "fixture/DescribeRecommendationFeedback.yaml"

requestListRecommendations :: ListRecommendations -> TestTree
requestListRecommendations =
  req
    "ListRecommendations"
    "fixture/ListRecommendations.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateCodeReview :: CreateCodeReview -> TestTree
requestCreateCodeReview =
  req
    "CreateCodeReview"
    "fixture/CreateCodeReview.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListCodeReviews :: ListCodeReviews -> TestTree
requestListCodeReviews =
  req
    "ListCodeReviews"
    "fixture/ListCodeReviews.yaml"

requestAssociateRepository :: AssociateRepository -> TestTree
requestAssociateRepository =
  req
    "AssociateRepository"
    "fixture/AssociateRepository.yaml"

requestPutRecommendationFeedback :: PutRecommendationFeedback -> TestTree
requestPutRecommendationFeedback =
  req
    "PutRecommendationFeedback"
    "fixture/PutRecommendationFeedback.yaml"

-- Responses

responseListRecommendationFeedback :: ListRecommendationFeedbackResponse -> TestTree
responseListRecommendationFeedback =
  res
    "ListRecommendationFeedbackResponse"
    "fixture/ListRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendationFeedback)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDisassociateRepository :: DisassociateRepositoryResponse -> TestTree
responseDisassociateRepository =
  res
    "DisassociateRepositoryResponse"
    "fixture/DisassociateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRepository)

responseDescribeRepositoryAssociation :: DescribeRepositoryAssociationResponse -> TestTree
responseDescribeRepositoryAssociation =
  res
    "DescribeRepositoryAssociationResponse"
    "fixture/DescribeRepositoryAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepositoryAssociation)

responseDescribeCodeReview :: DescribeCodeReviewResponse -> TestTree
responseDescribeCodeReview =
  res
    "DescribeCodeReviewResponse"
    "fixture/DescribeCodeReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeReview)

responseListRepositoryAssociations :: ListRepositoryAssociationsResponse -> TestTree
responseListRepositoryAssociations =
  res
    "ListRepositoryAssociationsResponse"
    "fixture/ListRepositoryAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoryAssociations)

responseDescribeRecommendationFeedback :: DescribeRecommendationFeedbackResponse -> TestTree
responseDescribeRecommendationFeedback =
  res
    "DescribeRecommendationFeedbackResponse"
    "fixture/DescribeRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecommendationFeedback)

responseListRecommendations :: ListRecommendationsResponse -> TestTree
responseListRecommendations =
  res
    "ListRecommendationsResponse"
    "fixture/ListRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendations)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateCodeReview :: CreateCodeReviewResponse -> TestTree
responseCreateCodeReview =
  res
    "CreateCodeReviewResponse"
    "fixture/CreateCodeReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeReview)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListCodeReviews :: ListCodeReviewsResponse -> TestTree
responseListCodeReviews =
  res
    "ListCodeReviewsResponse"
    "fixture/ListCodeReviewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeReviews)

responseAssociateRepository :: AssociateRepositoryResponse -> TestTree
responseAssociateRepository =
  res
    "AssociateRepositoryResponse"
    "fixture/AssociateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRepository)

responsePutRecommendationFeedback :: PutRecommendationFeedbackResponse -> TestTree
responsePutRecommendationFeedback =
  res
    "PutRecommendationFeedbackResponse"
    "fixture/PutRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecommendationFeedback)
