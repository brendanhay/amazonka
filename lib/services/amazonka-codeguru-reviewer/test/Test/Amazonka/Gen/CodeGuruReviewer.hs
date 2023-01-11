{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeGuruReviewer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeGuruReviewer where

import Amazonka.CodeGuruReviewer
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeGuruReviewer.Internal
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
--         [ requestAssociateRepository $
--             newAssociateRepository
--
--         , requestCreateCodeReview $
--             newCreateCodeReview
--
--         , requestDescribeCodeReview $
--             newDescribeCodeReview
--
--         , requestDescribeRecommendationFeedback $
--             newDescribeRecommendationFeedback
--
--         , requestDescribeRepositoryAssociation $
--             newDescribeRepositoryAssociation
--
--         , requestDisassociateRepository $
--             newDisassociateRepository
--
--         , requestListCodeReviews $
--             newListCodeReviews
--
--         , requestListRecommendationFeedback $
--             newListRecommendationFeedback
--
--         , requestListRecommendations $
--             newListRecommendations
--
--         , requestListRepositoryAssociations $
--             newListRepositoryAssociations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutRecommendationFeedback $
--             newPutRecommendationFeedback
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateRepository $
--             newAssociateRepositoryResponse
--
--         , responseCreateCodeReview $
--             newCreateCodeReviewResponse
--
--         , responseDescribeCodeReview $
--             newDescribeCodeReviewResponse
--
--         , responseDescribeRecommendationFeedback $
--             newDescribeRecommendationFeedbackResponse
--
--         , responseDescribeRepositoryAssociation $
--             newDescribeRepositoryAssociationResponse
--
--         , responseDisassociateRepository $
--             newDisassociateRepositoryResponse
--
--         , responseListCodeReviews $
--             newListCodeReviewsResponse
--
--         , responseListRecommendationFeedback $
--             newListRecommendationFeedbackResponse
--
--         , responseListRecommendations $
--             newListRecommendationsResponse
--
--         , responseListRepositoryAssociations $
--             newListRepositoryAssociationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutRecommendationFeedback $
--             newPutRecommendationFeedbackResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAssociateRepository :: AssociateRepository -> TestTree
requestAssociateRepository =
  req
    "AssociateRepository"
    "fixture/AssociateRepository.yaml"

requestCreateCodeReview :: CreateCodeReview -> TestTree
requestCreateCodeReview =
  req
    "CreateCodeReview"
    "fixture/CreateCodeReview.yaml"

requestDescribeCodeReview :: DescribeCodeReview -> TestTree
requestDescribeCodeReview =
  req
    "DescribeCodeReview"
    "fixture/DescribeCodeReview.yaml"

requestDescribeRecommendationFeedback :: DescribeRecommendationFeedback -> TestTree
requestDescribeRecommendationFeedback =
  req
    "DescribeRecommendationFeedback"
    "fixture/DescribeRecommendationFeedback.yaml"

requestDescribeRepositoryAssociation :: DescribeRepositoryAssociation -> TestTree
requestDescribeRepositoryAssociation =
  req
    "DescribeRepositoryAssociation"
    "fixture/DescribeRepositoryAssociation.yaml"

requestDisassociateRepository :: DisassociateRepository -> TestTree
requestDisassociateRepository =
  req
    "DisassociateRepository"
    "fixture/DisassociateRepository.yaml"

requestListCodeReviews :: ListCodeReviews -> TestTree
requestListCodeReviews =
  req
    "ListCodeReviews"
    "fixture/ListCodeReviews.yaml"

requestListRecommendationFeedback :: ListRecommendationFeedback -> TestTree
requestListRecommendationFeedback =
  req
    "ListRecommendationFeedback"
    "fixture/ListRecommendationFeedback.yaml"

requestListRecommendations :: ListRecommendations -> TestTree
requestListRecommendations =
  req
    "ListRecommendations"
    "fixture/ListRecommendations.yaml"

requestListRepositoryAssociations :: ListRepositoryAssociations -> TestTree
requestListRepositoryAssociations =
  req
    "ListRepositoryAssociations"
    "fixture/ListRepositoryAssociations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutRecommendationFeedback :: PutRecommendationFeedback -> TestTree
requestPutRecommendationFeedback =
  req
    "PutRecommendationFeedback"
    "fixture/PutRecommendationFeedback.yaml"

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

-- Responses

responseAssociateRepository :: AssociateRepositoryResponse -> TestTree
responseAssociateRepository =
  res
    "AssociateRepositoryResponse"
    "fixture/AssociateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRepository)

responseCreateCodeReview :: CreateCodeReviewResponse -> TestTree
responseCreateCodeReview =
  res
    "CreateCodeReviewResponse"
    "fixture/CreateCodeReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeReview)

responseDescribeCodeReview :: DescribeCodeReviewResponse -> TestTree
responseDescribeCodeReview =
  res
    "DescribeCodeReviewResponse"
    "fixture/DescribeCodeReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeReview)

responseDescribeRecommendationFeedback :: DescribeRecommendationFeedbackResponse -> TestTree
responseDescribeRecommendationFeedback =
  res
    "DescribeRecommendationFeedbackResponse"
    "fixture/DescribeRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecommendationFeedback)

responseDescribeRepositoryAssociation :: DescribeRepositoryAssociationResponse -> TestTree
responseDescribeRepositoryAssociation =
  res
    "DescribeRepositoryAssociationResponse"
    "fixture/DescribeRepositoryAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepositoryAssociation)

responseDisassociateRepository :: DisassociateRepositoryResponse -> TestTree
responseDisassociateRepository =
  res
    "DisassociateRepositoryResponse"
    "fixture/DisassociateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRepository)

responseListCodeReviews :: ListCodeReviewsResponse -> TestTree
responseListCodeReviews =
  res
    "ListCodeReviewsResponse"
    "fixture/ListCodeReviewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeReviews)

responseListRecommendationFeedback :: ListRecommendationFeedbackResponse -> TestTree
responseListRecommendationFeedback =
  res
    "ListRecommendationFeedbackResponse"
    "fixture/ListRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendationFeedback)

responseListRecommendations :: ListRecommendationsResponse -> TestTree
responseListRecommendations =
  res
    "ListRecommendationsResponse"
    "fixture/ListRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendations)

responseListRepositoryAssociations :: ListRepositoryAssociationsResponse -> TestTree
responseListRepositoryAssociations =
  res
    "ListRepositoryAssociationsResponse"
    "fixture/ListRepositoryAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoryAssociations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutRecommendationFeedback :: PutRecommendationFeedbackResponse -> TestTree
responsePutRecommendationFeedback =
  res
    "PutRecommendationFeedbackResponse"
    "fixture/PutRecommendationFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecommendationFeedback)

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
