{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WellArchitected
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.WellArchitected where

import Data.Proxy
import Network.AWS.WellArchitected
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WellArchitected.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListNotifications $
--             newListNotifications
--
--         , requestGetLensVersionDifference $
--             newGetLensVersionDifference
--
--         , requestListLensReviewImprovements $
--             newListLensReviewImprovements
--
--         , requestListMilestones $
--             newListMilestones
--
--         , requestCreateMilestone $
--             newCreateMilestone
--
--         , requestGetAnswer $
--             newGetAnswer
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateAnswer $
--             newUpdateAnswer
--
--         , requestUpdateShareInvitation $
--             newUpdateShareInvitation
--
--         , requestListAnswers $
--             newListAnswers
--
--         , requestDisassociateLenses $
--             newDisassociateLenses
--
--         , requestGetMilestone $
--             newGetMilestone
--
--         , requestListLenses $
--             newListLenses
--
--         , requestListWorkloadShares $
--             newListWorkloadShares
--
--         , requestUpdateWorkload $
--             newUpdateWorkload
--
--         , requestDeleteWorkload $
--             newDeleteWorkload
--
--         , requestListLensReviews $
--             newListLensReviews
--
--         , requestUpdateLensReview $
--             newUpdateLensReview
--
--         , requestListShareInvitations $
--             newListShareInvitations
--
--         , requestGetLensReview $
--             newGetLensReview
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateWorkload $
--             newCreateWorkload
--
--         , requestDeleteWorkloadShare $
--             newDeleteWorkloadShare
--
--         , requestUpdateWorkloadShare $
--             newUpdateWorkloadShare
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAssociateLenses $
--             newAssociateLenses
--
--         , requestListWorkloads $
--             newListWorkloads
--
--         , requestCreateWorkloadShare $
--             newCreateWorkloadShare
--
--         , requestGetLensReviewReport $
--             newGetLensReviewReport
--
--         , requestUpgradeLensReview $
--             newUpgradeLensReview
--
--         , requestGetWorkload $
--             newGetWorkload
--
--           ]

--     , testGroup "response"
--         [ responseListNotifications $
--             newListNotificationsResponse
--
--         , responseGetLensVersionDifference $
--             newGetLensVersionDifferenceResponse
--
--         , responseListLensReviewImprovements $
--             newListLensReviewImprovementsResponse
--
--         , responseListMilestones $
--             newListMilestonesResponse
--
--         , responseCreateMilestone $
--             newCreateMilestoneResponse
--
--         , responseGetAnswer $
--             newGetAnswerResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateAnswer $
--             newUpdateAnswerResponse
--
--         , responseUpdateShareInvitation $
--             newUpdateShareInvitationResponse
--
--         , responseListAnswers $
--             newListAnswersResponse
--
--         , responseDisassociateLenses $
--             newDisassociateLensesResponse
--
--         , responseGetMilestone $
--             newGetMilestoneResponse
--
--         , responseListLenses $
--             newListLensesResponse
--
--         , responseListWorkloadShares $
--             newListWorkloadSharesResponse
--
--         , responseUpdateWorkload $
--             newUpdateWorkloadResponse
--
--         , responseDeleteWorkload $
--             newDeleteWorkloadResponse
--
--         , responseListLensReviews $
--             newListLensReviewsResponse
--
--         , responseUpdateLensReview $
--             newUpdateLensReviewResponse
--
--         , responseListShareInvitations $
--             newListShareInvitationsResponse
--
--         , responseGetLensReview $
--             newGetLensReviewResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateWorkload $
--             newCreateWorkloadResponse
--
--         , responseDeleteWorkloadShare $
--             newDeleteWorkloadShareResponse
--
--         , responseUpdateWorkloadShare $
--             newUpdateWorkloadShareResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAssociateLenses $
--             newAssociateLensesResponse
--
--         , responseListWorkloads $
--             newListWorkloadsResponse
--
--         , responseCreateWorkloadShare $
--             newCreateWorkloadShareResponse
--
--         , responseGetLensReviewReport $
--             newGetLensReviewReportResponse
--
--         , responseUpgradeLensReview $
--             newUpgradeLensReviewResponse
--
--         , responseGetWorkload $
--             newGetWorkloadResponse
--
--           ]
--     ]

-- Requests

requestListNotifications :: ListNotifications -> TestTree
requestListNotifications =
  req
    "ListNotifications"
    "fixture/ListNotifications.yaml"

requestGetLensVersionDifference :: GetLensVersionDifference -> TestTree
requestGetLensVersionDifference =
  req
    "GetLensVersionDifference"
    "fixture/GetLensVersionDifference.yaml"

requestListLensReviewImprovements :: ListLensReviewImprovements -> TestTree
requestListLensReviewImprovements =
  req
    "ListLensReviewImprovements"
    "fixture/ListLensReviewImprovements.yaml"

requestListMilestones :: ListMilestones -> TestTree
requestListMilestones =
  req
    "ListMilestones"
    "fixture/ListMilestones.yaml"

requestCreateMilestone :: CreateMilestone -> TestTree
requestCreateMilestone =
  req
    "CreateMilestone"
    "fixture/CreateMilestone.yaml"

requestGetAnswer :: GetAnswer -> TestTree
requestGetAnswer =
  req
    "GetAnswer"
    "fixture/GetAnswer.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestUpdateAnswer :: UpdateAnswer -> TestTree
requestUpdateAnswer =
  req
    "UpdateAnswer"
    "fixture/UpdateAnswer.yaml"

requestUpdateShareInvitation :: UpdateShareInvitation -> TestTree
requestUpdateShareInvitation =
  req
    "UpdateShareInvitation"
    "fixture/UpdateShareInvitation.yaml"

requestListAnswers :: ListAnswers -> TestTree
requestListAnswers =
  req
    "ListAnswers"
    "fixture/ListAnswers.yaml"

requestDisassociateLenses :: DisassociateLenses -> TestTree
requestDisassociateLenses =
  req
    "DisassociateLenses"
    "fixture/DisassociateLenses.yaml"

requestGetMilestone :: GetMilestone -> TestTree
requestGetMilestone =
  req
    "GetMilestone"
    "fixture/GetMilestone.yaml"

requestListLenses :: ListLenses -> TestTree
requestListLenses =
  req
    "ListLenses"
    "fixture/ListLenses.yaml"

requestListWorkloadShares :: ListWorkloadShares -> TestTree
requestListWorkloadShares =
  req
    "ListWorkloadShares"
    "fixture/ListWorkloadShares.yaml"

requestUpdateWorkload :: UpdateWorkload -> TestTree
requestUpdateWorkload =
  req
    "UpdateWorkload"
    "fixture/UpdateWorkload.yaml"

requestDeleteWorkload :: DeleteWorkload -> TestTree
requestDeleteWorkload =
  req
    "DeleteWorkload"
    "fixture/DeleteWorkload.yaml"

requestListLensReviews :: ListLensReviews -> TestTree
requestListLensReviews =
  req
    "ListLensReviews"
    "fixture/ListLensReviews.yaml"

requestUpdateLensReview :: UpdateLensReview -> TestTree
requestUpdateLensReview =
  req
    "UpdateLensReview"
    "fixture/UpdateLensReview.yaml"

requestListShareInvitations :: ListShareInvitations -> TestTree
requestListShareInvitations =
  req
    "ListShareInvitations"
    "fixture/ListShareInvitations.yaml"

requestGetLensReview :: GetLensReview -> TestTree
requestGetLensReview =
  req
    "GetLensReview"
    "fixture/GetLensReview.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateWorkload :: CreateWorkload -> TestTree
requestCreateWorkload =
  req
    "CreateWorkload"
    "fixture/CreateWorkload.yaml"

requestDeleteWorkloadShare :: DeleteWorkloadShare -> TestTree
requestDeleteWorkloadShare =
  req
    "DeleteWorkloadShare"
    "fixture/DeleteWorkloadShare.yaml"

requestUpdateWorkloadShare :: UpdateWorkloadShare -> TestTree
requestUpdateWorkloadShare =
  req
    "UpdateWorkloadShare"
    "fixture/UpdateWorkloadShare.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAssociateLenses :: AssociateLenses -> TestTree
requestAssociateLenses =
  req
    "AssociateLenses"
    "fixture/AssociateLenses.yaml"

requestListWorkloads :: ListWorkloads -> TestTree
requestListWorkloads =
  req
    "ListWorkloads"
    "fixture/ListWorkloads.yaml"

requestCreateWorkloadShare :: CreateWorkloadShare -> TestTree
requestCreateWorkloadShare =
  req
    "CreateWorkloadShare"
    "fixture/CreateWorkloadShare.yaml"

requestGetLensReviewReport :: GetLensReviewReport -> TestTree
requestGetLensReviewReport =
  req
    "GetLensReviewReport"
    "fixture/GetLensReviewReport.yaml"

requestUpgradeLensReview :: UpgradeLensReview -> TestTree
requestUpgradeLensReview =
  req
    "UpgradeLensReview"
    "fixture/UpgradeLensReview.yaml"

requestGetWorkload :: GetWorkload -> TestTree
requestGetWorkload =
  req
    "GetWorkload"
    "fixture/GetWorkload.yaml"

-- Responses

responseListNotifications :: ListNotificationsResponse -> TestTree
responseListNotifications =
  res
    "ListNotificationsResponse"
    "fixture/ListNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotifications)

responseGetLensVersionDifference :: GetLensVersionDifferenceResponse -> TestTree
responseGetLensVersionDifference =
  res
    "GetLensVersionDifferenceResponse"
    "fixture/GetLensVersionDifferenceResponse.proto"
    defaultService
    (Proxy :: Proxy GetLensVersionDifference)

responseListLensReviewImprovements :: ListLensReviewImprovementsResponse -> TestTree
responseListLensReviewImprovements =
  res
    "ListLensReviewImprovementsResponse"
    "fixture/ListLensReviewImprovementsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLensReviewImprovements)

responseListMilestones :: ListMilestonesResponse -> TestTree
responseListMilestones =
  res
    "ListMilestonesResponse"
    "fixture/ListMilestonesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMilestones)

responseCreateMilestone :: CreateMilestoneResponse -> TestTree
responseCreateMilestone =
  res
    "CreateMilestoneResponse"
    "fixture/CreateMilestoneResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMilestone)

responseGetAnswer :: GetAnswerResponse -> TestTree
responseGetAnswer =
  res
    "GetAnswerResponse"
    "fixture/GetAnswerResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnswer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseUpdateAnswer :: UpdateAnswerResponse -> TestTree
responseUpdateAnswer =
  res
    "UpdateAnswerResponse"
    "fixture/UpdateAnswerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAnswer)

responseUpdateShareInvitation :: UpdateShareInvitationResponse -> TestTree
responseUpdateShareInvitation =
  res
    "UpdateShareInvitationResponse"
    "fixture/UpdateShareInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateShareInvitation)

responseListAnswers :: ListAnswersResponse -> TestTree
responseListAnswers =
  res
    "ListAnswersResponse"
    "fixture/ListAnswersResponse.proto"
    defaultService
    (Proxy :: Proxy ListAnswers)

responseDisassociateLenses :: DisassociateLensesResponse -> TestTree
responseDisassociateLenses =
  res
    "DisassociateLensesResponse"
    "fixture/DisassociateLensesResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateLenses)

responseGetMilestone :: GetMilestoneResponse -> TestTree
responseGetMilestone =
  res
    "GetMilestoneResponse"
    "fixture/GetMilestoneResponse.proto"
    defaultService
    (Proxy :: Proxy GetMilestone)

responseListLenses :: ListLensesResponse -> TestTree
responseListLenses =
  res
    "ListLensesResponse"
    "fixture/ListLensesResponse.proto"
    defaultService
    (Proxy :: Proxy ListLenses)

responseListWorkloadShares :: ListWorkloadSharesResponse -> TestTree
responseListWorkloadShares =
  res
    "ListWorkloadSharesResponse"
    "fixture/ListWorkloadSharesResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkloadShares)

responseUpdateWorkload :: UpdateWorkloadResponse -> TestTree
responseUpdateWorkload =
  res
    "UpdateWorkloadResponse"
    "fixture/UpdateWorkloadResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkload)

responseDeleteWorkload :: DeleteWorkloadResponse -> TestTree
responseDeleteWorkload =
  res
    "DeleteWorkloadResponse"
    "fixture/DeleteWorkloadResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkload)

responseListLensReviews :: ListLensReviewsResponse -> TestTree
responseListLensReviews =
  res
    "ListLensReviewsResponse"
    "fixture/ListLensReviewsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLensReviews)

responseUpdateLensReview :: UpdateLensReviewResponse -> TestTree
responseUpdateLensReview =
  res
    "UpdateLensReviewResponse"
    "fixture/UpdateLensReviewResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLensReview)

responseListShareInvitations :: ListShareInvitationsResponse -> TestTree
responseListShareInvitations =
  res
    "ListShareInvitationsResponse"
    "fixture/ListShareInvitationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListShareInvitations)

responseGetLensReview :: GetLensReviewResponse -> TestTree
responseGetLensReview =
  res
    "GetLensReviewResponse"
    "fixture/GetLensReviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetLensReview)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateWorkload :: CreateWorkloadResponse -> TestTree
responseCreateWorkload =
  res
    "CreateWorkloadResponse"
    "fixture/CreateWorkloadResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkload)

responseDeleteWorkloadShare :: DeleteWorkloadShareResponse -> TestTree
responseDeleteWorkloadShare =
  res
    "DeleteWorkloadShareResponse"
    "fixture/DeleteWorkloadShareResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkloadShare)

responseUpdateWorkloadShare :: UpdateWorkloadShareResponse -> TestTree
responseUpdateWorkloadShare =
  res
    "UpdateWorkloadShareResponse"
    "fixture/UpdateWorkloadShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkloadShare)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseAssociateLenses :: AssociateLensesResponse -> TestTree
responseAssociateLenses =
  res
    "AssociateLensesResponse"
    "fixture/AssociateLensesResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLenses)

responseListWorkloads :: ListWorkloadsResponse -> TestTree
responseListWorkloads =
  res
    "ListWorkloadsResponse"
    "fixture/ListWorkloadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkloads)

responseCreateWorkloadShare :: CreateWorkloadShareResponse -> TestTree
responseCreateWorkloadShare =
  res
    "CreateWorkloadShareResponse"
    "fixture/CreateWorkloadShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkloadShare)

responseGetLensReviewReport :: GetLensReviewReportResponse -> TestTree
responseGetLensReviewReport =
  res
    "GetLensReviewReportResponse"
    "fixture/GetLensReviewReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetLensReviewReport)

responseUpgradeLensReview :: UpgradeLensReviewResponse -> TestTree
responseUpgradeLensReview =
  res
    "UpgradeLensReviewResponse"
    "fixture/UpgradeLensReviewResponse.proto"
    defaultService
    (Proxy :: Proxy UpgradeLensReview)

responseGetWorkload :: GetWorkloadResponse -> TestTree
responseGetWorkload =
  res
    "GetWorkloadResponse"
    "fixture/GetWorkloadResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkload)
