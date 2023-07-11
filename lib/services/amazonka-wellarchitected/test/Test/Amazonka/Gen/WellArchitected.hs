{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WellArchitected
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WellArchitected where

import Amazonka.WellArchitected
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WellArchitected.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateLenses $
--             newAssociateLenses
--
--         , requestCreateLensShare $
--             newCreateLensShare
--
--         , requestCreateLensVersion $
--             newCreateLensVersion
--
--         , requestCreateMilestone $
--             newCreateMilestone
--
--         , requestCreateWorkload $
--             newCreateWorkload
--
--         , requestCreateWorkloadShare $
--             newCreateWorkloadShare
--
--         , requestDeleteLens $
--             newDeleteLens
--
--         , requestDeleteLensShare $
--             newDeleteLensShare
--
--         , requestDeleteWorkload $
--             newDeleteWorkload
--
--         , requestDeleteWorkloadShare $
--             newDeleteWorkloadShare
--
--         , requestDisassociateLenses $
--             newDisassociateLenses
--
--         , requestExportLens $
--             newExportLens
--
--         , requestGetAnswer $
--             newGetAnswer
--
--         , requestGetLens $
--             newGetLens
--
--         , requestGetLensReview $
--             newGetLensReview
--
--         , requestGetLensReviewReport $
--             newGetLensReviewReport
--
--         , requestGetLensVersionDifference $
--             newGetLensVersionDifference
--
--         , requestGetMilestone $
--             newGetMilestone
--
--         , requestGetWorkload $
--             newGetWorkload
--
--         , requestImportLens $
--             newImportLens
--
--         , requestListAnswers $
--             newListAnswers
--
--         , requestListCheckDetails $
--             newListCheckDetails
--
--         , requestListCheckSummaries $
--             newListCheckSummaries
--
--         , requestListLensReviewImprovements $
--             newListLensReviewImprovements
--
--         , requestListLensReviews $
--             newListLensReviews
--
--         , requestListLensShares $
--             newListLensShares
--
--         , requestListLenses $
--             newListLenses
--
--         , requestListMilestones $
--             newListMilestones
--
--         , requestListNotifications $
--             newListNotifications
--
--         , requestListShareInvitations $
--             newListShareInvitations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkloadShares $
--             newListWorkloadShares
--
--         , requestListWorkloads $
--             newListWorkloads
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAnswer $
--             newUpdateAnswer
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestUpdateLensReview $
--             newUpdateLensReview
--
--         , requestUpdateShareInvitation $
--             newUpdateShareInvitation
--
--         , requestUpdateWorkload $
--             newUpdateWorkload
--
--         , requestUpdateWorkloadShare $
--             newUpdateWorkloadShare
--
--         , requestUpgradeLensReview $
--             newUpgradeLensReview
--
--           ]

--     , testGroup "response"
--         [ responseAssociateLenses $
--             newAssociateLensesResponse
--
--         , responseCreateLensShare $
--             newCreateLensShareResponse
--
--         , responseCreateLensVersion $
--             newCreateLensVersionResponse
--
--         , responseCreateMilestone $
--             newCreateMilestoneResponse
--
--         , responseCreateWorkload $
--             newCreateWorkloadResponse
--
--         , responseCreateWorkloadShare $
--             newCreateWorkloadShareResponse
--
--         , responseDeleteLens $
--             newDeleteLensResponse
--
--         , responseDeleteLensShare $
--             newDeleteLensShareResponse
--
--         , responseDeleteWorkload $
--             newDeleteWorkloadResponse
--
--         , responseDeleteWorkloadShare $
--             newDeleteWorkloadShareResponse
--
--         , responseDisassociateLenses $
--             newDisassociateLensesResponse
--
--         , responseExportLens $
--             newExportLensResponse
--
--         , responseGetAnswer $
--             newGetAnswerResponse
--
--         , responseGetLens $
--             newGetLensResponse
--
--         , responseGetLensReview $
--             newGetLensReviewResponse
--
--         , responseGetLensReviewReport $
--             newGetLensReviewReportResponse
--
--         , responseGetLensVersionDifference $
--             newGetLensVersionDifferenceResponse
--
--         , responseGetMilestone $
--             newGetMilestoneResponse
--
--         , responseGetWorkload $
--             newGetWorkloadResponse
--
--         , responseImportLens $
--             newImportLensResponse
--
--         , responseListAnswers $
--             newListAnswersResponse
--
--         , responseListCheckDetails $
--             newListCheckDetailsResponse
--
--         , responseListCheckSummaries $
--             newListCheckSummariesResponse
--
--         , responseListLensReviewImprovements $
--             newListLensReviewImprovementsResponse
--
--         , responseListLensReviews $
--             newListLensReviewsResponse
--
--         , responseListLensShares $
--             newListLensSharesResponse
--
--         , responseListLenses $
--             newListLensesResponse
--
--         , responseListMilestones $
--             newListMilestonesResponse
--
--         , responseListNotifications $
--             newListNotificationsResponse
--
--         , responseListShareInvitations $
--             newListShareInvitationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkloadShares $
--             newListWorkloadSharesResponse
--
--         , responseListWorkloads $
--             newListWorkloadsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAnswer $
--             newUpdateAnswerResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseUpdateLensReview $
--             newUpdateLensReviewResponse
--
--         , responseUpdateShareInvitation $
--             newUpdateShareInvitationResponse
--
--         , responseUpdateWorkload $
--             newUpdateWorkloadResponse
--
--         , responseUpdateWorkloadShare $
--             newUpdateWorkloadShareResponse
--
--         , responseUpgradeLensReview $
--             newUpgradeLensReviewResponse
--
--           ]
--     ]

-- Requests

requestAssociateLenses :: AssociateLenses -> TestTree
requestAssociateLenses =
  req
    "AssociateLenses"
    "fixture/AssociateLenses.yaml"

requestCreateLensShare :: CreateLensShare -> TestTree
requestCreateLensShare =
  req
    "CreateLensShare"
    "fixture/CreateLensShare.yaml"

requestCreateLensVersion :: CreateLensVersion -> TestTree
requestCreateLensVersion =
  req
    "CreateLensVersion"
    "fixture/CreateLensVersion.yaml"

requestCreateMilestone :: CreateMilestone -> TestTree
requestCreateMilestone =
  req
    "CreateMilestone"
    "fixture/CreateMilestone.yaml"

requestCreateWorkload :: CreateWorkload -> TestTree
requestCreateWorkload =
  req
    "CreateWorkload"
    "fixture/CreateWorkload.yaml"

requestCreateWorkloadShare :: CreateWorkloadShare -> TestTree
requestCreateWorkloadShare =
  req
    "CreateWorkloadShare"
    "fixture/CreateWorkloadShare.yaml"

requestDeleteLens :: DeleteLens -> TestTree
requestDeleteLens =
  req
    "DeleteLens"
    "fixture/DeleteLens.yaml"

requestDeleteLensShare :: DeleteLensShare -> TestTree
requestDeleteLensShare =
  req
    "DeleteLensShare"
    "fixture/DeleteLensShare.yaml"

requestDeleteWorkload :: DeleteWorkload -> TestTree
requestDeleteWorkload =
  req
    "DeleteWorkload"
    "fixture/DeleteWorkload.yaml"

requestDeleteWorkloadShare :: DeleteWorkloadShare -> TestTree
requestDeleteWorkloadShare =
  req
    "DeleteWorkloadShare"
    "fixture/DeleteWorkloadShare.yaml"

requestDisassociateLenses :: DisassociateLenses -> TestTree
requestDisassociateLenses =
  req
    "DisassociateLenses"
    "fixture/DisassociateLenses.yaml"

requestExportLens :: ExportLens -> TestTree
requestExportLens =
  req
    "ExportLens"
    "fixture/ExportLens.yaml"

requestGetAnswer :: GetAnswer -> TestTree
requestGetAnswer =
  req
    "GetAnswer"
    "fixture/GetAnswer.yaml"

requestGetLens :: GetLens -> TestTree
requestGetLens =
  req
    "GetLens"
    "fixture/GetLens.yaml"

requestGetLensReview :: GetLensReview -> TestTree
requestGetLensReview =
  req
    "GetLensReview"
    "fixture/GetLensReview.yaml"

requestGetLensReviewReport :: GetLensReviewReport -> TestTree
requestGetLensReviewReport =
  req
    "GetLensReviewReport"
    "fixture/GetLensReviewReport.yaml"

requestGetLensVersionDifference :: GetLensVersionDifference -> TestTree
requestGetLensVersionDifference =
  req
    "GetLensVersionDifference"
    "fixture/GetLensVersionDifference.yaml"

requestGetMilestone :: GetMilestone -> TestTree
requestGetMilestone =
  req
    "GetMilestone"
    "fixture/GetMilestone.yaml"

requestGetWorkload :: GetWorkload -> TestTree
requestGetWorkload =
  req
    "GetWorkload"
    "fixture/GetWorkload.yaml"

requestImportLens :: ImportLens -> TestTree
requestImportLens =
  req
    "ImportLens"
    "fixture/ImportLens.yaml"

requestListAnswers :: ListAnswers -> TestTree
requestListAnswers =
  req
    "ListAnswers"
    "fixture/ListAnswers.yaml"

requestListCheckDetails :: ListCheckDetails -> TestTree
requestListCheckDetails =
  req
    "ListCheckDetails"
    "fixture/ListCheckDetails.yaml"

requestListCheckSummaries :: ListCheckSummaries -> TestTree
requestListCheckSummaries =
  req
    "ListCheckSummaries"
    "fixture/ListCheckSummaries.yaml"

requestListLensReviewImprovements :: ListLensReviewImprovements -> TestTree
requestListLensReviewImprovements =
  req
    "ListLensReviewImprovements"
    "fixture/ListLensReviewImprovements.yaml"

requestListLensReviews :: ListLensReviews -> TestTree
requestListLensReviews =
  req
    "ListLensReviews"
    "fixture/ListLensReviews.yaml"

requestListLensShares :: ListLensShares -> TestTree
requestListLensShares =
  req
    "ListLensShares"
    "fixture/ListLensShares.yaml"

requestListLenses :: ListLenses -> TestTree
requestListLenses =
  req
    "ListLenses"
    "fixture/ListLenses.yaml"

requestListMilestones :: ListMilestones -> TestTree
requestListMilestones =
  req
    "ListMilestones"
    "fixture/ListMilestones.yaml"

requestListNotifications :: ListNotifications -> TestTree
requestListNotifications =
  req
    "ListNotifications"
    "fixture/ListNotifications.yaml"

requestListShareInvitations :: ListShareInvitations -> TestTree
requestListShareInvitations =
  req
    "ListShareInvitations"
    "fixture/ListShareInvitations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkloadShares :: ListWorkloadShares -> TestTree
requestListWorkloadShares =
  req
    "ListWorkloadShares"
    "fixture/ListWorkloadShares.yaml"

requestListWorkloads :: ListWorkloads -> TestTree
requestListWorkloads =
  req
    "ListWorkloads"
    "fixture/ListWorkloads.yaml"

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

requestUpdateAnswer :: UpdateAnswer -> TestTree
requestUpdateAnswer =
  req
    "UpdateAnswer"
    "fixture/UpdateAnswer.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestUpdateLensReview :: UpdateLensReview -> TestTree
requestUpdateLensReview =
  req
    "UpdateLensReview"
    "fixture/UpdateLensReview.yaml"

requestUpdateShareInvitation :: UpdateShareInvitation -> TestTree
requestUpdateShareInvitation =
  req
    "UpdateShareInvitation"
    "fixture/UpdateShareInvitation.yaml"

requestUpdateWorkload :: UpdateWorkload -> TestTree
requestUpdateWorkload =
  req
    "UpdateWorkload"
    "fixture/UpdateWorkload.yaml"

requestUpdateWorkloadShare :: UpdateWorkloadShare -> TestTree
requestUpdateWorkloadShare =
  req
    "UpdateWorkloadShare"
    "fixture/UpdateWorkloadShare.yaml"

requestUpgradeLensReview :: UpgradeLensReview -> TestTree
requestUpgradeLensReview =
  req
    "UpgradeLensReview"
    "fixture/UpgradeLensReview.yaml"

-- Responses

responseAssociateLenses :: AssociateLensesResponse -> TestTree
responseAssociateLenses =
  res
    "AssociateLensesResponse"
    "fixture/AssociateLensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLenses)

responseCreateLensShare :: CreateLensShareResponse -> TestTree
responseCreateLensShare =
  res
    "CreateLensShareResponse"
    "fixture/CreateLensShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLensShare)

responseCreateLensVersion :: CreateLensVersionResponse -> TestTree
responseCreateLensVersion =
  res
    "CreateLensVersionResponse"
    "fixture/CreateLensVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLensVersion)

responseCreateMilestone :: CreateMilestoneResponse -> TestTree
responseCreateMilestone =
  res
    "CreateMilestoneResponse"
    "fixture/CreateMilestoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMilestone)

responseCreateWorkload :: CreateWorkloadResponse -> TestTree
responseCreateWorkload =
  res
    "CreateWorkloadResponse"
    "fixture/CreateWorkloadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkload)

responseCreateWorkloadShare :: CreateWorkloadShareResponse -> TestTree
responseCreateWorkloadShare =
  res
    "CreateWorkloadShareResponse"
    "fixture/CreateWorkloadShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkloadShare)

responseDeleteLens :: DeleteLensResponse -> TestTree
responseDeleteLens =
  res
    "DeleteLensResponse"
    "fixture/DeleteLensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLens)

responseDeleteLensShare :: DeleteLensShareResponse -> TestTree
responseDeleteLensShare =
  res
    "DeleteLensShareResponse"
    "fixture/DeleteLensShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLensShare)

responseDeleteWorkload :: DeleteWorkloadResponse -> TestTree
responseDeleteWorkload =
  res
    "DeleteWorkloadResponse"
    "fixture/DeleteWorkloadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkload)

responseDeleteWorkloadShare :: DeleteWorkloadShareResponse -> TestTree
responseDeleteWorkloadShare =
  res
    "DeleteWorkloadShareResponse"
    "fixture/DeleteWorkloadShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkloadShare)

responseDisassociateLenses :: DisassociateLensesResponse -> TestTree
responseDisassociateLenses =
  res
    "DisassociateLensesResponse"
    "fixture/DisassociateLensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLenses)

responseExportLens :: ExportLensResponse -> TestTree
responseExportLens =
  res
    "ExportLensResponse"
    "fixture/ExportLensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportLens)

responseGetAnswer :: GetAnswerResponse -> TestTree
responseGetAnswer =
  res
    "GetAnswerResponse"
    "fixture/GetAnswerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnswer)

responseGetLens :: GetLensResponse -> TestTree
responseGetLens =
  res
    "GetLensResponse"
    "fixture/GetLensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLens)

responseGetLensReview :: GetLensReviewResponse -> TestTree
responseGetLensReview =
  res
    "GetLensReviewResponse"
    "fixture/GetLensReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLensReview)

responseGetLensReviewReport :: GetLensReviewReportResponse -> TestTree
responseGetLensReviewReport =
  res
    "GetLensReviewReportResponse"
    "fixture/GetLensReviewReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLensReviewReport)

responseGetLensVersionDifference :: GetLensVersionDifferenceResponse -> TestTree
responseGetLensVersionDifference =
  res
    "GetLensVersionDifferenceResponse"
    "fixture/GetLensVersionDifferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLensVersionDifference)

responseGetMilestone :: GetMilestoneResponse -> TestTree
responseGetMilestone =
  res
    "GetMilestoneResponse"
    "fixture/GetMilestoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMilestone)

responseGetWorkload :: GetWorkloadResponse -> TestTree
responseGetWorkload =
  res
    "GetWorkloadResponse"
    "fixture/GetWorkloadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkload)

responseImportLens :: ImportLensResponse -> TestTree
responseImportLens =
  res
    "ImportLensResponse"
    "fixture/ImportLensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportLens)

responseListAnswers :: ListAnswersResponse -> TestTree
responseListAnswers =
  res
    "ListAnswersResponse"
    "fixture/ListAnswersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnswers)

responseListCheckDetails :: ListCheckDetailsResponse -> TestTree
responseListCheckDetails =
  res
    "ListCheckDetailsResponse"
    "fixture/ListCheckDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCheckDetails)

responseListCheckSummaries :: ListCheckSummariesResponse -> TestTree
responseListCheckSummaries =
  res
    "ListCheckSummariesResponse"
    "fixture/ListCheckSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCheckSummaries)

responseListLensReviewImprovements :: ListLensReviewImprovementsResponse -> TestTree
responseListLensReviewImprovements =
  res
    "ListLensReviewImprovementsResponse"
    "fixture/ListLensReviewImprovementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLensReviewImprovements)

responseListLensReviews :: ListLensReviewsResponse -> TestTree
responseListLensReviews =
  res
    "ListLensReviewsResponse"
    "fixture/ListLensReviewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLensReviews)

responseListLensShares :: ListLensSharesResponse -> TestTree
responseListLensShares =
  res
    "ListLensSharesResponse"
    "fixture/ListLensSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLensShares)

responseListLenses :: ListLensesResponse -> TestTree
responseListLenses =
  res
    "ListLensesResponse"
    "fixture/ListLensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLenses)

responseListMilestones :: ListMilestonesResponse -> TestTree
responseListMilestones =
  res
    "ListMilestonesResponse"
    "fixture/ListMilestonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMilestones)

responseListNotifications :: ListNotificationsResponse -> TestTree
responseListNotifications =
  res
    "ListNotificationsResponse"
    "fixture/ListNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotifications)

responseListShareInvitations :: ListShareInvitationsResponse -> TestTree
responseListShareInvitations =
  res
    "ListShareInvitationsResponse"
    "fixture/ListShareInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListShareInvitations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkloadShares :: ListWorkloadSharesResponse -> TestTree
responseListWorkloadShares =
  res
    "ListWorkloadSharesResponse"
    "fixture/ListWorkloadSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkloadShares)

responseListWorkloads :: ListWorkloadsResponse -> TestTree
responseListWorkloads =
  res
    "ListWorkloadsResponse"
    "fixture/ListWorkloadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkloads)

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

responseUpdateAnswer :: UpdateAnswerResponse -> TestTree
responseUpdateAnswer =
  res
    "UpdateAnswerResponse"
    "fixture/UpdateAnswerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnswer)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalSettings)

responseUpdateLensReview :: UpdateLensReviewResponse -> TestTree
responseUpdateLensReview =
  res
    "UpdateLensReviewResponse"
    "fixture/UpdateLensReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLensReview)

responseUpdateShareInvitation :: UpdateShareInvitationResponse -> TestTree
responseUpdateShareInvitation =
  res
    "UpdateShareInvitationResponse"
    "fixture/UpdateShareInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateShareInvitation)

responseUpdateWorkload :: UpdateWorkloadResponse -> TestTree
responseUpdateWorkload =
  res
    "UpdateWorkloadResponse"
    "fixture/UpdateWorkloadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkload)

responseUpdateWorkloadShare :: UpdateWorkloadShareResponse -> TestTree
responseUpdateWorkloadShare =
  res
    "UpdateWorkloadShareResponse"
    "fixture/UpdateWorkloadShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkloadShare)

responseUpgradeLensReview :: UpgradeLensReviewResponse -> TestTree
responseUpgradeLensReview =
  res
    "UpgradeLensReviewResponse"
    "fixture/UpgradeLensReviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeLensReview)
