{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WellArchitected
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         , requestAssociateProfiles $
--             newAssociateProfiles
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
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestCreateProfileShare $
--             newCreateProfileShare
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
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestDeleteProfileShare $
--             newDeleteProfileShare
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
--         , requestDisassociateProfiles $
--             newDisassociateProfiles
--
--         , requestExportLens $
--             newExportLens
--
--         , requestGetAnswer $
--             newGetAnswer
--
--         , requestGetConsolidatedReport $
--             newGetConsolidatedReport
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
--         , requestGetProfile $
--             newGetProfile
--
--         , requestGetProfileTemplate $
--             newGetProfileTemplate
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
--         , requestListProfileNotifications $
--             newListProfileNotifications
--
--         , requestListProfileShares $
--             newListProfileShares
--
--         , requestListProfiles $
--             newListProfiles
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
--         , requestUpdateProfile $
--             newUpdateProfile
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
--         , requestUpgradeProfileVersion $
--             newUpgradeProfileVersion
--
--           ]

--     , testGroup "response"
--         [ responseAssociateLenses $
--             newAssociateLensesResponse
--
--         , responseAssociateProfiles $
--             newAssociateProfilesResponse
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
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responseCreateProfileShare $
--             newCreateProfileShareResponse
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
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseDeleteProfileShare $
--             newDeleteProfileShareResponse
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
--         , responseDisassociateProfiles $
--             newDisassociateProfilesResponse
--
--         , responseExportLens $
--             newExportLensResponse
--
--         , responseGetAnswer $
--             newGetAnswerResponse
--
--         , responseGetConsolidatedReport $
--             newGetConsolidatedReportResponse
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
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseGetProfileTemplate $
--             newGetProfileTemplateResponse
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
--         , responseListProfileNotifications $
--             newListProfileNotificationsResponse
--
--         , responseListProfileShares $
--             newListProfileSharesResponse
--
--         , responseListProfiles $
--             newListProfilesResponse
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
--         , responseUpdateProfile $
--             newUpdateProfileResponse
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
--         , responseUpgradeProfileVersion $
--             newUpgradeProfileVersionResponse
--
--           ]
--     ]

-- Requests

requestAssociateLenses :: AssociateLenses -> TestTree
requestAssociateLenses =
  req
    "AssociateLenses"
    "fixture/AssociateLenses.yaml"

requestAssociateProfiles :: AssociateProfiles -> TestTree
requestAssociateProfiles =
  req
    "AssociateProfiles"
    "fixture/AssociateProfiles.yaml"

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

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestCreateProfileShare :: CreateProfileShare -> TestTree
requestCreateProfileShare =
  req
    "CreateProfileShare"
    "fixture/CreateProfileShare.yaml"

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

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestDeleteProfileShare :: DeleteProfileShare -> TestTree
requestDeleteProfileShare =
  req
    "DeleteProfileShare"
    "fixture/DeleteProfileShare.yaml"

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

requestDisassociateProfiles :: DisassociateProfiles -> TestTree
requestDisassociateProfiles =
  req
    "DisassociateProfiles"
    "fixture/DisassociateProfiles.yaml"

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

requestGetConsolidatedReport :: GetConsolidatedReport -> TestTree
requestGetConsolidatedReport =
  req
    "GetConsolidatedReport"
    "fixture/GetConsolidatedReport.yaml"

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

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestGetProfileTemplate :: GetProfileTemplate -> TestTree
requestGetProfileTemplate =
  req
    "GetProfileTemplate"
    "fixture/GetProfileTemplate.yaml"

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

requestListProfileNotifications :: ListProfileNotifications -> TestTree
requestListProfileNotifications =
  req
    "ListProfileNotifications"
    "fixture/ListProfileNotifications.yaml"

requestListProfileShares :: ListProfileShares -> TestTree
requestListProfileShares =
  req
    "ListProfileShares"
    "fixture/ListProfileShares.yaml"

requestListProfiles :: ListProfiles -> TestTree
requestListProfiles =
  req
    "ListProfiles"
    "fixture/ListProfiles.yaml"

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

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

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

requestUpgradeProfileVersion :: UpgradeProfileVersion -> TestTree
requestUpgradeProfileVersion =
  req
    "UpgradeProfileVersion"
    "fixture/UpgradeProfileVersion.yaml"

-- Responses

responseAssociateLenses :: AssociateLensesResponse -> TestTree
responseAssociateLenses =
  res
    "AssociateLensesResponse"
    "fixture/AssociateLensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLenses)

responseAssociateProfiles :: AssociateProfilesResponse -> TestTree
responseAssociateProfiles =
  res
    "AssociateProfilesResponse"
    "fixture/AssociateProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateProfiles)

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

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responseCreateProfileShare :: CreateProfileShareResponse -> TestTree
responseCreateProfileShare =
  res
    "CreateProfileShareResponse"
    "fixture/CreateProfileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfileShare)

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

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

responseDeleteProfileShare :: DeleteProfileShareResponse -> TestTree
responseDeleteProfileShare =
  res
    "DeleteProfileShareResponse"
    "fixture/DeleteProfileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileShare)

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

responseDisassociateProfiles :: DisassociateProfilesResponse -> TestTree
responseDisassociateProfiles =
  res
    "DisassociateProfilesResponse"
    "fixture/DisassociateProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateProfiles)

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

responseGetConsolidatedReport :: GetConsolidatedReportResponse -> TestTree
responseGetConsolidatedReport =
  res
    "GetConsolidatedReportResponse"
    "fixture/GetConsolidatedReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConsolidatedReport)

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

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfile)

responseGetProfileTemplate :: GetProfileTemplateResponse -> TestTree
responseGetProfileTemplate =
  res
    "GetProfileTemplateResponse"
    "fixture/GetProfileTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfileTemplate)

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

responseListProfileNotifications :: ListProfileNotificationsResponse -> TestTree
responseListProfileNotifications =
  res
    "ListProfileNotificationsResponse"
    "fixture/ListProfileNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileNotifications)

responseListProfileShares :: ListProfileSharesResponse -> TestTree
responseListProfileShares =
  res
    "ListProfileSharesResponse"
    "fixture/ListProfileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileShares)

responseListProfiles :: ListProfilesResponse -> TestTree
responseListProfiles =
  res
    "ListProfilesResponse"
    "fixture/ListProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfiles)

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

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)

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

responseUpgradeProfileVersion :: UpgradeProfileVersionResponse -> TestTree
responseUpgradeProfileVersion =
  res
    "UpgradeProfileVersionResponse"
    "fixture/UpgradeProfileVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeProfileVersion)
