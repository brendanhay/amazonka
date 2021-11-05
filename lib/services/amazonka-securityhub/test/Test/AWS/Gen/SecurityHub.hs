{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SecurityHub
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SecurityHub where

import qualified Data.Proxy as Proxy
import Network.AWS.SecurityHub
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SecurityHub.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestGetAdministratorAccount $
--             newGetAdministratorAccount
--
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
--
--         , requestCreateInsight $
--             newCreateInsight
--
--         , requestAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitation
--
--         , requestDeleteMembers $
--             newDeleteMembers
--
--         , requestDescribeHub $
--             newDescribeHub
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetInsightResults $
--             newGetInsightResults
--
--         , requestEnableImportFindingsForProduct $
--             newEnableImportFindingsForProduct
--
--         , requestDescribeStandards $
--             newDescribeStandards
--
--         , requestDescribeProducts $
--             newDescribeProducts
--
--         , requestDeleteFindingAggregator $
--             newDeleteFindingAggregator
--
--         , requestUpdateFindingAggregator $
--             newUpdateFindingAggregator
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestDeleteInvitations $
--             newDeleteInvitations
--
--         , requestGetEnabledStandards $
--             newGetEnabledStandards
--
--         , requestDeclineInvitations $
--             newDeclineInvitations
--
--         , requestUpdateActionTarget $
--             newUpdateActionTarget
--
--         , requestDeleteActionTarget $
--             newDeleteActionTarget
--
--         , requestUpdateStandardsControl $
--             newUpdateStandardsControl
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDescribeActionTargets $
--             newDescribeActionTargets
--
--         , requestDisassociateMembers $
--             newDisassociateMembers
--
--         , requestListEnabledProductsForImport $
--             newListEnabledProductsForImport
--
--         , requestDescribeStandardsControls $
--             newDescribeStandardsControls
--
--         , requestListMembers $
--             newListMembers
--
--         , requestCreateMembers $
--             newCreateMembers
--
--         , requestBatchImportFindings $
--             newBatchImportFindings
--
--         , requestGetInvitationsCount $
--             newGetInvitationsCount
--
--         , requestDeleteInsight $
--             newDeleteInsight
--
--         , requestUpdateInsight $
--             newUpdateInsight
--
--         , requestDisableImportFindingsForProduct $
--             newDisableImportFindingsForProduct
--
--         , requestUpdateFindings $
--             newUpdateFindings
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--         , requestGetFindingAggregator $
--             newGetFindingAggregator
--
--         , requestInviteMembers $
--             newInviteMembers
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestDisableSecurityHub $
--             newDisableSecurityHub
--
--         , requestListFindingAggregators $
--             newListFindingAggregators
--
--         , requestBatchEnableStandards $
--             newBatchEnableStandards
--
--         , requestCreateFindingAggregator $
--             newCreateFindingAggregator
--
--         , requestBatchDisableStandards $
--             newBatchDisableStandards
--
--         , requestTagResource $
--             newTagResource
--
--         , requestEnableSecurityHub $
--             newEnableSecurityHub
--
--         , requestUpdateSecurityHubConfiguration $
--             newUpdateSecurityHubConfiguration
--
--         , requestGetFindings $
--             newGetFindings
--
--         , requestGetInsights $
--             newGetInsights
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestBatchUpdateFindings $
--             newBatchUpdateFindings
--
--         , requestCreateActionTarget $
--             newCreateActionTarget
--
--         , requestDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccount
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--           ]

--     , testGroup "response"
--         [ responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseGetAdministratorAccount $
--             newGetAdministratorAccountResponse
--
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
--
--         , responseCreateInsight $
--             newCreateInsightResponse
--
--         , responseAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitationResponse
--
--         , responseDeleteMembers $
--             newDeleteMembersResponse
--
--         , responseDescribeHub $
--             newDescribeHubResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetInsightResults $
--             newGetInsightResultsResponse
--
--         , responseEnableImportFindingsForProduct $
--             newEnableImportFindingsForProductResponse
--
--         , responseDescribeStandards $
--             newDescribeStandardsResponse
--
--         , responseDescribeProducts $
--             newDescribeProductsResponse
--
--         , responseDeleteFindingAggregator $
--             newDeleteFindingAggregatorResponse
--
--         , responseUpdateFindingAggregator $
--             newUpdateFindingAggregatorResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseDeleteInvitations $
--             newDeleteInvitationsResponse
--
--         , responseGetEnabledStandards $
--             newGetEnabledStandardsResponse
--
--         , responseDeclineInvitations $
--             newDeclineInvitationsResponse
--
--         , responseUpdateActionTarget $
--             newUpdateActionTargetResponse
--
--         , responseDeleteActionTarget $
--             newDeleteActionTargetResponse
--
--         , responseUpdateStandardsControl $
--             newUpdateStandardsControlResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDescribeActionTargets $
--             newDescribeActionTargetsResponse
--
--         , responseDisassociateMembers $
--             newDisassociateMembersResponse
--
--         , responseListEnabledProductsForImport $
--             newListEnabledProductsForImportResponse
--
--         , responseDescribeStandardsControls $
--             newDescribeStandardsControlsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseCreateMembers $
--             newCreateMembersResponse
--
--         , responseBatchImportFindings $
--             newBatchImportFindingsResponse
--
--         , responseGetInvitationsCount $
--             newGetInvitationsCountResponse
--
--         , responseDeleteInsight $
--             newDeleteInsightResponse
--
--         , responseUpdateInsight $
--             newUpdateInsightResponse
--
--         , responseDisableImportFindingsForProduct $
--             newDisableImportFindingsForProductResponse
--
--         , responseUpdateFindings $
--             newUpdateFindingsResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--         , responseGetFindingAggregator $
--             newGetFindingAggregatorResponse
--
--         , responseInviteMembers $
--             newInviteMembersResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseDisableSecurityHub $
--             newDisableSecurityHubResponse
--
--         , responseListFindingAggregators $
--             newListFindingAggregatorsResponse
--
--         , responseBatchEnableStandards $
--             newBatchEnableStandardsResponse
--
--         , responseCreateFindingAggregator $
--             newCreateFindingAggregatorResponse
--
--         , responseBatchDisableStandards $
--             newBatchDisableStandardsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseEnableSecurityHub $
--             newEnableSecurityHubResponse
--
--         , responseUpdateSecurityHubConfiguration $
--             newUpdateSecurityHubConfigurationResponse
--
--         , responseGetFindings $
--             newGetFindingsResponse
--
--         , responseGetInsights $
--             newGetInsightsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseBatchUpdateFindings $
--             newBatchUpdateFindingsResponse
--
--         , responseCreateActionTarget $
--             newCreateActionTargetResponse
--
--         , responseDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccountResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--           ]
--     ]

-- Requests

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestGetAdministratorAccount :: GetAdministratorAccount -> TestTree
requestGetAdministratorAccount =
  req
    "GetAdministratorAccount"
    "fixture/GetAdministratorAccount.yaml"

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

requestCreateInsight :: CreateInsight -> TestTree
requestCreateInsight =
  req
    "CreateInsight"
    "fixture/CreateInsight.yaml"

requestAcceptAdministratorInvitation :: AcceptAdministratorInvitation -> TestTree
requestAcceptAdministratorInvitation =
  req
    "AcceptAdministratorInvitation"
    "fixture/AcceptAdministratorInvitation.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestDescribeHub :: DescribeHub -> TestTree
requestDescribeHub =
  req
    "DescribeHub"
    "fixture/DescribeHub.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetInsightResults :: GetInsightResults -> TestTree
requestGetInsightResults =
  req
    "GetInsightResults"
    "fixture/GetInsightResults.yaml"

requestEnableImportFindingsForProduct :: EnableImportFindingsForProduct -> TestTree
requestEnableImportFindingsForProduct =
  req
    "EnableImportFindingsForProduct"
    "fixture/EnableImportFindingsForProduct.yaml"

requestDescribeStandards :: DescribeStandards -> TestTree
requestDescribeStandards =
  req
    "DescribeStandards"
    "fixture/DescribeStandards.yaml"

requestDescribeProducts :: DescribeProducts -> TestTree
requestDescribeProducts =
  req
    "DescribeProducts"
    "fixture/DescribeProducts.yaml"

requestDeleteFindingAggregator :: DeleteFindingAggregator -> TestTree
requestDeleteFindingAggregator =
  req
    "DeleteFindingAggregator"
    "fixture/DeleteFindingAggregator.yaml"

requestUpdateFindingAggregator :: UpdateFindingAggregator -> TestTree
requestUpdateFindingAggregator =
  req
    "UpdateFindingAggregator"
    "fixture/UpdateFindingAggregator.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations =
  req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestGetEnabledStandards :: GetEnabledStandards -> TestTree
requestGetEnabledStandards =
  req
    "GetEnabledStandards"
    "fixture/GetEnabledStandards.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestUpdateActionTarget :: UpdateActionTarget -> TestTree
requestUpdateActionTarget =
  req
    "UpdateActionTarget"
    "fixture/UpdateActionTarget.yaml"

requestDeleteActionTarget :: DeleteActionTarget -> TestTree
requestDeleteActionTarget =
  req
    "DeleteActionTarget"
    "fixture/DeleteActionTarget.yaml"

requestUpdateStandardsControl :: UpdateStandardsControl -> TestTree
requestUpdateStandardsControl =
  req
    "UpdateStandardsControl"
    "fixture/UpdateStandardsControl.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDescribeActionTargets :: DescribeActionTargets -> TestTree
requestDescribeActionTargets =
  req
    "DescribeActionTargets"
    "fixture/DescribeActionTargets.yaml"

requestDisassociateMembers :: DisassociateMembers -> TestTree
requestDisassociateMembers =
  req
    "DisassociateMembers"
    "fixture/DisassociateMembers.yaml"

requestListEnabledProductsForImport :: ListEnabledProductsForImport -> TestTree
requestListEnabledProductsForImport =
  req
    "ListEnabledProductsForImport"
    "fixture/ListEnabledProductsForImport.yaml"

requestDescribeStandardsControls :: DescribeStandardsControls -> TestTree
requestDescribeStandardsControls =
  req
    "DescribeStandardsControls"
    "fixture/DescribeStandardsControls.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestBatchImportFindings :: BatchImportFindings -> TestTree
requestBatchImportFindings =
  req
    "BatchImportFindings"
    "fixture/BatchImportFindings.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestDeleteInsight :: DeleteInsight -> TestTree
requestDeleteInsight =
  req
    "DeleteInsight"
    "fixture/DeleteInsight.yaml"

requestUpdateInsight :: UpdateInsight -> TestTree
requestUpdateInsight =
  req
    "UpdateInsight"
    "fixture/UpdateInsight.yaml"

requestDisableImportFindingsForProduct :: DisableImportFindingsForProduct -> TestTree
requestDisableImportFindingsForProduct =
  req
    "DisableImportFindingsForProduct"
    "fixture/DisableImportFindingsForProduct.yaml"

requestUpdateFindings :: UpdateFindings -> TestTree
requestUpdateFindings =
  req
    "UpdateFindings"
    "fixture/UpdateFindings.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestGetFindingAggregator :: GetFindingAggregator -> TestTree
requestGetFindingAggregator =
  req
    "GetFindingAggregator"
    "fixture/GetFindingAggregator.yaml"

requestInviteMembers :: InviteMembers -> TestTree
requestInviteMembers =
  req
    "InviteMembers"
    "fixture/InviteMembers.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestDisableSecurityHub :: DisableSecurityHub -> TestTree
requestDisableSecurityHub =
  req
    "DisableSecurityHub"
    "fixture/DisableSecurityHub.yaml"

requestListFindingAggregators :: ListFindingAggregators -> TestTree
requestListFindingAggregators =
  req
    "ListFindingAggregators"
    "fixture/ListFindingAggregators.yaml"

requestBatchEnableStandards :: BatchEnableStandards -> TestTree
requestBatchEnableStandards =
  req
    "BatchEnableStandards"
    "fixture/BatchEnableStandards.yaml"

requestCreateFindingAggregator :: CreateFindingAggregator -> TestTree
requestCreateFindingAggregator =
  req
    "CreateFindingAggregator"
    "fixture/CreateFindingAggregator.yaml"

requestBatchDisableStandards :: BatchDisableStandards -> TestTree
requestBatchDisableStandards =
  req
    "BatchDisableStandards"
    "fixture/BatchDisableStandards.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestEnableSecurityHub :: EnableSecurityHub -> TestTree
requestEnableSecurityHub =
  req
    "EnableSecurityHub"
    "fixture/EnableSecurityHub.yaml"

requestUpdateSecurityHubConfiguration :: UpdateSecurityHubConfiguration -> TestTree
requestUpdateSecurityHubConfiguration =
  req
    "UpdateSecurityHubConfiguration"
    "fixture/UpdateSecurityHubConfiguration.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings =
  req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestGetInsights :: GetInsights -> TestTree
requestGetInsights =
  req
    "GetInsights"
    "fixture/GetInsights.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestBatchUpdateFindings :: BatchUpdateFindings -> TestTree
requestBatchUpdateFindings =
  req
    "BatchUpdateFindings"
    "fixture/BatchUpdateFindings.yaml"

requestCreateActionTarget :: CreateActionTarget -> TestTree
requestCreateActionTarget =
  req
    "CreateActionTarget"
    "fixture/CreateActionTarget.yaml"

requestDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccount -> TestTree
requestDisassociateFromAdministratorAccount =
  req
    "DisassociateFromAdministratorAccount"
    "fixture/DisassociateFromAdministratorAccount.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

-- Responses

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableOrganizationAdminAccount)

responseGetAdministratorAccount :: GetAdministratorAccountResponse -> TestTree
responseGetAdministratorAccount =
  res
    "GetAdministratorAccountResponse"
    "fixture/GetAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdministratorAccount)

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationAdminAccounts)

responseCreateInsight :: CreateInsightResponse -> TestTree
responseCreateInsight =
  res
    "CreateInsightResponse"
    "fixture/CreateInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInsight)

responseAcceptAdministratorInvitation :: AcceptAdministratorInvitationResponse -> TestTree
responseAcceptAdministratorInvitation =
  res
    "AcceptAdministratorInvitationResponse"
    "fixture/AcceptAdministratorInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptAdministratorInvitation)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembers)

responseDescribeHub :: DescribeHubResponse -> TestTree
responseDescribeHub =
  res
    "DescribeHubResponse"
    "fixture/DescribeHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHub)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetInsightResults :: GetInsightResultsResponse -> TestTree
responseGetInsightResults =
  res
    "GetInsightResultsResponse"
    "fixture/GetInsightResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightResults)

responseEnableImportFindingsForProduct :: EnableImportFindingsForProductResponse -> TestTree
responseEnableImportFindingsForProduct =
  res
    "EnableImportFindingsForProductResponse"
    "fixture/EnableImportFindingsForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableImportFindingsForProduct)

responseDescribeStandards :: DescribeStandardsResponse -> TestTree
responseDescribeStandards =
  res
    "DescribeStandardsResponse"
    "fixture/DescribeStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStandards)

responseDescribeProducts :: DescribeProductsResponse -> TestTree
responseDescribeProducts =
  res
    "DescribeProductsResponse"
    "fixture/DescribeProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProducts)

responseDeleteFindingAggregator :: DeleteFindingAggregatorResponse -> TestTree
responseDeleteFindingAggregator =
  res
    "DeleteFindingAggregatorResponse"
    "fixture/DeleteFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFindingAggregator)

responseUpdateFindingAggregator :: UpdateFindingAggregatorResponse -> TestTree
responseUpdateFindingAggregator =
  res
    "UpdateFindingAggregatorResponse"
    "fixture/UpdateFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindingAggregator)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInvitations)

responseGetEnabledStandards :: GetEnabledStandardsResponse -> TestTree
responseGetEnabledStandards =
  res
    "GetEnabledStandardsResponse"
    "fixture/GetEnabledStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnabledStandards)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineInvitations)

responseUpdateActionTarget :: UpdateActionTargetResponse -> TestTree
responseUpdateActionTarget =
  res
    "UpdateActionTargetResponse"
    "fixture/UpdateActionTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateActionTarget)

responseDeleteActionTarget :: DeleteActionTargetResponse -> TestTree
responseDeleteActionTarget =
  res
    "DeleteActionTargetResponse"
    "fixture/DeleteActionTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteActionTarget)

responseUpdateStandardsControl :: UpdateStandardsControlResponse -> TestTree
responseUpdateStandardsControl =
  res
    "UpdateStandardsControlResponse"
    "fixture/UpdateStandardsControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStandardsControl)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDescribeActionTargets :: DescribeActionTargetsResponse -> TestTree
responseDescribeActionTargets =
  res
    "DescribeActionTargetsResponse"
    "fixture/DescribeActionTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActionTargets)

responseDisassociateMembers :: DisassociateMembersResponse -> TestTree
responseDisassociateMembers =
  res
    "DisassociateMembersResponse"
    "fixture/DisassociateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembers)

responseListEnabledProductsForImport :: ListEnabledProductsForImportResponse -> TestTree
responseListEnabledProductsForImport =
  res
    "ListEnabledProductsForImportResponse"
    "fixture/ListEnabledProductsForImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnabledProductsForImport)

responseDescribeStandardsControls :: DescribeStandardsControlsResponse -> TestTree
responseDescribeStandardsControls =
  res
    "DescribeStandardsControlsResponse"
    "fixture/DescribeStandardsControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStandardsControls)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembers)

responseBatchImportFindings :: BatchImportFindingsResponse -> TestTree
responseBatchImportFindings =
  res
    "BatchImportFindingsResponse"
    "fixture/BatchImportFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchImportFindings)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationsCount)

responseDeleteInsight :: DeleteInsightResponse -> TestTree
responseDeleteInsight =
  res
    "DeleteInsightResponse"
    "fixture/DeleteInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInsight)

responseUpdateInsight :: UpdateInsightResponse -> TestTree
responseUpdateInsight =
  res
    "UpdateInsightResponse"
    "fixture/UpdateInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInsight)

responseDisableImportFindingsForProduct :: DisableImportFindingsForProductResponse -> TestTree
responseDisableImportFindingsForProduct =
  res
    "DisableImportFindingsForProductResponse"
    "fixture/DisableImportFindingsForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableImportFindingsForProduct)

responseUpdateFindings :: UpdateFindingsResponse -> TestTree
responseUpdateFindings =
  res
    "UpdateFindingsResponse"
    "fixture/UpdateFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindings)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

responseGetFindingAggregator :: GetFindingAggregatorResponse -> TestTree
responseGetFindingAggregator =
  res
    "GetFindingAggregatorResponse"
    "fixture/GetFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingAggregator)

responseInviteMembers :: InviteMembersResponse -> TestTree
responseInviteMembers =
  res
    "InviteMembersResponse"
    "fixture/InviteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteMembers)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembers)

responseDisableSecurityHub :: DisableSecurityHubResponse -> TestTree
responseDisableSecurityHub =
  res
    "DisableSecurityHubResponse"
    "fixture/DisableSecurityHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSecurityHub)

responseListFindingAggregators :: ListFindingAggregatorsResponse -> TestTree
responseListFindingAggregators =
  res
    "ListFindingAggregatorsResponse"
    "fixture/ListFindingAggregatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingAggregators)

responseBatchEnableStandards :: BatchEnableStandardsResponse -> TestTree
responseBatchEnableStandards =
  res
    "BatchEnableStandardsResponse"
    "fixture/BatchEnableStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEnableStandards)

responseCreateFindingAggregator :: CreateFindingAggregatorResponse -> TestTree
responseCreateFindingAggregator =
  res
    "CreateFindingAggregatorResponse"
    "fixture/CreateFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFindingAggregator)

responseBatchDisableStandards :: BatchDisableStandardsResponse -> TestTree
responseBatchDisableStandards =
  res
    "BatchDisableStandardsResponse"
    "fixture/BatchDisableStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisableStandards)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseEnableSecurityHub :: EnableSecurityHubResponse -> TestTree
responseEnableSecurityHub =
  res
    "EnableSecurityHubResponse"
    "fixture/EnableSecurityHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSecurityHub)

responseUpdateSecurityHubConfiguration :: UpdateSecurityHubConfigurationResponse -> TestTree
responseUpdateSecurityHubConfiguration =
  res
    "UpdateSecurityHubConfigurationResponse"
    "fixture/UpdateSecurityHubConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityHubConfiguration)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindings)

responseGetInsights :: GetInsightsResponse -> TestTree
responseGetInsights =
  res
    "GetInsightsResponse"
    "fixture/GetInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsights)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseBatchUpdateFindings :: BatchUpdateFindingsResponse -> TestTree
responseBatchUpdateFindings =
  res
    "BatchUpdateFindingsResponse"
    "fixture/BatchUpdateFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateFindings)

responseCreateActionTarget :: CreateActionTargetResponse -> TestTree
responseCreateActionTarget =
  res
    "CreateActionTargetResponse"
    "fixture/CreateActionTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateActionTarget)

responseDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccountResponse -> TestTree
responseDisassociateFromAdministratorAccount =
  res
    "DisassociateFromAdministratorAccountResponse"
    "fixture/DisassociateFromAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromAdministratorAccount)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableOrganizationAdminAccount)
