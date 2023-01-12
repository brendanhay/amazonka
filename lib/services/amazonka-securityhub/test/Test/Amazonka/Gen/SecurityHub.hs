{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SecurityHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SecurityHub where

import Amazonka.SecurityHub
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SecurityHub.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitation
--
--         , requestBatchDisableStandards $
--             newBatchDisableStandards
--
--         , requestBatchEnableStandards $
--             newBatchEnableStandards
--
--         , requestBatchImportFindings $
--             newBatchImportFindings
--
--         , requestBatchUpdateFindings $
--             newBatchUpdateFindings
--
--         , requestCreateActionTarget $
--             newCreateActionTarget
--
--         , requestCreateFindingAggregator $
--             newCreateFindingAggregator
--
--         , requestCreateInsight $
--             newCreateInsight
--
--         , requestCreateMembers $
--             newCreateMembers
--
--         , requestDeclineInvitations $
--             newDeclineInvitations
--
--         , requestDeleteActionTarget $
--             newDeleteActionTarget
--
--         , requestDeleteFindingAggregator $
--             newDeleteFindingAggregator
--
--         , requestDeleteInsight $
--             newDeleteInsight
--
--         , requestDeleteInvitations $
--             newDeleteInvitations
--
--         , requestDeleteMembers $
--             newDeleteMembers
--
--         , requestDescribeActionTargets $
--             newDescribeActionTargets
--
--         , requestDescribeHub $
--             newDescribeHub
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDescribeProducts $
--             newDescribeProducts
--
--         , requestDescribeStandards $
--             newDescribeStandards
--
--         , requestDescribeStandardsControls $
--             newDescribeStandardsControls
--
--         , requestDisableImportFindingsForProduct $
--             newDisableImportFindingsForProduct
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--         , requestDisableSecurityHub $
--             newDisableSecurityHub
--
--         , requestDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccount
--
--         , requestDisassociateMembers $
--             newDisassociateMembers
--
--         , requestEnableImportFindingsForProduct $
--             newEnableImportFindingsForProduct
--
--         , requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestEnableSecurityHub $
--             newEnableSecurityHub
--
--         , requestGetAdministratorAccount $
--             newGetAdministratorAccount
--
--         , requestGetEnabledStandards $
--             newGetEnabledStandards
--
--         , requestGetFindingAggregator $
--             newGetFindingAggregator
--
--         , requestGetFindings $
--             newGetFindings
--
--         , requestGetInsightResults $
--             newGetInsightResults
--
--         , requestGetInsights $
--             newGetInsights
--
--         , requestGetInvitationsCount $
--             newGetInvitationsCount
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestInviteMembers $
--             newInviteMembers
--
--         , requestListEnabledProductsForImport $
--             newListEnabledProductsForImport
--
--         , requestListFindingAggregators $
--             newListFindingAggregators
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateActionTarget $
--             newUpdateActionTarget
--
--         , requestUpdateFindingAggregator $
--             newUpdateFindingAggregator
--
--         , requestUpdateFindings $
--             newUpdateFindings
--
--         , requestUpdateInsight $
--             newUpdateInsight
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--         , requestUpdateSecurityHubConfiguration $
--             newUpdateSecurityHubConfiguration
--
--         , requestUpdateStandardsControl $
--             newUpdateStandardsControl
--
--           ]

--     , testGroup "response"
--         [ responseAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitationResponse
--
--         , responseBatchDisableStandards $
--             newBatchDisableStandardsResponse
--
--         , responseBatchEnableStandards $
--             newBatchEnableStandardsResponse
--
--         , responseBatchImportFindings $
--             newBatchImportFindingsResponse
--
--         , responseBatchUpdateFindings $
--             newBatchUpdateFindingsResponse
--
--         , responseCreateActionTarget $
--             newCreateActionTargetResponse
--
--         , responseCreateFindingAggregator $
--             newCreateFindingAggregatorResponse
--
--         , responseCreateInsight $
--             newCreateInsightResponse
--
--         , responseCreateMembers $
--             newCreateMembersResponse
--
--         , responseDeclineInvitations $
--             newDeclineInvitationsResponse
--
--         , responseDeleteActionTarget $
--             newDeleteActionTargetResponse
--
--         , responseDeleteFindingAggregator $
--             newDeleteFindingAggregatorResponse
--
--         , responseDeleteInsight $
--             newDeleteInsightResponse
--
--         , responseDeleteInvitations $
--             newDeleteInvitationsResponse
--
--         , responseDeleteMembers $
--             newDeleteMembersResponse
--
--         , responseDescribeActionTargets $
--             newDescribeActionTargetsResponse
--
--         , responseDescribeHub $
--             newDescribeHubResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDescribeProducts $
--             newDescribeProductsResponse
--
--         , responseDescribeStandards $
--             newDescribeStandardsResponse
--
--         , responseDescribeStandardsControls $
--             newDescribeStandardsControlsResponse
--
--         , responseDisableImportFindingsForProduct $
--             newDisableImportFindingsForProductResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--         , responseDisableSecurityHub $
--             newDisableSecurityHubResponse
--
--         , responseDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccountResponse
--
--         , responseDisassociateMembers $
--             newDisassociateMembersResponse
--
--         , responseEnableImportFindingsForProduct $
--             newEnableImportFindingsForProductResponse
--
--         , responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseEnableSecurityHub $
--             newEnableSecurityHubResponse
--
--         , responseGetAdministratorAccount $
--             newGetAdministratorAccountResponse
--
--         , responseGetEnabledStandards $
--             newGetEnabledStandardsResponse
--
--         , responseGetFindingAggregator $
--             newGetFindingAggregatorResponse
--
--         , responseGetFindings $
--             newGetFindingsResponse
--
--         , responseGetInsightResults $
--             newGetInsightResultsResponse
--
--         , responseGetInsights $
--             newGetInsightsResponse
--
--         , responseGetInvitationsCount $
--             newGetInvitationsCountResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseInviteMembers $
--             newInviteMembersResponse
--
--         , responseListEnabledProductsForImport $
--             newListEnabledProductsForImportResponse
--
--         , responseListFindingAggregators $
--             newListFindingAggregatorsResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateActionTarget $
--             newUpdateActionTargetResponse
--
--         , responseUpdateFindingAggregator $
--             newUpdateFindingAggregatorResponse
--
--         , responseUpdateFindings $
--             newUpdateFindingsResponse
--
--         , responseUpdateInsight $
--             newUpdateInsightResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--         , responseUpdateSecurityHubConfiguration $
--             newUpdateSecurityHubConfigurationResponse
--
--         , responseUpdateStandardsControl $
--             newUpdateStandardsControlResponse
--
--           ]
--     ]

-- Requests

requestAcceptAdministratorInvitation :: AcceptAdministratorInvitation -> TestTree
requestAcceptAdministratorInvitation =
  req
    "AcceptAdministratorInvitation"
    "fixture/AcceptAdministratorInvitation.yaml"

requestBatchDisableStandards :: BatchDisableStandards -> TestTree
requestBatchDisableStandards =
  req
    "BatchDisableStandards"
    "fixture/BatchDisableStandards.yaml"

requestBatchEnableStandards :: BatchEnableStandards -> TestTree
requestBatchEnableStandards =
  req
    "BatchEnableStandards"
    "fixture/BatchEnableStandards.yaml"

requestBatchImportFindings :: BatchImportFindings -> TestTree
requestBatchImportFindings =
  req
    "BatchImportFindings"
    "fixture/BatchImportFindings.yaml"

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

requestCreateFindingAggregator :: CreateFindingAggregator -> TestTree
requestCreateFindingAggregator =
  req
    "CreateFindingAggregator"
    "fixture/CreateFindingAggregator.yaml"

requestCreateInsight :: CreateInsight -> TestTree
requestCreateInsight =
  req
    "CreateInsight"
    "fixture/CreateInsight.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestDeleteActionTarget :: DeleteActionTarget -> TestTree
requestDeleteActionTarget =
  req
    "DeleteActionTarget"
    "fixture/DeleteActionTarget.yaml"

requestDeleteFindingAggregator :: DeleteFindingAggregator -> TestTree
requestDeleteFindingAggregator =
  req
    "DeleteFindingAggregator"
    "fixture/DeleteFindingAggregator.yaml"

requestDeleteInsight :: DeleteInsight -> TestTree
requestDeleteInsight =
  req
    "DeleteInsight"
    "fixture/DeleteInsight.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations =
  req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestDescribeActionTargets :: DescribeActionTargets -> TestTree
requestDescribeActionTargets =
  req
    "DescribeActionTargets"
    "fixture/DescribeActionTargets.yaml"

requestDescribeHub :: DescribeHub -> TestTree
requestDescribeHub =
  req
    "DescribeHub"
    "fixture/DescribeHub.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDescribeProducts :: DescribeProducts -> TestTree
requestDescribeProducts =
  req
    "DescribeProducts"
    "fixture/DescribeProducts.yaml"

requestDescribeStandards :: DescribeStandards -> TestTree
requestDescribeStandards =
  req
    "DescribeStandards"
    "fixture/DescribeStandards.yaml"

requestDescribeStandardsControls :: DescribeStandardsControls -> TestTree
requestDescribeStandardsControls =
  req
    "DescribeStandardsControls"
    "fixture/DescribeStandardsControls.yaml"

requestDisableImportFindingsForProduct :: DisableImportFindingsForProduct -> TestTree
requestDisableImportFindingsForProduct =
  req
    "DisableImportFindingsForProduct"
    "fixture/DisableImportFindingsForProduct.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

requestDisableSecurityHub :: DisableSecurityHub -> TestTree
requestDisableSecurityHub =
  req
    "DisableSecurityHub"
    "fixture/DisableSecurityHub.yaml"

requestDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccount -> TestTree
requestDisassociateFromAdministratorAccount =
  req
    "DisassociateFromAdministratorAccount"
    "fixture/DisassociateFromAdministratorAccount.yaml"

requestDisassociateMembers :: DisassociateMembers -> TestTree
requestDisassociateMembers =
  req
    "DisassociateMembers"
    "fixture/DisassociateMembers.yaml"

requestEnableImportFindingsForProduct :: EnableImportFindingsForProduct -> TestTree
requestEnableImportFindingsForProduct =
  req
    "EnableImportFindingsForProduct"
    "fixture/EnableImportFindingsForProduct.yaml"

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestEnableSecurityHub :: EnableSecurityHub -> TestTree
requestEnableSecurityHub =
  req
    "EnableSecurityHub"
    "fixture/EnableSecurityHub.yaml"

requestGetAdministratorAccount :: GetAdministratorAccount -> TestTree
requestGetAdministratorAccount =
  req
    "GetAdministratorAccount"
    "fixture/GetAdministratorAccount.yaml"

requestGetEnabledStandards :: GetEnabledStandards -> TestTree
requestGetEnabledStandards =
  req
    "GetEnabledStandards"
    "fixture/GetEnabledStandards.yaml"

requestGetFindingAggregator :: GetFindingAggregator -> TestTree
requestGetFindingAggregator =
  req
    "GetFindingAggregator"
    "fixture/GetFindingAggregator.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings =
  req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestGetInsightResults :: GetInsightResults -> TestTree
requestGetInsightResults =
  req
    "GetInsightResults"
    "fixture/GetInsightResults.yaml"

requestGetInsights :: GetInsights -> TestTree
requestGetInsights =
  req
    "GetInsights"
    "fixture/GetInsights.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestInviteMembers :: InviteMembers -> TestTree
requestInviteMembers =
  req
    "InviteMembers"
    "fixture/InviteMembers.yaml"

requestListEnabledProductsForImport :: ListEnabledProductsForImport -> TestTree
requestListEnabledProductsForImport =
  req
    "ListEnabledProductsForImport"
    "fixture/ListEnabledProductsForImport.yaml"

requestListFindingAggregators :: ListFindingAggregators -> TestTree
requestListFindingAggregators =
  req
    "ListFindingAggregators"
    "fixture/ListFindingAggregators.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateActionTarget :: UpdateActionTarget -> TestTree
requestUpdateActionTarget =
  req
    "UpdateActionTarget"
    "fixture/UpdateActionTarget.yaml"

requestUpdateFindingAggregator :: UpdateFindingAggregator -> TestTree
requestUpdateFindingAggregator =
  req
    "UpdateFindingAggregator"
    "fixture/UpdateFindingAggregator.yaml"

requestUpdateFindings :: UpdateFindings -> TestTree
requestUpdateFindings =
  req
    "UpdateFindings"
    "fixture/UpdateFindings.yaml"

requestUpdateInsight :: UpdateInsight -> TestTree
requestUpdateInsight =
  req
    "UpdateInsight"
    "fixture/UpdateInsight.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestUpdateSecurityHubConfiguration :: UpdateSecurityHubConfiguration -> TestTree
requestUpdateSecurityHubConfiguration =
  req
    "UpdateSecurityHubConfiguration"
    "fixture/UpdateSecurityHubConfiguration.yaml"

requestUpdateStandardsControl :: UpdateStandardsControl -> TestTree
requestUpdateStandardsControl =
  req
    "UpdateStandardsControl"
    "fixture/UpdateStandardsControl.yaml"

-- Responses

responseAcceptAdministratorInvitation :: AcceptAdministratorInvitationResponse -> TestTree
responseAcceptAdministratorInvitation =
  res
    "AcceptAdministratorInvitationResponse"
    "fixture/AcceptAdministratorInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptAdministratorInvitation)

responseBatchDisableStandards :: BatchDisableStandardsResponse -> TestTree
responseBatchDisableStandards =
  res
    "BatchDisableStandardsResponse"
    "fixture/BatchDisableStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisableStandards)

responseBatchEnableStandards :: BatchEnableStandardsResponse -> TestTree
responseBatchEnableStandards =
  res
    "BatchEnableStandardsResponse"
    "fixture/BatchEnableStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEnableStandards)

responseBatchImportFindings :: BatchImportFindingsResponse -> TestTree
responseBatchImportFindings =
  res
    "BatchImportFindingsResponse"
    "fixture/BatchImportFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchImportFindings)

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

responseCreateFindingAggregator :: CreateFindingAggregatorResponse -> TestTree
responseCreateFindingAggregator =
  res
    "CreateFindingAggregatorResponse"
    "fixture/CreateFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFindingAggregator)

responseCreateInsight :: CreateInsightResponse -> TestTree
responseCreateInsight =
  res
    "CreateInsightResponse"
    "fixture/CreateInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInsight)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembers)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineInvitations)

responseDeleteActionTarget :: DeleteActionTargetResponse -> TestTree
responseDeleteActionTarget =
  res
    "DeleteActionTargetResponse"
    "fixture/DeleteActionTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteActionTarget)

responseDeleteFindingAggregator :: DeleteFindingAggregatorResponse -> TestTree
responseDeleteFindingAggregator =
  res
    "DeleteFindingAggregatorResponse"
    "fixture/DeleteFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFindingAggregator)

responseDeleteInsight :: DeleteInsightResponse -> TestTree
responseDeleteInsight =
  res
    "DeleteInsightResponse"
    "fixture/DeleteInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInsight)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInvitations)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembers)

responseDescribeActionTargets :: DescribeActionTargetsResponse -> TestTree
responseDescribeActionTargets =
  res
    "DescribeActionTargetsResponse"
    "fixture/DescribeActionTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActionTargets)

responseDescribeHub :: DescribeHubResponse -> TestTree
responseDescribeHub =
  res
    "DescribeHubResponse"
    "fixture/DescribeHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHub)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDescribeProducts :: DescribeProductsResponse -> TestTree
responseDescribeProducts =
  res
    "DescribeProductsResponse"
    "fixture/DescribeProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProducts)

responseDescribeStandards :: DescribeStandardsResponse -> TestTree
responseDescribeStandards =
  res
    "DescribeStandardsResponse"
    "fixture/DescribeStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStandards)

responseDescribeStandardsControls :: DescribeStandardsControlsResponse -> TestTree
responseDescribeStandardsControls =
  res
    "DescribeStandardsControlsResponse"
    "fixture/DescribeStandardsControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStandardsControls)

responseDisableImportFindingsForProduct :: DisableImportFindingsForProductResponse -> TestTree
responseDisableImportFindingsForProduct =
  res
    "DisableImportFindingsForProductResponse"
    "fixture/DisableImportFindingsForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableImportFindingsForProduct)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableOrganizationAdminAccount)

responseDisableSecurityHub :: DisableSecurityHubResponse -> TestTree
responseDisableSecurityHub =
  res
    "DisableSecurityHubResponse"
    "fixture/DisableSecurityHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSecurityHub)

responseDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccountResponse -> TestTree
responseDisassociateFromAdministratorAccount =
  res
    "DisassociateFromAdministratorAccountResponse"
    "fixture/DisassociateFromAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromAdministratorAccount)

responseDisassociateMembers :: DisassociateMembersResponse -> TestTree
responseDisassociateMembers =
  res
    "DisassociateMembersResponse"
    "fixture/DisassociateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembers)

responseEnableImportFindingsForProduct :: EnableImportFindingsForProductResponse -> TestTree
responseEnableImportFindingsForProduct =
  res
    "EnableImportFindingsForProductResponse"
    "fixture/EnableImportFindingsForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableImportFindingsForProduct)

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableOrganizationAdminAccount)

responseEnableSecurityHub :: EnableSecurityHubResponse -> TestTree
responseEnableSecurityHub =
  res
    "EnableSecurityHubResponse"
    "fixture/EnableSecurityHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSecurityHub)

responseGetAdministratorAccount :: GetAdministratorAccountResponse -> TestTree
responseGetAdministratorAccount =
  res
    "GetAdministratorAccountResponse"
    "fixture/GetAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdministratorAccount)

responseGetEnabledStandards :: GetEnabledStandardsResponse -> TestTree
responseGetEnabledStandards =
  res
    "GetEnabledStandardsResponse"
    "fixture/GetEnabledStandardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnabledStandards)

responseGetFindingAggregator :: GetFindingAggregatorResponse -> TestTree
responseGetFindingAggregator =
  res
    "GetFindingAggregatorResponse"
    "fixture/GetFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingAggregator)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindings)

responseGetInsightResults :: GetInsightResultsResponse -> TestTree
responseGetInsightResults =
  res
    "GetInsightResultsResponse"
    "fixture/GetInsightResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightResults)

responseGetInsights :: GetInsightsResponse -> TestTree
responseGetInsights =
  res
    "GetInsightsResponse"
    "fixture/GetInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsights)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationsCount)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembers)

responseInviteMembers :: InviteMembersResponse -> TestTree
responseInviteMembers =
  res
    "InviteMembersResponse"
    "fixture/InviteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteMembers)

responseListEnabledProductsForImport :: ListEnabledProductsForImportResponse -> TestTree
responseListEnabledProductsForImport =
  res
    "ListEnabledProductsForImportResponse"
    "fixture/ListEnabledProductsForImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnabledProductsForImport)

responseListFindingAggregators :: ListFindingAggregatorsResponse -> TestTree
responseListFindingAggregators =
  res
    "ListFindingAggregatorsResponse"
    "fixture/ListFindingAggregatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingAggregators)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationAdminAccounts)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateActionTarget :: UpdateActionTargetResponse -> TestTree
responseUpdateActionTarget =
  res
    "UpdateActionTargetResponse"
    "fixture/UpdateActionTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateActionTarget)

responseUpdateFindingAggregator :: UpdateFindingAggregatorResponse -> TestTree
responseUpdateFindingAggregator =
  res
    "UpdateFindingAggregatorResponse"
    "fixture/UpdateFindingAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindingAggregator)

responseUpdateFindings :: UpdateFindingsResponse -> TestTree
responseUpdateFindings =
  res
    "UpdateFindingsResponse"
    "fixture/UpdateFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindings)

responseUpdateInsight :: UpdateInsightResponse -> TestTree
responseUpdateInsight =
  res
    "UpdateInsightResponse"
    "fixture/UpdateInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInsight)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

responseUpdateSecurityHubConfiguration :: UpdateSecurityHubConfigurationResponse -> TestTree
responseUpdateSecurityHubConfiguration =
  res
    "UpdateSecurityHubConfigurationResponse"
    "fixture/UpdateSecurityHubConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityHubConfiguration)

responseUpdateStandardsControl :: UpdateStandardsControlResponse -> TestTree
responseUpdateStandardsControl =
  res
    "UpdateStandardsControlResponse"
    "fixture/UpdateStandardsControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStandardsControl)
