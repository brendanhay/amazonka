{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IoT where

import Data.Proxy
import Network.AWS.IoT
import Test.AWS.Fixture
import Test.AWS.IoT.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPolicies $
--             listPolicies
--
--         , requestCreatePolicy $
--             createPolicy
--
--         , requestRegisterCertificate $
--             registerCertificate
--
--         , requestListThingPrincipals $
--             listThingPrincipals
--
--         , requestDescribeRoleAlias $
--             describeRoleAlias
--
--         , requestCreateOTAUpdate $
--             createOTAUpdate
--
--         , requestDescribeDefaultAuthorizer $
--             describeDefaultAuthorizer
--
--         , requestListThingRegistrationTaskReports $
--             listThingRegistrationTaskReports
--
--         , requestListPrincipalThings $
--             listPrincipalThings
--
--         , requestRemoveThingFromThingGroup $
--             removeThingFromThingGroup
--
--         , requestDescribeEventConfigurations $
--             describeEventConfigurations
--
--         , requestListThingGroups $
--             listThingGroups
--
--         , requestDescribeThingRegistrationTask $
--             describeThingRegistrationTask
--
--         , requestGetLoggingOptions $
--             getLoggingOptions
--
--         , requestGetOTAUpdate $
--             getOTAUpdate
--
--         , requestGetEffectivePolicies $
--             getEffectivePolicies
--
--         , requestListThingTypes $
--             listThingTypes
--
--         , requestSetV2LoggingOptions $
--             setV2LoggingOptions
--
--         , requestListThingGroupsForThing $
--             listThingGroupsForThing
--
--         , requestCreateCertificateFromCSR $
--             createCertificateFromCSR
--
--         , requestDeleteThing $
--             deleteThing
--
--         , requestUpdateThing $
--             updateThing
--
--         , requestStartThingRegistrationTask $
--             startThingRegistrationTask
--
--         , requestListAuthorizers $
--             listAuthorizers
--
--         , requestListJobExecutionsForJob $
--             listJobExecutionsForJob
--
--         , requestSearchIndex $
--             searchIndex
--
--         , requestCreateThingType $
--             createThingType
--
--         , requestDeleteV2LoggingLevel $
--             deleteV2LoggingLevel
--
--         , requestSetDefaultAuthorizer $
--             setDefaultAuthorizer
--
--         , requestDescribeJobExecution $
--             describeJobExecution
--
--         , requestCancelCertificateTransfer $
--             cancelCertificateTransfer
--
--         , requestGetIndexingConfiguration $
--             getIndexingConfiguration
--
--         , requestDeleteRoleAlias $
--             deleteRoleAlias
--
--         , requestUpdateRoleAlias $
--             updateRoleAlias
--
--         , requestDeletePolicyVersion $
--             deletePolicyVersion
--
--         , requestDisableTopicRule $
--             disableTopicRule
--
--         , requestCreateTopicRule $
--             createTopicRule
--
--         , requestCreateJob $
--             createJob
--
--         , requestDescribeIndex $
--             describeIndex
--
--         , requestAssociateTargetsWithJob $
--             associateTargetsWithJob
--
--         , requestListAttachedPolicies $
--             listAttachedPolicies
--
--         , requestCreatePolicyVersion $
--             createPolicyVersion
--
--         , requestListCACertificates $
--             listCACertificates
--
--         , requestDeleteTopicRule $
--             deleteTopicRule
--
--         , requestGetJobDocument $
--             getJobDocument
--
--         , requestCreateRoleAlias $
--             createRoleAlias
--
--         , requestDeleteCACertificate $
--             deleteCACertificate
--
--         , requestUpdateCACertificate $
--             updateCACertificate
--
--         , requestListTopicRules $
--             listTopicRules
--
--         , requestTransferCertificate $
--             transferCertificate
--
--         , requestListJobs $
--             listJobs
--
--         , requestListRoleAliases $
--             listRoleAliases
--
--         , requestDescribeThingGroup $
--             describeThingGroup
--
--         , requestGetTopicRule $
--             getTopicRule
--
--         , requestDescribeThing $
--             describeThing
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestListThingsInThingGroup $
--             listThingsInThingGroup
--
--         , requestListCertificates $
--             listCertificates
--
--         , requestDescribeAuthorizer $
--             describeAuthorizer
--
--         , requestGetPolicyVersion $
--             getPolicyVersion
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestUpdateCertificate $
--             updateCertificate
--
--         , requestUpdateIndexingConfiguration $
--             updateIndexingConfiguration
--
--         , requestTestInvokeAuthorizer $
--             testInvokeAuthorizer
--
--         , requestCreateThingGroup $
--             createThingGroup
--
--         , requestDetachPolicy $
--             detachPolicy
--
--         , requestDescribeJob $
--             describeJob
--
--         , requestDeleteThingGroup $
--             deleteThingGroup
--
--         , requestUpdateThingGroup $
--             updateThingGroup
--
--         , requestListOTAUpdates $
--             listOTAUpdates
--
--         , requestDeleteOTAUpdate $
--             deleteOTAUpdate
--
--         , requestListOutgoingCertificates $
--             listOutgoingCertificates
--
--         , requestDescribeCACertificate $
--             describeCACertificate
--
--         , requestGetRegistrationCode $
--             getRegistrationCode
--
--         , requestDeleteThingType $
--             deleteThingType
--
--         , requestAddThingToThingGroup $
--             addThingToThingGroup
--
--         , requestListCertificatesByCA $
--             listCertificatesByCA
--
--         , requestAttachThingPrincipal $
--             attachThingPrincipal
--
--         , requestListThings $
--             listThings
--
--         , requestRegisterThing $
--             registerThing
--
--         , requestDeleteRegistrationCode $
--             deleteRegistrationCode
--
--         , requestUpdateStream $
--             updateStream
--
--         , requestDeleteStream $
--             deleteStream
--
--         , requestListStreams $
--             listStreams
--
--         , requestCreateAuthorizer $
--             createAuthorizer
--
--         , requestTestAuthorization $
--             testAuthorization
--
--         , requestListIndices $
--             listIndices
--
--         , requestUpdateAuthorizer $
--             updateAuthorizer
--
--         , requestDeleteAuthorizer $
--             deleteAuthorizer
--
--         , requestCreateThing $
--             createThing
--
--         , requestCreateStream $
--             createStream
--
--         , requestListV2LoggingLevels $
--             listV2LoggingLevels
--
--         , requestStopThingRegistrationTask $
--             stopThingRegistrationTask
--
--         , requestDescribeCertificate $
--             describeCertificate
--
--         , requestListTargetsForPolicy $
--             listTargetsForPolicy
--
--         , requestClearDefaultAuthorizer $
--             clearDefaultAuthorizer
--
--         , requestReplaceTopicRule $
--             replaceTopicRule
--
--         , requestSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , requestListPolicyVersions $
--             listPolicyVersions
--
--         , requestSetV2LoggingLevel $
--             setV2LoggingLevel
--
--         , requestListJobExecutionsForThing $
--             listJobExecutionsForThing
--
--         , requestAttachPolicy $
--             attachPolicy
--
--         , requestCreateKeysAndCertificate $
--             createKeysAndCertificate
--
--         , requestUpdateThingGroupsForThing $
--             updateThingGroupsForThing
--
--         , requestEnableTopicRule $
--             enableTopicRule
--
--         , requestAcceptCertificateTransfer $
--             acceptCertificateTransfer
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestDescribeEndpoint $
--             describeEndpoint
--
--         , requestUpdateEventConfigurations $
--             updateEventConfigurations
--
--         , requestRegisterCACertificate $
--             registerCACertificate
--
--         , requestSetLoggingOptions $
--             setLoggingOptions
--
--         , requestDescribeThingType $
--             describeThingType
--
--         , requestGetV2LoggingOptions $
--             getV2LoggingOptions
--
--         , requestListThingRegistrationTasks $
--             listThingRegistrationTasks
--
--         , requestRejectCertificateTransfer $
--             rejectCertificateTransfer
--
--         , requestDescribeStream $
--             describeStream
--
--         , requestDetachThingPrincipal $
--             detachThingPrincipal
--
--         , requestCancelJob $
--             cancelJob
--
--         , requestDeprecateThingType $
--             deprecateThingType
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             listPoliciesResponse
--
--         , responseCreatePolicy $
--             createPolicyResponse
--
--         , responseRegisterCertificate $
--             registerCertificateResponse
--
--         , responseListThingPrincipals $
--             listThingPrincipalsResponse
--
--         , responseDescribeRoleAlias $
--             describeRoleAliasResponse
--
--         , responseCreateOTAUpdate $
--             createOTAUpdateResponse
--
--         , responseDescribeDefaultAuthorizer $
--             describeDefaultAuthorizerResponse
--
--         , responseListThingRegistrationTaskReports $
--             listThingRegistrationTaskReportsResponse
--
--         , responseListPrincipalThings $
--             listPrincipalThingsResponse
--
--         , responseRemoveThingFromThingGroup $
--             removeThingFromThingGroupResponse
--
--         , responseDescribeEventConfigurations $
--             describeEventConfigurationsResponse
--
--         , responseListThingGroups $
--             listThingGroupsResponse
--
--         , responseDescribeThingRegistrationTask $
--             describeThingRegistrationTaskResponse
--
--         , responseGetLoggingOptions $
--             getLoggingOptionsResponse
--
--         , responseGetOTAUpdate $
--             getOTAUpdateResponse
--
--         , responseGetEffectivePolicies $
--             getEffectivePoliciesResponse
--
--         , responseListThingTypes $
--             listThingTypesResponse
--
--         , responseSetV2LoggingOptions $
--             setV2LoggingOptionsResponse
--
--         , responseListThingGroupsForThing $
--             listThingGroupsForThingResponse
--
--         , responseCreateCertificateFromCSR $
--             createCertificateFromCSRResponse
--
--         , responseDeleteThing $
--             deleteThingResponse
--
--         , responseUpdateThing $
--             updateThingResponse
--
--         , responseStartThingRegistrationTask $
--             startThingRegistrationTaskResponse
--
--         , responseListAuthorizers $
--             listAuthorizersResponse
--
--         , responseListJobExecutionsForJob $
--             listJobExecutionsForJobResponse
--
--         , responseSearchIndex $
--             searchIndexResponse
--
--         , responseCreateThingType $
--             createThingTypeResponse
--
--         , responseDeleteV2LoggingLevel $
--             deleteV2LoggingLevelResponse
--
--         , responseSetDefaultAuthorizer $
--             setDefaultAuthorizerResponse
--
--         , responseDescribeJobExecution $
--             describeJobExecutionResponse
--
--         , responseCancelCertificateTransfer $
--             cancelCertificateTransferResponse
--
--         , responseGetIndexingConfiguration $
--             getIndexingConfigurationResponse
--
--         , responseDeleteRoleAlias $
--             deleteRoleAliasResponse
--
--         , responseUpdateRoleAlias $
--             updateRoleAliasResponse
--
--         , responseDeletePolicyVersion $
--             deletePolicyVersionResponse
--
--         , responseDisableTopicRule $
--             disableTopicRuleResponse
--
--         , responseCreateTopicRule $
--             createTopicRuleResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseDescribeIndex $
--             describeIndexResponse
--
--         , responseAssociateTargetsWithJob $
--             associateTargetsWithJobResponse
--
--         , responseListAttachedPolicies $
--             listAttachedPoliciesResponse
--
--         , responseCreatePolicyVersion $
--             createPolicyVersionResponse
--
--         , responseListCACertificates $
--             listCACertificatesResponse
--
--         , responseDeleteTopicRule $
--             deleteTopicRuleResponse
--
--         , responseGetJobDocument $
--             getJobDocumentResponse
--
--         , responseCreateRoleAlias $
--             createRoleAliasResponse
--
--         , responseDeleteCACertificate $
--             deleteCACertificateResponse
--
--         , responseUpdateCACertificate $
--             updateCACertificateResponse
--
--         , responseListTopicRules $
--             listTopicRulesResponse
--
--         , responseTransferCertificate $
--             transferCertificateResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseListRoleAliases $
--             listRoleAliasesResponse
--
--         , responseDescribeThingGroup $
--             describeThingGroupResponse
--
--         , responseGetTopicRule $
--             getTopicRuleResponse
--
--         , responseDescribeThing $
--             describeThingResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseListThingsInThingGroup $
--             listThingsInThingGroupResponse
--
--         , responseListCertificates $
--             listCertificatesResponse
--
--         , responseDescribeAuthorizer $
--             describeAuthorizerResponse
--
--         , responseGetPolicyVersion $
--             getPolicyVersionResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseUpdateCertificate $
--             updateCertificateResponse
--
--         , responseUpdateIndexingConfiguration $
--             updateIndexingConfigurationResponse
--
--         , responseTestInvokeAuthorizer $
--             testInvokeAuthorizerResponse
--
--         , responseCreateThingGroup $
--             createThingGroupResponse
--
--         , responseDetachPolicy $
--             detachPolicyResponse
--
--         , responseDescribeJob $
--             describeJobResponse
--
--         , responseDeleteThingGroup $
--             deleteThingGroupResponse
--
--         , responseUpdateThingGroup $
--             updateThingGroupResponse
--
--         , responseListOTAUpdates $
--             listOTAUpdatesResponse
--
--         , responseDeleteOTAUpdate $
--             deleteOTAUpdateResponse
--
--         , responseListOutgoingCertificates $
--             listOutgoingCertificatesResponse
--
--         , responseDescribeCACertificate $
--             describeCACertificateResponse
--
--         , responseGetRegistrationCode $
--             getRegistrationCodeResponse
--
--         , responseDeleteThingType $
--             deleteThingTypeResponse
--
--         , responseAddThingToThingGroup $
--             addThingToThingGroupResponse
--
--         , responseListCertificatesByCA $
--             listCertificatesByCAResponse
--
--         , responseAttachThingPrincipal $
--             attachThingPrincipalResponse
--
--         , responseListThings $
--             listThingsResponse
--
--         , responseRegisterThing $
--             registerThingResponse
--
--         , responseDeleteRegistrationCode $
--             deleteRegistrationCodeResponse
--
--         , responseUpdateStream $
--             updateStreamResponse
--
--         , responseDeleteStream $
--             deleteStreamResponse
--
--         , responseListStreams $
--             listStreamsResponse
--
--         , responseCreateAuthorizer $
--             createAuthorizerResponse
--
--         , responseTestAuthorization $
--             testAuthorizationResponse
--
--         , responseListIndices $
--             listIndicesResponse
--
--         , responseUpdateAuthorizer $
--             updateAuthorizerResponse
--
--         , responseDeleteAuthorizer $
--             deleteAuthorizerResponse
--
--         , responseCreateThing $
--             createThingResponse
--
--         , responseCreateStream $
--             createStreamResponse
--
--         , responseListV2LoggingLevels $
--             listV2LoggingLevelsResponse
--
--         , responseStopThingRegistrationTask $
--             stopThingRegistrationTaskResponse
--
--         , responseDescribeCertificate $
--             describeCertificateResponse
--
--         , responseListTargetsForPolicy $
--             listTargetsForPolicyResponse
--
--         , responseClearDefaultAuthorizer $
--             clearDefaultAuthorizerResponse
--
--         , responseReplaceTopicRule $
--             replaceTopicRuleResponse
--
--         , responseSetDefaultPolicyVersion $
--             setDefaultPolicyVersionResponse
--
--         , responseListPolicyVersions $
--             listPolicyVersionsResponse
--
--         , responseSetV2LoggingLevel $
--             setV2LoggingLevelResponse
--
--         , responseListJobExecutionsForThing $
--             listJobExecutionsForThingResponse
--
--         , responseAttachPolicy $
--             attachPolicyResponse
--
--         , responseCreateKeysAndCertificate $
--             createKeysAndCertificateResponse
--
--         , responseUpdateThingGroupsForThing $
--             updateThingGroupsForThingResponse
--
--         , responseEnableTopicRule $
--             enableTopicRuleResponse
--
--         , responseAcceptCertificateTransfer $
--             acceptCertificateTransferResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseDescribeEndpoint $
--             describeEndpointResponse
--
--         , responseUpdateEventConfigurations $
--             updateEventConfigurationsResponse
--
--         , responseRegisterCACertificate $
--             registerCACertificateResponse
--
--         , responseSetLoggingOptions $
--             setLoggingOptionsResponse
--
--         , responseDescribeThingType $
--             describeThingTypeResponse
--
--         , responseGetV2LoggingOptions $
--             getV2LoggingOptionsResponse
--
--         , responseListThingRegistrationTasks $
--             listThingRegistrationTasksResponse
--
--         , responseRejectCertificateTransfer $
--             rejectCertificateTransferResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
--
--         , responseDetachThingPrincipal $
--             detachThingPrincipalResponse
--
--         , responseCancelJob $
--             cancelJobResponse
--
--         , responseDeprecateThingType $
--             deprecateThingTypeResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate = req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals = req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestDescribeRoleAlias :: DescribeRoleAlias -> TestTree
requestDescribeRoleAlias = req
    "DescribeRoleAlias"
    "fixture/DescribeRoleAlias.yaml"

requestCreateOTAUpdate :: CreateOTAUpdate -> TestTree
requestCreateOTAUpdate = req
    "CreateOTAUpdate"
    "fixture/CreateOTAUpdate.yaml"

requestDescribeDefaultAuthorizer :: DescribeDefaultAuthorizer -> TestTree
requestDescribeDefaultAuthorizer = req
    "DescribeDefaultAuthorizer"
    "fixture/DescribeDefaultAuthorizer.yaml"

requestListThingRegistrationTaskReports :: ListThingRegistrationTaskReports -> TestTree
requestListThingRegistrationTaskReports = req
    "ListThingRegistrationTaskReports"
    "fixture/ListThingRegistrationTaskReports.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings = req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestRemoveThingFromThingGroup :: RemoveThingFromThingGroup -> TestTree
requestRemoveThingFromThingGroup = req
    "RemoveThingFromThingGroup"
    "fixture/RemoveThingFromThingGroup.yaml"

requestDescribeEventConfigurations :: DescribeEventConfigurations -> TestTree
requestDescribeEventConfigurations = req
    "DescribeEventConfigurations"
    "fixture/DescribeEventConfigurations.yaml"

requestListThingGroups :: ListThingGroups -> TestTree
requestListThingGroups = req
    "ListThingGroups"
    "fixture/ListThingGroups.yaml"

requestDescribeThingRegistrationTask :: DescribeThingRegistrationTask -> TestTree
requestDescribeThingRegistrationTask = req
    "DescribeThingRegistrationTask"
    "fixture/DescribeThingRegistrationTask.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions = req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestGetOTAUpdate :: GetOTAUpdate -> TestTree
requestGetOTAUpdate = req
    "GetOTAUpdate"
    "fixture/GetOTAUpdate.yaml"

requestGetEffectivePolicies :: GetEffectivePolicies -> TestTree
requestGetEffectivePolicies = req
    "GetEffectivePolicies"
    "fixture/GetEffectivePolicies.yaml"

requestListThingTypes :: ListThingTypes -> TestTree
requestListThingTypes = req
    "ListThingTypes"
    "fixture/ListThingTypes.yaml"

requestSetV2LoggingOptions :: SetV2LoggingOptions -> TestTree
requestSetV2LoggingOptions = req
    "SetV2LoggingOptions"
    "fixture/SetV2LoggingOptions.yaml"

requestListThingGroupsForThing :: ListThingGroupsForThing -> TestTree
requestListThingGroupsForThing = req
    "ListThingGroupsForThing"
    "fixture/ListThingGroupsForThing.yaml"

requestCreateCertificateFromCSR :: CreateCertificateFromCSR -> TestTree
requestCreateCertificateFromCSR = req
    "CreateCertificateFromCSR"
    "fixture/CreateCertificateFromCSR.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing = req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestUpdateThing :: UpdateThing -> TestTree
requestUpdateThing = req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

requestStartThingRegistrationTask :: StartThingRegistrationTask -> TestTree
requestStartThingRegistrationTask = req
    "StartThingRegistrationTask"
    "fixture/StartThingRegistrationTask.yaml"

requestListAuthorizers :: ListAuthorizers -> TestTree
requestListAuthorizers = req
    "ListAuthorizers"
    "fixture/ListAuthorizers.yaml"

requestListJobExecutionsForJob :: ListJobExecutionsForJob -> TestTree
requestListJobExecutionsForJob = req
    "ListJobExecutionsForJob"
    "fixture/ListJobExecutionsForJob.yaml"

requestSearchIndex :: SearchIndex -> TestTree
requestSearchIndex = req
    "SearchIndex"
    "fixture/SearchIndex.yaml"

requestCreateThingType :: CreateThingType -> TestTree
requestCreateThingType = req
    "CreateThingType"
    "fixture/CreateThingType.yaml"

requestDeleteV2LoggingLevel :: DeleteV2LoggingLevel -> TestTree
requestDeleteV2LoggingLevel = req
    "DeleteV2LoggingLevel"
    "fixture/DeleteV2LoggingLevel.yaml"

requestSetDefaultAuthorizer :: SetDefaultAuthorizer -> TestTree
requestSetDefaultAuthorizer = req
    "SetDefaultAuthorizer"
    "fixture/SetDefaultAuthorizer.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution = req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer = req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

requestGetIndexingConfiguration :: GetIndexingConfiguration -> TestTree
requestGetIndexingConfiguration = req
    "GetIndexingConfiguration"
    "fixture/GetIndexingConfiguration.yaml"

requestDeleteRoleAlias :: DeleteRoleAlias -> TestTree
requestDeleteRoleAlias = req
    "DeleteRoleAlias"
    "fixture/DeleteRoleAlias.yaml"

requestUpdateRoleAlias :: UpdateRoleAlias -> TestTree
requestUpdateRoleAlias = req
    "UpdateRoleAlias"
    "fixture/UpdateRoleAlias.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule = req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestCreateTopicRule :: CreateTopicRule -> TestTree
requestCreateTopicRule = req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex = req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestAssociateTargetsWithJob :: AssociateTargetsWithJob -> TestTree
requestAssociateTargetsWithJob = req
    "AssociateTargetsWithJob"
    "fixture/AssociateTargetsWithJob.yaml"

requestListAttachedPolicies :: ListAttachedPolicies -> TestTree
requestListAttachedPolicies = req
    "ListAttachedPolicies"
    "fixture/ListAttachedPolicies.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates = req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule = req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

requestGetJobDocument :: GetJobDocument -> TestTree
requestGetJobDocument = req
    "GetJobDocument"
    "fixture/GetJobDocument.yaml"

requestCreateRoleAlias :: CreateRoleAlias -> TestTree
requestCreateRoleAlias = req
    "CreateRoleAlias"
    "fixture/CreateRoleAlias.yaml"

requestDeleteCACertificate :: DeleteCACertificate -> TestTree
requestDeleteCACertificate = req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

requestUpdateCACertificate :: UpdateCACertificate -> TestTree
requestUpdateCACertificate = req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules = req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate = req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListRoleAliases :: ListRoleAliases -> TestTree
requestListRoleAliases = req
    "ListRoleAliases"
    "fixture/ListRoleAliases.yaml"

requestDescribeThingGroup :: DescribeThingGroup -> TestTree
requestDescribeThingGroup = req
    "DescribeThingGroup"
    "fixture/DescribeThingGroup.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule = req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestDescribeThing :: DescribeThing -> TestTree
requestDescribeThing = req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestListThingsInThingGroup :: ListThingsInThingGroup -> TestTree
requestListThingsInThingGroup = req
    "ListThingsInThingGroup"
    "fixture/ListThingsInThingGroup.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestDescribeAuthorizer :: DescribeAuthorizer -> TestTree
requestDescribeAuthorizer = req
    "DescribeAuthorizer"
    "fixture/DescribeAuthorizer.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate = req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestUpdateIndexingConfiguration :: UpdateIndexingConfiguration -> TestTree
requestUpdateIndexingConfiguration = req
    "UpdateIndexingConfiguration"
    "fixture/UpdateIndexingConfiguration.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer = req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup = req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy = req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob = req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDeleteThingGroup :: DeleteThingGroup -> TestTree
requestDeleteThingGroup = req
    "DeleteThingGroup"
    "fixture/DeleteThingGroup.yaml"

requestUpdateThingGroup :: UpdateThingGroup -> TestTree
requestUpdateThingGroup = req
    "UpdateThingGroup"
    "fixture/UpdateThingGroup.yaml"

requestListOTAUpdates :: ListOTAUpdates -> TestTree
requestListOTAUpdates = req
    "ListOTAUpdates"
    "fixture/ListOTAUpdates.yaml"

requestDeleteOTAUpdate :: DeleteOTAUpdate -> TestTree
requestDeleteOTAUpdate = req
    "DeleteOTAUpdate"
    "fixture/DeleteOTAUpdate.yaml"

requestListOutgoingCertificates :: ListOutgoingCertificates -> TestTree
requestListOutgoingCertificates = req
    "ListOutgoingCertificates"
    "fixture/ListOutgoingCertificates.yaml"

requestDescribeCACertificate :: DescribeCACertificate -> TestTree
requestDescribeCACertificate = req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

requestGetRegistrationCode :: GetRegistrationCode -> TestTree
requestGetRegistrationCode = req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

requestDeleteThingType :: DeleteThingType -> TestTree
requestDeleteThingType = req
    "DeleteThingType"
    "fixture/DeleteThingType.yaml"

requestAddThingToThingGroup :: AddThingToThingGroup -> TestTree
requestAddThingToThingGroup = req
    "AddThingToThingGroup"
    "fixture/AddThingToThingGroup.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA = req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal = req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestListThings :: ListThings -> TestTree
requestListThings = req
    "ListThings"
    "fixture/ListThings.yaml"

requestRegisterThing :: RegisterThing -> TestTree
requestRegisterThing = req
    "RegisterThing"
    "fixture/RegisterThing.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode = req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream = req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream = req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer = req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestTestAuthorization :: TestAuthorization -> TestTree
requestTestAuthorization = req
    "TestAuthorization"
    "fixture/TestAuthorization.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices = req
    "ListIndices"
    "fixture/ListIndices.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer = req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer = req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateThing :: CreateThing -> TestTree
requestCreateThing = req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream = req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestListV2LoggingLevels :: ListV2LoggingLevels -> TestTree
requestListV2LoggingLevels = req
    "ListV2LoggingLevels"
    "fixture/ListV2LoggingLevels.yaml"

requestStopThingRegistrationTask :: StopThingRegistrationTask -> TestTree
requestStopThingRegistrationTask = req
    "StopThingRegistrationTask"
    "fixture/StopThingRegistrationTask.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy = req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestClearDefaultAuthorizer :: ClearDefaultAuthorizer -> TestTree
requestClearDefaultAuthorizer = req
    "ClearDefaultAuthorizer"
    "fixture/ClearDefaultAuthorizer.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule = req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestSetV2LoggingLevel :: SetV2LoggingLevel -> TestTree
requestSetV2LoggingLevel = req
    "SetV2LoggingLevel"
    "fixture/SetV2LoggingLevel.yaml"

requestListJobExecutionsForThing :: ListJobExecutionsForThing -> TestTree
requestListJobExecutionsForThing = req
    "ListJobExecutionsForThing"
    "fixture/ListJobExecutionsForThing.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy = req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
requestCreateKeysAndCertificate = req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

requestUpdateThingGroupsForThing :: UpdateThingGroupsForThing -> TestTree
requestUpdateThingGroupsForThing = req
    "UpdateThingGroupsForThing"
    "fixture/UpdateThingGroupsForThing.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule = req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
requestAcceptCertificateTransfer = req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint = req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestUpdateEventConfigurations :: UpdateEventConfigurations -> TestTree
requestUpdateEventConfigurations = req
    "UpdateEventConfigurations"
    "fixture/UpdateEventConfigurations.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate = req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions = req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestDescribeThingType :: DescribeThingType -> TestTree
requestDescribeThingType = req
    "DescribeThingType"
    "fixture/DescribeThingType.yaml"

requestGetV2LoggingOptions :: GetV2LoggingOptions -> TestTree
requestGetV2LoggingOptions = req
    "GetV2LoggingOptions"
    "fixture/GetV2LoggingOptions.yaml"

requestListThingRegistrationTasks :: ListThingRegistrationTasks -> TestTree
requestListThingRegistrationTasks = req
    "ListThingRegistrationTasks"
    "fixture/ListThingRegistrationTasks.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer = req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal = req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestDeprecateThingType :: DeprecateThingType -> TestTree
requestDeprecateThingType = req
    "DeprecateThingType"
    "fixture/DeprecateThingType.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicy)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate = res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCertificate)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals = res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingPrincipals)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias = res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy DescribeRoleAlias)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate = res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy CreateOTAUpdate)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer = res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DescribeDefaultAuthorizer)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports = res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingRegistrationTaskReports)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings = res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalThings)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup = res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy RemoveThingFromThingGroup)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations = res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEventConfigurations)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups = res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingGroups)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask = res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingRegistrationTask)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions = res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetLoggingOptions)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate = res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy GetOTAUpdate)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies = res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    ioT
    (Proxy :: Proxy GetEffectivePolicies)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes = res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    ioT
    (Proxy :: Proxy ListThingTypes)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions = res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetV2LoggingOptions)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing = res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    ioT
    (Proxy :: Proxy ListThingGroupsForThing)

responseCreateCertificateFromCSR :: CreateCertificateFromCSRResponse -> TestTree
responseCreateCertificateFromCSR = res
    "CreateCertificateFromCSRResponse"
    "fixture/CreateCertificateFromCSRResponse.proto"
    ioT
    (Proxy :: Proxy CreateCertificateFromCSR)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing = res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThing)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing = res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThing)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask = res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy StartThingRegistrationTask)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers = res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    ioT
    (Proxy :: Proxy ListAuthorizers)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob = res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    ioT
    (Proxy :: Proxy ListJobExecutionsForJob)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex = res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    ioT
    (Proxy :: Proxy SearchIndex)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType = res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy CreateThingType)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel = res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    ioT
    (Proxy :: Proxy DeleteV2LoggingLevel)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer = res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultAuthorizer)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution = res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeJobExecution)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer = res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy CancelCertificateTransfer)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration = res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy GetIndexingConfiguration)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias = res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRoleAlias)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias = res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy UpdateRoleAlias)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicyVersion)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule = res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DisableTopicRule)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule = res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy CreateTopicRule)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    ioT
    (Proxy :: Proxy CreateJob)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex = res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    ioT
    (Proxy :: Proxy DescribeIndex)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob = res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    ioT
    (Proxy :: Proxy AssociateTargetsWithJob)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies = res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListAttachedPolicies)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicyVersion)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates = res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCACertificates)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule = res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DeleteTopicRule)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument = res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    ioT
    (Proxy :: Proxy GetJobDocument)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias = res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy CreateRoleAlias)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate = res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCACertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate = res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCACertificate)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules = res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    ioT
    (Proxy :: Proxy ListTopicRules)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate = res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    ioT
    (Proxy :: Proxy TransferCertificate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    ioT
    (Proxy :: Proxy ListJobs)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases = res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    ioT
    (Proxy :: Proxy ListRoleAliases)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup = res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingGroup)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule = res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy GetTopicRule)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing = res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThing)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicy)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup = res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy ListThingsInThingGroup)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificates)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer = res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuthorizer)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicyVersion)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate = res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCertificate)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration = res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy UpdateIndexingConfiguration)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer = res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy TestInvokeAuthorizer)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup = res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy CreateThingGroup)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy = res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    ioT
    (Proxy :: Proxy DetachPolicy)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob = res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    ioT
    (Proxy :: Proxy DescribeJob)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup = res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThingGroup)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup = res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThingGroup)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates = res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    ioT
    (Proxy :: Proxy ListOTAUpdates)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate = res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteOTAUpdate)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates = res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListOutgoingCertificates)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate = res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCACertificate)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode = res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy GetRegistrationCode)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType = res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThingType)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup = res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy AddThingToThingGroup)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA = res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificatesByCA)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal = res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy AttachThingPrincipal)

responseListThings :: ListThingsResponse -> TestTree
responseListThings = res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListThings)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing = res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    ioT
    (Proxy :: Proxy RegisterThing)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode = res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRegistrationCode)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream = res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    ioT
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream = res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    ioT
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    ioT
    (Proxy :: Proxy ListStreams)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer = res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy CreateAuthorizer)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization = res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    ioT
    (Proxy :: Proxy TestAuthorization)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices = res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    ioT
    (Proxy :: Proxy ListIndices)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer = res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer = res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing = res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    ioT
    (Proxy :: Proxy CreateThing)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream = res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    ioT
    (Proxy :: Proxy CreateStream)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels = res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    ioT
    (Proxy :: Proxy ListV2LoggingLevels)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask = res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy StopThingRegistrationTask)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCertificate)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy = res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    ioT
    (Proxy :: Proxy ListTargetsForPolicy)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer = res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy ClearDefaultAuthorizer)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule = res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy ReplaceTopicRule)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicyVersions)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel = res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    ioT
    (Proxy :: Proxy SetV2LoggingLevel)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing = res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    ioT
    (Proxy :: Proxy ListJobExecutionsForThing)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy = res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    ioT
    (Proxy :: Proxy AttachPolicy)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate = res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    ioT
    (Proxy :: Proxy CreateKeysAndCertificate)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing = res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThingGroupsForThing)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule = res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy EnableTopicRule)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer = res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy AcceptCertificateTransfer)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicy)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint = res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEndpoint)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations = res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    ioT
    (Proxy :: Proxy UpdateEventConfigurations)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate = res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCACertificate)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions = res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetLoggingOptions)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType = res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingType)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions = res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetV2LoggingOptions)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks = res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    ioT
    (Proxy :: Proxy ListThingRegistrationTasks)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer = res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy RejectCertificateTransfer)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    ioT
    (Proxy :: Proxy DescribeStream)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal = res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy DetachThingPrincipal)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    ioT
    (Proxy :: Proxy CancelJob)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType = res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DeprecateThingType)
