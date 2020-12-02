{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestGetCardinality $
--             getCardinality
--
--         , requestCreateDomainConfiguration $
--             createDomainConfiguration
--
--         , requestDeleteSecurityProfile $
--             deleteSecurityProfile
--
--         , requestUpdateSecurityProfile $
--             updateSecurityProfile
--
--         , requestListSecurityProfiles $
--             listSecurityProfiles
--
--         , requestListPolicies $
--             listPolicies
--
--         , requestDescribeProvisioningTemplate $
--             describeProvisioningTemplate
--
--         , requestUpdateMitigationAction $
--             updateMitigationAction
--
--         , requestDeleteMitigationAction $
--             deleteMitigationAction
--
--         , requestDeleteJobExecution $
--             deleteJobExecution
--
--         , requestCreatePolicy $
--             createPolicy
--
--         , requestRegisterCertificate $
--             registerCertificate
--
--         , requestDeleteDynamicThingGroup $
--             deleteDynamicThingGroup
--
--         , requestListThingPrincipals $
--             listThingPrincipals
--
--         , requestUpdateDynamicThingGroup $
--             updateDynamicThingGroup
--
--         , requestDescribeRoleAlias $
--             describeRoleAlias
--
--         , requestCreateProvisioningTemplateVersion $
--             createProvisioningTemplateVersion
--
--         , requestCreateOTAUpdate $
--             createOTAUpdate
--
--         , requestDescribeDefaultAuthorizer $
--             describeDefaultAuthorizer
--
--         , requestListAuditMitigationActionsTasks $
--             listAuditMitigationActionsTasks
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
--         , requestListTopicRuleDestinations $
--             listTopicRuleDestinations
--
--         , requestRegisterCertificateWithoutCA $
--             registerCertificateWithoutCA
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestListThingGroups $
--             listThingGroups
--
--         , requestListScheduledAudits $
--             listScheduledAudits
--
--         , requestDescribeThingRegistrationTask $
--             describeThingRegistrationTask
--
--         , requestUpdateScheduledAudit $
--             updateScheduledAudit
--
--         , requestDeleteScheduledAudit $
--             deleteScheduledAudit
--
--         , requestDescribeAuditFinding $
--             describeAuditFinding
--
--         , requestDescribeDimension $
--             describeDimension
--
--         , requestGetLoggingOptions $
--             getLoggingOptions
--
--         , requestDeleteAccountAuditConfiguration $
--             deleteAccountAuditConfiguration
--
--         , requestUpdateAccountAuditConfiguration $
--             updateAccountAuditConfiguration
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
--         , requestCreateProvisioningTemplate $
--             createProvisioningTemplate
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
--         , requestDeleteProvisioningTemplate $
--             deleteProvisioningTemplate
--
--         , requestUpdateProvisioningTemplate $
--             updateProvisioningTemplate
--
--         , requestDescribeMitigationAction $
--             describeMitigationAction
--
--         , requestStartThingRegistrationTask $
--             startThingRegistrationTask
--
--         , requestCreateScheduledAudit $
--             createScheduledAudit
--
--         , requestListAuthorizers $
--             listAuthorizers
--
--         , requestListJobExecutionsForJob $
--             listJobExecutionsForJob
--
--         , requestRemoveThingFromBillingGroup $
--             removeThingFromBillingGroup
--
--         , requestSearchIndex $
--             searchIndex
--
--         , requestCreateThingType $
--             createThingType
--
--         , requestDescribeSecurityProfile $
--             describeSecurityProfile
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
--         , requestListAuditMitigationActionsExecutions $
--             listAuditMitigationActionsExecutions
--
--         , requestDescribeAuditMitigationActionsTask $
--             describeAuditMitigationActionsTask
--
--         , requestGetStatistics $
--             getStatistics
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
--         , requestAttachSecurityProfile $
--             attachSecurityProfile
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
--         , requestDescribeProvisioningTemplateVersion $
--             describeProvisioningTemplateVersion
--
--         , requestCancelAuditTask $
--             cancelAuditTask
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
--         , requestStartOnDemandAuditTask $
--             startOnDemandAuditTask
--
--         , requestDescribeThingGroup $
--             describeThingGroup
--
--         , requestDeleteJob $
--             deleteJob
--
--         , requestListTargetsForSecurityProfile $
--             listTargetsForSecurityProfile
--
--         , requestUpdateJob $
--             updateJob
--
--         , requestStartAuditMitigationActionsTask $
--             startAuditMitigationActionsTask
--
--         , requestGetTopicRule $
--             getTopicRule
--
--         , requestDescribeThing $
--             describeThing
--
--         , requestListDomainConfigurations $
--             listDomainConfigurations
--
--         , requestListAuditTasks $
--             listAuditTasks
--
--         , requestDescribeAccountAuditConfiguration $
--             describeAccountAuditConfiguration
--
--         , requestDeleteDimension $
--             deleteDimension
--
--         , requestUpdateDimension $
--             updateDimension
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestListThingsInThingGroup $
--             listThingsInThingGroup
--
--         , requestListAuditFindings $
--             listAuditFindings
--
--         , requestDescribeScheduledAudit $
--             describeScheduledAudit
--
--         , requestCreateMitigationAction $
--             createMitigationAction
--
--         , requestConfirmTopicRuleDestination $
--             confirmTopicRuleDestination
--
--         , requestListCertificates $
--             listCertificates
--
--         , requestListMitigationActions $
--             listMitigationActions
--
--         , requestDescribeAuthorizer $
--             describeAuthorizer
--
--         , requestGetPolicyVersion $
--             getPolicyVersion
--
--         , requestListActiveViolations $
--             listActiveViolations
--
--         , requestValidateSecurityProfileBehaviors $
--             validateSecurityProfileBehaviors
--
--         , requestListViolationEvents $
--             listViolationEvents
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestUpdateCertificate $
--             updateCertificate
--
--         , requestCreateDimension $
--             createDimension
--
--         , requestUpdateIndexingConfiguration $
--             updateIndexingConfiguration
--
--         , requestCreateProvisioningClaim $
--             createProvisioningClaim
--
--         , requestTestInvokeAuthorizer $
--             testInvokeAuthorizer
--
--         , requestCreateThingGroup $
--             createThingGroup
--
--         , requestCreateTopicRuleDestination $
--             createTopicRuleDestination
--
--         , requestDetachPolicy $
--             detachPolicy
--
--         , requestDescribeJob $
--             describeJob
--
--         , requestAddThingToBillingGroup $
--             addThingToBillingGroup
--
--         , requestUpdateTopicRuleDestination $
--             updateTopicRuleDestination
--
--         , requestDeleteTopicRuleDestination $
--             deleteTopicRuleDestination
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
--         , requestCreateDynamicThingGroup $
--             createDynamicThingGroup
--
--         , requestDetachSecurityProfile $
--             detachSecurityProfile
--
--         , requestListOutgoingCertificates $
--             listOutgoingCertificates
--
--         , requestDeleteProvisioningTemplateVersion $
--             deleteProvisioningTemplateVersion
--
--         , requestDescribeCACertificate $
--             describeCACertificate
--
--         , requestListProvisioningTemplateVersions $
--             listProvisioningTemplateVersions
--
--         , requestGetRegistrationCode $
--             getRegistrationCode
--
--         , requestListBillingGroups $
--             listBillingGroups
--
--         , requestDeleteThingType $
--             deleteThingType
--
--         , requestDeleteBillingGroup $
--             deleteBillingGroup
--
--         , requestAddThingToThingGroup $
--             addThingToThingGroup
--
--         , requestUpdateBillingGroup $
--             updateBillingGroup
--
--         , requestGetTopicRuleDestination $
--             getTopicRuleDestination
--
--         , requestListCertificatesByCA $
--             listCertificatesByCA
--
--         , requestUpdateAuditSuppression $
--             updateAuditSuppression
--
--         , requestAttachThingPrincipal $
--             attachThingPrincipal
--
--         , requestListThings $
--             listThings
--
--         , requestDeleteAuditSuppression $
--             deleteAuditSuppression
--
--         , requestRegisterThing $
--             registerThing
--
--         , requestListAuditSuppressions $
--             listAuditSuppressions
--
--         , requestDescribeDomainConfiguration $
--             describeDomainConfiguration
--
--         , requestDescribeAuditTask $
--             describeAuditTask
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
--         , requestCancelAuditMitigationActionsTask $
--             cancelAuditMitigationActionsTask
--
--         , requestCreateAuditSuppression $
--             createAuditSuppression
--
--         , requestCreateBillingGroup $
--             createBillingGroup
--
--         , requestListProvisioningTemplates $
--             listProvisioningTemplates
--
--         , requestListV2LoggingLevels $
--             listV2LoggingLevels
--
--         , requestTagResource $
--             tagResource
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
--         , requestUntagResource $
--             untagResource
--
--         , requestSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , requestCancelJobExecution $
--             cancelJobExecution
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
--         , requestListThingsInBillingGroup $
--             listThingsInBillingGroup
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
--         , requestGetPercentiles $
--             getPercentiles
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestDescribeEndpoint $
--             describeEndpoint
--
--         , requestListSecurityProfilesForTarget $
--             listSecurityProfilesForTarget
--
--         , requestUpdateEventConfigurations $
--             updateEventConfigurations
--
--         , requestRegisterCACertificate $
--             registerCACertificate
--
--         , requestDeleteDomainConfiguration $
--             deleteDomainConfiguration
--
--         , requestUpdateDomainConfiguration $
--             updateDomainConfiguration
--
--         , requestSetLoggingOptions $
--             setLoggingOptions
--
--         , requestDescribeThingType $
--             describeThingType
--
--         , requestListDimensions $
--             listDimensions
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
--         , requestDescribeAuditSuppression $
--             describeAuditSuppression
--
--         , requestDescribeStream $
--             describeStream
--
--         , requestCreateSecurityProfile $
--             createSecurityProfile
--
--         , requestDescribeBillingGroup $
--             describeBillingGroup
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
--         [ responseGetCardinality $
--             getCardinalityResponse
--
--         , responseCreateDomainConfiguration $
--             createDomainConfigurationResponse
--
--         , responseDeleteSecurityProfile $
--             deleteSecurityProfileResponse
--
--         , responseUpdateSecurityProfile $
--             updateSecurityProfileResponse
--
--         , responseListSecurityProfiles $
--             listSecurityProfilesResponse
--
--         , responseListPolicies $
--             listPoliciesResponse
--
--         , responseDescribeProvisioningTemplate $
--             describeProvisioningTemplateResponse
--
--         , responseUpdateMitigationAction $
--             updateMitigationActionResponse
--
--         , responseDeleteMitigationAction $
--             deleteMitigationActionResponse
--
--         , responseDeleteJobExecution $
--             deleteJobExecutionResponse
--
--         , responseCreatePolicy $
--             createPolicyResponse
--
--         , responseRegisterCertificate $
--             registerCertificateResponse
--
--         , responseDeleteDynamicThingGroup $
--             deleteDynamicThingGroupResponse
--
--         , responseListThingPrincipals $
--             listThingPrincipalsResponse
--
--         , responseUpdateDynamicThingGroup $
--             updateDynamicThingGroupResponse
--
--         , responseDescribeRoleAlias $
--             describeRoleAliasResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             createProvisioningTemplateVersionResponse
--
--         , responseCreateOTAUpdate $
--             createOTAUpdateResponse
--
--         , responseDescribeDefaultAuthorizer $
--             describeDefaultAuthorizerResponse
--
--         , responseListAuditMitigationActionsTasks $
--             listAuditMitigationActionsTasksResponse
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
--         , responseListTopicRuleDestinations $
--             listTopicRuleDestinationsResponse
--
--         , responseRegisterCertificateWithoutCA $
--             registerCertificateWithoutCAResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseListThingGroups $
--             listThingGroupsResponse
--
--         , responseListScheduledAudits $
--             listScheduledAuditsResponse
--
--         , responseDescribeThingRegistrationTask $
--             describeThingRegistrationTaskResponse
--
--         , responseUpdateScheduledAudit $
--             updateScheduledAuditResponse
--
--         , responseDeleteScheduledAudit $
--             deleteScheduledAuditResponse
--
--         , responseDescribeAuditFinding $
--             describeAuditFindingResponse
--
--         , responseDescribeDimension $
--             describeDimensionResponse
--
--         , responseGetLoggingOptions $
--             getLoggingOptionsResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             deleteAccountAuditConfigurationResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             updateAccountAuditConfigurationResponse
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
--         , responseCreateProvisioningTemplate $
--             createProvisioningTemplateResponse
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
--         , responseDeleteProvisioningTemplate $
--             deleteProvisioningTemplateResponse
--
--         , responseUpdateProvisioningTemplate $
--             updateProvisioningTemplateResponse
--
--         , responseDescribeMitigationAction $
--             describeMitigationActionResponse
--
--         , responseStartThingRegistrationTask $
--             startThingRegistrationTaskResponse
--
--         , responseCreateScheduledAudit $
--             createScheduledAuditResponse
--
--         , responseListAuthorizers $
--             listAuthorizersResponse
--
--         , responseListJobExecutionsForJob $
--             listJobExecutionsForJobResponse
--
--         , responseRemoveThingFromBillingGroup $
--             removeThingFromBillingGroupResponse
--
--         , responseSearchIndex $
--             searchIndexResponse
--
--         , responseCreateThingType $
--             createThingTypeResponse
--
--         , responseDescribeSecurityProfile $
--             describeSecurityProfileResponse
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
--         , responseListAuditMitigationActionsExecutions $
--             listAuditMitigationActionsExecutionsResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             describeAuditMitigationActionsTaskResponse
--
--         , responseGetStatistics $
--             getStatisticsResponse
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
--         , responseAttachSecurityProfile $
--             attachSecurityProfileResponse
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
--         , responseDescribeProvisioningTemplateVersion $
--             describeProvisioningTemplateVersionResponse
--
--         , responseCancelAuditTask $
--             cancelAuditTaskResponse
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
--         , responseStartOnDemandAuditTask $
--             startOnDemandAuditTaskResponse
--
--         , responseDescribeThingGroup $
--             describeThingGroupResponse
--
--         , responseDeleteJob $
--             deleteJobResponse
--
--         , responseListTargetsForSecurityProfile $
--             listTargetsForSecurityProfileResponse
--
--         , responseUpdateJob $
--             updateJobResponse
--
--         , responseStartAuditMitigationActionsTask $
--             startAuditMitigationActionsTaskResponse
--
--         , responseGetTopicRule $
--             getTopicRuleResponse
--
--         , responseDescribeThing $
--             describeThingResponse
--
--         , responseListDomainConfigurations $
--             listDomainConfigurationsResponse
--
--         , responseListAuditTasks $
--             listAuditTasksResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             describeAccountAuditConfigurationResponse
--
--         , responseDeleteDimension $
--             deleteDimensionResponse
--
--         , responseUpdateDimension $
--             updateDimensionResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseListThingsInThingGroup $
--             listThingsInThingGroupResponse
--
--         , responseListAuditFindings $
--             listAuditFindingsResponse
--
--         , responseDescribeScheduledAudit $
--             describeScheduledAuditResponse
--
--         , responseCreateMitigationAction $
--             createMitigationActionResponse
--
--         , responseConfirmTopicRuleDestination $
--             confirmTopicRuleDestinationResponse
--
--         , responseListCertificates $
--             listCertificatesResponse
--
--         , responseListMitigationActions $
--             listMitigationActionsResponse
--
--         , responseDescribeAuthorizer $
--             describeAuthorizerResponse
--
--         , responseGetPolicyVersion $
--             getPolicyVersionResponse
--
--         , responseListActiveViolations $
--             listActiveViolationsResponse
--
--         , responseValidateSecurityProfileBehaviors $
--             validateSecurityProfileBehaviorsResponse
--
--         , responseListViolationEvents $
--             listViolationEventsResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseUpdateCertificate $
--             updateCertificateResponse
--
--         , responseCreateDimension $
--             createDimensionResponse
--
--         , responseUpdateIndexingConfiguration $
--             updateIndexingConfigurationResponse
--
--         , responseCreateProvisioningClaim $
--             createProvisioningClaimResponse
--
--         , responseTestInvokeAuthorizer $
--             testInvokeAuthorizerResponse
--
--         , responseCreateThingGroup $
--             createThingGroupResponse
--
--         , responseCreateTopicRuleDestination $
--             createTopicRuleDestinationResponse
--
--         , responseDetachPolicy $
--             detachPolicyResponse
--
--         , responseDescribeJob $
--             describeJobResponse
--
--         , responseAddThingToBillingGroup $
--             addThingToBillingGroupResponse
--
--         , responseUpdateTopicRuleDestination $
--             updateTopicRuleDestinationResponse
--
--         , responseDeleteTopicRuleDestination $
--             deleteTopicRuleDestinationResponse
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
--         , responseCreateDynamicThingGroup $
--             createDynamicThingGroupResponse
--
--         , responseDetachSecurityProfile $
--             detachSecurityProfileResponse
--
--         , responseListOutgoingCertificates $
--             listOutgoingCertificatesResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             deleteProvisioningTemplateVersionResponse
--
--         , responseDescribeCACertificate $
--             describeCACertificateResponse
--
--         , responseListProvisioningTemplateVersions $
--             listProvisioningTemplateVersionsResponse
--
--         , responseGetRegistrationCode $
--             getRegistrationCodeResponse
--
--         , responseListBillingGroups $
--             listBillingGroupsResponse
--
--         , responseDeleteThingType $
--             deleteThingTypeResponse
--
--         , responseDeleteBillingGroup $
--             deleteBillingGroupResponse
--
--         , responseAddThingToThingGroup $
--             addThingToThingGroupResponse
--
--         , responseUpdateBillingGroup $
--             updateBillingGroupResponse
--
--         , responseGetTopicRuleDestination $
--             getTopicRuleDestinationResponse
--
--         , responseListCertificatesByCA $
--             listCertificatesByCAResponse
--
--         , responseUpdateAuditSuppression $
--             updateAuditSuppressionResponse
--
--         , responseAttachThingPrincipal $
--             attachThingPrincipalResponse
--
--         , responseListThings $
--             listThingsResponse
--
--         , responseDeleteAuditSuppression $
--             deleteAuditSuppressionResponse
--
--         , responseRegisterThing $
--             registerThingResponse
--
--         , responseListAuditSuppressions $
--             listAuditSuppressionsResponse
--
--         , responseDescribeDomainConfiguration $
--             describeDomainConfigurationResponse
--
--         , responseDescribeAuditTask $
--             describeAuditTaskResponse
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
--         , responseCancelAuditMitigationActionsTask $
--             cancelAuditMitigationActionsTaskResponse
--
--         , responseCreateAuditSuppression $
--             createAuditSuppressionResponse
--
--         , responseCreateBillingGroup $
--             createBillingGroupResponse
--
--         , responseListProvisioningTemplates $
--             listProvisioningTemplatesResponse
--
--         , responseListV2LoggingLevels $
--             listV2LoggingLevelsResponse
--
--         , responseTagResource $
--             tagResourceResponse
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
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseSetDefaultPolicyVersion $
--             setDefaultPolicyVersionResponse
--
--         , responseCancelJobExecution $
--             cancelJobExecutionResponse
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
--         , responseListThingsInBillingGroup $
--             listThingsInBillingGroupResponse
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
--         , responseGetPercentiles $
--             getPercentilesResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseDescribeEndpoint $
--             describeEndpointResponse
--
--         , responseListSecurityProfilesForTarget $
--             listSecurityProfilesForTargetResponse
--
--         , responseUpdateEventConfigurations $
--             updateEventConfigurationsResponse
--
--         , responseRegisterCACertificate $
--             registerCACertificateResponse
--
--         , responseDeleteDomainConfiguration $
--             deleteDomainConfigurationResponse
--
--         , responseUpdateDomainConfiguration $
--             updateDomainConfigurationResponse
--
--         , responseSetLoggingOptions $
--             setLoggingOptionsResponse
--
--         , responseDescribeThingType $
--             describeThingTypeResponse
--
--         , responseListDimensions $
--             listDimensionsResponse
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
--         , responseDescribeAuditSuppression $
--             describeAuditSuppressionResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
--
--         , responseCreateSecurityProfile $
--             createSecurityProfileResponse
--
--         , responseDescribeBillingGroup $
--             describeBillingGroupResponse
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

requestGetCardinality :: GetCardinality -> TestTree
requestGetCardinality =
  req
    "GetCardinality"
    "fixture/GetCardinality.yaml"

requestCreateDomainConfiguration :: CreateDomainConfiguration -> TestTree
requestCreateDomainConfiguration =
  req
    "CreateDomainConfiguration"
    "fixture/CreateDomainConfiguration.yaml"

requestDeleteSecurityProfile :: DeleteSecurityProfile -> TestTree
requestDeleteSecurityProfile =
  req
    "DeleteSecurityProfile"
    "fixture/DeleteSecurityProfile.yaml"

requestUpdateSecurityProfile :: UpdateSecurityProfile -> TestTree
requestUpdateSecurityProfile =
  req
    "UpdateSecurityProfile"
    "fixture/UpdateSecurityProfile.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestDescribeProvisioningTemplate :: DescribeProvisioningTemplate -> TestTree
requestDescribeProvisioningTemplate =
  req
    "DescribeProvisioningTemplate"
    "fixture/DescribeProvisioningTemplate.yaml"

requestUpdateMitigationAction :: UpdateMitigationAction -> TestTree
requestUpdateMitigationAction =
  req
    "UpdateMitigationAction"
    "fixture/UpdateMitigationAction.yaml"

requestDeleteMitigationAction :: DeleteMitigationAction -> TestTree
requestDeleteMitigationAction =
  req
    "DeleteMitigationAction"
    "fixture/DeleteMitigationAction.yaml"

requestDeleteJobExecution :: DeleteJobExecution -> TestTree
requestDeleteJobExecution =
  req
    "DeleteJobExecution"
    "fixture/DeleteJobExecution.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestDeleteDynamicThingGroup :: DeleteDynamicThingGroup -> TestTree
requestDeleteDynamicThingGroup =
  req
    "DeleteDynamicThingGroup"
    "fixture/DeleteDynamicThingGroup.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals =
  req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestUpdateDynamicThingGroup :: UpdateDynamicThingGroup -> TestTree
requestUpdateDynamicThingGroup =
  req
    "UpdateDynamicThingGroup"
    "fixture/UpdateDynamicThingGroup.yaml"

requestDescribeRoleAlias :: DescribeRoleAlias -> TestTree
requestDescribeRoleAlias =
  req
    "DescribeRoleAlias"
    "fixture/DescribeRoleAlias.yaml"

requestCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersion -> TestTree
requestCreateProvisioningTemplateVersion =
  req
    "CreateProvisioningTemplateVersion"
    "fixture/CreateProvisioningTemplateVersion.yaml"

requestCreateOTAUpdate :: CreateOTAUpdate -> TestTree
requestCreateOTAUpdate =
  req
    "CreateOTAUpdate"
    "fixture/CreateOTAUpdate.yaml"

requestDescribeDefaultAuthorizer :: DescribeDefaultAuthorizer -> TestTree
requestDescribeDefaultAuthorizer =
  req
    "DescribeDefaultAuthorizer"
    "fixture/DescribeDefaultAuthorizer.yaml"

requestListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasks -> TestTree
requestListAuditMitigationActionsTasks =
  req
    "ListAuditMitigationActionsTasks"
    "fixture/ListAuditMitigationActionsTasks.yaml"

requestListThingRegistrationTaskReports :: ListThingRegistrationTaskReports -> TestTree
requestListThingRegistrationTaskReports =
  req
    "ListThingRegistrationTaskReports"
    "fixture/ListThingRegistrationTaskReports.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings =
  req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestRemoveThingFromThingGroup :: RemoveThingFromThingGroup -> TestTree
requestRemoveThingFromThingGroup =
  req
    "RemoveThingFromThingGroup"
    "fixture/RemoveThingFromThingGroup.yaml"

requestDescribeEventConfigurations :: DescribeEventConfigurations -> TestTree
requestDescribeEventConfigurations =
  req
    "DescribeEventConfigurations"
    "fixture/DescribeEventConfigurations.yaml"

requestListTopicRuleDestinations :: ListTopicRuleDestinations -> TestTree
requestListTopicRuleDestinations =
  req
    "ListTopicRuleDestinations"
    "fixture/ListTopicRuleDestinations.yaml"

requestRegisterCertificateWithoutCA :: RegisterCertificateWithoutCA -> TestTree
requestRegisterCertificateWithoutCA =
  req
    "RegisterCertificateWithoutCA"
    "fixture/RegisterCertificateWithoutCA.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListThingGroups :: ListThingGroups -> TestTree
requestListThingGroups =
  req
    "ListThingGroups"
    "fixture/ListThingGroups.yaml"

requestListScheduledAudits :: ListScheduledAudits -> TestTree
requestListScheduledAudits =
  req
    "ListScheduledAudits"
    "fixture/ListScheduledAudits.yaml"

requestDescribeThingRegistrationTask :: DescribeThingRegistrationTask -> TestTree
requestDescribeThingRegistrationTask =
  req
    "DescribeThingRegistrationTask"
    "fixture/DescribeThingRegistrationTask.yaml"

requestUpdateScheduledAudit :: UpdateScheduledAudit -> TestTree
requestUpdateScheduledAudit =
  req
    "UpdateScheduledAudit"
    "fixture/UpdateScheduledAudit.yaml"

requestDeleteScheduledAudit :: DeleteScheduledAudit -> TestTree
requestDeleteScheduledAudit =
  req
    "DeleteScheduledAudit"
    "fixture/DeleteScheduledAudit.yaml"

requestDescribeAuditFinding :: DescribeAuditFinding -> TestTree
requestDescribeAuditFinding =
  req
    "DescribeAuditFinding"
    "fixture/DescribeAuditFinding.yaml"

requestDescribeDimension :: DescribeDimension -> TestTree
requestDescribeDimension =
  req
    "DescribeDimension"
    "fixture/DescribeDimension.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions =
  req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestDeleteAccountAuditConfiguration :: DeleteAccountAuditConfiguration -> TestTree
requestDeleteAccountAuditConfiguration =
  req
    "DeleteAccountAuditConfiguration"
    "fixture/DeleteAccountAuditConfiguration.yaml"

requestUpdateAccountAuditConfiguration :: UpdateAccountAuditConfiguration -> TestTree
requestUpdateAccountAuditConfiguration =
  req
    "UpdateAccountAuditConfiguration"
    "fixture/UpdateAccountAuditConfiguration.yaml"

requestGetOTAUpdate :: GetOTAUpdate -> TestTree
requestGetOTAUpdate =
  req
    "GetOTAUpdate"
    "fixture/GetOTAUpdate.yaml"

requestGetEffectivePolicies :: GetEffectivePolicies -> TestTree
requestGetEffectivePolicies =
  req
    "GetEffectivePolicies"
    "fixture/GetEffectivePolicies.yaml"

requestListThingTypes :: ListThingTypes -> TestTree
requestListThingTypes =
  req
    "ListThingTypes"
    "fixture/ListThingTypes.yaml"

requestSetV2LoggingOptions :: SetV2LoggingOptions -> TestTree
requestSetV2LoggingOptions =
  req
    "SetV2LoggingOptions"
    "fixture/SetV2LoggingOptions.yaml"

requestCreateProvisioningTemplate :: CreateProvisioningTemplate -> TestTree
requestCreateProvisioningTemplate =
  req
    "CreateProvisioningTemplate"
    "fixture/CreateProvisioningTemplate.yaml"

requestListThingGroupsForThing :: ListThingGroupsForThing -> TestTree
requestListThingGroupsForThing =
  req
    "ListThingGroupsForThing"
    "fixture/ListThingGroupsForThing.yaml"

requestCreateCertificateFromCSR :: CreateCertificateFromCSR -> TestTree
requestCreateCertificateFromCSR =
  req
    "CreateCertificateFromCSR"
    "fixture/CreateCertificateFromCSR.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing =
  req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestUpdateThing :: UpdateThing -> TestTree
requestUpdateThing =
  req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

requestDeleteProvisioningTemplate :: DeleteProvisioningTemplate -> TestTree
requestDeleteProvisioningTemplate =
  req
    "DeleteProvisioningTemplate"
    "fixture/DeleteProvisioningTemplate.yaml"

requestUpdateProvisioningTemplate :: UpdateProvisioningTemplate -> TestTree
requestUpdateProvisioningTemplate =
  req
    "UpdateProvisioningTemplate"
    "fixture/UpdateProvisioningTemplate.yaml"

requestDescribeMitigationAction :: DescribeMitigationAction -> TestTree
requestDescribeMitigationAction =
  req
    "DescribeMitigationAction"
    "fixture/DescribeMitigationAction.yaml"

requestStartThingRegistrationTask :: StartThingRegistrationTask -> TestTree
requestStartThingRegistrationTask =
  req
    "StartThingRegistrationTask"
    "fixture/StartThingRegistrationTask.yaml"

requestCreateScheduledAudit :: CreateScheduledAudit -> TestTree
requestCreateScheduledAudit =
  req
    "CreateScheduledAudit"
    "fixture/CreateScheduledAudit.yaml"

requestListAuthorizers :: ListAuthorizers -> TestTree
requestListAuthorizers =
  req
    "ListAuthorizers"
    "fixture/ListAuthorizers.yaml"

requestListJobExecutionsForJob :: ListJobExecutionsForJob -> TestTree
requestListJobExecutionsForJob =
  req
    "ListJobExecutionsForJob"
    "fixture/ListJobExecutionsForJob.yaml"

requestRemoveThingFromBillingGroup :: RemoveThingFromBillingGroup -> TestTree
requestRemoveThingFromBillingGroup =
  req
    "RemoveThingFromBillingGroup"
    "fixture/RemoveThingFromBillingGroup.yaml"

requestSearchIndex :: SearchIndex -> TestTree
requestSearchIndex =
  req
    "SearchIndex"
    "fixture/SearchIndex.yaml"

requestCreateThingType :: CreateThingType -> TestTree
requestCreateThingType =
  req
    "CreateThingType"
    "fixture/CreateThingType.yaml"

requestDescribeSecurityProfile :: DescribeSecurityProfile -> TestTree
requestDescribeSecurityProfile =
  req
    "DescribeSecurityProfile"
    "fixture/DescribeSecurityProfile.yaml"

requestDeleteV2LoggingLevel :: DeleteV2LoggingLevel -> TestTree
requestDeleteV2LoggingLevel =
  req
    "DeleteV2LoggingLevel"
    "fixture/DeleteV2LoggingLevel.yaml"

requestSetDefaultAuthorizer :: SetDefaultAuthorizer -> TestTree
requestSetDefaultAuthorizer =
  req
    "SetDefaultAuthorizer"
    "fixture/SetDefaultAuthorizer.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution =
  req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer =
  req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

requestGetIndexingConfiguration :: GetIndexingConfiguration -> TestTree
requestGetIndexingConfiguration =
  req
    "GetIndexingConfiguration"
    "fixture/GetIndexingConfiguration.yaml"

requestListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutions -> TestTree
requestListAuditMitigationActionsExecutions =
  req
    "ListAuditMitigationActionsExecutions"
    "fixture/ListAuditMitigationActionsExecutions.yaml"

requestDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTask -> TestTree
requestDescribeAuditMitigationActionsTask =
  req
    "DescribeAuditMitigationActionsTask"
    "fixture/DescribeAuditMitigationActionsTask.yaml"

requestGetStatistics :: GetStatistics -> TestTree
requestGetStatistics =
  req
    "GetStatistics"
    "fixture/GetStatistics.yaml"

requestDeleteRoleAlias :: DeleteRoleAlias -> TestTree
requestDeleteRoleAlias =
  req
    "DeleteRoleAlias"
    "fixture/DeleteRoleAlias.yaml"

requestUpdateRoleAlias :: UpdateRoleAlias -> TestTree
requestUpdateRoleAlias =
  req
    "UpdateRoleAlias"
    "fixture/UpdateRoleAlias.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule =
  req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestCreateTopicRule :: CreateTopicRule -> TestTree
requestCreateTopicRule =
  req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestAssociateTargetsWithJob :: AssociateTargetsWithJob -> TestTree
requestAssociateTargetsWithJob =
  req
    "AssociateTargetsWithJob"
    "fixture/AssociateTargetsWithJob.yaml"

requestAttachSecurityProfile :: AttachSecurityProfile -> TestTree
requestAttachSecurityProfile =
  req
    "AttachSecurityProfile"
    "fixture/AttachSecurityProfile.yaml"

requestListAttachedPolicies :: ListAttachedPolicies -> TestTree
requestListAttachedPolicies =
  req
    "ListAttachedPolicies"
    "fixture/ListAttachedPolicies.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates =
  req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule =
  req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

requestGetJobDocument :: GetJobDocument -> TestTree
requestGetJobDocument =
  req
    "GetJobDocument"
    "fixture/GetJobDocument.yaml"

requestDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersion -> TestTree
requestDescribeProvisioningTemplateVersion =
  req
    "DescribeProvisioningTemplateVersion"
    "fixture/DescribeProvisioningTemplateVersion.yaml"

requestCancelAuditTask :: CancelAuditTask -> TestTree
requestCancelAuditTask =
  req
    "CancelAuditTask"
    "fixture/CancelAuditTask.yaml"

requestCreateRoleAlias :: CreateRoleAlias -> TestTree
requestCreateRoleAlias =
  req
    "CreateRoleAlias"
    "fixture/CreateRoleAlias.yaml"

requestDeleteCACertificate :: DeleteCACertificate -> TestTree
requestDeleteCACertificate =
  req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

requestUpdateCACertificate :: UpdateCACertificate -> TestTree
requestUpdateCACertificate =
  req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules =
  req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate =
  req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListRoleAliases :: ListRoleAliases -> TestTree
requestListRoleAliases =
  req
    "ListRoleAliases"
    "fixture/ListRoleAliases.yaml"

requestStartOnDemandAuditTask :: StartOnDemandAuditTask -> TestTree
requestStartOnDemandAuditTask =
  req
    "StartOnDemandAuditTask"
    "fixture/StartOnDemandAuditTask.yaml"

requestDescribeThingGroup :: DescribeThingGroup -> TestTree
requestDescribeThingGroup =
  req
    "DescribeThingGroup"
    "fixture/DescribeThingGroup.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestListTargetsForSecurityProfile :: ListTargetsForSecurityProfile -> TestTree
requestListTargetsForSecurityProfile =
  req
    "ListTargetsForSecurityProfile"
    "fixture/ListTargetsForSecurityProfile.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestStartAuditMitigationActionsTask :: StartAuditMitigationActionsTask -> TestTree
requestStartAuditMitigationActionsTask =
  req
    "StartAuditMitigationActionsTask"
    "fixture/StartAuditMitigationActionsTask.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule =
  req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestDescribeThing :: DescribeThing -> TestTree
requestDescribeThing =
  req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

requestListDomainConfigurations :: ListDomainConfigurations -> TestTree
requestListDomainConfigurations =
  req
    "ListDomainConfigurations"
    "fixture/ListDomainConfigurations.yaml"

requestListAuditTasks :: ListAuditTasks -> TestTree
requestListAuditTasks =
  req
    "ListAuditTasks"
    "fixture/ListAuditTasks.yaml"

requestDescribeAccountAuditConfiguration :: DescribeAccountAuditConfiguration -> TestTree
requestDescribeAccountAuditConfiguration =
  req
    "DescribeAccountAuditConfiguration"
    "fixture/DescribeAccountAuditConfiguration.yaml"

requestDeleteDimension :: DeleteDimension -> TestTree
requestDeleteDimension =
  req
    "DeleteDimension"
    "fixture/DeleteDimension.yaml"

requestUpdateDimension :: UpdateDimension -> TestTree
requestUpdateDimension =
  req
    "UpdateDimension"
    "fixture/UpdateDimension.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestListThingsInThingGroup :: ListThingsInThingGroup -> TestTree
requestListThingsInThingGroup =
  req
    "ListThingsInThingGroup"
    "fixture/ListThingsInThingGroup.yaml"

requestListAuditFindings :: ListAuditFindings -> TestTree
requestListAuditFindings =
  req
    "ListAuditFindings"
    "fixture/ListAuditFindings.yaml"

requestDescribeScheduledAudit :: DescribeScheduledAudit -> TestTree
requestDescribeScheduledAudit =
  req
    "DescribeScheduledAudit"
    "fixture/DescribeScheduledAudit.yaml"

requestCreateMitigationAction :: CreateMitigationAction -> TestTree
requestCreateMitigationAction =
  req
    "CreateMitigationAction"
    "fixture/CreateMitigationAction.yaml"

requestConfirmTopicRuleDestination :: ConfirmTopicRuleDestination -> TestTree
requestConfirmTopicRuleDestination =
  req
    "ConfirmTopicRuleDestination"
    "fixture/ConfirmTopicRuleDestination.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestListMitigationActions :: ListMitigationActions -> TestTree
requestListMitigationActions =
  req
    "ListMitigationActions"
    "fixture/ListMitigationActions.yaml"

requestDescribeAuthorizer :: DescribeAuthorizer -> TestTree
requestDescribeAuthorizer =
  req
    "DescribeAuthorizer"
    "fixture/DescribeAuthorizer.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion =
  req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestListActiveViolations :: ListActiveViolations -> TestTree
requestListActiveViolations =
  req
    "ListActiveViolations"
    "fixture/ListActiveViolations.yaml"

requestValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviors -> TestTree
requestValidateSecurityProfileBehaviors =
  req
    "ValidateSecurityProfileBehaviors"
    "fixture/ValidateSecurityProfileBehaviors.yaml"

requestListViolationEvents :: ListViolationEvents -> TestTree
requestListViolationEvents =
  req
    "ListViolationEvents"
    "fixture/ListViolationEvents.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate =
  req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestCreateDimension :: CreateDimension -> TestTree
requestCreateDimension =
  req
    "CreateDimension"
    "fixture/CreateDimension.yaml"

requestUpdateIndexingConfiguration :: UpdateIndexingConfiguration -> TestTree
requestUpdateIndexingConfiguration =
  req
    "UpdateIndexingConfiguration"
    "fixture/UpdateIndexingConfiguration.yaml"

requestCreateProvisioningClaim :: CreateProvisioningClaim -> TestTree
requestCreateProvisioningClaim =
  req
    "CreateProvisioningClaim"
    "fixture/CreateProvisioningClaim.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup =
  req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestCreateTopicRuleDestination :: CreateTopicRuleDestination -> TestTree
requestCreateTopicRuleDestination =
  req
    "CreateTopicRuleDestination"
    "fixture/CreateTopicRuleDestination.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestAddThingToBillingGroup :: AddThingToBillingGroup -> TestTree
requestAddThingToBillingGroup =
  req
    "AddThingToBillingGroup"
    "fixture/AddThingToBillingGroup.yaml"

requestUpdateTopicRuleDestination :: UpdateTopicRuleDestination -> TestTree
requestUpdateTopicRuleDestination =
  req
    "UpdateTopicRuleDestination"
    "fixture/UpdateTopicRuleDestination.yaml"

requestDeleteTopicRuleDestination :: DeleteTopicRuleDestination -> TestTree
requestDeleteTopicRuleDestination =
  req
    "DeleteTopicRuleDestination"
    "fixture/DeleteTopicRuleDestination.yaml"

requestDeleteThingGroup :: DeleteThingGroup -> TestTree
requestDeleteThingGroup =
  req
    "DeleteThingGroup"
    "fixture/DeleteThingGroup.yaml"

requestUpdateThingGroup :: UpdateThingGroup -> TestTree
requestUpdateThingGroup =
  req
    "UpdateThingGroup"
    "fixture/UpdateThingGroup.yaml"

requestListOTAUpdates :: ListOTAUpdates -> TestTree
requestListOTAUpdates =
  req
    "ListOTAUpdates"
    "fixture/ListOTAUpdates.yaml"

requestDeleteOTAUpdate :: DeleteOTAUpdate -> TestTree
requestDeleteOTAUpdate =
  req
    "DeleteOTAUpdate"
    "fixture/DeleteOTAUpdate.yaml"

requestCreateDynamicThingGroup :: CreateDynamicThingGroup -> TestTree
requestCreateDynamicThingGroup =
  req
    "CreateDynamicThingGroup"
    "fixture/CreateDynamicThingGroup.yaml"

requestDetachSecurityProfile :: DetachSecurityProfile -> TestTree
requestDetachSecurityProfile =
  req
    "DetachSecurityProfile"
    "fixture/DetachSecurityProfile.yaml"

requestListOutgoingCertificates :: ListOutgoingCertificates -> TestTree
requestListOutgoingCertificates =
  req
    "ListOutgoingCertificates"
    "fixture/ListOutgoingCertificates.yaml"

requestDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersion -> TestTree
requestDeleteProvisioningTemplateVersion =
  req
    "DeleteProvisioningTemplateVersion"
    "fixture/DeleteProvisioningTemplateVersion.yaml"

requestDescribeCACertificate :: DescribeCACertificate -> TestTree
requestDescribeCACertificate =
  req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

requestListProvisioningTemplateVersions :: ListProvisioningTemplateVersions -> TestTree
requestListProvisioningTemplateVersions =
  req
    "ListProvisioningTemplateVersions"
    "fixture/ListProvisioningTemplateVersions.yaml"

requestGetRegistrationCode :: GetRegistrationCode -> TestTree
requestGetRegistrationCode =
  req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

requestListBillingGroups :: ListBillingGroups -> TestTree
requestListBillingGroups =
  req
    "ListBillingGroups"
    "fixture/ListBillingGroups.yaml"

requestDeleteThingType :: DeleteThingType -> TestTree
requestDeleteThingType =
  req
    "DeleteThingType"
    "fixture/DeleteThingType.yaml"

requestDeleteBillingGroup :: DeleteBillingGroup -> TestTree
requestDeleteBillingGroup =
  req
    "DeleteBillingGroup"
    "fixture/DeleteBillingGroup.yaml"

requestAddThingToThingGroup :: AddThingToThingGroup -> TestTree
requestAddThingToThingGroup =
  req
    "AddThingToThingGroup"
    "fixture/AddThingToThingGroup.yaml"

requestUpdateBillingGroup :: UpdateBillingGroup -> TestTree
requestUpdateBillingGroup =
  req
    "UpdateBillingGroup"
    "fixture/UpdateBillingGroup.yaml"

requestGetTopicRuleDestination :: GetTopicRuleDestination -> TestTree
requestGetTopicRuleDestination =
  req
    "GetTopicRuleDestination"
    "fixture/GetTopicRuleDestination.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA =
  req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestUpdateAuditSuppression :: UpdateAuditSuppression -> TestTree
requestUpdateAuditSuppression =
  req
    "UpdateAuditSuppression"
    "fixture/UpdateAuditSuppression.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal =
  req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestListThings :: ListThings -> TestTree
requestListThings =
  req
    "ListThings"
    "fixture/ListThings.yaml"

requestDeleteAuditSuppression :: DeleteAuditSuppression -> TestTree
requestDeleteAuditSuppression =
  req
    "DeleteAuditSuppression"
    "fixture/DeleteAuditSuppression.yaml"

requestRegisterThing :: RegisterThing -> TestTree
requestRegisterThing =
  req
    "RegisterThing"
    "fixture/RegisterThing.yaml"

requestListAuditSuppressions :: ListAuditSuppressions -> TestTree
requestListAuditSuppressions =
  req
    "ListAuditSuppressions"
    "fixture/ListAuditSuppressions.yaml"

requestDescribeDomainConfiguration :: DescribeDomainConfiguration -> TestTree
requestDescribeDomainConfiguration =
  req
    "DescribeDomainConfiguration"
    "fixture/DescribeDomainConfiguration.yaml"

requestDescribeAuditTask :: DescribeAuditTask -> TestTree
requestDescribeAuditTask =
  req
    "DescribeAuditTask"
    "fixture/DescribeAuditTask.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode =
  req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream =
  req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestTestAuthorization :: TestAuthorization -> TestTree
requestTestAuthorization =
  req
    "TestAuthorization"
    "fixture/TestAuthorization.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices =
  req
    "ListIndices"
    "fixture/ListIndices.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateThing :: CreateThing -> TestTree
requestCreateThing =
  req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTask -> TestTree
requestCancelAuditMitigationActionsTask =
  req
    "CancelAuditMitigationActionsTask"
    "fixture/CancelAuditMitigationActionsTask.yaml"

requestCreateAuditSuppression :: CreateAuditSuppression -> TestTree
requestCreateAuditSuppression =
  req
    "CreateAuditSuppression"
    "fixture/CreateAuditSuppression.yaml"

requestCreateBillingGroup :: CreateBillingGroup -> TestTree
requestCreateBillingGroup =
  req
    "CreateBillingGroup"
    "fixture/CreateBillingGroup.yaml"

requestListProvisioningTemplates :: ListProvisioningTemplates -> TestTree
requestListProvisioningTemplates =
  req
    "ListProvisioningTemplates"
    "fixture/ListProvisioningTemplates.yaml"

requestListV2LoggingLevels :: ListV2LoggingLevels -> TestTree
requestListV2LoggingLevels =
  req
    "ListV2LoggingLevels"
    "fixture/ListV2LoggingLevels.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStopThingRegistrationTask :: StopThingRegistrationTask -> TestTree
requestStopThingRegistrationTask =
  req
    "StopThingRegistrationTask"
    "fixture/StopThingRegistrationTask.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestClearDefaultAuthorizer :: ClearDefaultAuthorizer -> TestTree
requestClearDefaultAuthorizer =
  req
    "ClearDefaultAuthorizer"
    "fixture/ClearDefaultAuthorizer.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule =
  req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestCancelJobExecution :: CancelJobExecution -> TestTree
requestCancelJobExecution =
  req
    "CancelJobExecution"
    "fixture/CancelJobExecution.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestSetV2LoggingLevel :: SetV2LoggingLevel -> TestTree
requestSetV2LoggingLevel =
  req
    "SetV2LoggingLevel"
    "fixture/SetV2LoggingLevel.yaml"

requestListJobExecutionsForThing :: ListJobExecutionsForThing -> TestTree
requestListJobExecutionsForThing =
  req
    "ListJobExecutionsForThing"
    "fixture/ListJobExecutionsForThing.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
requestCreateKeysAndCertificate =
  req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

requestListThingsInBillingGroup :: ListThingsInBillingGroup -> TestTree
requestListThingsInBillingGroup =
  req
    "ListThingsInBillingGroup"
    "fixture/ListThingsInBillingGroup.yaml"

requestUpdateThingGroupsForThing :: UpdateThingGroupsForThing -> TestTree
requestUpdateThingGroupsForThing =
  req
    "UpdateThingGroupsForThing"
    "fixture/UpdateThingGroupsForThing.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule =
  req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
requestAcceptCertificateTransfer =
  req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

requestGetPercentiles :: GetPercentiles -> TestTree
requestGetPercentiles =
  req
    "GetPercentiles"
    "fixture/GetPercentiles.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListSecurityProfilesForTarget :: ListSecurityProfilesForTarget -> TestTree
requestListSecurityProfilesForTarget =
  req
    "ListSecurityProfilesForTarget"
    "fixture/ListSecurityProfilesForTarget.yaml"

requestUpdateEventConfigurations :: UpdateEventConfigurations -> TestTree
requestUpdateEventConfigurations =
  req
    "UpdateEventConfigurations"
    "fixture/UpdateEventConfigurations.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate =
  req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

requestDeleteDomainConfiguration :: DeleteDomainConfiguration -> TestTree
requestDeleteDomainConfiguration =
  req
    "DeleteDomainConfiguration"
    "fixture/DeleteDomainConfiguration.yaml"

requestUpdateDomainConfiguration :: UpdateDomainConfiguration -> TestTree
requestUpdateDomainConfiguration =
  req
    "UpdateDomainConfiguration"
    "fixture/UpdateDomainConfiguration.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions =
  req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestDescribeThingType :: DescribeThingType -> TestTree
requestDescribeThingType =
  req
    "DescribeThingType"
    "fixture/DescribeThingType.yaml"

requestListDimensions :: ListDimensions -> TestTree
requestListDimensions =
  req
    "ListDimensions"
    "fixture/ListDimensions.yaml"

requestGetV2LoggingOptions :: GetV2LoggingOptions -> TestTree
requestGetV2LoggingOptions =
  req
    "GetV2LoggingOptions"
    "fixture/GetV2LoggingOptions.yaml"

requestListThingRegistrationTasks :: ListThingRegistrationTasks -> TestTree
requestListThingRegistrationTasks =
  req
    "ListThingRegistrationTasks"
    "fixture/ListThingRegistrationTasks.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer =
  req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestDescribeAuditSuppression :: DescribeAuditSuppression -> TestTree
requestDescribeAuditSuppression =
  req
    "DescribeAuditSuppression"
    "fixture/DescribeAuditSuppression.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestCreateSecurityProfile :: CreateSecurityProfile -> TestTree
requestCreateSecurityProfile =
  req
    "CreateSecurityProfile"
    "fixture/CreateSecurityProfile.yaml"

requestDescribeBillingGroup :: DescribeBillingGroup -> TestTree
requestDescribeBillingGroup =
  req
    "DescribeBillingGroup"
    "fixture/DescribeBillingGroup.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal =
  req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestDeprecateThingType :: DeprecateThingType -> TestTree
requestDeprecateThingType =
  req
    "DeprecateThingType"
    "fixture/DeprecateThingType.yaml"

-- Responses

responseGetCardinality :: GetCardinalityResponse -> TestTree
responseGetCardinality =
  res
    "GetCardinalityResponse"
    "fixture/GetCardinalityResponse.proto"
    ioT
    (Proxy :: Proxy GetCardinality)

responseCreateDomainConfiguration :: CreateDomainConfigurationResponse -> TestTree
responseCreateDomainConfiguration =
  res
    "CreateDomainConfigurationResponse"
    "fixture/CreateDomainConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy CreateDomainConfiguration)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy DeleteSecurityProfile)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy UpdateSecurityProfile)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    ioT
    (Proxy :: Proxy ListSecurityProfiles)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicies)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeProvisioningTemplate)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    ioT
    (Proxy :: Proxy UpdateMitigationAction)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    ioT
    (Proxy :: Proxy DeleteMitigationAction)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    ioT
    (Proxy :: Proxy DeleteJobExecution)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicy)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCertificate)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DeleteDynamicThingGroup)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingPrincipals)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy UpdateDynamicThingGroup)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy DescribeRoleAlias)

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    ioT
    (Proxy :: Proxy CreateProvisioningTemplateVersion)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy CreateOTAUpdate)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DescribeDefaultAuthorizer)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    ioT
    (Proxy :: Proxy ListAuditMitigationActionsTasks)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingRegistrationTaskReports)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalThings)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup =
  res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy RemoveThingFromThingGroup)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEventConfigurations)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    ioT
    (Proxy :: Proxy ListTopicRuleDestinations)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCertificateWithoutCA)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ioT
    (Proxy :: Proxy ListTagsForResource)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingGroups)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    ioT
    (Proxy :: Proxy ListScheduledAudits)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingRegistrationTask)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    ioT
    (Proxy :: Proxy UpdateScheduledAudit)

responseDeleteScheduledAudit :: DeleteScheduledAuditResponse -> TestTree
responseDeleteScheduledAudit =
  res
    "DeleteScheduledAuditResponse"
    "fixture/DeleteScheduledAuditResponse.proto"
    ioT
    (Proxy :: Proxy DeleteScheduledAudit)

responseDescribeAuditFinding :: DescribeAuditFindingResponse -> TestTree
responseDescribeAuditFinding =
  res
    "DescribeAuditFindingResponse"
    "fixture/DescribeAuditFindingResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuditFinding)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeDimension)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetLoggingOptions)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy DeleteAccountAuditConfiguration)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy UpdateAccountAuditConfiguration)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy GetOTAUpdate)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    ioT
    (Proxy :: Proxy GetEffectivePolicies)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    ioT
    (Proxy :: Proxy ListThingTypes)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetV2LoggingOptions)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    ioT
    (Proxy :: Proxy CreateProvisioningTemplate)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    ioT
    (Proxy :: Proxy ListThingGroupsForThing)

responseCreateCertificateFromCSR :: CreateCertificateFromCSRResponse -> TestTree
responseCreateCertificateFromCSR =
  res
    "CreateCertificateFromCSRResponse"
    "fixture/CreateCertificateFromCSRResponse.proto"
    ioT
    (Proxy :: Proxy CreateCertificateFromCSR)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThing)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThing)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteProvisioningTemplate)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateProvisioningTemplate)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeMitigationAction)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy StartThingRegistrationTask)

responseCreateScheduledAudit :: CreateScheduledAuditResponse -> TestTree
responseCreateScheduledAudit =
  res
    "CreateScheduledAuditResponse"
    "fixture/CreateScheduledAuditResponse.proto"
    ioT
    (Proxy :: Proxy CreateScheduledAudit)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    ioT
    (Proxy :: Proxy ListAuthorizers)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    ioT
    (Proxy :: Proxy ListJobExecutionsForJob)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy RemoveThingFromBillingGroup)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    ioT
    (Proxy :: Proxy SearchIndex)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType =
  res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy CreateThingType)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy DescribeSecurityProfile)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    ioT
    (Proxy :: Proxy DeleteV2LoggingLevel)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultAuthorizer)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeJobExecution)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy CancelCertificateTransfer)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy GetIndexingConfiguration)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    ioT
    (Proxy :: Proxy ListAuditMitigationActionsExecutions)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuditMitigationActionsTask)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    ioT
    (Proxy :: Proxy GetStatistics)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRoleAlias)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy UpdateRoleAlias)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicyVersion)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DisableTopicRule)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule =
  res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy CreateTopicRule)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    ioT
    (Proxy :: Proxy CreateJob)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    ioT
    (Proxy :: Proxy DescribeIndex)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    ioT
    (Proxy :: Proxy AssociateTargetsWithJob)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy AttachSecurityProfile)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListAttachedPolicies)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicyVersion)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCACertificates)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DeleteTopicRule)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    ioT
    (Proxy :: Proxy GetJobDocument)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeProvisioningTemplateVersion)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    ioT
    (Proxy :: Proxy CancelAuditTask)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    ioT
    (Proxy :: Proxy CreateRoleAlias)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate =
  res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCACertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate =
  res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCACertificate)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    ioT
    (Proxy :: Proxy ListTopicRules)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    ioT
    (Proxy :: Proxy TransferCertificate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    ioT
    (Proxy :: Proxy ListJobs)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    ioT
    (Proxy :: Proxy ListRoleAliases)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    ioT
    (Proxy :: Proxy StartOnDemandAuditTask)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingGroup)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    ioT
    (Proxy :: Proxy DeleteJob)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy ListTargetsForSecurityProfile)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    ioT
    (Proxy :: Proxy UpdateJob)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    ioT
    (Proxy :: Proxy StartAuditMitigationActionsTask)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy GetTopicRule)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing =
  res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThing)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    ioT
    (Proxy :: Proxy ListDomainConfigurations)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    ioT
    (Proxy :: Proxy ListAuditTasks)

responseDescribeAccountAuditConfiguration :: DescribeAccountAuditConfigurationResponse -> TestTree
responseDescribeAccountAuditConfiguration =
  res
    "DescribeAccountAuditConfigurationResponse"
    "fixture/DescribeAccountAuditConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAccountAuditConfiguration)

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    ioT
    (Proxy :: Proxy DeleteDimension)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    ioT
    (Proxy :: Proxy UpdateDimension)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicy)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy ListThingsInThingGroup)

responseListAuditFindings :: ListAuditFindingsResponse -> TestTree
responseListAuditFindings =
  res
    "ListAuditFindingsResponse"
    "fixture/ListAuditFindingsResponse.proto"
    ioT
    (Proxy :: Proxy ListAuditFindings)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    ioT
    (Proxy :: Proxy DescribeScheduledAudit)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    ioT
    (Proxy :: Proxy CreateMitigationAction)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    ioT
    (Proxy :: Proxy ConfirmTopicRuleDestination)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificates)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    ioT
    (Proxy :: Proxy ListMitigationActions)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuthorizer)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicyVersion)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    ioT
    (Proxy :: Proxy ListActiveViolations)

responseValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviorsResponse -> TestTree
responseValidateSecurityProfileBehaviors =
  res
    "ValidateSecurityProfileBehaviorsResponse"
    "fixture/ValidateSecurityProfileBehaviorsResponse.proto"
    ioT
    (Proxy :: Proxy ValidateSecurityProfileBehaviors)

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    ioT
    (Proxy :: Proxy ListViolationEvents)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCertificate)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    ioT
    (Proxy :: Proxy CreateDimension)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy UpdateIndexingConfiguration)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    ioT
    (Proxy :: Proxy CreateProvisioningClaim)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy TestInvokeAuthorizer)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy CreateThingGroup)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    ioT
    (Proxy :: Proxy CreateTopicRuleDestination)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    ioT
    (Proxy :: Proxy DetachPolicy)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    ioT
    (Proxy :: Proxy DescribeJob)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy AddThingToBillingGroup)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    ioT
    (Proxy :: Proxy UpdateTopicRuleDestination)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    ioT
    (Proxy :: Proxy DeleteTopicRuleDestination)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThingGroup)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThingGroup)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    ioT
    (Proxy :: Proxy ListOTAUpdates)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteOTAUpdate)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy CreateDynamicThingGroup)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy DetachSecurityProfile)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListOutgoingCertificates)

responseDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersionResponse -> TestTree
responseDeleteProvisioningTemplateVersion =
  res
    "DeleteProvisioningTemplateVersionResponse"
    "fixture/DeleteProvisioningTemplateVersionResponse.proto"
    ioT
    (Proxy :: Proxy DeleteProvisioningTemplateVersion)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate =
  res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCACertificate)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    ioT
    (Proxy :: Proxy ListProvisioningTemplateVersions)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy GetRegistrationCode)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    ioT
    (Proxy :: Proxy ListBillingGroups)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType =
  res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThingType)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DeleteBillingGroup)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    ioT
    (Proxy :: Proxy AddThingToThingGroup)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy UpdateBillingGroup)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    ioT
    (Proxy :: Proxy GetTopicRuleDestination)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificatesByCA)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    ioT
    (Proxy :: Proxy UpdateAuditSuppression)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy AttachThingPrincipal)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListThings)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    ioT
    (Proxy :: Proxy DeleteAuditSuppression)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing =
  res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    ioT
    (Proxy :: Proxy RegisterThing)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    ioT
    (Proxy :: Proxy ListAuditSuppressions)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy DescribeDomainConfiguration)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuditTask)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRegistrationCode)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    ioT
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    ioT
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    ioT
    (Proxy :: Proxy ListStreams)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy CreateAuthorizer)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    ioT
    (Proxy :: Proxy TestAuthorization)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    ioT
    (Proxy :: Proxy ListIndices)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    ioT
    (Proxy :: Proxy CreateThing)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    ioT
    (Proxy :: Proxy CreateStream)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    ioT
    (Proxy :: Proxy CancelAuditMitigationActionsTask)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    ioT
    (Proxy :: Proxy CreateAuditSuppression)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy CreateBillingGroup)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    ioT
    (Proxy :: Proxy ListProvisioningTemplates)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    ioT
    (Proxy :: Proxy ListV2LoggingLevels)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    ioT
    (Proxy :: Proxy TagResource)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask =
  res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    ioT
    (Proxy :: Proxy StopThingRegistrationTask)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCertificate)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    ioT
    (Proxy :: Proxy ListTargetsForPolicy)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer =
  res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    ioT
    (Proxy :: Proxy ClearDefaultAuthorizer)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy ReplaceTopicRule)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    ioT
    (Proxy :: Proxy UntagResource)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    ioT
    (Proxy :: Proxy CancelJobExecution)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicyVersions)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    ioT
    (Proxy :: Proxy SetV2LoggingLevel)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    ioT
    (Proxy :: Proxy ListJobExecutionsForThing)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    ioT
    (Proxy :: Proxy AttachPolicy)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate =
  res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    ioT
    (Proxy :: Proxy CreateKeysAndCertificate)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy ListThingsInBillingGroup)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThingGroupsForThing)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy EnableTopicRule)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy AcceptCertificateTransfer)

responseGetPercentiles :: GetPercentilesResponse -> TestTree
responseGetPercentiles =
  res
    "GetPercentilesResponse"
    "fixture/GetPercentilesResponse.proto"
    ioT
    (Proxy :: Proxy GetPercentiles)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicy)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEndpoint)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    ioT
    (Proxy :: Proxy ListSecurityProfilesForTarget)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    ioT
    (Proxy :: Proxy UpdateEventConfigurations)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCACertificate)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy DeleteDomainConfiguration)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    ioT
    (Proxy :: Proxy UpdateDomainConfiguration)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetLoggingOptions)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThingType)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    ioT
    (Proxy :: Proxy ListDimensions)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetV2LoggingOptions)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    ioT
    (Proxy :: Proxy ListThingRegistrationTasks)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy RejectCertificateTransfer)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    ioT
    (Proxy :: Proxy DescribeAuditSuppression)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    ioT
    (Proxy :: Proxy DescribeStream)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    ioT
    (Proxy :: Proxy CreateSecurityProfile)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    ioT
    (Proxy :: Proxy DescribeBillingGroup)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy DetachThingPrincipal)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    ioT
    (Proxy :: Proxy CancelJob)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType =
  res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    ioT
    (Proxy :: Proxy DeprecateThingType)
