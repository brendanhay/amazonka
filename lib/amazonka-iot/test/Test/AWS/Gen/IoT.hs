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
--             mkGetCardinality
--
--         , requestCreateDomainConfiguration $
--             mkCreateDomainConfiguration
--
--         , requestDeleteSecurityProfile $
--             mkDeleteSecurityProfile
--
--         , requestUpdateSecurityProfile $
--             mkUpdateSecurityProfile
--
--         , requestListSecurityProfiles $
--             mkListSecurityProfiles
--
--         , requestListPolicies $
--             mkListPolicies
--
--         , requestDescribeProvisioningTemplate $
--             mkDescribeProvisioningTemplate
--
--         , requestUpdateMitigationAction $
--             mkUpdateMitigationAction
--
--         , requestDeleteMitigationAction $
--             mkDeleteMitigationAction
--
--         , requestDeleteJobExecution $
--             mkDeleteJobExecution
--
--         , requestCreatePolicy $
--             mkCreatePolicy
--
--         , requestRegisterCertificate $
--             mkRegisterCertificate
--
--         , requestDeleteDynamicThingGroup $
--             mkDeleteDynamicThingGroup
--
--         , requestListThingPrincipals $
--             mkListThingPrincipals
--
--         , requestUpdateDynamicThingGroup $
--             mkUpdateDynamicThingGroup
--
--         , requestDescribeRoleAlias $
--             mkDescribeRoleAlias
--
--         , requestCreateProvisioningTemplateVersion $
--             mkCreateProvisioningTemplateVersion
--
--         , requestCreateOTAUpdate $
--             mkCreateOTAUpdate
--
--         , requestDescribeDefaultAuthorizer $
--             mkDescribeDefaultAuthorizer
--
--         , requestListAuditMitigationActionsTasks $
--             mkListAuditMitigationActionsTasks
--
--         , requestListThingRegistrationTaskReports $
--             mkListThingRegistrationTaskReports
--
--         , requestListPrincipalThings $
--             mkListPrincipalThings
--
--         , requestRemoveThingFromThingGroup $
--             mkRemoveThingFromThingGroup
--
--         , requestDescribeEventConfigurations $
--             mkDescribeEventConfigurations
--
--         , requestListTopicRuleDestinations $
--             mkListTopicRuleDestinations
--
--         , requestRegisterCertificateWithoutCA $
--             mkRegisterCertificateWithoutCA
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListThingGroups $
--             mkListThingGroups
--
--         , requestListScheduledAudits $
--             mkListScheduledAudits
--
--         , requestDescribeThingRegistrationTask $
--             mkDescribeThingRegistrationTask
--
--         , requestUpdateScheduledAudit $
--             mkUpdateScheduledAudit
--
--         , requestDeleteScheduledAudit $
--             mkDeleteScheduledAudit
--
--         , requestDescribeAuditFinding $
--             mkDescribeAuditFinding
--
--         , requestDescribeDimension $
--             mkDescribeDimension
--
--         , requestGetLoggingOptions $
--             mkGetLoggingOptions
--
--         , requestDeleteAccountAuditConfiguration $
--             mkDeleteAccountAuditConfiguration
--
--         , requestUpdateAccountAuditConfiguration $
--             mkUpdateAccountAuditConfiguration
--
--         , requestGetOTAUpdate $
--             mkGetOTAUpdate
--
--         , requestGetEffectivePolicies $
--             mkGetEffectivePolicies
--
--         , requestListThingTypes $
--             mkListThingTypes
--
--         , requestSetV2LoggingOptions $
--             mkSetV2LoggingOptions
--
--         , requestCreateProvisioningTemplate $
--             mkCreateProvisioningTemplate
--
--         , requestListThingGroupsForThing $
--             mkListThingGroupsForThing
--
--         , requestCreateCertificateFromCsr $
--             mkCreateCertificateFromCsr
--
--         , requestDeleteThing $
--             mkDeleteThing
--
--         , requestUpdateThing $
--             mkUpdateThing
--
--         , requestDeleteProvisioningTemplate $
--             mkDeleteProvisioningTemplate
--
--         , requestUpdateProvisioningTemplate $
--             mkUpdateProvisioningTemplate
--
--         , requestDescribeMitigationAction $
--             mkDescribeMitigationAction
--
--         , requestStartThingRegistrationTask $
--             mkStartThingRegistrationTask
--
--         , requestCreateScheduledAudit $
--             mkCreateScheduledAudit
--
--         , requestListAuthorizers $
--             mkListAuthorizers
--
--         , requestListJobExecutionsForJob $
--             mkListJobExecutionsForJob
--
--         , requestRemoveThingFromBillingGroup $
--             mkRemoveThingFromBillingGroup
--
--         , requestSearchIndex $
--             mkSearchIndex
--
--         , requestCreateThingType $
--             mkCreateThingType
--
--         , requestDescribeSecurityProfile $
--             mkDescribeSecurityProfile
--
--         , requestDeleteV2LoggingLevel $
--             mkDeleteV2LoggingLevel
--
--         , requestSetDefaultAuthorizer $
--             mkSetDefaultAuthorizer
--
--         , requestDescribeJobExecution $
--             mkDescribeJobExecution
--
--         , requestCancelCertificateTransfer $
--             mkCancelCertificateTransfer
--
--         , requestGetIndexingConfiguration $
--             mkGetIndexingConfiguration
--
--         , requestListAuditMitigationActionsExecutions $
--             mkListAuditMitigationActionsExecutions
--
--         , requestDescribeAuditMitigationActionsTask $
--             mkDescribeAuditMitigationActionsTask
--
--         , requestGetStatistics $
--             mkGetStatistics
--
--         , requestDeleteRoleAlias $
--             mkDeleteRoleAlias
--
--         , requestUpdateRoleAlias $
--             mkUpdateRoleAlias
--
--         , requestDeletePolicyVersion $
--             mkDeletePolicyVersion
--
--         , requestDisableTopicRule $
--             mkDisableTopicRule
--
--         , requestCreateTopicRule $
--             mkCreateTopicRule
--
--         , requestCreateJob $
--             mkCreateJob
--
--         , requestDescribeIndex $
--             mkDescribeIndex
--
--         , requestAssociateTargetsWithJob $
--             mkAssociateTargetsWithJob
--
--         , requestAttachSecurityProfile $
--             mkAttachSecurityProfile
--
--         , requestListAttachedPolicies $
--             mkListAttachedPolicies
--
--         , requestCreatePolicyVersion $
--             mkCreatePolicyVersion
--
--         , requestListCACertificates $
--             mkListCACertificates
--
--         , requestDeleteTopicRule $
--             mkDeleteTopicRule
--
--         , requestGetJobDocument $
--             mkGetJobDocument
--
--         , requestDescribeProvisioningTemplateVersion $
--             mkDescribeProvisioningTemplateVersion
--
--         , requestCancelAuditTask $
--             mkCancelAuditTask
--
--         , requestCreateRoleAlias $
--             mkCreateRoleAlias
--
--         , requestDeleteCACertificate $
--             mkDeleteCACertificate
--
--         , requestUpdateCACertificate $
--             mkUpdateCACertificate
--
--         , requestListTopicRules $
--             mkListTopicRules
--
--         , requestTransferCertificate $
--             mkTransferCertificate
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestListRoleAliases $
--             mkListRoleAliases
--
--         , requestStartOnDemandAuditTask $
--             mkStartOnDemandAuditTask
--
--         , requestDescribeThingGroup $
--             mkDescribeThingGroup
--
--         , requestDeleteJob $
--             mkDeleteJob
--
--         , requestListTargetsForSecurityProfile $
--             mkListTargetsForSecurityProfile
--
--         , requestUpdateJob $
--             mkUpdateJob
--
--         , requestStartAuditMitigationActionsTask $
--             mkStartAuditMitigationActionsTask
--
--         , requestGetTopicRule $
--             mkGetTopicRule
--
--         , requestDescribeThing $
--             mkDescribeThing
--
--         , requestListDomainConfigurations $
--             mkListDomainConfigurations
--
--         , requestListAuditTasks $
--             mkListAuditTasks
--
--         , requestDescribeAccountAuditConfiguration $
--             mkDescribeAccountAuditConfiguration
--
--         , requestDeleteDimension $
--             mkDeleteDimension
--
--         , requestUpdateDimension $
--             mkUpdateDimension
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestListThingsInThingGroup $
--             mkListThingsInThingGroup
--
--         , requestListAuditFindings $
--             mkListAuditFindings
--
--         , requestDescribeScheduledAudit $
--             mkDescribeScheduledAudit
--
--         , requestCreateMitigationAction $
--             mkCreateMitigationAction
--
--         , requestConfirmTopicRuleDestination $
--             mkConfirmTopicRuleDestination
--
--         , requestListCertificates $
--             mkListCertificates
--
--         , requestListMitigationActions $
--             mkListMitigationActions
--
--         , requestDescribeAuthorizer $
--             mkDescribeAuthorizer
--
--         , requestGetPolicyVersion $
--             mkGetPolicyVersion
--
--         , requestListActiveViolations $
--             mkListActiveViolations
--
--         , requestValidateSecurityProfileBehaviors $
--             mkValidateSecurityProfileBehaviors
--
--         , requestListViolationEvents $
--             mkListViolationEvents
--
--         , requestDeleteCertificate $
--             mkDeleteCertificate
--
--         , requestUpdateCertificate $
--             mkUpdateCertificate
--
--         , requestCreateDimension $
--             mkCreateDimension
--
--         , requestUpdateIndexingConfiguration $
--             mkUpdateIndexingConfiguration
--
--         , requestCreateProvisioningClaim $
--             mkCreateProvisioningClaim
--
--         , requestTestInvokeAuthorizer $
--             mkTestInvokeAuthorizer
--
--         , requestCreateThingGroup $
--             mkCreateThingGroup
--
--         , requestCreateTopicRuleDestination $
--             mkCreateTopicRuleDestination
--
--         , requestDetachPolicy $
--             mkDetachPolicy
--
--         , requestDescribeJob $
--             mkDescribeJob
--
--         , requestAddThingToBillingGroup $
--             mkAddThingToBillingGroup
--
--         , requestUpdateTopicRuleDestination $
--             mkUpdateTopicRuleDestination
--
--         , requestDeleteTopicRuleDestination $
--             mkDeleteTopicRuleDestination
--
--         , requestDeleteThingGroup $
--             mkDeleteThingGroup
--
--         , requestUpdateThingGroup $
--             mkUpdateThingGroup
--
--         , requestListOTAUpdates $
--             mkListOTAUpdates
--
--         , requestDeleteOTAUpdate $
--             mkDeleteOTAUpdate
--
--         , requestCreateDynamicThingGroup $
--             mkCreateDynamicThingGroup
--
--         , requestDetachSecurityProfile $
--             mkDetachSecurityProfile
--
--         , requestListOutgoingCertificates $
--             mkListOutgoingCertificates
--
--         , requestDeleteProvisioningTemplateVersion $
--             mkDeleteProvisioningTemplateVersion
--
--         , requestDescribeCACertificate $
--             mkDescribeCACertificate
--
--         , requestListProvisioningTemplateVersions $
--             mkListProvisioningTemplateVersions
--
--         , requestGetRegistrationCode $
--             mkGetRegistrationCode
--
--         , requestListBillingGroups $
--             mkListBillingGroups
--
--         , requestDeleteThingType $
--             mkDeleteThingType
--
--         , requestDeleteBillingGroup $
--             mkDeleteBillingGroup
--
--         , requestAddThingToThingGroup $
--             mkAddThingToThingGroup
--
--         , requestUpdateBillingGroup $
--             mkUpdateBillingGroup
--
--         , requestGetTopicRuleDestination $
--             mkGetTopicRuleDestination
--
--         , requestListCertificatesByCA $
--             mkListCertificatesByCA
--
--         , requestUpdateAuditSuppression $
--             mkUpdateAuditSuppression
--
--         , requestAttachThingPrincipal $
--             mkAttachThingPrincipal
--
--         , requestListThings $
--             mkListThings
--
--         , requestDeleteAuditSuppression $
--             mkDeleteAuditSuppression
--
--         , requestRegisterThing $
--             mkRegisterThing
--
--         , requestListAuditSuppressions $
--             mkListAuditSuppressions
--
--         , requestDescribeDomainConfiguration $
--             mkDescribeDomainConfiguration
--
--         , requestDescribeAuditTask $
--             mkDescribeAuditTask
--
--         , requestDeleteRegistrationCode $
--             mkDeleteRegistrationCode
--
--         , requestUpdateStream $
--             mkUpdateStream
--
--         , requestDeleteStream $
--             mkDeleteStream
--
--         , requestListStreams $
--             mkListStreams
--
--         , requestCreateAuthorizer $
--             mkCreateAuthorizer
--
--         , requestTestAuthorization $
--             mkTestAuthorization
--
--         , requestListIndices $
--             mkListIndices
--
--         , requestUpdateAuthorizer $
--             mkUpdateAuthorizer
--
--         , requestDeleteAuthorizer $
--             mkDeleteAuthorizer
--
--         , requestCreateThing $
--             mkCreateThing
--
--         , requestCreateStream $
--             mkCreateStream
--
--         , requestCancelAuditMitigationActionsTask $
--             mkCancelAuditMitigationActionsTask
--
--         , requestCreateAuditSuppression $
--             mkCreateAuditSuppression
--
--         , requestCreateBillingGroup $
--             mkCreateBillingGroup
--
--         , requestListProvisioningTemplates $
--             mkListProvisioningTemplates
--
--         , requestListV2LoggingLevels $
--             mkListV2LoggingLevels
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestStopThingRegistrationTask $
--             mkStopThingRegistrationTask
--
--         , requestDescribeCertificate $
--             mkDescribeCertificate
--
--         , requestListTargetsForPolicy $
--             mkListTargetsForPolicy
--
--         , requestClearDefaultAuthorizer $
--             mkClearDefaultAuthorizer
--
--         , requestReplaceTopicRule $
--             mkReplaceTopicRule
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestSetDefaultPolicyVersion $
--             mkSetDefaultPolicyVersion
--
--         , requestCancelJobExecution $
--             mkCancelJobExecution
--
--         , requestListPolicyVersions $
--             mkListPolicyVersions
--
--         , requestSetV2LoggingLevel $
--             mkSetV2LoggingLevel
--
--         , requestListJobExecutionsForThing $
--             mkListJobExecutionsForThing
--
--         , requestAttachPolicy $
--             mkAttachPolicy
--
--         , requestCreateKeysAndCertificate $
--             mkCreateKeysAndCertificate
--
--         , requestListThingsInBillingGroup $
--             mkListThingsInBillingGroup
--
--         , requestUpdateThingGroupsForThing $
--             mkUpdateThingGroupsForThing
--
--         , requestEnableTopicRule $
--             mkEnableTopicRule
--
--         , requestAcceptCertificateTransfer $
--             mkAcceptCertificateTransfer
--
--         , requestGetPercentiles $
--             mkGetPercentiles
--
--         , requestGetPolicy $
--             mkGetPolicy
--
--         , requestDescribeEndpoint $
--             mkDescribeEndpoint
--
--         , requestListSecurityProfilesForTarget $
--             mkListSecurityProfilesForTarget
--
--         , requestUpdateEventConfigurations $
--             mkUpdateEventConfigurations
--
--         , requestRegisterCACertificate $
--             mkRegisterCACertificate
--
--         , requestDeleteDomainConfiguration $
--             mkDeleteDomainConfiguration
--
--         , requestUpdateDomainConfiguration $
--             mkUpdateDomainConfiguration
--
--         , requestSetLoggingOptions $
--             mkSetLoggingOptions
--
--         , requestDescribeThingType $
--             mkDescribeThingType
--
--         , requestListDimensions $
--             mkListDimensions
--
--         , requestGetV2LoggingOptions $
--             mkGetV2LoggingOptions
--
--         , requestListThingRegistrationTasks $
--             mkListThingRegistrationTasks
--
--         , requestRejectCertificateTransfer $
--             mkRejectCertificateTransfer
--
--         , requestDescribeAuditSuppression $
--             mkDescribeAuditSuppression
--
--         , requestDescribeStream $
--             mkDescribeStream
--
--         , requestCreateSecurityProfile $
--             mkCreateSecurityProfile
--
--         , requestDescribeBillingGroup $
--             mkDescribeBillingGroup
--
--         , requestDetachThingPrincipal $
--             mkDetachThingPrincipal
--
--         , requestCancelJob $
--             mkCancelJob
--
--         , requestDeprecateThingType $
--             mkDeprecateThingType
--
--           ]

--     , testGroup "response"
--         [ responseGetCardinality $
--             mkGetCardinalityResponse
--
--         , responseCreateDomainConfiguration $
--             mkCreateDomainConfigurationResponse
--
--         , responseDeleteSecurityProfile $
--             mkDeleteSecurityProfileResponse
--
--         , responseUpdateSecurityProfile $
--             mkUpdateSecurityProfileResponse
--
--         , responseListSecurityProfiles $
--             mkListSecurityProfilesResponse
--
--         , responseListPolicies $
--             mkListPoliciesResponse
--
--         , responseDescribeProvisioningTemplate $
--             mkDescribeProvisioningTemplateResponse
--
--         , responseUpdateMitigationAction $
--             mkUpdateMitigationActionResponse
--
--         , responseDeleteMitigationAction $
--             mkDeleteMitigationActionResponse
--
--         , responseDeleteJobExecution $
--             mkDeleteJobExecutionResponse
--
--         , responseCreatePolicy $
--             mkCreatePolicyResponse
--
--         , responseRegisterCertificate $
--             mkRegisterCertificateResponse
--
--         , responseDeleteDynamicThingGroup $
--             mkDeleteDynamicThingGroupResponse
--
--         , responseListThingPrincipals $
--             mkListThingPrincipalsResponse
--
--         , responseUpdateDynamicThingGroup $
--             mkUpdateDynamicThingGroupResponse
--
--         , responseDescribeRoleAlias $
--             mkDescribeRoleAliasResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             mkCreateProvisioningTemplateVersionResponse
--
--         , responseCreateOTAUpdate $
--             mkCreateOTAUpdateResponse
--
--         , responseDescribeDefaultAuthorizer $
--             mkDescribeDefaultAuthorizerResponse
--
--         , responseListAuditMitigationActionsTasks $
--             mkListAuditMitigationActionsTasksResponse
--
--         , responseListThingRegistrationTaskReports $
--             mkListThingRegistrationTaskReportsResponse
--
--         , responseListPrincipalThings $
--             mkListPrincipalThingsResponse
--
--         , responseRemoveThingFromThingGroup $
--             mkRemoveThingFromThingGroupResponse
--
--         , responseDescribeEventConfigurations $
--             mkDescribeEventConfigurationsResponse
--
--         , responseListTopicRuleDestinations $
--             mkListTopicRuleDestinationsResponse
--
--         , responseRegisterCertificateWithoutCA $
--             mkRegisterCertificateWithoutCAResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListThingGroups $
--             mkListThingGroupsResponse
--
--         , responseListScheduledAudits $
--             mkListScheduledAuditsResponse
--
--         , responseDescribeThingRegistrationTask $
--             mkDescribeThingRegistrationTaskResponse
--
--         , responseUpdateScheduledAudit $
--             mkUpdateScheduledAuditResponse
--
--         , responseDeleteScheduledAudit $
--             mkDeleteScheduledAuditResponse
--
--         , responseDescribeAuditFinding $
--             mkDescribeAuditFindingResponse
--
--         , responseDescribeDimension $
--             mkDescribeDimensionResponse
--
--         , responseGetLoggingOptions $
--             mkGetLoggingOptionsResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             mkDeleteAccountAuditConfigurationResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             mkUpdateAccountAuditConfigurationResponse
--
--         , responseGetOTAUpdate $
--             mkGetOTAUpdateResponse
--
--         , responseGetEffectivePolicies $
--             mkGetEffectivePoliciesResponse
--
--         , responseListThingTypes $
--             mkListThingTypesResponse
--
--         , responseSetV2LoggingOptions $
--             mkSetV2LoggingOptionsResponse
--
--         , responseCreateProvisioningTemplate $
--             mkCreateProvisioningTemplateResponse
--
--         , responseListThingGroupsForThing $
--             mkListThingGroupsForThingResponse
--
--         , responseCreateCertificateFromCsr $
--             mkCreateCertificateFromCsrResponse
--
--         , responseDeleteThing $
--             mkDeleteThingResponse
--
--         , responseUpdateThing $
--             mkUpdateThingResponse
--
--         , responseDeleteProvisioningTemplate $
--             mkDeleteProvisioningTemplateResponse
--
--         , responseUpdateProvisioningTemplate $
--             mkUpdateProvisioningTemplateResponse
--
--         , responseDescribeMitigationAction $
--             mkDescribeMitigationActionResponse
--
--         , responseStartThingRegistrationTask $
--             mkStartThingRegistrationTaskResponse
--
--         , responseCreateScheduledAudit $
--             mkCreateScheduledAuditResponse
--
--         , responseListAuthorizers $
--             mkListAuthorizersResponse
--
--         , responseListJobExecutionsForJob $
--             mkListJobExecutionsForJobResponse
--
--         , responseRemoveThingFromBillingGroup $
--             mkRemoveThingFromBillingGroupResponse
--
--         , responseSearchIndex $
--             mkSearchIndexResponse
--
--         , responseCreateThingType $
--             mkCreateThingTypeResponse
--
--         , responseDescribeSecurityProfile $
--             mkDescribeSecurityProfileResponse
--
--         , responseDeleteV2LoggingLevel $
--             mkDeleteV2LoggingLevelResponse
--
--         , responseSetDefaultAuthorizer $
--             mkSetDefaultAuthorizerResponse
--
--         , responseDescribeJobExecution $
--             mkDescribeJobExecutionResponse
--
--         , responseCancelCertificateTransfer $
--             mkCancelCertificateTransferResponse
--
--         , responseGetIndexingConfiguration $
--             mkGetIndexingConfigurationResponse
--
--         , responseListAuditMitigationActionsExecutions $
--             mkListAuditMitigationActionsExecutionsResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             mkDescribeAuditMitigationActionsTaskResponse
--
--         , responseGetStatistics $
--             mkGetStatisticsResponse
--
--         , responseDeleteRoleAlias $
--             mkDeleteRoleAliasResponse
--
--         , responseUpdateRoleAlias $
--             mkUpdateRoleAliasResponse
--
--         , responseDeletePolicyVersion $
--             mkDeletePolicyVersionResponse
--
--         , responseDisableTopicRule $
--             mkDisableTopicRuleResponse
--
--         , responseCreateTopicRule $
--             mkCreateTopicRuleResponse
--
--         , responseCreateJob $
--             mkCreateJobResponse
--
--         , responseDescribeIndex $
--             mkDescribeIndexResponse
--
--         , responseAssociateTargetsWithJob $
--             mkAssociateTargetsWithJobResponse
--
--         , responseAttachSecurityProfile $
--             mkAttachSecurityProfileResponse
--
--         , responseListAttachedPolicies $
--             mkListAttachedPoliciesResponse
--
--         , responseCreatePolicyVersion $
--             mkCreatePolicyVersionResponse
--
--         , responseListCACertificates $
--             mkListCACertificatesResponse
--
--         , responseDeleteTopicRule $
--             mkDeleteTopicRuleResponse
--
--         , responseGetJobDocument $
--             mkGetJobDocumentResponse
--
--         , responseDescribeProvisioningTemplateVersion $
--             mkDescribeProvisioningTemplateVersionResponse
--
--         , responseCancelAuditTask $
--             mkCancelAuditTaskResponse
--
--         , responseCreateRoleAlias $
--             mkCreateRoleAliasResponse
--
--         , responseDeleteCACertificate $
--             mkDeleteCACertificateResponse
--
--         , responseUpdateCACertificate $
--             mkUpdateCACertificateResponse
--
--         , responseListTopicRules $
--             mkListTopicRulesResponse
--
--         , responseTransferCertificate $
--             mkTransferCertificateResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseListRoleAliases $
--             mkListRoleAliasesResponse
--
--         , responseStartOnDemandAuditTask $
--             mkStartOnDemandAuditTaskResponse
--
--         , responseDescribeThingGroup $
--             mkDescribeThingGroupResponse
--
--         , responseDeleteJob $
--             mkDeleteJobResponse
--
--         , responseListTargetsForSecurityProfile $
--             mkListTargetsForSecurityProfileResponse
--
--         , responseUpdateJob $
--             mkUpdateJobResponse
--
--         , responseStartAuditMitigationActionsTask $
--             mkStartAuditMitigationActionsTaskResponse
--
--         , responseGetTopicRule $
--             mkGetTopicRuleResponse
--
--         , responseDescribeThing $
--             mkDescribeThingResponse
--
--         , responseListDomainConfigurations $
--             mkListDomainConfigurationsResponse
--
--         , responseListAuditTasks $
--             mkListAuditTasksResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             mkDescribeAccountAuditConfigurationResponse
--
--         , responseDeleteDimension $
--             mkDeleteDimensionResponse
--
--         , responseUpdateDimension $
--             mkUpdateDimensionResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseListThingsInThingGroup $
--             mkListThingsInThingGroupResponse
--
--         , responseListAuditFindings $
--             mkListAuditFindingsResponse
--
--         , responseDescribeScheduledAudit $
--             mkDescribeScheduledAuditResponse
--
--         , responseCreateMitigationAction $
--             mkCreateMitigationActionResponse
--
--         , responseConfirmTopicRuleDestination $
--             mkConfirmTopicRuleDestinationResponse
--
--         , responseListCertificates $
--             mkListCertificatesResponse
--
--         , responseListMitigationActions $
--             mkListMitigationActionsResponse
--
--         , responseDescribeAuthorizer $
--             mkDescribeAuthorizerResponse
--
--         , responseGetPolicyVersion $
--             mkGetPolicyVersionResponse
--
--         , responseListActiveViolations $
--             mkListActiveViolationsResponse
--
--         , responseValidateSecurityProfileBehaviors $
--             mkValidateSecurityProfileBehaviorsResponse
--
--         , responseListViolationEvents $
--             mkListViolationEventsResponse
--
--         , responseDeleteCertificate $
--             mkDeleteCertificateResponse
--
--         , responseUpdateCertificate $
--             mkUpdateCertificateResponse
--
--         , responseCreateDimension $
--             mkCreateDimensionResponse
--
--         , responseUpdateIndexingConfiguration $
--             mkUpdateIndexingConfigurationResponse
--
--         , responseCreateProvisioningClaim $
--             mkCreateProvisioningClaimResponse
--
--         , responseTestInvokeAuthorizer $
--             mkTestInvokeAuthorizerResponse
--
--         , responseCreateThingGroup $
--             mkCreateThingGroupResponse
--
--         , responseCreateTopicRuleDestination $
--             mkCreateTopicRuleDestinationResponse
--
--         , responseDetachPolicy $
--             mkDetachPolicyResponse
--
--         , responseDescribeJob $
--             mkDescribeJobResponse
--
--         , responseAddThingToBillingGroup $
--             mkAddThingToBillingGroupResponse
--
--         , responseUpdateTopicRuleDestination $
--             mkUpdateTopicRuleDestinationResponse
--
--         , responseDeleteTopicRuleDestination $
--             mkDeleteTopicRuleDestinationResponse
--
--         , responseDeleteThingGroup $
--             mkDeleteThingGroupResponse
--
--         , responseUpdateThingGroup $
--             mkUpdateThingGroupResponse
--
--         , responseListOTAUpdates $
--             mkListOTAUpdatesResponse
--
--         , responseDeleteOTAUpdate $
--             mkDeleteOTAUpdateResponse
--
--         , responseCreateDynamicThingGroup $
--             mkCreateDynamicThingGroupResponse
--
--         , responseDetachSecurityProfile $
--             mkDetachSecurityProfileResponse
--
--         , responseListOutgoingCertificates $
--             mkListOutgoingCertificatesResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             mkDeleteProvisioningTemplateVersionResponse
--
--         , responseDescribeCACertificate $
--             mkDescribeCACertificateResponse
--
--         , responseListProvisioningTemplateVersions $
--             mkListProvisioningTemplateVersionsResponse
--
--         , responseGetRegistrationCode $
--             mkGetRegistrationCodeResponse
--
--         , responseListBillingGroups $
--             mkListBillingGroupsResponse
--
--         , responseDeleteThingType $
--             mkDeleteThingTypeResponse
--
--         , responseDeleteBillingGroup $
--             mkDeleteBillingGroupResponse
--
--         , responseAddThingToThingGroup $
--             mkAddThingToThingGroupResponse
--
--         , responseUpdateBillingGroup $
--             mkUpdateBillingGroupResponse
--
--         , responseGetTopicRuleDestination $
--             mkGetTopicRuleDestinationResponse
--
--         , responseListCertificatesByCA $
--             mkListCertificatesByCAResponse
--
--         , responseUpdateAuditSuppression $
--             mkUpdateAuditSuppressionResponse
--
--         , responseAttachThingPrincipal $
--             mkAttachThingPrincipalResponse
--
--         , responseListThings $
--             mkListThingsResponse
--
--         , responseDeleteAuditSuppression $
--             mkDeleteAuditSuppressionResponse
--
--         , responseRegisterThing $
--             mkRegisterThingResponse
--
--         , responseListAuditSuppressions $
--             mkListAuditSuppressionsResponse
--
--         , responseDescribeDomainConfiguration $
--             mkDescribeDomainConfigurationResponse
--
--         , responseDescribeAuditTask $
--             mkDescribeAuditTaskResponse
--
--         , responseDeleteRegistrationCode $
--             mkDeleteRegistrationCodeResponse
--
--         , responseUpdateStream $
--             mkUpdateStreamResponse
--
--         , responseDeleteStream $
--             mkDeleteStreamResponse
--
--         , responseListStreams $
--             mkListStreamsResponse
--
--         , responseCreateAuthorizer $
--             mkCreateAuthorizerResponse
--
--         , responseTestAuthorization $
--             mkTestAuthorizationResponse
--
--         , responseListIndices $
--             mkListIndicesResponse
--
--         , responseUpdateAuthorizer $
--             mkUpdateAuthorizerResponse
--
--         , responseDeleteAuthorizer $
--             mkDeleteAuthorizerResponse
--
--         , responseCreateThing $
--             mkCreateThingResponse
--
--         , responseCreateStream $
--             mkCreateStreamResponse
--
--         , responseCancelAuditMitigationActionsTask $
--             mkCancelAuditMitigationActionsTaskResponse
--
--         , responseCreateAuditSuppression $
--             mkCreateAuditSuppressionResponse
--
--         , responseCreateBillingGroup $
--             mkCreateBillingGroupResponse
--
--         , responseListProvisioningTemplates $
--             mkListProvisioningTemplatesResponse
--
--         , responseListV2LoggingLevels $
--             mkListV2LoggingLevelsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseStopThingRegistrationTask $
--             mkStopThingRegistrationTaskResponse
--
--         , responseDescribeCertificate $
--             mkDescribeCertificateResponse
--
--         , responseListTargetsForPolicy $
--             mkListTargetsForPolicyResponse
--
--         , responseClearDefaultAuthorizer $
--             mkClearDefaultAuthorizerResponse
--
--         , responseReplaceTopicRule $
--             mkReplaceTopicRuleResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseSetDefaultPolicyVersion $
--             mkSetDefaultPolicyVersionResponse
--
--         , responseCancelJobExecution $
--             mkCancelJobExecutionResponse
--
--         , responseListPolicyVersions $
--             mkListPolicyVersionsResponse
--
--         , responseSetV2LoggingLevel $
--             mkSetV2LoggingLevelResponse
--
--         , responseListJobExecutionsForThing $
--             mkListJobExecutionsForThingResponse
--
--         , responseAttachPolicy $
--             mkAttachPolicyResponse
--
--         , responseCreateKeysAndCertificate $
--             mkCreateKeysAndCertificateResponse
--
--         , responseListThingsInBillingGroup $
--             mkListThingsInBillingGroupResponse
--
--         , responseUpdateThingGroupsForThing $
--             mkUpdateThingGroupsForThingResponse
--
--         , responseEnableTopicRule $
--             mkEnableTopicRuleResponse
--
--         , responseAcceptCertificateTransfer $
--             mkAcceptCertificateTransferResponse
--
--         , responseGetPercentiles $
--             mkGetPercentilesResponse
--
--         , responseGetPolicy $
--             mkGetPolicyResponse
--
--         , responseDescribeEndpoint $
--             mkDescribeEndpointResponse
--
--         , responseListSecurityProfilesForTarget $
--             mkListSecurityProfilesForTargetResponse
--
--         , responseUpdateEventConfigurations $
--             mkUpdateEventConfigurationsResponse
--
--         , responseRegisterCACertificate $
--             mkRegisterCACertificateResponse
--
--         , responseDeleteDomainConfiguration $
--             mkDeleteDomainConfigurationResponse
--
--         , responseUpdateDomainConfiguration $
--             mkUpdateDomainConfigurationResponse
--
--         , responseSetLoggingOptions $
--             mkSetLoggingOptionsResponse
--
--         , responseDescribeThingType $
--             mkDescribeThingTypeResponse
--
--         , responseListDimensions $
--             mkListDimensionsResponse
--
--         , responseGetV2LoggingOptions $
--             mkGetV2LoggingOptionsResponse
--
--         , responseListThingRegistrationTasks $
--             mkListThingRegistrationTasksResponse
--
--         , responseRejectCertificateTransfer $
--             mkRejectCertificateTransferResponse
--
--         , responseDescribeAuditSuppression $
--             mkDescribeAuditSuppressionResponse
--
--         , responseDescribeStream $
--             mkDescribeStreamResponse
--
--         , responseCreateSecurityProfile $
--             mkCreateSecurityProfileResponse
--
--         , responseDescribeBillingGroup $
--             mkDescribeBillingGroupResponse
--
--         , responseDetachThingPrincipal $
--             mkDetachThingPrincipalResponse
--
--         , responseCancelJob $
--             mkCancelJobResponse
--
--         , responseDeprecateThingType $
--             mkDeprecateThingTypeResponse
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

requestCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
requestCreateCertificateFromCsr =
  req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy GetCardinality)

responseCreateDomainConfiguration :: CreateDomainConfigurationResponse -> TestTree
responseCreateDomainConfiguration =
  res
    "CreateDomainConfigurationResponse"
    "fixture/CreateDomainConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDomainConfiguration)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSecurityProfile)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSecurityProfile)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecurityProfiles)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicies)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeProvisioningTemplate)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMitigationAction)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMitigationAction)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteJobExecution)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePolicy)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterCertificate)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDynamicThingGroup)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingPrincipals)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDynamicThingGroup)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRoleAlias)

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProvisioningTemplateVersion)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateOTAUpdate)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDefaultAuthorizer)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuditMitigationActionsTasks)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingRegistrationTaskReports)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPrincipalThings)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup =
  res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveThingFromThingGroup)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEventConfigurations)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTopicRuleDestinations)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterCertificateWithoutCA)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingGroups)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListScheduledAudits)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeThingRegistrationTask)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateScheduledAudit)

responseDeleteScheduledAudit :: DeleteScheduledAuditResponse -> TestTree
responseDeleteScheduledAudit =
  res
    "DeleteScheduledAuditResponse"
    "fixture/DeleteScheduledAuditResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScheduledAudit)

responseDescribeAuditFinding :: DescribeAuditFindingResponse -> TestTree
responseDescribeAuditFinding =
  res
    "DescribeAuditFindingResponse"
    "fixture/DescribeAuditFindingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAuditFinding)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDimension)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoggingOptions)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAccountAuditConfiguration)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAccountAuditConfiguration)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOTAUpdate)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEffectivePolicies)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingTypes)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetV2LoggingOptions)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProvisioningTemplate)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingGroupsForThing)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr =
  res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCertificateFromCsr)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteThing)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateThing)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProvisioningTemplate)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateProvisioningTemplate)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMitigationAction)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartThingRegistrationTask)

responseCreateScheduledAudit :: CreateScheduledAuditResponse -> TestTree
responseCreateScheduledAudit =
  res
    "CreateScheduledAuditResponse"
    "fixture/CreateScheduledAuditResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateScheduledAudit)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuthorizers)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListJobExecutionsForJob)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveThingFromBillingGroup)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchIndex)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType =
  res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateThingType)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSecurityProfile)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteV2LoggingLevel)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetDefaultAuthorizer)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeJobExecution)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelCertificateTransfer)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIndexingConfiguration)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuditMitigationActionsExecutions)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAuditMitigationActionsTask)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStatistics)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRoleAlias)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoleAlias)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicyVersion)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableTopicRule)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule =
  res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTopicRule)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateJob)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIndex)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateTargetsWithJob)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachSecurityProfile)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAttachedPolicies)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePolicyVersion)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCACertificates)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTopicRule)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJobDocument)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeProvisioningTemplateVersion)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelAuditTask)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRoleAlias)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate =
  res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCACertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate =
  res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCACertificate)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTopicRules)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TransferCertificate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListJobs)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoleAliases)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartOnDemandAuditTask)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeThingGroup)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteJob)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTargetsForSecurityProfile)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateJob)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartAuditMitigationActionsTask)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTopicRule)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing =
  res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeThing)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDomainConfigurations)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuditTasks)

responseDescribeAccountAuditConfiguration :: DescribeAccountAuditConfigurationResponse -> TestTree
responseDescribeAccountAuditConfiguration =
  res
    "DescribeAccountAuditConfigurationResponse"
    "fixture/DescribeAccountAuditConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountAuditConfiguration)

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDimension)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDimension)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicy)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingsInThingGroup)

responseListAuditFindings :: ListAuditFindingsResponse -> TestTree
responseListAuditFindings =
  res
    "ListAuditFindingsResponse"
    "fixture/ListAuditFindingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuditFindings)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScheduledAudit)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMitigationAction)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConfirmTopicRuleDestination)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCertificates)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListMitigationActions)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAuthorizer)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicyVersion)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListActiveViolations)

responseValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviorsResponse -> TestTree
responseValidateSecurityProfileBehaviors =
  res
    "ValidateSecurityProfileBehaviorsResponse"
    "fixture/ValidateSecurityProfileBehaviorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ValidateSecurityProfileBehaviors)

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListViolationEvents)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCertificate)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDimension)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateIndexingConfiguration)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProvisioningClaim)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestInvokeAuthorizer)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateThingGroup)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTopicRuleDestination)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachPolicy)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeJob)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddThingToBillingGroup)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTopicRuleDestination)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTopicRuleDestination)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteThingGroup)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateThingGroup)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOTAUpdates)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOTAUpdate)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDynamicThingGroup)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachSecurityProfile)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOutgoingCertificates)

responseDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersionResponse -> TestTree
responseDeleteProvisioningTemplateVersion =
  res
    "DeleteProvisioningTemplateVersionResponse"
    "fixture/DeleteProvisioningTemplateVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProvisioningTemplateVersion)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate =
  res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCACertificate)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProvisioningTemplateVersions)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRegistrationCode)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBillingGroups)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType =
  res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteThingType)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBillingGroup)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddThingToThingGroup)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBillingGroup)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTopicRuleDestination)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCertificatesByCA)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAuditSuppression)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachThingPrincipal)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThings)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAuditSuppression)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing =
  res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterThing)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAuditSuppressions)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDomainConfiguration)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAuditTask)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRegistrationCode)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStreams)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAuthorizer)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestAuthorization)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIndices)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateThing)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStream)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelAuditMitigationActionsTask)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAuditSuppression)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBillingGroup)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProvisioningTemplates)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListV2LoggingLevels)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask =
  res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopThingRegistrationTask)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCertificate)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTargetsForPolicy)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer =
  res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ClearDefaultAuthorizer)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceTopicRule)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelJobExecution)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicyVersions)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetV2LoggingLevel)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListJobExecutionsForThing)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachPolicy)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate =
  res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateKeysAndCertificate)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingsInBillingGroup)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateThingGroupsForThing)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableTopicRule)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptCertificateTransfer)

responseGetPercentiles :: GetPercentilesResponse -> TestTree
responseGetPercentiles =
  res
    "GetPercentilesResponse"
    "fixture/GetPercentilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPercentiles)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicy)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEndpoint)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecurityProfilesForTarget)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEventConfigurations)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterCACertificate)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDomainConfiguration)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainConfiguration)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetLoggingOptions)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeThingType)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDimensions)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetV2LoggingOptions)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListThingRegistrationTasks)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectCertificateTransfer)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAuditSuppression)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStream)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSecurityProfile)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBillingGroup)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachThingPrincipal)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelJob)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType =
  res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeprecateThingType)
