{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoT where

import qualified Data.Proxy as Proxy
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
--             newGetCardinality
--
--         , requestCreateDomainConfiguration $
--             newCreateDomainConfiguration
--
--         , requestStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTask
--
--         , requestDeleteSecurityProfile $
--             newDeleteSecurityProfile
--
--         , requestUpdateSecurityProfile $
--             newUpdateSecurityProfile
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplate
--
--         , requestUpdateMitigationAction $
--             newUpdateMitigationAction
--
--         , requestDeleteMitigationAction $
--             newDeleteMitigationAction
--
--         , requestDeleteJobExecution $
--             newDeleteJobExecution
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroup
--
--         , requestListThingPrincipals $
--             newListThingPrincipals
--
--         , requestUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroup
--
--         , requestDescribeRoleAlias $
--             newDescribeRoleAlias
--
--         , requestCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersion
--
--         , requestCreateOTAUpdate $
--             newCreateOTAUpdate
--
--         , requestDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizer
--
--         , requestListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasks
--
--         , requestListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReports
--
--         , requestGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummaries
--
--         , requestListPrincipalThings $
--             newListPrincipalThings
--
--         , requestRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroup
--
--         , requestDescribeEventConfigurations $
--             newDescribeEventConfigurations
--
--         , requestCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTask
--
--         , requestListTopicRuleDestinations $
--             newListTopicRuleDestinations
--
--         , requestRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCA
--
--         , requestDescribeCustomMetric $
--             newDescribeCustomMetric
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListThingGroups $
--             newListThingGroups
--
--         , requestDescribeJobTemplate $
--             newDescribeJobTemplate
--
--         , requestListScheduledAudits $
--             newListScheduledAudits
--
--         , requestDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTask
--
--         , requestUpdateScheduledAudit $
--             newUpdateScheduledAudit
--
--         , requestDeleteScheduledAudit $
--             newDeleteScheduledAudit
--
--         , requestDescribeAuditFinding $
--             newDescribeAuditFinding
--
--         , requestDescribeDimension $
--             newDescribeDimension
--
--         , requestGetLoggingOptions $
--             newGetLoggingOptions
--
--         , requestDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfiguration
--
--         , requestUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfiguration
--
--         , requestGetOTAUpdate $
--             newGetOTAUpdate
--
--         , requestGetEffectivePolicies $
--             newGetEffectivePolicies
--
--         , requestListThingTypes $
--             newListThingTypes
--
--         , requestSetV2LoggingOptions $
--             newSetV2LoggingOptions
--
--         , requestCreateProvisioningTemplate $
--             newCreateProvisioningTemplate
--
--         , requestListThingGroupsForThing $
--             newListThingGroupsForThing
--
--         , requestCreateCertificateFromCsr $
--             newCreateCertificateFromCsr
--
--         , requestDeleteThing $
--             newDeleteThing
--
--         , requestUpdateThing $
--             newUpdateThing
--
--         , requestDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplate
--
--         , requestUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplate
--
--         , requestDescribeMitigationAction $
--             newDescribeMitigationAction
--
--         , requestStartThingRegistrationTask $
--             newStartThingRegistrationTask
--
--         , requestCreateScheduledAudit $
--             newCreateScheduledAudit
--
--         , requestListAuthorizers $
--             newListAuthorizers
--
--         , requestListJobExecutionsForJob $
--             newListJobExecutionsForJob
--
--         , requestRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroup
--
--         , requestSearchIndex $
--             newSearchIndex
--
--         , requestCreateThingType $
--             newCreateThingType
--
--         , requestDescribeSecurityProfile $
--             newDescribeSecurityProfile
--
--         , requestDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevel
--
--         , requestSetDefaultAuthorizer $
--             newSetDefaultAuthorizer
--
--         , requestDescribeJobExecution $
--             newDescribeJobExecution
--
--         , requestCancelCertificateTransfer $
--             newCancelCertificateTransfer
--
--         , requestGetIndexingConfiguration $
--             newGetIndexingConfiguration
--
--         , requestListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutions
--
--         , requestCreateCustomMetric $
--             newCreateCustomMetric
--
--         , requestDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTask
--
--         , requestGetStatistics $
--             newGetStatistics
--
--         , requestDeleteRoleAlias $
--             newDeleteRoleAlias
--
--         , requestUpdateRoleAlias $
--             newUpdateRoleAlias
--
--         , requestListFleetMetrics $
--             newListFleetMetrics
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestDisableTopicRule $
--             newDisableTopicRule
--
--         , requestCreateTopicRule $
--             newCreateTopicRule
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestAssociateTargetsWithJob $
--             newAssociateTargetsWithJob
--
--         , requestAttachSecurityProfile $
--             newAttachSecurityProfile
--
--         , requestListAttachedPolicies $
--             newListAttachedPolicies
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestListCACertificates $
--             newListCACertificates
--
--         , requestDeleteTopicRule $
--             newDeleteTopicRule
--
--         , requestGetJobDocument $
--             newGetJobDocument
--
--         , requestDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersion
--
--         , requestListCustomMetrics $
--             newListCustomMetrics
--
--         , requestCancelAuditTask $
--             newCancelAuditTask
--
--         , requestCreateRoleAlias $
--             newCreateRoleAlias
--
--         , requestDeleteCACertificate $
--             newDeleteCACertificate
--
--         , requestUpdateCACertificate $
--             newUpdateCACertificate
--
--         , requestListTopicRules $
--             newListTopicRules
--
--         , requestTransferCertificate $
--             newTransferCertificate
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListRoleAliases $
--             newListRoleAliases
--
--         , requestStartOnDemandAuditTask $
--             newStartOnDemandAuditTask
--
--         , requestDescribeThingGroup $
--             newDescribeThingGroup
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfile
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTask
--
--         , requestDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTask
--
--         , requestGetTopicRule $
--             newGetTopicRule
--
--         , requestDescribeThing $
--             newDescribeThing
--
--         , requestListDomainConfigurations $
--             newListDomainConfigurations
--
--         , requestListAuditTasks $
--             newListAuditTasks
--
--         , requestDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfiguration
--
--         , requestDeleteDimension $
--             newDeleteDimension
--
--         , requestUpdateDimension $
--             newUpdateDimension
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestListThingsInThingGroup $
--             newListThingsInThingGroup
--
--         , requestListAuditFindings $
--             newListAuditFindings
--
--         , requestDescribeScheduledAudit $
--             newDescribeScheduledAudit
--
--         , requestCreateMitigationAction $
--             newCreateMitigationAction
--
--         , requestConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestination
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestListMitigationActions $
--             newListMitigationActions
--
--         , requestDescribeAuthorizer $
--             newDescribeAuthorizer
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestListActiveViolations $
--             newListActiveViolations
--
--         , requestValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviors
--
--         , requestListViolationEvents $
--             newListViolationEvents
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestUpdateCertificate $
--             newUpdateCertificate
--
--         , requestCreateDimension $
--             newCreateDimension
--
--         , requestUpdateIndexingConfiguration $
--             newUpdateIndexingConfiguration
--
--         , requestGetBucketsAggregation $
--             newGetBucketsAggregation
--
--         , requestCreateProvisioningClaim $
--             newCreateProvisioningClaim
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestPutVerificationStateOnViolation $
--             newPutVerificationStateOnViolation
--
--         , requestCreateThingGroup $
--             newCreateThingGroup
--
--         , requestDescribeFleetMetric $
--             newDescribeFleetMetric
--
--         , requestCreateTopicRuleDestination $
--             newCreateTopicRuleDestination
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestAddThingToBillingGroup $
--             newAddThingToBillingGroup
--
--         , requestUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestination
--
--         , requestDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestination
--
--         , requestDeleteThingGroup $
--             newDeleteThingGroup
--
--         , requestUpdateThingGroup $
--             newUpdateThingGroup
--
--         , requestListOTAUpdates $
--             newListOTAUpdates
--
--         , requestDeleteOTAUpdate $
--             newDeleteOTAUpdate
--
--         , requestCreateDynamicThingGroup $
--             newCreateDynamicThingGroup
--
--         , requestDetachSecurityProfile $
--             newDetachSecurityProfile
--
--         , requestListOutgoingCertificates $
--             newListOutgoingCertificates
--
--         , requestDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersion
--
--         , requestDescribeCACertificate $
--             newDescribeCACertificate
--
--         , requestListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersions
--
--         , requestGetRegistrationCode $
--             newGetRegistrationCode
--
--         , requestListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutions
--
--         , requestListBillingGroups $
--             newListBillingGroups
--
--         , requestDeleteThingType $
--             newDeleteThingType
--
--         , requestDeleteBillingGroup $
--             newDeleteBillingGroup
--
--         , requestAddThingToThingGroup $
--             newAddThingToThingGroup
--
--         , requestUpdateBillingGroup $
--             newUpdateBillingGroup
--
--         , requestGetTopicRuleDestination $
--             newGetTopicRuleDestination
--
--         , requestListCertificatesByCA $
--             newListCertificatesByCA
--
--         , requestUpdateAuditSuppression $
--             newUpdateAuditSuppression
--
--         , requestAttachThingPrincipal $
--             newAttachThingPrincipal
--
--         , requestListThings $
--             newListThings
--
--         , requestDeleteAuditSuppression $
--             newDeleteAuditSuppression
--
--         , requestListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasks
--
--         , requestRegisterThing $
--             newRegisterThing
--
--         , requestListAuditSuppressions $
--             newListAuditSuppressions
--
--         , requestDescribeDomainConfiguration $
--             newDescribeDomainConfiguration
--
--         , requestDescribeAuditTask $
--             newDescribeAuditTask
--
--         , requestDeleteRegistrationCode $
--             newDeleteRegistrationCode
--
--         , requestUpdateStream $
--             newUpdateStream
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestListStreams $
--             newListStreams
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestTestAuthorization $
--             newTestAuthorization
--
--         , requestListIndices $
--             newListIndices
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestCreateThing $
--             newCreateThing
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTask
--
--         , requestCreateAuditSuppression $
--             newCreateAuditSuppression
--
--         , requestCreateBillingGroup $
--             newCreateBillingGroup
--
--         , requestListProvisioningTemplates $
--             newListProvisioningTemplates
--
--         , requestListV2LoggingLevels $
--             newListV2LoggingLevels
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopThingRegistrationTask $
--             newStopThingRegistrationTask
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestClearDefaultAuthorizer $
--             newClearDefaultAuthorizer
--
--         , requestReplaceTopicRule $
--             newReplaceTopicRule
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteFleetMetric $
--             newDeleteFleetMetric
--
--         , requestUpdateFleetMetric $
--             newUpdateFleetMetric
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestCancelJobExecution $
--             newCancelJobExecution
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestSetV2LoggingLevel $
--             newSetV2LoggingLevel
--
--         , requestListJobExecutionsForThing $
--             newListJobExecutionsForThing
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestCreateKeysAndCertificate $
--             newCreateKeysAndCertificate
--
--         , requestListThingsInBillingGroup $
--             newListThingsInBillingGroup
--
--         , requestUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThing
--
--         , requestCreateFleetMetric $
--             newCreateFleetMetric
--
--         , requestEnableTopicRule $
--             newEnableTopicRule
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestAcceptCertificateTransfer $
--             newAcceptCertificateTransfer
--
--         , requestGetPercentiles $
--             newGetPercentiles
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestListSecurityProfilesForTarget $
--             newListSecurityProfilesForTarget
--
--         , requestUpdateEventConfigurations $
--             newUpdateEventConfigurations
--
--         , requestUpdateCustomMetric $
--             newUpdateCustomMetric
--
--         , requestDeleteCustomMetric $
--             newDeleteCustomMetric
--
--         , requestRegisterCACertificate $
--             newRegisterCACertificate
--
--         , requestDeleteDomainConfiguration $
--             newDeleteDomainConfiguration
--
--         , requestUpdateDomainConfiguration $
--             newUpdateDomainConfiguration
--
--         , requestSetLoggingOptions $
--             newSetLoggingOptions
--
--         , requestDescribeThingType $
--             newDescribeThingType
--
--         , requestListDimensions $
--             newListDimensions
--
--         , requestGetV2LoggingOptions $
--             newGetV2LoggingOptions
--
--         , requestListThingRegistrationTasks $
--             newListThingRegistrationTasks
--
--         , requestRejectCertificateTransfer $
--             newRejectCertificateTransfer
--
--         , requestDescribeAuditSuppression $
--             newDescribeAuditSuppression
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestCreateSecurityProfile $
--             newCreateSecurityProfile
--
--         , requestDescribeBillingGroup $
--             newDescribeBillingGroup
--
--         , requestDetachThingPrincipal $
--             newDetachThingPrincipal
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestDeprecateThingType $
--             newDeprecateThingType
--
--           ]

--     , testGroup "response"
--         [ responseGetCardinality $
--             newGetCardinalityResponse
--
--         , responseCreateDomainConfiguration $
--             newCreateDomainConfigurationResponse
--
--         , responseStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTaskResponse
--
--         , responseDeleteSecurityProfile $
--             newDeleteSecurityProfileResponse
--
--         , responseUpdateSecurityProfile $
--             newUpdateSecurityProfileResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplateResponse
--
--         , responseUpdateMitigationAction $
--             newUpdateMitigationActionResponse
--
--         , responseDeleteMitigationAction $
--             newDeleteMitigationActionResponse
--
--         , responseDeleteJobExecution $
--             newDeleteJobExecutionResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroupResponse
--
--         , responseListThingPrincipals $
--             newListThingPrincipalsResponse
--
--         , responseUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroupResponse
--
--         , responseDescribeRoleAlias $
--             newDescribeRoleAliasResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersionResponse
--
--         , responseCreateOTAUpdate $
--             newCreateOTAUpdateResponse
--
--         , responseDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizerResponse
--
--         , responseListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasksResponse
--
--         , responseListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReportsResponse
--
--         , responseGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummariesResponse
--
--         , responseListPrincipalThings $
--             newListPrincipalThingsResponse
--
--         , responseRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroupResponse
--
--         , responseDescribeEventConfigurations $
--             newDescribeEventConfigurationsResponse
--
--         , responseCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTaskResponse
--
--         , responseListTopicRuleDestinations $
--             newListTopicRuleDestinationsResponse
--
--         , responseRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCAResponse
--
--         , responseDescribeCustomMetric $
--             newDescribeCustomMetricResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListThingGroups $
--             newListThingGroupsResponse
--
--         , responseDescribeJobTemplate $
--             newDescribeJobTemplateResponse
--
--         , responseListScheduledAudits $
--             newListScheduledAuditsResponse
--
--         , responseDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTaskResponse
--
--         , responseUpdateScheduledAudit $
--             newUpdateScheduledAuditResponse
--
--         , responseDeleteScheduledAudit $
--             newDeleteScheduledAuditResponse
--
--         , responseDescribeAuditFinding $
--             newDescribeAuditFindingResponse
--
--         , responseDescribeDimension $
--             newDescribeDimensionResponse
--
--         , responseGetLoggingOptions $
--             newGetLoggingOptionsResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfigurationResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfigurationResponse
--
--         , responseGetOTAUpdate $
--             newGetOTAUpdateResponse
--
--         , responseGetEffectivePolicies $
--             newGetEffectivePoliciesResponse
--
--         , responseListThingTypes $
--             newListThingTypesResponse
--
--         , responseSetV2LoggingOptions $
--             newSetV2LoggingOptionsResponse
--
--         , responseCreateProvisioningTemplate $
--             newCreateProvisioningTemplateResponse
--
--         , responseListThingGroupsForThing $
--             newListThingGroupsForThingResponse
--
--         , responseCreateCertificateFromCsr $
--             newCreateCertificateFromCsrResponse
--
--         , responseDeleteThing $
--             newDeleteThingResponse
--
--         , responseUpdateThing $
--             newUpdateThingResponse
--
--         , responseDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplateResponse
--
--         , responseUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplateResponse
--
--         , responseDescribeMitigationAction $
--             newDescribeMitigationActionResponse
--
--         , responseStartThingRegistrationTask $
--             newStartThingRegistrationTaskResponse
--
--         , responseCreateScheduledAudit $
--             newCreateScheduledAuditResponse
--
--         , responseListAuthorizers $
--             newListAuthorizersResponse
--
--         , responseListJobExecutionsForJob $
--             newListJobExecutionsForJobResponse
--
--         , responseRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroupResponse
--
--         , responseSearchIndex $
--             newSearchIndexResponse
--
--         , responseCreateThingType $
--             newCreateThingTypeResponse
--
--         , responseDescribeSecurityProfile $
--             newDescribeSecurityProfileResponse
--
--         , responseDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevelResponse
--
--         , responseSetDefaultAuthorizer $
--             newSetDefaultAuthorizerResponse
--
--         , responseDescribeJobExecution $
--             newDescribeJobExecutionResponse
--
--         , responseCancelCertificateTransfer $
--             newCancelCertificateTransferResponse
--
--         , responseGetIndexingConfiguration $
--             newGetIndexingConfigurationResponse
--
--         , responseListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutionsResponse
--
--         , responseCreateCustomMetric $
--             newCreateCustomMetricResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTaskResponse
--
--         , responseGetStatistics $
--             newGetStatisticsResponse
--
--         , responseDeleteRoleAlias $
--             newDeleteRoleAliasResponse
--
--         , responseUpdateRoleAlias $
--             newUpdateRoleAliasResponse
--
--         , responseListFleetMetrics $
--             newListFleetMetricsResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseDisableTopicRule $
--             newDisableTopicRuleResponse
--
--         , responseCreateTopicRule $
--             newCreateTopicRuleResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseAssociateTargetsWithJob $
--             newAssociateTargetsWithJobResponse
--
--         , responseAttachSecurityProfile $
--             newAttachSecurityProfileResponse
--
--         , responseListAttachedPolicies $
--             newListAttachedPoliciesResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseListCACertificates $
--             newListCACertificatesResponse
--
--         , responseDeleteTopicRule $
--             newDeleteTopicRuleResponse
--
--         , responseGetJobDocument $
--             newGetJobDocumentResponse
--
--         , responseDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersionResponse
--
--         , responseListCustomMetrics $
--             newListCustomMetricsResponse
--
--         , responseCancelAuditTask $
--             newCancelAuditTaskResponse
--
--         , responseCreateRoleAlias $
--             newCreateRoleAliasResponse
--
--         , responseDeleteCACertificate $
--             newDeleteCACertificateResponse
--
--         , responseUpdateCACertificate $
--             newUpdateCACertificateResponse
--
--         , responseListTopicRules $
--             newListTopicRulesResponse
--
--         , responseTransferCertificate $
--             newTransferCertificateResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListRoleAliases $
--             newListRoleAliasesResponse
--
--         , responseStartOnDemandAuditTask $
--             newStartOnDemandAuditTaskResponse
--
--         , responseDescribeThingGroup $
--             newDescribeThingGroupResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfileResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTaskResponse
--
--         , responseDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTaskResponse
--
--         , responseGetTopicRule $
--             newGetTopicRuleResponse
--
--         , responseDescribeThing $
--             newDescribeThingResponse
--
--         , responseListDomainConfigurations $
--             newListDomainConfigurationsResponse
--
--         , responseListAuditTasks $
--             newListAuditTasksResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfigurationResponse
--
--         , responseDeleteDimension $
--             newDeleteDimensionResponse
--
--         , responseUpdateDimension $
--             newUpdateDimensionResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseListThingsInThingGroup $
--             newListThingsInThingGroupResponse
--
--         , responseListAuditFindings $
--             newListAuditFindingsResponse
--
--         , responseDescribeScheduledAudit $
--             newDescribeScheduledAuditResponse
--
--         , responseCreateMitigationAction $
--             newCreateMitigationActionResponse
--
--         , responseConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestinationResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseListMitigationActions $
--             newListMitigationActionsResponse
--
--         , responseDescribeAuthorizer $
--             newDescribeAuthorizerResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseListActiveViolations $
--             newListActiveViolationsResponse
--
--         , responseValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviorsResponse
--
--         , responseListViolationEvents $
--             newListViolationEventsResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseUpdateCertificate $
--             newUpdateCertificateResponse
--
--         , responseCreateDimension $
--             newCreateDimensionResponse
--
--         , responseUpdateIndexingConfiguration $
--             newUpdateIndexingConfigurationResponse
--
--         , responseGetBucketsAggregation $
--             newGetBucketsAggregationResponse
--
--         , responseCreateProvisioningClaim $
--             newCreateProvisioningClaimResponse
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responsePutVerificationStateOnViolation $
--             newPutVerificationStateOnViolationResponse
--
--         , responseCreateThingGroup $
--             newCreateThingGroupResponse
--
--         , responseDescribeFleetMetric $
--             newDescribeFleetMetricResponse
--
--         , responseCreateTopicRuleDestination $
--             newCreateTopicRuleDestinationResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseAddThingToBillingGroup $
--             newAddThingToBillingGroupResponse
--
--         , responseUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestinationResponse
--
--         , responseDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestinationResponse
--
--         , responseDeleteThingGroup $
--             newDeleteThingGroupResponse
--
--         , responseUpdateThingGroup $
--             newUpdateThingGroupResponse
--
--         , responseListOTAUpdates $
--             newListOTAUpdatesResponse
--
--         , responseDeleteOTAUpdate $
--             newDeleteOTAUpdateResponse
--
--         , responseCreateDynamicThingGroup $
--             newCreateDynamicThingGroupResponse
--
--         , responseDetachSecurityProfile $
--             newDetachSecurityProfileResponse
--
--         , responseListOutgoingCertificates $
--             newListOutgoingCertificatesResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersionResponse
--
--         , responseDescribeCACertificate $
--             newDescribeCACertificateResponse
--
--         , responseListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersionsResponse
--
--         , responseGetRegistrationCode $
--             newGetRegistrationCodeResponse
--
--         , responseListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutionsResponse
--
--         , responseListBillingGroups $
--             newListBillingGroupsResponse
--
--         , responseDeleteThingType $
--             newDeleteThingTypeResponse
--
--         , responseDeleteBillingGroup $
--             newDeleteBillingGroupResponse
--
--         , responseAddThingToThingGroup $
--             newAddThingToThingGroupResponse
--
--         , responseUpdateBillingGroup $
--             newUpdateBillingGroupResponse
--
--         , responseGetTopicRuleDestination $
--             newGetTopicRuleDestinationResponse
--
--         , responseListCertificatesByCA $
--             newListCertificatesByCAResponse
--
--         , responseUpdateAuditSuppression $
--             newUpdateAuditSuppressionResponse
--
--         , responseAttachThingPrincipal $
--             newAttachThingPrincipalResponse
--
--         , responseListThings $
--             newListThingsResponse
--
--         , responseDeleteAuditSuppression $
--             newDeleteAuditSuppressionResponse
--
--         , responseListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasksResponse
--
--         , responseRegisterThing $
--             newRegisterThingResponse
--
--         , responseListAuditSuppressions $
--             newListAuditSuppressionsResponse
--
--         , responseDescribeDomainConfiguration $
--             newDescribeDomainConfigurationResponse
--
--         , responseDescribeAuditTask $
--             newDescribeAuditTaskResponse
--
--         , responseDeleteRegistrationCode $
--             newDeleteRegistrationCodeResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseTestAuthorization $
--             newTestAuthorizationResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseCreateThing $
--             newCreateThingResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTaskResponse
--
--         , responseCreateAuditSuppression $
--             newCreateAuditSuppressionResponse
--
--         , responseCreateBillingGroup $
--             newCreateBillingGroupResponse
--
--         , responseListProvisioningTemplates $
--             newListProvisioningTemplatesResponse
--
--         , responseListV2LoggingLevels $
--             newListV2LoggingLevelsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopThingRegistrationTask $
--             newStopThingRegistrationTaskResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseClearDefaultAuthorizer $
--             newClearDefaultAuthorizerResponse
--
--         , responseReplaceTopicRule $
--             newReplaceTopicRuleResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteFleetMetric $
--             newDeleteFleetMetricResponse
--
--         , responseUpdateFleetMetric $
--             newUpdateFleetMetricResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseCancelJobExecution $
--             newCancelJobExecutionResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseSetV2LoggingLevel $
--             newSetV2LoggingLevelResponse
--
--         , responseListJobExecutionsForThing $
--             newListJobExecutionsForThingResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseCreateKeysAndCertificate $
--             newCreateKeysAndCertificateResponse
--
--         , responseListThingsInBillingGroup $
--             newListThingsInBillingGroupResponse
--
--         , responseUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThingResponse
--
--         , responseCreateFleetMetric $
--             newCreateFleetMetricResponse
--
--         , responseEnableTopicRule $
--             newEnableTopicRuleResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseAcceptCertificateTransfer $
--             newAcceptCertificateTransferResponse
--
--         , responseGetPercentiles $
--             newGetPercentilesResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseListSecurityProfilesForTarget $
--             newListSecurityProfilesForTargetResponse
--
--         , responseUpdateEventConfigurations $
--             newUpdateEventConfigurationsResponse
--
--         , responseUpdateCustomMetric $
--             newUpdateCustomMetricResponse
--
--         , responseDeleteCustomMetric $
--             newDeleteCustomMetricResponse
--
--         , responseRegisterCACertificate $
--             newRegisterCACertificateResponse
--
--         , responseDeleteDomainConfiguration $
--             newDeleteDomainConfigurationResponse
--
--         , responseUpdateDomainConfiguration $
--             newUpdateDomainConfigurationResponse
--
--         , responseSetLoggingOptions $
--             newSetLoggingOptionsResponse
--
--         , responseDescribeThingType $
--             newDescribeThingTypeResponse
--
--         , responseListDimensions $
--             newListDimensionsResponse
--
--         , responseGetV2LoggingOptions $
--             newGetV2LoggingOptionsResponse
--
--         , responseListThingRegistrationTasks $
--             newListThingRegistrationTasksResponse
--
--         , responseRejectCertificateTransfer $
--             newRejectCertificateTransferResponse
--
--         , responseDescribeAuditSuppression $
--             newDescribeAuditSuppressionResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseCreateSecurityProfile $
--             newCreateSecurityProfileResponse
--
--         , responseDescribeBillingGroup $
--             newDescribeBillingGroupResponse
--
--         , responseDetachThingPrincipal $
--             newDetachThingPrincipalResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseDeprecateThingType $
--             newDeprecateThingTypeResponse
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

requestStartDetectMitigationActionsTask :: StartDetectMitigationActionsTask -> TestTree
requestStartDetectMitigationActionsTask =
  req
    "StartDetectMitigationActionsTask"
    "fixture/StartDetectMitigationActionsTask.yaml"

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

requestGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummaries -> TestTree
requestGetBehaviorModelTrainingSummaries =
  req
    "GetBehaviorModelTrainingSummaries"
    "fixture/GetBehaviorModelTrainingSummaries.yaml"

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

requestCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTask -> TestTree
requestCancelDetectMitigationActionsTask =
  req
    "CancelDetectMitigationActionsTask"
    "fixture/CancelDetectMitigationActionsTask.yaml"

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

requestDescribeCustomMetric :: DescribeCustomMetric -> TestTree
requestDescribeCustomMetric =
  req
    "DescribeCustomMetric"
    "fixture/DescribeCustomMetric.yaml"

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

requestDescribeJobTemplate :: DescribeJobTemplate -> TestTree
requestDescribeJobTemplate =
  req
    "DescribeJobTemplate"
    "fixture/DescribeJobTemplate.yaml"

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

requestCreateCustomMetric :: CreateCustomMetric -> TestTree
requestCreateCustomMetric =
  req
    "CreateCustomMetric"
    "fixture/CreateCustomMetric.yaml"

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

requestListFleetMetrics :: ListFleetMetrics -> TestTree
requestListFleetMetrics =
  req
    "ListFleetMetrics"
    "fixture/ListFleetMetrics.yaml"

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

requestListCustomMetrics :: ListCustomMetrics -> TestTree
requestListCustomMetrics =
  req
    "ListCustomMetrics"
    "fixture/ListCustomMetrics.yaml"

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

requestDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTask -> TestTree
requestDescribeDetectMitigationActionsTask =
  req
    "DescribeDetectMitigationActionsTask"
    "fixture/DescribeDetectMitigationActionsTask.yaml"

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

requestGetBucketsAggregation :: GetBucketsAggregation -> TestTree
requestGetBucketsAggregation =
  req
    "GetBucketsAggregation"
    "fixture/GetBucketsAggregation.yaml"

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

requestPutVerificationStateOnViolation :: PutVerificationStateOnViolation -> TestTree
requestPutVerificationStateOnViolation =
  req
    "PutVerificationStateOnViolation"
    "fixture/PutVerificationStateOnViolation.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup =
  req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestDescribeFleetMetric :: DescribeFleetMetric -> TestTree
requestDescribeFleetMetric =
  req
    "DescribeFleetMetric"
    "fixture/DescribeFleetMetric.yaml"

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

requestListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutions -> TestTree
requestListDetectMitigationActionsExecutions =
  req
    "ListDetectMitigationActionsExecutions"
    "fixture/ListDetectMitigationActionsExecutions.yaml"

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

requestListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasks -> TestTree
requestListDetectMitigationActionsTasks =
  req
    "ListDetectMitigationActionsTasks"
    "fixture/ListDetectMitigationActionsTasks.yaml"

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

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

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

requestDeleteFleetMetric :: DeleteFleetMetric -> TestTree
requestDeleteFleetMetric =
  req
    "DeleteFleetMetric"
    "fixture/DeleteFleetMetric.yaml"

requestUpdateFleetMetric :: UpdateFleetMetric -> TestTree
requestUpdateFleetMetric =
  req
    "UpdateFleetMetric"
    "fixture/UpdateFleetMetric.yaml"

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

requestCreateFleetMetric :: CreateFleetMetric -> TestTree
requestCreateFleetMetric =
  req
    "CreateFleetMetric"
    "fixture/CreateFleetMetric.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule =
  req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

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

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

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

requestUpdateCustomMetric :: UpdateCustomMetric -> TestTree
requestUpdateCustomMetric =
  req
    "UpdateCustomMetric"
    "fixture/UpdateCustomMetric.yaml"

requestDeleteCustomMetric :: DeleteCustomMetric -> TestTree
requestDeleteCustomMetric =
  req
    "DeleteCustomMetric"
    "fixture/DeleteCustomMetric.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCardinality)

responseCreateDomainConfiguration :: CreateDomainConfigurationResponse -> TestTree
responseCreateDomainConfiguration =
  res
    "CreateDomainConfigurationResponse"
    "fixture/CreateDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainConfiguration)

responseStartDetectMitigationActionsTask :: StartDetectMitigationActionsTaskResponse -> TestTree
responseStartDetectMitigationActionsTask =
  res
    "StartDetectMitigationActionsTaskResponse"
    "fixture/StartDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDetectMitigationActionsTask)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityProfile)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityProfile)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfiles)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningTemplate)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMitigationAction)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMitigationAction)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobExecution)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificate)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDynamicThingGroup)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingPrincipals)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDynamicThingGroup)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoleAlias)

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningTemplateVersion)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOTAUpdate)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultAuthorizer)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditMitigationActionsTasks)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingRegistrationTaskReports)

responseGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummariesResponse -> TestTree
responseGetBehaviorModelTrainingSummaries =
  res
    "GetBehaviorModelTrainingSummariesResponse"
    "fixture/GetBehaviorModelTrainingSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBehaviorModelTrainingSummaries)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipalThings)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup =
  res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveThingFromThingGroup)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventConfigurations)

responseCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTaskResponse -> TestTree
responseCancelDetectMitigationActionsTask =
  res
    "CancelDetectMitigationActionsTaskResponse"
    "fixture/CancelDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDetectMitigationActionsTask)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicRuleDestinations)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificateWithoutCA)

responseDescribeCustomMetric :: DescribeCustomMetricResponse -> TestTree
responseDescribeCustomMetric =
  res
    "DescribeCustomMetricResponse"
    "fixture/DescribeCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomMetric)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingGroups)

responseDescribeJobTemplate :: DescribeJobTemplateResponse -> TestTree
responseDescribeJobTemplate =
  res
    "DescribeJobTemplateResponse"
    "fixture/DescribeJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobTemplate)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScheduledAudits)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingRegistrationTask)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScheduledAudit)

responseDeleteScheduledAudit :: DeleteScheduledAuditResponse -> TestTree
responseDeleteScheduledAudit =
  res
    "DeleteScheduledAuditResponse"
    "fixture/DeleteScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAudit)

responseDescribeAuditFinding :: DescribeAuditFindingResponse -> TestTree
responseDescribeAuditFinding =
  res
    "DescribeAuditFindingResponse"
    "fixture/DescribeAuditFindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditFinding)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDimension)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingOptions)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAuditConfiguration)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountAuditConfiguration)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOTAUpdate)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEffectivePolicies)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingTypes)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetV2LoggingOptions)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningTemplate)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingGroupsForThing)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr =
  res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateFromCsr)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThing)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThing)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningTemplate)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisioningTemplate)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMitigationAction)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartThingRegistrationTask)

responseCreateScheduledAudit :: CreateScheduledAuditResponse -> TestTree
responseCreateScheduledAudit =
  res
    "CreateScheduledAuditResponse"
    "fixture/CreateScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduledAudit)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuthorizers)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobExecutionsForJob)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveThingFromBillingGroup)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchIndex)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType =
  res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThingType)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityProfile)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteV2LoggingLevel)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultAuthorizer)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobExecution)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCertificateTransfer)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIndexingConfiguration)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditMitigationActionsExecutions)

responseCreateCustomMetric :: CreateCustomMetricResponse -> TestTree
responseCreateCustomMetric =
  res
    "CreateCustomMetricResponse"
    "fixture/CreateCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomMetric)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditMitigationActionsTask)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStatistics)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoleAlias)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoleAlias)

responseListFleetMetrics :: ListFleetMetricsResponse -> TestTree
responseListFleetMetrics =
  res
    "ListFleetMetricsResponse"
    "fixture/ListFleetMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleetMetrics)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicyVersion)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableTopicRule)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule =
  res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopicRule)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndex)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTargetsWithJob)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachSecurityProfile)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedPolicies)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicyVersion)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCACertificates)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopicRule)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobDocument)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningTemplateVersion)

responseListCustomMetrics :: ListCustomMetricsResponse -> TestTree
responseListCustomMetrics =
  res
    "ListCustomMetricsResponse"
    "fixture/ListCustomMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomMetrics)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelAuditTask)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoleAlias)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate =
  res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCACertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate =
  res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCACertificate)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicRules)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferCertificate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoleAliases)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandAuditTask)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingGroup)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForSecurityProfile)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJob)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAuditMitigationActionsTask)

responseDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTaskResponse -> TestTree
responseDescribeDetectMitigationActionsTask =
  res
    "DescribeDetectMitigationActionsTaskResponse"
    "fixture/DescribeDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectMitigationActionsTask)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicRule)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing =
  res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThing)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainConfigurations)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditTasks)

responseDescribeAccountAuditConfiguration :: DescribeAccountAuditConfigurationResponse -> TestTree
responseDescribeAccountAuditConfiguration =
  res
    "DescribeAccountAuditConfigurationResponse"
    "fixture/DescribeAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAuditConfiguration)

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDimension)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDimension)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingsInThingGroup)

responseListAuditFindings :: ListAuditFindingsResponse -> TestTree
responseListAuditFindings =
  res
    "ListAuditFindingsResponse"
    "fixture/ListAuditFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditFindings)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledAudit)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMitigationAction)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmTopicRuleDestination)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMitigationActions)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuthorizer)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicyVersion)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActiveViolations)

responseValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviorsResponse -> TestTree
responseValidateSecurityProfileBehaviors =
  res
    "ValidateSecurityProfileBehaviorsResponse"
    "fixture/ValidateSecurityProfileBehaviorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateSecurityProfileBehaviors)

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListViolationEvents)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificate)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDimension)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIndexingConfiguration)

responseGetBucketsAggregation :: GetBucketsAggregationResponse -> TestTree
responseGetBucketsAggregation =
  res
    "GetBucketsAggregationResponse"
    "fixture/GetBucketsAggregationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketsAggregation)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningClaim)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeAuthorizer)

responsePutVerificationStateOnViolation :: PutVerificationStateOnViolationResponse -> TestTree
responsePutVerificationStateOnViolation =
  res
    "PutVerificationStateOnViolationResponse"
    "fixture/PutVerificationStateOnViolationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVerificationStateOnViolation)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThingGroup)

responseDescribeFleetMetric :: DescribeFleetMetricResponse -> TestTree
responseDescribeFleetMetric =
  res
    "DescribeFleetMetricResponse"
    "fixture/DescribeFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetMetric)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopicRuleDestination)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddThingToBillingGroup)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTopicRuleDestination)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopicRuleDestination)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingGroup)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingGroup)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOTAUpdates)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOTAUpdate)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDynamicThingGroup)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachSecurityProfile)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOutgoingCertificates)

responseDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersionResponse -> TestTree
responseDeleteProvisioningTemplateVersion =
  res
    "DeleteProvisioningTemplateVersionResponse"
    "fixture/DeleteProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningTemplateVersion)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate =
  res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCACertificate)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningTemplateVersions)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistrationCode)

responseListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutionsResponse -> TestTree
responseListDetectMitigationActionsExecutions =
  res
    "ListDetectMitigationActionsExecutionsResponse"
    "fixture/ListDetectMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectMitigationActionsExecutions)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBillingGroups)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType =
  res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingType)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBillingGroup)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddThingToThingGroup)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBillingGroup)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicRuleDestination)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificatesByCA)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuditSuppression)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachThingPrincipal)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThings)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuditSuppression)

responseListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasksResponse -> TestTree
responseListDetectMitigationActionsTasks =
  res
    "ListDetectMitigationActionsTasksResponse"
    "fixture/ListDetectMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectMitigationActionsTasks)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing =
  res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterThing)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditSuppressions)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainConfiguration)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditTask)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistrationCode)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestAuthorization)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndices)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthorizer)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThing)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStream)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelAuditMitigationActionsTask)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuditSuppression)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBillingGroup)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningTemplates)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListV2LoggingLevels)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask =
  res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopThingRegistrationTask)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForPolicy)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobTemplate)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer =
  res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClearDefaultAuthorizer)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceTopicRule)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteFleetMetric :: DeleteFleetMetricResponse -> TestTree
responseDeleteFleetMetric =
  res
    "DeleteFleetMetricResponse"
    "fixture/DeleteFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetMetric)

responseUpdateFleetMetric :: UpdateFleetMetricResponse -> TestTree
responseUpdateFleetMetric =
  res
    "UpdateFleetMetricResponse"
    "fixture/UpdateFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetMetric)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultPolicyVersion)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJobExecution)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyVersions)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetV2LoggingLevel)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobExecutionsForThing)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate =
  res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeysAndCertificate)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingsInBillingGroup)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingGroupsForThing)

responseCreateFleetMetric :: CreateFleetMetricResponse -> TestTree
responseCreateFleetMetric =
  res
    "CreateFleetMetricResponse"
    "fixture/CreateFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleetMetric)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableTopicRule)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobTemplate)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptCertificateTransfer)

responseGetPercentiles :: GetPercentilesResponse -> TestTree
responseGetPercentiles =
  res
    "GetPercentilesResponse"
    "fixture/GetPercentilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPercentiles)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobTemplates)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfilesForTarget)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventConfigurations)

responseUpdateCustomMetric :: UpdateCustomMetricResponse -> TestTree
responseUpdateCustomMetric =
  res
    "UpdateCustomMetricResponse"
    "fixture/UpdateCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomMetric)

responseDeleteCustomMetric :: DeleteCustomMetricResponse -> TestTree
responseDeleteCustomMetric =
  res
    "DeleteCustomMetricResponse"
    "fixture/DeleteCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomMetric)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCACertificate)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainConfiguration)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainConfiguration)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoggingOptions)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingType)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDimensions)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetV2LoggingOptions)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingRegistrationTasks)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectCertificateTransfer)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditSuppression)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityProfile)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBillingGroup)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachThingPrincipal)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType =
  res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateThingType)
