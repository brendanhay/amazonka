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
--         [ requestGetBucketsAggregation $
--             newGetBucketsAggregation
--
--         , requestUpdateIndexingConfiguration $
--             newUpdateIndexingConfiguration
--
--         , requestPutVerificationStateOnViolation $
--             newPutVerificationStateOnViolation
--
--         , requestListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReports
--
--         , requestDescribeFleetMetric $
--             newDescribeFleetMetric
--
--         , requestCreateProvisioningClaim $
--             newCreateProvisioningClaim
--
--         , requestUpdateCertificate $
--             newUpdateCertificate
--
--         , requestListMitigationActions $
--             newListMitigationActions
--
--         , requestDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplate
--
--         , requestDeleteJobExecution $
--             newDeleteJobExecution
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestUpdateMitigationAction $
--             newUpdateMitigationAction
--
--         , requestDeleteMitigationAction $
--             newDeleteMitigationAction
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTask
--
--         , requestGetCardinality $
--             newGetCardinality
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestListViolationEvents $
--             newListViolationEvents
--
--         , requestCreateDimension $
--             newCreateDimension
--
--         , requestListAuditTasks $
--             newListAuditTasks
--
--         , requestDescribeThingType $
--             newDescribeThingType
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestCreateMitigationAction $
--             newCreateMitigationAction
--
--         , requestDeleteDomainConfiguration $
--             newDeleteDomainConfiguration
--
--         , requestGetTopicRule $
--             newGetTopicRule
--
--         , requestUpdateDomainConfiguration $
--             newUpdateDomainConfiguration
--
--         , requestRejectCertificateTransfer $
--             newRejectCertificateTransfer
--
--         , requestSetLoggingOptions $
--             newSetLoggingOptions
--
--         , requestCreateSecurityProfile $
--             newCreateSecurityProfile
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestListDomainConfigurations $
--             newListDomainConfigurations
--
--         , requestDescribeScheduledAudit $
--             newDescribeScheduledAudit
--
--         , requestGetV2LoggingOptions $
--             newGetV2LoggingOptions
--
--         , requestListThingsInThingGroup $
--             newListThingsInThingGroup
--
--         , requestListCustomMetrics $
--             newListCustomMetrics
--
--         , requestAttachSecurityProfile $
--             newAttachSecurityProfile
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteCACertificate $
--             newDeleteCACertificate
--
--         , requestDeleteCustomMetric $
--             newDeleteCustomMetric
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestListRoleAliases $
--             newListRoleAliases
--
--         , requestCancelAuditTask $
--             newCancelAuditTask
--
--         , requestCreateRoleAlias $
--             newCreateRoleAlias
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersion
--
--         , requestCreateKeysAndCertificate $
--             newCreateKeysAndCertificate
--
--         , requestUpdateCACertificate $
--             newUpdateCACertificate
--
--         , requestUpdateCustomMetric $
--             newUpdateCustomMetric
--
--         , requestStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTask
--
--         , requestTransferCertificate $
--             newTransferCertificate
--
--         , requestGetPercentiles $
--             newGetPercentiles
--
--         , requestDeleteTopicRule $
--             newDeleteTopicRule
--
--         , requestListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfile
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestListThingsInBillingGroup $
--             newListThingsInBillingGroup
--
--         , requestSetV2LoggingLevel $
--             newSetV2LoggingLevel
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestCreateCustomMetric $
--             newCreateCustomMetric
--
--         , requestListFleetMetrics $
--             newListFleetMetrics
--
--         , requestUpdateFleetMetric $
--             newUpdateFleetMetric
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListJobExecutionsForThing $
--             newListJobExecutionsForThing
--
--         , requestDeleteFleetMetric $
--             newDeleteFleetMetric
--
--         , requestDisableTopicRule $
--             newDisableTopicRule
--
--         , requestDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTask
--
--         , requestCreateThing $
--             newCreateThing
--
--         , requestListV2LoggingLevels $
--             newListV2LoggingLevels
--
--         , requestListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutions
--
--         , requestListProvisioningTemplates $
--             newListProvisioningTemplates
--
--         , requestStartThingRegistrationTask $
--             newStartThingRegistrationTask
--
--         , requestDescribeJobExecution $
--             newDescribeJobExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplate
--
--         , requestUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplate
--
--         , requestGetIndexingConfiguration $
--             newGetIndexingConfiguration
--
--         , requestStopThingRegistrationTask $
--             newStopThingRegistrationTask
--
--         , requestCreateScheduledAudit $
--             newCreateScheduledAudit
--
--         , requestSetDefaultAuthorizer $
--             newSetDefaultAuthorizer
--
--         , requestDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevel
--
--         , requestDescribeMitigationAction $
--             newDescribeMitigationAction
--
--         , requestDescribeDimension $
--             newDescribeDimension
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestSetV2LoggingOptions $
--             newSetV2LoggingOptions
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasks
--
--         , requestUpdateAuditSuppression $
--             newUpdateAuditSuppression
--
--         , requestUpdateStream $
--             newUpdateStream
--
--         , requestUpdateThing $
--             newUpdateThing
--
--         , requestDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTask
--
--         , requestDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfiguration
--
--         , requestListThingGroupsForThing $
--             newListThingGroupsForThing
--
--         , requestListCertificatesByCA $
--             newListCertificatesByCA
--
--         , requestListThings $
--             newListThings
--
--         , requestAddThingToThingGroup $
--             newAddThingToThingGroup
--
--         , requestDeleteBillingGroup $
--             newDeleteBillingGroup
--
--         , requestListScheduledAudits $
--             newListScheduledAudits
--
--         , requestDeleteRegistrationCode $
--             newDeleteRegistrationCode
--
--         , requestDeleteThing $
--             newDeleteThing
--
--         , requestAttachThingPrincipal $
--             newAttachThingPrincipal
--
--         , requestUpdateBillingGroup $
--             newUpdateBillingGroup
--
--         , requestUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfiguration
--
--         , requestGetLoggingOptions $
--             newGetLoggingOptions
--
--         , requestDeleteAuditSuppression $
--             newDeleteAuditSuppression
--
--         , requestDescribeCustomMetric $
--             newDescribeCustomMetric
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDeleteOTAUpdate $
--             newDeleteOTAUpdate
--
--         , requestGetRegistrationCode $
--             newGetRegistrationCode
--
--         , requestListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutions
--
--         , requestRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCA
--
--         , requestCreateDynamicThingGroup $
--             newCreateDynamicThingGroup
--
--         , requestDescribeCACertificate $
--             newDescribeCACertificate
--
--         , requestDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersion
--
--         , requestDetachSecurityProfile $
--             newDetachSecurityProfile
--
--         , requestListThingPrincipals $
--             newListThingPrincipals
--
--         , requestRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroup
--
--         , requestGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummaries
--
--         , requestUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroup
--
--         , requestCreateTopicRuleDestination $
--             newCreateTopicRuleDestination
--
--         , requestListPrincipalThings $
--             newListPrincipalThings
--
--         , requestCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersion
--
--         , requestDescribeRoleAlias $
--             newDescribeRoleAlias
--
--         , requestDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroup
--
--         , requestCreateThingGroup $
--             newCreateThingGroup
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestCreateOTAUpdate $
--             newCreateOTAUpdate
--
--         , requestDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizer
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasks
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestUpdateSecurityProfile $
--             newUpdateSecurityProfile
--
--         , requestDescribeAuthorizer $
--             newDescribeAuthorizer
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestDeleteSecurityProfile $
--             newDeleteSecurityProfile
--
--         , requestValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviors
--
--         , requestCreateDomainConfiguration $
--             newCreateDomainConfiguration
--
--         , requestListActiveViolations $
--             newListActiveViolations
--
--         , requestDescribeBillingGroup $
--             newDescribeBillingGroup
--
--         , requestListThingRegistrationTasks $
--             newListThingRegistrationTasks
--
--         , requestUpdateDimension $
--             newUpdateDimension
--
--         , requestDescribeAuditSuppression $
--             newDescribeAuditSuppression
--
--         , requestDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfiguration
--
--         , requestDeprecateThingType $
--             newDeprecateThingType
--
--         , requestDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTask
--
--         , requestDeleteDimension $
--             newDeleteDimension
--
--         , requestListAuditFindings $
--             newListAuditFindings
--
--         , requestDescribeThing $
--             newDescribeThing
--
--         , requestListDimensions $
--             newListDimensions
--
--         , requestDetachThingPrincipal $
--             newDetachThingPrincipal
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestination
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListTopicRules $
--             newListTopicRules
--
--         , requestListCACertificates $
--             newListCACertificates
--
--         , requestStartOnDemandAuditTask $
--             newStartOnDemandAuditTask
--
--         , requestUpdateEventConfigurations $
--             newUpdateEventConfigurations
--
--         , requestUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThing
--
--         , requestListSecurityProfilesForTarget $
--             newListSecurityProfilesForTarget
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestEnableTopicRule $
--             newEnableTopicRule
--
--         , requestAcceptCertificateTransfer $
--             newAcceptCertificateTransfer
--
--         , requestGetJobDocument $
--             newGetJobDocument
--
--         , requestListAttachedPolicies $
--             newListAttachedPolicies
--
--         , requestDescribeThingGroup $
--             newDescribeThingGroup
--
--         , requestListJobs $
--             newListJobs
--
--         , requestCreateFleetMetric $
--             newCreateFleetMetric
--
--         , requestRegisterCACertificate $
--             newRegisterCACertificate
--
--         , requestReplaceTopicRule $
--             newReplaceTopicRule
--
--         , requestGetStatistics $
--             newGetStatistics
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestUpdateRoleAlias $
--             newUpdateRoleAlias
--
--         , requestClearDefaultAuthorizer $
--             newClearDefaultAuthorizer
--
--         , requestCreateTopicRule $
--             newCreateTopicRule
--
--         , requestCancelJobExecution $
--             newCancelJobExecution
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestAssociateTargetsWithJob $
--             newAssociateTargetsWithJob
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestDeleteRoleAlias $
--             newDeleteRoleAlias
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestCancelCertificateTransfer $
--             newCancelCertificateTransfer
--
--         , requestListAuthorizers $
--             newListAuthorizers
--
--         , requestCreateThingType $
--             newCreateThingType
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestCreateAuditSuppression $
--             newCreateAuditSuppression
--
--         , requestListJobExecutionsForJob $
--             newListJobExecutionsForJob
--
--         , requestDescribeSecurityProfile $
--             newDescribeSecurityProfile
--
--         , requestRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroup
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestSearchIndex $
--             newSearchIndex
--
--         , requestCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTask
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestCreateBillingGroup $
--             newCreateBillingGroup
--
--         , requestDescribeAuditFinding $
--             newDescribeAuditFinding
--
--         , requestDeleteScheduledAudit $
--             newDeleteScheduledAudit
--
--         , requestGetEffectivePolicies $
--             newGetEffectivePolicies
--
--         , requestGetOTAUpdate $
--             newGetOTAUpdate
--
--         , requestCreateProvisioningTemplate $
--             newCreateProvisioningTemplate
--
--         , requestListThingTypes $
--             newListThingTypes
--
--         , requestDeleteThingType $
--             newDeleteThingType
--
--         , requestRegisterThing $
--             newRegisterThing
--
--         , requestListBillingGroups $
--             newListBillingGroups
--
--         , requestListStreams $
--             newListStreams
--
--         , requestTestAuthorization $
--             newTestAuthorization
--
--         , requestListIndices $
--             newListIndices
--
--         , requestDescribeAuditTask $
--             newDescribeAuditTask
--
--         , requestCreateCertificateFromCsr $
--             newCreateCertificateFromCsr
--
--         , requestListAuditSuppressions $
--             newListAuditSuppressions
--
--         , requestDescribeDomainConfiguration $
--             newDescribeDomainConfiguration
--
--         , requestUpdateScheduledAudit $
--             newUpdateScheduledAudit
--
--         , requestGetTopicRuleDestination $
--             newGetTopicRuleDestination
--
--         , requestDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestination
--
--         , requestListOutgoingCertificates $
--             newListOutgoingCertificates
--
--         , requestDescribeJobTemplate $
--             newDescribeJobTemplate
--
--         , requestAddThingToBillingGroup $
--             newAddThingToBillingGroup
--
--         , requestListOTAUpdates $
--             newListOTAUpdates
--
--         , requestUpdateThingGroup $
--             newUpdateThingGroup
--
--         , requestDeleteThingGroup $
--             newDeleteThingGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeEventConfigurations $
--             newDescribeEventConfigurations
--
--         , requestListTopicRuleDestinations $
--             newListTopicRuleDestinations
--
--         , requestListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersions
--
--         , requestUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestination
--
--         , requestListThingGroups $
--             newListThingGroups
--
--         , requestCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTask
--
--           ]

--     , testGroup "response"
--         [ responseGetBucketsAggregation $
--             newGetBucketsAggregationResponse
--
--         , responseUpdateIndexingConfiguration $
--             newUpdateIndexingConfigurationResponse
--
--         , responsePutVerificationStateOnViolation $
--             newPutVerificationStateOnViolationResponse
--
--         , responseListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReportsResponse
--
--         , responseDescribeFleetMetric $
--             newDescribeFleetMetricResponse
--
--         , responseCreateProvisioningClaim $
--             newCreateProvisioningClaimResponse
--
--         , responseUpdateCertificate $
--             newUpdateCertificateResponse
--
--         , responseListMitigationActions $
--             newListMitigationActionsResponse
--
--         , responseDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplateResponse
--
--         , responseDeleteJobExecution $
--             newDeleteJobExecutionResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseUpdateMitigationAction $
--             newUpdateMitigationActionResponse
--
--         , responseDeleteMitigationAction $
--             newDeleteMitigationActionResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTaskResponse
--
--         , responseGetCardinality $
--             newGetCardinalityResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseListViolationEvents $
--             newListViolationEventsResponse
--
--         , responseCreateDimension $
--             newCreateDimensionResponse
--
--         , responseListAuditTasks $
--             newListAuditTasksResponse
--
--         , responseDescribeThingType $
--             newDescribeThingTypeResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseCreateMitigationAction $
--             newCreateMitigationActionResponse
--
--         , responseDeleteDomainConfiguration $
--             newDeleteDomainConfigurationResponse
--
--         , responseGetTopicRule $
--             newGetTopicRuleResponse
--
--         , responseUpdateDomainConfiguration $
--             newUpdateDomainConfigurationResponse
--
--         , responseRejectCertificateTransfer $
--             newRejectCertificateTransferResponse
--
--         , responseSetLoggingOptions $
--             newSetLoggingOptionsResponse
--
--         , responseCreateSecurityProfile $
--             newCreateSecurityProfileResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseListDomainConfigurations $
--             newListDomainConfigurationsResponse
--
--         , responseDescribeScheduledAudit $
--             newDescribeScheduledAuditResponse
--
--         , responseGetV2LoggingOptions $
--             newGetV2LoggingOptionsResponse
--
--         , responseListThingsInThingGroup $
--             newListThingsInThingGroupResponse
--
--         , responseListCustomMetrics $
--             newListCustomMetricsResponse
--
--         , responseAttachSecurityProfile $
--             newAttachSecurityProfileResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteCACertificate $
--             newDeleteCACertificateResponse
--
--         , responseDeleteCustomMetric $
--             newDeleteCustomMetricResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseListRoleAliases $
--             newListRoleAliasesResponse
--
--         , responseCancelAuditTask $
--             newCancelAuditTaskResponse
--
--         , responseCreateRoleAlias $
--             newCreateRoleAliasResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersionResponse
--
--         , responseCreateKeysAndCertificate $
--             newCreateKeysAndCertificateResponse
--
--         , responseUpdateCACertificate $
--             newUpdateCACertificateResponse
--
--         , responseUpdateCustomMetric $
--             newUpdateCustomMetricResponse
--
--         , responseStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTaskResponse
--
--         , responseTransferCertificate $
--             newTransferCertificateResponse
--
--         , responseGetPercentiles $
--             newGetPercentilesResponse
--
--         , responseDeleteTopicRule $
--             newDeleteTopicRuleResponse
--
--         , responseListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfileResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseListThingsInBillingGroup $
--             newListThingsInBillingGroupResponse
--
--         , responseSetV2LoggingLevel $
--             newSetV2LoggingLevelResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseCreateCustomMetric $
--             newCreateCustomMetricResponse
--
--         , responseListFleetMetrics $
--             newListFleetMetricsResponse
--
--         , responseUpdateFleetMetric $
--             newUpdateFleetMetricResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListJobExecutionsForThing $
--             newListJobExecutionsForThingResponse
--
--         , responseDeleteFleetMetric $
--             newDeleteFleetMetricResponse
--
--         , responseDisableTopicRule $
--             newDisableTopicRuleResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTaskResponse
--
--         , responseCreateThing $
--             newCreateThingResponse
--
--         , responseListV2LoggingLevels $
--             newListV2LoggingLevelsResponse
--
--         , responseListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutionsResponse
--
--         , responseListProvisioningTemplates $
--             newListProvisioningTemplatesResponse
--
--         , responseStartThingRegistrationTask $
--             newStartThingRegistrationTaskResponse
--
--         , responseDescribeJobExecution $
--             newDescribeJobExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplateResponse
--
--         , responseUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplateResponse
--
--         , responseGetIndexingConfiguration $
--             newGetIndexingConfigurationResponse
--
--         , responseStopThingRegistrationTask $
--             newStopThingRegistrationTaskResponse
--
--         , responseCreateScheduledAudit $
--             newCreateScheduledAuditResponse
--
--         , responseSetDefaultAuthorizer $
--             newSetDefaultAuthorizerResponse
--
--         , responseDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevelResponse
--
--         , responseDescribeMitigationAction $
--             newDescribeMitigationActionResponse
--
--         , responseDescribeDimension $
--             newDescribeDimensionResponse
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseSetV2LoggingOptions $
--             newSetV2LoggingOptionsResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasksResponse
--
--         , responseUpdateAuditSuppression $
--             newUpdateAuditSuppressionResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--         , responseUpdateThing $
--             newUpdateThingResponse
--
--         , responseDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTaskResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfigurationResponse
--
--         , responseListThingGroupsForThing $
--             newListThingGroupsForThingResponse
--
--         , responseListCertificatesByCA $
--             newListCertificatesByCAResponse
--
--         , responseListThings $
--             newListThingsResponse
--
--         , responseAddThingToThingGroup $
--             newAddThingToThingGroupResponse
--
--         , responseDeleteBillingGroup $
--             newDeleteBillingGroupResponse
--
--         , responseListScheduledAudits $
--             newListScheduledAuditsResponse
--
--         , responseDeleteRegistrationCode $
--             newDeleteRegistrationCodeResponse
--
--         , responseDeleteThing $
--             newDeleteThingResponse
--
--         , responseAttachThingPrincipal $
--             newAttachThingPrincipalResponse
--
--         , responseUpdateBillingGroup $
--             newUpdateBillingGroupResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfigurationResponse
--
--         , responseGetLoggingOptions $
--             newGetLoggingOptionsResponse
--
--         , responseDeleteAuditSuppression $
--             newDeleteAuditSuppressionResponse
--
--         , responseDescribeCustomMetric $
--             newDescribeCustomMetricResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDeleteOTAUpdate $
--             newDeleteOTAUpdateResponse
--
--         , responseGetRegistrationCode $
--             newGetRegistrationCodeResponse
--
--         , responseListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutionsResponse
--
--         , responseRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCAResponse
--
--         , responseCreateDynamicThingGroup $
--             newCreateDynamicThingGroupResponse
--
--         , responseDescribeCACertificate $
--             newDescribeCACertificateResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersionResponse
--
--         , responseDetachSecurityProfile $
--             newDetachSecurityProfileResponse
--
--         , responseListThingPrincipals $
--             newListThingPrincipalsResponse
--
--         , responseRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroupResponse
--
--         , responseGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummariesResponse
--
--         , responseUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroupResponse
--
--         , responseCreateTopicRuleDestination $
--             newCreateTopicRuleDestinationResponse
--
--         , responseListPrincipalThings $
--             newListPrincipalThingsResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersionResponse
--
--         , responseDescribeRoleAlias $
--             newDescribeRoleAliasResponse
--
--         , responseDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroupResponse
--
--         , responseCreateThingGroup $
--             newCreateThingGroupResponse
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responseCreateOTAUpdate $
--             newCreateOTAUpdateResponse
--
--         , responseDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizerResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasksResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseUpdateSecurityProfile $
--             newUpdateSecurityProfileResponse
--
--         , responseDescribeAuthorizer $
--             newDescribeAuthorizerResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseDeleteSecurityProfile $
--             newDeleteSecurityProfileResponse
--
--         , responseValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviorsResponse
--
--         , responseCreateDomainConfiguration $
--             newCreateDomainConfigurationResponse
--
--         , responseListActiveViolations $
--             newListActiveViolationsResponse
--
--         , responseDescribeBillingGroup $
--             newDescribeBillingGroupResponse
--
--         , responseListThingRegistrationTasks $
--             newListThingRegistrationTasksResponse
--
--         , responseUpdateDimension $
--             newUpdateDimensionResponse
--
--         , responseDescribeAuditSuppression $
--             newDescribeAuditSuppressionResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfigurationResponse
--
--         , responseDeprecateThingType $
--             newDeprecateThingTypeResponse
--
--         , responseDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTaskResponse
--
--         , responseDeleteDimension $
--             newDeleteDimensionResponse
--
--         , responseListAuditFindings $
--             newListAuditFindingsResponse
--
--         , responseDescribeThing $
--             newDescribeThingResponse
--
--         , responseListDimensions $
--             newListDimensionsResponse
--
--         , responseDetachThingPrincipal $
--             newDetachThingPrincipalResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestinationResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListTopicRules $
--             newListTopicRulesResponse
--
--         , responseListCACertificates $
--             newListCACertificatesResponse
--
--         , responseStartOnDemandAuditTask $
--             newStartOnDemandAuditTaskResponse
--
--         , responseUpdateEventConfigurations $
--             newUpdateEventConfigurationsResponse
--
--         , responseUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThingResponse
--
--         , responseListSecurityProfilesForTarget $
--             newListSecurityProfilesForTargetResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseEnableTopicRule $
--             newEnableTopicRuleResponse
--
--         , responseAcceptCertificateTransfer $
--             newAcceptCertificateTransferResponse
--
--         , responseGetJobDocument $
--             newGetJobDocumentResponse
--
--         , responseListAttachedPolicies $
--             newListAttachedPoliciesResponse
--
--         , responseDescribeThingGroup $
--             newDescribeThingGroupResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseCreateFleetMetric $
--             newCreateFleetMetricResponse
--
--         , responseRegisterCACertificate $
--             newRegisterCACertificateResponse
--
--         , responseReplaceTopicRule $
--             newReplaceTopicRuleResponse
--
--         , responseGetStatistics $
--             newGetStatisticsResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseUpdateRoleAlias $
--             newUpdateRoleAliasResponse
--
--         , responseClearDefaultAuthorizer $
--             newClearDefaultAuthorizerResponse
--
--         , responseCreateTopicRule $
--             newCreateTopicRuleResponse
--
--         , responseCancelJobExecution $
--             newCancelJobExecutionResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseAssociateTargetsWithJob $
--             newAssociateTargetsWithJobResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseDeleteRoleAlias $
--             newDeleteRoleAliasResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseCancelCertificateTransfer $
--             newCancelCertificateTransferResponse
--
--         , responseListAuthorizers $
--             newListAuthorizersResponse
--
--         , responseCreateThingType $
--             newCreateThingTypeResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseCreateAuditSuppression $
--             newCreateAuditSuppressionResponse
--
--         , responseListJobExecutionsForJob $
--             newListJobExecutionsForJobResponse
--
--         , responseDescribeSecurityProfile $
--             newDescribeSecurityProfileResponse
--
--         , responseRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroupResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseSearchIndex $
--             newSearchIndexResponse
--
--         , responseCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTaskResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseCreateBillingGroup $
--             newCreateBillingGroupResponse
--
--         , responseDescribeAuditFinding $
--             newDescribeAuditFindingResponse
--
--         , responseDeleteScheduledAudit $
--             newDeleteScheduledAuditResponse
--
--         , responseGetEffectivePolicies $
--             newGetEffectivePoliciesResponse
--
--         , responseGetOTAUpdate $
--             newGetOTAUpdateResponse
--
--         , responseCreateProvisioningTemplate $
--             newCreateProvisioningTemplateResponse
--
--         , responseListThingTypes $
--             newListThingTypesResponse
--
--         , responseDeleteThingType $
--             newDeleteThingTypeResponse
--
--         , responseRegisterThing $
--             newRegisterThingResponse
--
--         , responseListBillingGroups $
--             newListBillingGroupsResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseTestAuthorization $
--             newTestAuthorizationResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseDescribeAuditTask $
--             newDescribeAuditTaskResponse
--
--         , responseCreateCertificateFromCsr $
--             newCreateCertificateFromCsrResponse
--
--         , responseListAuditSuppressions $
--             newListAuditSuppressionsResponse
--
--         , responseDescribeDomainConfiguration $
--             newDescribeDomainConfigurationResponse
--
--         , responseUpdateScheduledAudit $
--             newUpdateScheduledAuditResponse
--
--         , responseGetTopicRuleDestination $
--             newGetTopicRuleDestinationResponse
--
--         , responseDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestinationResponse
--
--         , responseListOutgoingCertificates $
--             newListOutgoingCertificatesResponse
--
--         , responseDescribeJobTemplate $
--             newDescribeJobTemplateResponse
--
--         , responseAddThingToBillingGroup $
--             newAddThingToBillingGroupResponse
--
--         , responseListOTAUpdates $
--             newListOTAUpdatesResponse
--
--         , responseUpdateThingGroup $
--             newUpdateThingGroupResponse
--
--         , responseDeleteThingGroup $
--             newDeleteThingGroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeEventConfigurations $
--             newDescribeEventConfigurationsResponse
--
--         , responseListTopicRuleDestinations $
--             newListTopicRuleDestinationsResponse
--
--         , responseListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersionsResponse
--
--         , responseUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestinationResponse
--
--         , responseListThingGroups $
--             newListThingGroupsResponse
--
--         , responseCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTaskResponse
--
--           ]
--     ]

-- Requests

requestGetBucketsAggregation :: GetBucketsAggregation -> TestTree
requestGetBucketsAggregation =
  req
    "GetBucketsAggregation"
    "fixture/GetBucketsAggregation.yaml"

requestUpdateIndexingConfiguration :: UpdateIndexingConfiguration -> TestTree
requestUpdateIndexingConfiguration =
  req
    "UpdateIndexingConfiguration"
    "fixture/UpdateIndexingConfiguration.yaml"

requestPutVerificationStateOnViolation :: PutVerificationStateOnViolation -> TestTree
requestPutVerificationStateOnViolation =
  req
    "PutVerificationStateOnViolation"
    "fixture/PutVerificationStateOnViolation.yaml"

requestListThingRegistrationTaskReports :: ListThingRegistrationTaskReports -> TestTree
requestListThingRegistrationTaskReports =
  req
    "ListThingRegistrationTaskReports"
    "fixture/ListThingRegistrationTaskReports.yaml"

requestDescribeFleetMetric :: DescribeFleetMetric -> TestTree
requestDescribeFleetMetric =
  req
    "DescribeFleetMetric"
    "fixture/DescribeFleetMetric.yaml"

requestCreateProvisioningClaim :: CreateProvisioningClaim -> TestTree
requestCreateProvisioningClaim =
  req
    "CreateProvisioningClaim"
    "fixture/CreateProvisioningClaim.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate =
  req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestListMitigationActions :: ListMitigationActions -> TestTree
requestListMitigationActions =
  req
    "ListMitigationActions"
    "fixture/ListMitigationActions.yaml"

requestDescribeProvisioningTemplate :: DescribeProvisioningTemplate -> TestTree
requestDescribeProvisioningTemplate =
  req
    "DescribeProvisioningTemplate"
    "fixture/DescribeProvisioningTemplate.yaml"

requestDeleteJobExecution :: DeleteJobExecution -> TestTree
requestDeleteJobExecution =
  req
    "DeleteJobExecution"
    "fixture/DeleteJobExecution.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

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

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestStartDetectMitigationActionsTask :: StartDetectMitigationActionsTask -> TestTree
requestStartDetectMitigationActionsTask =
  req
    "StartDetectMitigationActionsTask"
    "fixture/StartDetectMitigationActionsTask.yaml"

requestGetCardinality :: GetCardinality -> TestTree
requestGetCardinality =
  req
    "GetCardinality"
    "fixture/GetCardinality.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestListViolationEvents :: ListViolationEvents -> TestTree
requestListViolationEvents =
  req
    "ListViolationEvents"
    "fixture/ListViolationEvents.yaml"

requestCreateDimension :: CreateDimension -> TestTree
requestCreateDimension =
  req
    "CreateDimension"
    "fixture/CreateDimension.yaml"

requestListAuditTasks :: ListAuditTasks -> TestTree
requestListAuditTasks =
  req
    "ListAuditTasks"
    "fixture/ListAuditTasks.yaml"

requestDescribeThingType :: DescribeThingType -> TestTree
requestDescribeThingType =
  req
    "DescribeThingType"
    "fixture/DescribeThingType.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestCreateMitigationAction :: CreateMitigationAction -> TestTree
requestCreateMitigationAction =
  req
    "CreateMitigationAction"
    "fixture/CreateMitigationAction.yaml"

requestDeleteDomainConfiguration :: DeleteDomainConfiguration -> TestTree
requestDeleteDomainConfiguration =
  req
    "DeleteDomainConfiguration"
    "fixture/DeleteDomainConfiguration.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule =
  req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestUpdateDomainConfiguration :: UpdateDomainConfiguration -> TestTree
requestUpdateDomainConfiguration =
  req
    "UpdateDomainConfiguration"
    "fixture/UpdateDomainConfiguration.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer =
  req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions =
  req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestCreateSecurityProfile :: CreateSecurityProfile -> TestTree
requestCreateSecurityProfile =
  req
    "CreateSecurityProfile"
    "fixture/CreateSecurityProfile.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestListDomainConfigurations :: ListDomainConfigurations -> TestTree
requestListDomainConfigurations =
  req
    "ListDomainConfigurations"
    "fixture/ListDomainConfigurations.yaml"

requestDescribeScheduledAudit :: DescribeScheduledAudit -> TestTree
requestDescribeScheduledAudit =
  req
    "DescribeScheduledAudit"
    "fixture/DescribeScheduledAudit.yaml"

requestGetV2LoggingOptions :: GetV2LoggingOptions -> TestTree
requestGetV2LoggingOptions =
  req
    "GetV2LoggingOptions"
    "fixture/GetV2LoggingOptions.yaml"

requestListThingsInThingGroup :: ListThingsInThingGroup -> TestTree
requestListThingsInThingGroup =
  req
    "ListThingsInThingGroup"
    "fixture/ListThingsInThingGroup.yaml"

requestListCustomMetrics :: ListCustomMetrics -> TestTree
requestListCustomMetrics =
  req
    "ListCustomMetrics"
    "fixture/ListCustomMetrics.yaml"

requestAttachSecurityProfile :: AttachSecurityProfile -> TestTree
requestAttachSecurityProfile =
  req
    "AttachSecurityProfile"
    "fixture/AttachSecurityProfile.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteCACertificate :: DeleteCACertificate -> TestTree
requestDeleteCACertificate =
  req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

requestDeleteCustomMetric :: DeleteCustomMetric -> TestTree
requestDeleteCustomMetric =
  req
    "DeleteCustomMetric"
    "fixture/DeleteCustomMetric.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestListRoleAliases :: ListRoleAliases -> TestTree
requestListRoleAliases =
  req
    "ListRoleAliases"
    "fixture/ListRoleAliases.yaml"

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

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

requestDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersion -> TestTree
requestDescribeProvisioningTemplateVersion =
  req
    "DescribeProvisioningTemplateVersion"
    "fixture/DescribeProvisioningTemplateVersion.yaml"

requestCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
requestCreateKeysAndCertificate =
  req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

requestUpdateCACertificate :: UpdateCACertificate -> TestTree
requestUpdateCACertificate =
  req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

requestUpdateCustomMetric :: UpdateCustomMetric -> TestTree
requestUpdateCustomMetric =
  req
    "UpdateCustomMetric"
    "fixture/UpdateCustomMetric.yaml"

requestStartAuditMitigationActionsTask :: StartAuditMitigationActionsTask -> TestTree
requestStartAuditMitigationActionsTask =
  req
    "StartAuditMitigationActionsTask"
    "fixture/StartAuditMitigationActionsTask.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate =
  req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

requestGetPercentiles :: GetPercentiles -> TestTree
requestGetPercentiles =
  req
    "GetPercentiles"
    "fixture/GetPercentiles.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule =
  req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

requestListTargetsForSecurityProfile :: ListTargetsForSecurityProfile -> TestTree
requestListTargetsForSecurityProfile =
  req
    "ListTargetsForSecurityProfile"
    "fixture/ListTargetsForSecurityProfile.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListThingsInBillingGroup :: ListThingsInBillingGroup -> TestTree
requestListThingsInBillingGroup =
  req
    "ListThingsInBillingGroup"
    "fixture/ListThingsInBillingGroup.yaml"

requestSetV2LoggingLevel :: SetV2LoggingLevel -> TestTree
requestSetV2LoggingLevel =
  req
    "SetV2LoggingLevel"
    "fixture/SetV2LoggingLevel.yaml"

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestCreateCustomMetric :: CreateCustomMetric -> TestTree
requestCreateCustomMetric =
  req
    "CreateCustomMetric"
    "fixture/CreateCustomMetric.yaml"

requestListFleetMetrics :: ListFleetMetrics -> TestTree
requestListFleetMetrics =
  req
    "ListFleetMetrics"
    "fixture/ListFleetMetrics.yaml"

requestUpdateFleetMetric :: UpdateFleetMetric -> TestTree
requestUpdateFleetMetric =
  req
    "UpdateFleetMetric"
    "fixture/UpdateFleetMetric.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListJobExecutionsForThing :: ListJobExecutionsForThing -> TestTree
requestListJobExecutionsForThing =
  req
    "ListJobExecutionsForThing"
    "fixture/ListJobExecutionsForThing.yaml"

requestDeleteFleetMetric :: DeleteFleetMetric -> TestTree
requestDeleteFleetMetric =
  req
    "DeleteFleetMetric"
    "fixture/DeleteFleetMetric.yaml"

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule =
  req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTask -> TestTree
requestDescribeAuditMitigationActionsTask =
  req
    "DescribeAuditMitigationActionsTask"
    "fixture/DescribeAuditMitigationActionsTask.yaml"

requestCreateThing :: CreateThing -> TestTree
requestCreateThing =
  req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestListV2LoggingLevels :: ListV2LoggingLevels -> TestTree
requestListV2LoggingLevels =
  req
    "ListV2LoggingLevels"
    "fixture/ListV2LoggingLevels.yaml"

requestListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutions -> TestTree
requestListAuditMitigationActionsExecutions =
  req
    "ListAuditMitigationActionsExecutions"
    "fixture/ListAuditMitigationActionsExecutions.yaml"

requestListProvisioningTemplates :: ListProvisioningTemplates -> TestTree
requestListProvisioningTemplates =
  req
    "ListProvisioningTemplates"
    "fixture/ListProvisioningTemplates.yaml"

requestStartThingRegistrationTask :: StartThingRegistrationTask -> TestTree
requestStartThingRegistrationTask =
  req
    "StartThingRegistrationTask"
    "fixture/StartThingRegistrationTask.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution =
  req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

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

requestGetIndexingConfiguration :: GetIndexingConfiguration -> TestTree
requestGetIndexingConfiguration =
  req
    "GetIndexingConfiguration"
    "fixture/GetIndexingConfiguration.yaml"

requestStopThingRegistrationTask :: StopThingRegistrationTask -> TestTree
requestStopThingRegistrationTask =
  req
    "StopThingRegistrationTask"
    "fixture/StopThingRegistrationTask.yaml"

requestCreateScheduledAudit :: CreateScheduledAudit -> TestTree
requestCreateScheduledAudit =
  req
    "CreateScheduledAudit"
    "fixture/CreateScheduledAudit.yaml"

requestSetDefaultAuthorizer :: SetDefaultAuthorizer -> TestTree
requestSetDefaultAuthorizer =
  req
    "SetDefaultAuthorizer"
    "fixture/SetDefaultAuthorizer.yaml"

requestDeleteV2LoggingLevel :: DeleteV2LoggingLevel -> TestTree
requestDeleteV2LoggingLevel =
  req
    "DeleteV2LoggingLevel"
    "fixture/DeleteV2LoggingLevel.yaml"

requestDescribeMitigationAction :: DescribeMitigationAction -> TestTree
requestDescribeMitigationAction =
  req
    "DescribeMitigationAction"
    "fixture/DescribeMitigationAction.yaml"

requestDescribeDimension :: DescribeDimension -> TestTree
requestDescribeDimension =
  req
    "DescribeDimension"
    "fixture/DescribeDimension.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestSetV2LoggingOptions :: SetV2LoggingOptions -> TestTree
requestSetV2LoggingOptions =
  req
    "SetV2LoggingOptions"
    "fixture/SetV2LoggingOptions.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasks -> TestTree
requestListDetectMitigationActionsTasks =
  req
    "ListDetectMitigationActionsTasks"
    "fixture/ListDetectMitigationActionsTasks.yaml"

requestUpdateAuditSuppression :: UpdateAuditSuppression -> TestTree
requestUpdateAuditSuppression =
  req
    "UpdateAuditSuppression"
    "fixture/UpdateAuditSuppression.yaml"

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream =
  req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

requestUpdateThing :: UpdateThing -> TestTree
requestUpdateThing =
  req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

requestDescribeThingRegistrationTask :: DescribeThingRegistrationTask -> TestTree
requestDescribeThingRegistrationTask =
  req
    "DescribeThingRegistrationTask"
    "fixture/DescribeThingRegistrationTask.yaml"

requestDeleteAccountAuditConfiguration :: DeleteAccountAuditConfiguration -> TestTree
requestDeleteAccountAuditConfiguration =
  req
    "DeleteAccountAuditConfiguration"
    "fixture/DeleteAccountAuditConfiguration.yaml"

requestListThingGroupsForThing :: ListThingGroupsForThing -> TestTree
requestListThingGroupsForThing =
  req
    "ListThingGroupsForThing"
    "fixture/ListThingGroupsForThing.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA =
  req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestListThings :: ListThings -> TestTree
requestListThings =
  req
    "ListThings"
    "fixture/ListThings.yaml"

requestAddThingToThingGroup :: AddThingToThingGroup -> TestTree
requestAddThingToThingGroup =
  req
    "AddThingToThingGroup"
    "fixture/AddThingToThingGroup.yaml"

requestDeleteBillingGroup :: DeleteBillingGroup -> TestTree
requestDeleteBillingGroup =
  req
    "DeleteBillingGroup"
    "fixture/DeleteBillingGroup.yaml"

requestListScheduledAudits :: ListScheduledAudits -> TestTree
requestListScheduledAudits =
  req
    "ListScheduledAudits"
    "fixture/ListScheduledAudits.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode =
  req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing =
  req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal =
  req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestUpdateBillingGroup :: UpdateBillingGroup -> TestTree
requestUpdateBillingGroup =
  req
    "UpdateBillingGroup"
    "fixture/UpdateBillingGroup.yaml"

requestUpdateAccountAuditConfiguration :: UpdateAccountAuditConfiguration -> TestTree
requestUpdateAccountAuditConfiguration =
  req
    "UpdateAccountAuditConfiguration"
    "fixture/UpdateAccountAuditConfiguration.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions =
  req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestDeleteAuditSuppression :: DeleteAuditSuppression -> TestTree
requestDeleteAuditSuppression =
  req
    "DeleteAuditSuppression"
    "fixture/DeleteAuditSuppression.yaml"

requestDescribeCustomMetric :: DescribeCustomMetric -> TestTree
requestDescribeCustomMetric =
  req
    "DescribeCustomMetric"
    "fixture/DescribeCustomMetric.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDeleteOTAUpdate :: DeleteOTAUpdate -> TestTree
requestDeleteOTAUpdate =
  req
    "DeleteOTAUpdate"
    "fixture/DeleteOTAUpdate.yaml"

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

requestRegisterCertificateWithoutCA :: RegisterCertificateWithoutCA -> TestTree
requestRegisterCertificateWithoutCA =
  req
    "RegisterCertificateWithoutCA"
    "fixture/RegisterCertificateWithoutCA.yaml"

requestCreateDynamicThingGroup :: CreateDynamicThingGroup -> TestTree
requestCreateDynamicThingGroup =
  req
    "CreateDynamicThingGroup"
    "fixture/CreateDynamicThingGroup.yaml"

requestDescribeCACertificate :: DescribeCACertificate -> TestTree
requestDescribeCACertificate =
  req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

requestDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersion -> TestTree
requestDeleteProvisioningTemplateVersion =
  req
    "DeleteProvisioningTemplateVersion"
    "fixture/DeleteProvisioningTemplateVersion.yaml"

requestDetachSecurityProfile :: DetachSecurityProfile -> TestTree
requestDetachSecurityProfile =
  req
    "DetachSecurityProfile"
    "fixture/DetachSecurityProfile.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals =
  req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestRemoveThingFromThingGroup :: RemoveThingFromThingGroup -> TestTree
requestRemoveThingFromThingGroup =
  req
    "RemoveThingFromThingGroup"
    "fixture/RemoveThingFromThingGroup.yaml"

requestGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummaries -> TestTree
requestGetBehaviorModelTrainingSummaries =
  req
    "GetBehaviorModelTrainingSummaries"
    "fixture/GetBehaviorModelTrainingSummaries.yaml"

requestUpdateDynamicThingGroup :: UpdateDynamicThingGroup -> TestTree
requestUpdateDynamicThingGroup =
  req
    "UpdateDynamicThingGroup"
    "fixture/UpdateDynamicThingGroup.yaml"

requestCreateTopicRuleDestination :: CreateTopicRuleDestination -> TestTree
requestCreateTopicRuleDestination =
  req
    "CreateTopicRuleDestination"
    "fixture/CreateTopicRuleDestination.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings =
  req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersion -> TestTree
requestCreateProvisioningTemplateVersion =
  req
    "CreateProvisioningTemplateVersion"
    "fixture/CreateProvisioningTemplateVersion.yaml"

requestDescribeRoleAlias :: DescribeRoleAlias -> TestTree
requestDescribeRoleAlias =
  req
    "DescribeRoleAlias"
    "fixture/DescribeRoleAlias.yaml"

requestDeleteDynamicThingGroup :: DeleteDynamicThingGroup -> TestTree
requestDeleteDynamicThingGroup =
  req
    "DeleteDynamicThingGroup"
    "fixture/DeleteDynamicThingGroup.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup =
  req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

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

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasks -> TestTree
requestListAuditMitigationActionsTasks =
  req
    "ListAuditMitigationActionsTasks"
    "fixture/ListAuditMitigationActionsTasks.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestUpdateSecurityProfile :: UpdateSecurityProfile -> TestTree
requestUpdateSecurityProfile =
  req
    "UpdateSecurityProfile"
    "fixture/UpdateSecurityProfile.yaml"

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

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestDeleteSecurityProfile :: DeleteSecurityProfile -> TestTree
requestDeleteSecurityProfile =
  req
    "DeleteSecurityProfile"
    "fixture/DeleteSecurityProfile.yaml"

requestValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviors -> TestTree
requestValidateSecurityProfileBehaviors =
  req
    "ValidateSecurityProfileBehaviors"
    "fixture/ValidateSecurityProfileBehaviors.yaml"

requestCreateDomainConfiguration :: CreateDomainConfiguration -> TestTree
requestCreateDomainConfiguration =
  req
    "CreateDomainConfiguration"
    "fixture/CreateDomainConfiguration.yaml"

requestListActiveViolations :: ListActiveViolations -> TestTree
requestListActiveViolations =
  req
    "ListActiveViolations"
    "fixture/ListActiveViolations.yaml"

requestDescribeBillingGroup :: DescribeBillingGroup -> TestTree
requestDescribeBillingGroup =
  req
    "DescribeBillingGroup"
    "fixture/DescribeBillingGroup.yaml"

requestListThingRegistrationTasks :: ListThingRegistrationTasks -> TestTree
requestListThingRegistrationTasks =
  req
    "ListThingRegistrationTasks"
    "fixture/ListThingRegistrationTasks.yaml"

requestUpdateDimension :: UpdateDimension -> TestTree
requestUpdateDimension =
  req
    "UpdateDimension"
    "fixture/UpdateDimension.yaml"

requestDescribeAuditSuppression :: DescribeAuditSuppression -> TestTree
requestDescribeAuditSuppression =
  req
    "DescribeAuditSuppression"
    "fixture/DescribeAuditSuppression.yaml"

requestDescribeAccountAuditConfiguration :: DescribeAccountAuditConfiguration -> TestTree
requestDescribeAccountAuditConfiguration =
  req
    "DescribeAccountAuditConfiguration"
    "fixture/DescribeAccountAuditConfiguration.yaml"

requestDeprecateThingType :: DeprecateThingType -> TestTree
requestDeprecateThingType =
  req
    "DeprecateThingType"
    "fixture/DeprecateThingType.yaml"

requestDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTask -> TestTree
requestDescribeDetectMitigationActionsTask =
  req
    "DescribeDetectMitigationActionsTask"
    "fixture/DescribeDetectMitigationActionsTask.yaml"

requestDeleteDimension :: DeleteDimension -> TestTree
requestDeleteDimension =
  req
    "DeleteDimension"
    "fixture/DeleteDimension.yaml"

requestListAuditFindings :: ListAuditFindings -> TestTree
requestListAuditFindings =
  req
    "ListAuditFindings"
    "fixture/ListAuditFindings.yaml"

requestDescribeThing :: DescribeThing -> TestTree
requestDescribeThing =
  req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

requestListDimensions :: ListDimensions -> TestTree
requestListDimensions =
  req
    "ListDimensions"
    "fixture/ListDimensions.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal =
  req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestConfirmTopicRuleDestination :: ConfirmTopicRuleDestination -> TestTree
requestConfirmTopicRuleDestination =
  req
    "ConfirmTopicRuleDestination"
    "fixture/ConfirmTopicRuleDestination.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules =
  req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates =
  req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestStartOnDemandAuditTask :: StartOnDemandAuditTask -> TestTree
requestStartOnDemandAuditTask =
  req
    "StartOnDemandAuditTask"
    "fixture/StartOnDemandAuditTask.yaml"

requestUpdateEventConfigurations :: UpdateEventConfigurations -> TestTree
requestUpdateEventConfigurations =
  req
    "UpdateEventConfigurations"
    "fixture/UpdateEventConfigurations.yaml"

requestUpdateThingGroupsForThing :: UpdateThingGroupsForThing -> TestTree
requestUpdateThingGroupsForThing =
  req
    "UpdateThingGroupsForThing"
    "fixture/UpdateThingGroupsForThing.yaml"

requestListSecurityProfilesForTarget :: ListSecurityProfilesForTarget -> TestTree
requestListSecurityProfilesForTarget =
  req
    "ListSecurityProfilesForTarget"
    "fixture/ListSecurityProfilesForTarget.yaml"

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

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

requestGetJobDocument :: GetJobDocument -> TestTree
requestGetJobDocument =
  req
    "GetJobDocument"
    "fixture/GetJobDocument.yaml"

requestListAttachedPolicies :: ListAttachedPolicies -> TestTree
requestListAttachedPolicies =
  req
    "ListAttachedPolicies"
    "fixture/ListAttachedPolicies.yaml"

requestDescribeThingGroup :: DescribeThingGroup -> TestTree
requestDescribeThingGroup =
  req
    "DescribeThingGroup"
    "fixture/DescribeThingGroup.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestCreateFleetMetric :: CreateFleetMetric -> TestTree
requestCreateFleetMetric =
  req
    "CreateFleetMetric"
    "fixture/CreateFleetMetric.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate =
  req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule =
  req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestGetStatistics :: GetStatistics -> TestTree
requestGetStatistics =
  req
    "GetStatistics"
    "fixture/GetStatistics.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestUpdateRoleAlias :: UpdateRoleAlias -> TestTree
requestUpdateRoleAlias =
  req
    "UpdateRoleAlias"
    "fixture/UpdateRoleAlias.yaml"

requestClearDefaultAuthorizer :: ClearDefaultAuthorizer -> TestTree
requestClearDefaultAuthorizer =
  req
    "ClearDefaultAuthorizer"
    "fixture/ClearDefaultAuthorizer.yaml"

requestCreateTopicRule :: CreateTopicRule -> TestTree
requestCreateTopicRule =
  req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

requestCancelJobExecution :: CancelJobExecution -> TestTree
requestCancelJobExecution =
  req
    "CancelJobExecution"
    "fixture/CancelJobExecution.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestAssociateTargetsWithJob :: AssociateTargetsWithJob -> TestTree
requestAssociateTargetsWithJob =
  req
    "AssociateTargetsWithJob"
    "fixture/AssociateTargetsWithJob.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDeleteRoleAlias :: DeleteRoleAlias -> TestTree
requestDeleteRoleAlias =
  req
    "DeleteRoleAlias"
    "fixture/DeleteRoleAlias.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer =
  req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

requestListAuthorizers :: ListAuthorizers -> TestTree
requestListAuthorizers =
  req
    "ListAuthorizers"
    "fixture/ListAuthorizers.yaml"

requestCreateThingType :: CreateThingType -> TestTree
requestCreateThingType =
  req
    "CreateThingType"
    "fixture/CreateThingType.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestCreateAuditSuppression :: CreateAuditSuppression -> TestTree
requestCreateAuditSuppression =
  req
    "CreateAuditSuppression"
    "fixture/CreateAuditSuppression.yaml"

requestListJobExecutionsForJob :: ListJobExecutionsForJob -> TestTree
requestListJobExecutionsForJob =
  req
    "ListJobExecutionsForJob"
    "fixture/ListJobExecutionsForJob.yaml"

requestDescribeSecurityProfile :: DescribeSecurityProfile -> TestTree
requestDescribeSecurityProfile =
  req
    "DescribeSecurityProfile"
    "fixture/DescribeSecurityProfile.yaml"

requestRemoveThingFromBillingGroup :: RemoveThingFromBillingGroup -> TestTree
requestRemoveThingFromBillingGroup =
  req
    "RemoveThingFromBillingGroup"
    "fixture/RemoveThingFromBillingGroup.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestSearchIndex :: SearchIndex -> TestTree
requestSearchIndex =
  req
    "SearchIndex"
    "fixture/SearchIndex.yaml"

requestCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTask -> TestTree
requestCancelAuditMitigationActionsTask =
  req
    "CancelAuditMitigationActionsTask"
    "fixture/CancelAuditMitigationActionsTask.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateBillingGroup :: CreateBillingGroup -> TestTree
requestCreateBillingGroup =
  req
    "CreateBillingGroup"
    "fixture/CreateBillingGroup.yaml"

requestDescribeAuditFinding :: DescribeAuditFinding -> TestTree
requestDescribeAuditFinding =
  req
    "DescribeAuditFinding"
    "fixture/DescribeAuditFinding.yaml"

requestDeleteScheduledAudit :: DeleteScheduledAudit -> TestTree
requestDeleteScheduledAudit =
  req
    "DeleteScheduledAudit"
    "fixture/DeleteScheduledAudit.yaml"

requestGetEffectivePolicies :: GetEffectivePolicies -> TestTree
requestGetEffectivePolicies =
  req
    "GetEffectivePolicies"
    "fixture/GetEffectivePolicies.yaml"

requestGetOTAUpdate :: GetOTAUpdate -> TestTree
requestGetOTAUpdate =
  req
    "GetOTAUpdate"
    "fixture/GetOTAUpdate.yaml"

requestCreateProvisioningTemplate :: CreateProvisioningTemplate -> TestTree
requestCreateProvisioningTemplate =
  req
    "CreateProvisioningTemplate"
    "fixture/CreateProvisioningTemplate.yaml"

requestListThingTypes :: ListThingTypes -> TestTree
requestListThingTypes =
  req
    "ListThingTypes"
    "fixture/ListThingTypes.yaml"

requestDeleteThingType :: DeleteThingType -> TestTree
requestDeleteThingType =
  req
    "DeleteThingType"
    "fixture/DeleteThingType.yaml"

requestRegisterThing :: RegisterThing -> TestTree
requestRegisterThing =
  req
    "RegisterThing"
    "fixture/RegisterThing.yaml"

requestListBillingGroups :: ListBillingGroups -> TestTree
requestListBillingGroups =
  req
    "ListBillingGroups"
    "fixture/ListBillingGroups.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

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

requestDescribeAuditTask :: DescribeAuditTask -> TestTree
requestDescribeAuditTask =
  req
    "DescribeAuditTask"
    "fixture/DescribeAuditTask.yaml"

requestCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
requestCreateCertificateFromCsr =
  req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

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

requestUpdateScheduledAudit :: UpdateScheduledAudit -> TestTree
requestUpdateScheduledAudit =
  req
    "UpdateScheduledAudit"
    "fixture/UpdateScheduledAudit.yaml"

requestGetTopicRuleDestination :: GetTopicRuleDestination -> TestTree
requestGetTopicRuleDestination =
  req
    "GetTopicRuleDestination"
    "fixture/GetTopicRuleDestination.yaml"

requestDeleteTopicRuleDestination :: DeleteTopicRuleDestination -> TestTree
requestDeleteTopicRuleDestination =
  req
    "DeleteTopicRuleDestination"
    "fixture/DeleteTopicRuleDestination.yaml"

requestListOutgoingCertificates :: ListOutgoingCertificates -> TestTree
requestListOutgoingCertificates =
  req
    "ListOutgoingCertificates"
    "fixture/ListOutgoingCertificates.yaml"

requestDescribeJobTemplate :: DescribeJobTemplate -> TestTree
requestDescribeJobTemplate =
  req
    "DescribeJobTemplate"
    "fixture/DescribeJobTemplate.yaml"

requestAddThingToBillingGroup :: AddThingToBillingGroup -> TestTree
requestAddThingToBillingGroup =
  req
    "AddThingToBillingGroup"
    "fixture/AddThingToBillingGroup.yaml"

requestListOTAUpdates :: ListOTAUpdates -> TestTree
requestListOTAUpdates =
  req
    "ListOTAUpdates"
    "fixture/ListOTAUpdates.yaml"

requestUpdateThingGroup :: UpdateThingGroup -> TestTree
requestUpdateThingGroup =
  req
    "UpdateThingGroup"
    "fixture/UpdateThingGroup.yaml"

requestDeleteThingGroup :: DeleteThingGroup -> TestTree
requestDeleteThingGroup =
  req
    "DeleteThingGroup"
    "fixture/DeleteThingGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestListProvisioningTemplateVersions :: ListProvisioningTemplateVersions -> TestTree
requestListProvisioningTemplateVersions =
  req
    "ListProvisioningTemplateVersions"
    "fixture/ListProvisioningTemplateVersions.yaml"

requestUpdateTopicRuleDestination :: UpdateTopicRuleDestination -> TestTree
requestUpdateTopicRuleDestination =
  req
    "UpdateTopicRuleDestination"
    "fixture/UpdateTopicRuleDestination.yaml"

requestListThingGroups :: ListThingGroups -> TestTree
requestListThingGroups =
  req
    "ListThingGroups"
    "fixture/ListThingGroups.yaml"

requestCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTask -> TestTree
requestCancelDetectMitigationActionsTask =
  req
    "CancelDetectMitigationActionsTask"
    "fixture/CancelDetectMitigationActionsTask.yaml"

-- Responses

responseGetBucketsAggregation :: GetBucketsAggregationResponse -> TestTree
responseGetBucketsAggregation =
  res
    "GetBucketsAggregationResponse"
    "fixture/GetBucketsAggregationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketsAggregation)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIndexingConfiguration)

responsePutVerificationStateOnViolation :: PutVerificationStateOnViolationResponse -> TestTree
responsePutVerificationStateOnViolation =
  res
    "PutVerificationStateOnViolationResponse"
    "fixture/PutVerificationStateOnViolationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVerificationStateOnViolation)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingRegistrationTaskReports)

responseDescribeFleetMetric :: DescribeFleetMetricResponse -> TestTree
responseDescribeFleetMetric =
  res
    "DescribeFleetMetricResponse"
    "fixture/DescribeFleetMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetMetric)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningClaim)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCertificate)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMitigationActions)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningTemplate)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJobExecution)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfiles)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMitigationAction)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMitigationAction)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseStartDetectMitigationActionsTask :: StartDetectMitigationActionsTaskResponse -> TestTree
responseStartDetectMitigationActionsTask =
  res
    "StartDetectMitigationActionsTaskResponse"
    "fixture/StartDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartDetectMitigationActionsTask)

responseGetCardinality :: GetCardinalityResponse -> TestTree
responseGetCardinality =
  res
    "GetCardinalityResponse"
    "fixture/GetCardinalityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCardinality)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificate)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListViolationEvents)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDimension)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditTasks)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingType)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMitigationAction)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainConfiguration)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicRule)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainConfiguration)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy RejectCertificateTransfer)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoggingOptions)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityProfile)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainConfigurations)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledAudit)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetV2LoggingOptions)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingsInThingGroup)

responseListCustomMetrics :: ListCustomMetricsResponse -> TestTree
responseListCustomMetrics =
  res
    "ListCustomMetricsResponse"
    "fixture/ListCustomMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCustomMetrics)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AttachSecurityProfile)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJob)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJob)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate =
  res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCACertificate)

responseDeleteCustomMetric :: DeleteCustomMetricResponse -> TestTree
responseDeleteCustomMetric =
  res
    "DeleteCustomMetricResponse"
    "fixture/DeleteCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomMetric)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicyVersion)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoleAliases)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelAuditTask)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoleAlias)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobTemplates)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningTemplateVersion)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate =
  res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeysAndCertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate =
  res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCACertificate)

responseUpdateCustomMetric :: UpdateCustomMetricResponse -> TestTree
responseUpdateCustomMetric =
  res
    "UpdateCustomMetricResponse"
    "fixture/UpdateCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomMetric)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartAuditMitigationActionsTask)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy TransferCertificate)

responseGetPercentiles :: GetPercentilesResponse -> TestTree
responseGetPercentiles =
  res
    "GetPercentilesResponse"
    "fixture/GetPercentilesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPercentiles)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTopicRule)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForSecurityProfile)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoint)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingsInBillingGroup)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    defaultService
    (Proxy :: Proxy SetV2LoggingLevel)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJobTemplate)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseCreateCustomMetric :: CreateCustomMetricResponse -> TestTree
responseCreateCustomMetric =
  res
    "CreateCustomMetricResponse"
    "fixture/CreateCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomMetric)

responseListFleetMetrics :: ListFleetMetricsResponse -> TestTree
responseListFleetMetrics =
  res
    "ListFleetMetricsResponse"
    "fixture/ListFleetMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFleetMetrics)

responseUpdateFleetMetric :: UpdateFleetMetricResponse -> TestTree
responseUpdateFleetMetric =
  res
    "UpdateFleetMetricResponse"
    "fixture/UpdateFleetMetricResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleetMetric)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobExecutionsForThing)

responseDeleteFleetMetric :: DeleteFleetMetricResponse -> TestTree
responseDeleteFleetMetric =
  res
    "DeleteFleetMetricResponse"
    "fixture/DeleteFleetMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleetMetric)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DisableTopicRule)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditMitigationActionsTask)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateThing)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListV2LoggingLevels)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditMitigationActionsExecutions)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningTemplates)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartThingRegistrationTask)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificate)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisioningTemplate)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisioningTemplate)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetIndexingConfiguration)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask =
  res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StopThingRegistrationTask)

responseCreateScheduledAudit :: CreateScheduledAuditResponse -> TestTree
responseCreateScheduledAudit =
  res
    "CreateScheduledAuditResponse"
    "fixture/CreateScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScheduledAudit)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultAuthorizer)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteV2LoggingLevel)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMitigationAction)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDimension)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuthorizer)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy SetV2LoggingOptions)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStream)

responseListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasksResponse -> TestTree
responseListDetectMitigationActionsTasks =
  res
    "ListDetectMitigationActionsTasksResponse"
    "fixture/ListDetectMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListDetectMitigationActionsTasks)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuditSuppression)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStream)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThing)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingRegistrationTask)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountAuditConfiguration)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingGroupsForThing)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    defaultService
    (Proxy :: Proxy ListCertificatesByCA)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThings)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddThingToThingGroup)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBillingGroup)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    defaultService
    (Proxy :: Proxy ListScheduledAudits)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegistrationCode)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThing)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    defaultService
    (Proxy :: Proxy AttachThingPrincipal)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBillingGroup)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountAuditConfiguration)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggingOptions)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuditSuppression)

responseDescribeCustomMetric :: DescribeCustomMetricResponse -> TestTree
responseDescribeCustomMetric =
  res
    "DescribeCustomMetricResponse"
    "fixture/DescribeCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomMetric)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJob)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOTAUpdate)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegistrationCode)

responseListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutionsResponse -> TestTree
responseListDetectMitigationActionsExecutions =
  res
    "ListDetectMitigationActionsExecutionsResponse"
    "fixture/ListDetectMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDetectMitigationActionsExecutions)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCertificateWithoutCA)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDynamicThingGroup)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate =
  res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCACertificate)

responseDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersionResponse -> TestTree
responseDeleteProvisioningTemplateVersion =
  res
    "DeleteProvisioningTemplateVersionResponse"
    "fixture/DeleteProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisioningTemplateVersion)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DetachSecurityProfile)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingPrincipals)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup =
  res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveThingFromThingGroup)

responseGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummariesResponse -> TestTree
responseGetBehaviorModelTrainingSummaries =
  res
    "GetBehaviorModelTrainingSummariesResponse"
    "fixture/GetBehaviorModelTrainingSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBehaviorModelTrainingSummaries)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDynamicThingGroup)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTopicRuleDestination)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrincipalThings)

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningTemplateVersion)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRoleAlias)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDynamicThingGroup)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateThingGroup)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeAuthorizer)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOTAUpdate)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDefaultAuthorizer)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachPolicy)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditMitigationActionsTasks)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCertificate)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityProfile)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuthorizer)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicyVersion)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCertificates)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityProfile)

responseValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviorsResponse -> TestTree
responseValidateSecurityProfileBehaviors =
  res
    "ValidateSecurityProfileBehaviorsResponse"
    "fixture/ValidateSecurityProfileBehaviorsResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateSecurityProfileBehaviors)

responseCreateDomainConfiguration :: CreateDomainConfigurationResponse -> TestTree
responseCreateDomainConfiguration =
  res
    "CreateDomainConfigurationResponse"
    "fixture/CreateDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomainConfiguration)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActiveViolations)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBillingGroup)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingRegistrationTasks)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDimension)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditSuppression)

responseDescribeAccountAuditConfiguration :: DescribeAccountAuditConfigurationResponse -> TestTree
responseDescribeAccountAuditConfiguration =
  res
    "DescribeAccountAuditConfigurationResponse"
    "fixture/DescribeAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAuditConfiguration)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType =
  res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateThingType)

responseDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTaskResponse -> TestTree
responseDescribeDetectMitigationActionsTask =
  res
    "DescribeDetectMitigationActionsTaskResponse"
    "fixture/DescribeDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDetectMitigationActionsTask)

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDimension)

responseListAuditFindings :: ListAuditFindingsResponse -> TestTree
responseListAuditFindings =
  res
    "ListAuditFindingsResponse"
    "fixture/ListAuditFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditFindings)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing =
  res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThing)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDimensions)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    defaultService
    (Proxy :: Proxy DetachThingPrincipal)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStream)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmTopicRuleDestination)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicRules)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCACertificates)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartOnDemandAuditTask)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEventConfigurations)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingGroupsForThing)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfilesForTarget)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJobTemplate)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy EnableTopicRule)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptCertificateTransfer)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobDocument)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedPolicies)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingGroup)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseCreateFleetMetric :: CreateFleetMetricResponse -> TestTree
responseCreateFleetMetric =
  res
    "CreateFleetMetricResponse"
    "fixture/CreateFleetMetricResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleetMetric)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCACertificate)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceTopicRule)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetStatistics)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIndex)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachPolicy)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoleAlias)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer =
  res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy ClearDefaultAuthorizer)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule =
  res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTopicRule)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJobExecution)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTargetsWithJob)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicyVersion)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoleAlias)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyVersions)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForPolicy)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCertificateTransfer)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuthorizers)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType =
  res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateThingType)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuthorizer)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuditSuppression)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobExecutionsForJob)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityProfile)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveThingFromBillingGroup)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStream)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    defaultService
    (Proxy :: Proxy SearchIndex)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelAuditMitigationActionsTask)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBillingGroup)

responseDescribeAuditFinding :: DescribeAuditFindingResponse -> TestTree
responseDescribeAuditFinding =
  res
    "DescribeAuditFindingResponse"
    "fixture/DescribeAuditFindingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditFinding)

responseDeleteScheduledAudit :: DeleteScheduledAuditResponse -> TestTree
responseDeleteScheduledAudit =
  res
    "DeleteScheduledAuditResponse"
    "fixture/DeleteScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScheduledAudit)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEffectivePolicies)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy GetOTAUpdate)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningTemplate)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingTypes)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType =
  res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThingType)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing =
  res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterThing)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBillingGroups)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreams)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy TestAuthorization)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIndices)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditTask)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr =
  res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificateFromCsr)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditSuppressions)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomainConfiguration)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScheduledAudit)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicRuleDestination)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTopicRuleDestination)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOutgoingCertificates)

responseDescribeJobTemplate :: DescribeJobTemplateResponse -> TestTree
responseDescribeJobTemplate =
  res
    "DescribeJobTemplateResponse"
    "fixture/DescribeJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobTemplate)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddThingToBillingGroup)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOTAUpdates)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingGroup)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThingGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventConfigurations)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicRuleDestinations)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningTemplateVersions)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTopicRuleDestination)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingGroups)

responseCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTaskResponse -> TestTree
responseCancelDetectMitigationActionsTask =
  res
    "CancelDetectMitigationActionsTaskResponse"
    "fixture/CancelDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelDetectMitigationActionsTask)
