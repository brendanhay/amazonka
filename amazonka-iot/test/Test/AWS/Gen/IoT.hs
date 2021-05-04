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
--         [ requestListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReports
--
--         , requestCreateProvisioningClaim $
--             newCreateProvisioningClaim
--
--         , requestUpdateIndexingConfiguration $
--             newUpdateIndexingConfiguration
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestDeleteJobExecution $
--             newDeleteJobExecution
--
--         , requestListMitigationActions $
--             newListMitigationActions
--
--         , requestStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTask
--
--         , requestGetCardinality $
--             newGetCardinality
--
--         , requestListViolationEvents $
--             newListViolationEvents
--
--         , requestUpdateCertificate $
--             newUpdateCertificate
--
--         , requestDeleteMitigationAction $
--             newDeleteMitigationAction
--
--         , requestUpdateMitigationAction $
--             newUpdateMitigationAction
--
--         , requestDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplate
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreateDimension $
--             newCreateDimension
--
--         , requestUpdateDomainConfiguration $
--             newUpdateDomainConfiguration
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestListAuditTasks $
--             newListAuditTasks
--
--         , requestRejectCertificateTransfer $
--             newRejectCertificateTransfer
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestSetLoggingOptions $
--             newSetLoggingOptions
--
--         , requestCreateMitigationAction $
--             newCreateMitigationAction
--
--         , requestGetTopicRule $
--             newGetTopicRule
--
--         , requestDescribeThingType $
--             newDescribeThingType
--
--         , requestListThingsInThingGroup $
--             newListThingsInThingGroup
--
--         , requestDescribeScheduledAudit $
--             newDescribeScheduledAudit
--
--         , requestListDomainConfigurations $
--             newListDomainConfigurations
--
--         , requestDeleteDomainConfiguration $
--             newDeleteDomainConfiguration
--
--         , requestGetV2LoggingOptions $
--             newGetV2LoggingOptions
--
--         , requestCreateSecurityProfile $
--             newCreateSecurityProfile
--
--         , requestDeleteTopicRule $
--             newDeleteTopicRule
--
--         , requestDeleteCACertificate $
--             newDeleteCACertificate
--
--         , requestDeleteCustomMetric $
--             newDeleteCustomMetric
--
--         , requestUpdateCustomMetric $
--             newUpdateCustomMetric
--
--         , requestCancelAuditTask $
--             newCancelAuditTask
--
--         , requestListRoleAliases $
--             newListRoleAliases
--
--         , requestStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTask
--
--         , requestAttachSecurityProfile $
--             newAttachSecurityProfile
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestTransferCertificate $
--             newTransferCertificate
--
--         , requestCreateKeysAndCertificate $
--             newCreateKeysAndCertificate
--
--         , requestUpdateCACertificate $
--             newUpdateCACertificate
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestCreateRoleAlias $
--             newCreateRoleAlias
--
--         , requestListThingsInBillingGroup $
--             newListThingsInBillingGroup
--
--         , requestListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfile
--
--         , requestListCustomMetrics $
--             newListCustomMetrics
--
--         , requestDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersion
--
--         , requestGetPercentiles $
--             newGetPercentiles
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestCreateCustomMetric $
--             newCreateCustomMetric
--
--         , requestDisableTopicRule $
--             newDisableTopicRule
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTask
--
--         , requestSetV2LoggingLevel $
--             newSetV2LoggingLevel
--
--         , requestListJobExecutionsForThing $
--             newListJobExecutionsForThing
--
--         , requestCreateThing $
--             newCreateThing
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplate
--
--         , requestStartThingRegistrationTask $
--             newStartThingRegistrationTask
--
--         , requestSetDefaultAuthorizer $
--             newSetDefaultAuthorizer
--
--         , requestDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplate
--
--         , requestDescribeMitigationAction $
--             newDescribeMitigationAction
--
--         , requestDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevel
--
--         , requestDescribeJobExecution $
--             newDescribeJobExecution
--
--         , requestStopThingRegistrationTask $
--             newStopThingRegistrationTask
--
--         , requestCreateScheduledAudit $
--             newCreateScheduledAudit
--
--         , requestGetIndexingConfiguration $
--             newGetIndexingConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListV2LoggingLevels $
--             newListV2LoggingLevels
--
--         , requestListProvisioningTemplates $
--             newListProvisioningTemplates
--
--         , requestListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutions
--
--         , requestDeleteAuditSuppression $
--             newDeleteAuditSuppression
--
--         , requestListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasks
--
--         , requestUpdateStream $
--             newUpdateStream
--
--         , requestDeleteRegistrationCode $
--             newDeleteRegistrationCode
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestDescribeDimension $
--             newDescribeDimension
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfiguration
--
--         , requestListThings $
--             newListThings
--
--         , requestSetV2LoggingOptions $
--             newSetV2LoggingOptions
--
--         , requestUpdateThing $
--             newUpdateThing
--
--         , requestAddThingToThingGroup $
--             newAddThingToThingGroup
--
--         , requestGetLoggingOptions $
--             newGetLoggingOptions
--
--         , requestUpdateAuditSuppression $
--             newUpdateAuditSuppression
--
--         , requestListScheduledAudits $
--             newListScheduledAudits
--
--         , requestAttachThingPrincipal $
--             newAttachThingPrincipal
--
--         , requestDeleteThing $
--             newDeleteThing
--
--         , requestListCertificatesByCA $
--             newListCertificatesByCA
--
--         , requestListThingGroupsForThing $
--             newListThingGroupsForThing
--
--         , requestUpdateBillingGroup $
--             newUpdateBillingGroup
--
--         , requestDeleteBillingGroup $
--             newDeleteBillingGroup
--
--         , requestUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfiguration
--
--         , requestDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTask
--
--         , requestDescribeCustomMetric $
--             newDescribeCustomMetric
--
--         , requestDescribeCACertificate $
--             newDescribeCACertificate
--
--         , requestDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersion
--
--         , requestDeleteOTAUpdate $
--             newDeleteOTAUpdate
--
--         , requestRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCA
--
--         , requestListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutions
--
--         , requestCreateDynamicThingGroup $
--             newCreateDynamicThingGroup
--
--         , requestGetRegistrationCode $
--             newGetRegistrationCode
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDetachSecurityProfile $
--             newDetachSecurityProfile
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroup
--
--         , requestGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummaries
--
--         , requestCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersion
--
--         , requestListPrincipalThings $
--             newListPrincipalThings
--
--         , requestListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasks
--
--         , requestDescribeRoleAlias $
--             newDescribeRoleAlias
--
--         , requestCreateTopicRuleDestination $
--             newCreateTopicRuleDestination
--
--         , requestCreateOTAUpdate $
--             newCreateOTAUpdate
--
--         , requestDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroup
--
--         , requestUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroup
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestListThingPrincipals $
--             newListThingPrincipals
--
--         , requestDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizer
--
--         , requestCreateThingGroup $
--             newCreateThingGroup
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
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
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestUpdateSecurityProfile $
--             newUpdateSecurityProfile
--
--         , requestListActiveViolations $
--             newListActiveViolations
--
--         , requestDescribeAuthorizer $
--             newDescribeAuthorizer
--
--         , requestDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfiguration
--
--         , requestDeprecateThingType $
--             newDeprecateThingType
--
--         , requestDeleteDimension $
--             newDeleteDimension
--
--         , requestDescribeBillingGroup $
--             newDescribeBillingGroup
--
--         , requestUpdateDimension $
--             newUpdateDimension
--
--         , requestConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestination
--
--         , requestDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTask
--
--         , requestListThingRegistrationTasks $
--             newListThingRegistrationTasks
--
--         , requestListDimensions $
--             newListDimensions
--
--         , requestDescribeAuditSuppression $
--             newDescribeAuditSuppression
--
--         , requestListAuditFindings $
--             newListAuditFindings
--
--         , requestDescribeThing $
--             newDescribeThing
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestDetachThingPrincipal $
--             newDetachThingPrincipal
--
--         , requestStartOnDemandAuditTask $
--             newStartOnDemandAuditTask
--
--         , requestListAttachedPolicies $
--             newListAttachedPolicies
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListCACertificates $
--             newListCACertificates
--
--         , requestEnableTopicRule $
--             newEnableTopicRule
--
--         , requestListJobs $
--             newListJobs
--
--         , requestRegisterCACertificate $
--             newRegisterCACertificate
--
--         , requestListSecurityProfilesForTarget $
--             newListSecurityProfilesForTarget
--
--         , requestUpdateEventConfigurations $
--             newUpdateEventConfigurations
--
--         , requestGetJobDocument $
--             newGetJobDocument
--
--         , requestListTopicRules $
--             newListTopicRules
--
--         , requestDescribeThingGroup $
--             newDescribeThingGroup
--
--         , requestAcceptCertificateTransfer $
--             newAcceptCertificateTransfer
--
--         , requestUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThing
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestReplaceTopicRule $
--             newReplaceTopicRule
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestClearDefaultAuthorizer $
--             newClearDefaultAuthorizer
--
--         , requestCreateTopicRule $
--             newCreateTopicRule
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestUpdateRoleAlias $
--             newUpdateRoleAlias
--
--         , requestDeleteRoleAlias $
--             newDeleteRoleAlias
--
--         , requestGetStatistics $
--             newGetStatistics
--
--         , requestAssociateTargetsWithJob $
--             newAssociateTargetsWithJob
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestCancelJobExecution $
--             newCancelJobExecution
--
--         , requestCancelCertificateTransfer $
--             newCancelCertificateTransfer
--
--         , requestCreateThingType $
--             newCreateThingType
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestSearchIndex $
--             newSearchIndex
--
--         , requestDescribeSecurityProfile $
--             newDescribeSecurityProfile
--
--         , requestListJobExecutionsForJob $
--             newListJobExecutionsForJob
--
--         , requestCreateBillingGroup $
--             newCreateBillingGroup
--
--         , requestCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTask
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroup
--
--         , requestListAuthorizers $
--             newListAuthorizers
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestCreateAuditSuppression $
--             newCreateAuditSuppression
--
--         , requestCreateProvisioningTemplate $
--             newCreateProvisioningTemplate
--
--         , requestGetTopicRuleDestination $
--             newGetTopicRuleDestination
--
--         , requestDescribeAuditTask $
--             newDescribeAuditTask
--
--         , requestDescribeDomainConfiguration $
--             newDescribeDomainConfiguration
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListAuditSuppressions $
--             newListAuditSuppressions
--
--         , requestCreateCertificateFromCsr $
--             newCreateCertificateFromCsr
--
--         , requestGetOTAUpdate $
--             newGetOTAUpdate
--
--         , requestGetEffectivePolicies $
--             newGetEffectivePolicies
--
--         , requestUpdateScheduledAudit $
--             newUpdateScheduledAudit
--
--         , requestDescribeAuditFinding $
--             newDescribeAuditFinding
--
--         , requestDeleteScheduledAudit $
--             newDeleteScheduledAudit
--
--         , requestListBillingGroups $
--             newListBillingGroups
--
--         , requestTestAuthorization $
--             newTestAuthorization
--
--         , requestListThingTypes $
--             newListThingTypes
--
--         , requestListIndices $
--             newListIndices
--
--         , requestDeleteThingType $
--             newDeleteThingType
--
--         , requestRegisterThing $
--             newRegisterThing
--
--         , requestListOutgoingCertificates $
--             newListOutgoingCertificates
--
--         , requestDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestination
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTopicRuleDestinations $
--             newListTopicRuleDestinations
--
--         , requestCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTask
--
--         , requestAddThingToBillingGroup $
--             newAddThingToBillingGroup
--
--         , requestDeleteThingGroup $
--             newDeleteThingGroup
--
--         , requestDescribeEventConfigurations $
--             newDescribeEventConfigurations
--
--         , requestUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestination
--
--         , requestListOTAUpdates $
--             newListOTAUpdates
--
--         , requestListThingGroups $
--             newListThingGroups
--
--         , requestListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersions
--
--         , requestUpdateThingGroup $
--             newUpdateThingGroup
--
--           ]

--     , testGroup "response"
--         [ responseListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReportsResponse
--
--         , responseCreateProvisioningClaim $
--             newCreateProvisioningClaimResponse
--
--         , responseUpdateIndexingConfiguration $
--             newUpdateIndexingConfigurationResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseDeleteJobExecution $
--             newDeleteJobExecutionResponse
--
--         , responseListMitigationActions $
--             newListMitigationActionsResponse
--
--         , responseStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTaskResponse
--
--         , responseGetCardinality $
--             newGetCardinalityResponse
--
--         , responseListViolationEvents $
--             newListViolationEventsResponse
--
--         , responseUpdateCertificate $
--             newUpdateCertificateResponse
--
--         , responseDeleteMitigationAction $
--             newDeleteMitigationActionResponse
--
--         , responseUpdateMitigationAction $
--             newUpdateMitigationActionResponse
--
--         , responseDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplateResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreateDimension $
--             newCreateDimensionResponse
--
--         , responseUpdateDomainConfiguration $
--             newUpdateDomainConfigurationResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseListAuditTasks $
--             newListAuditTasksResponse
--
--         , responseRejectCertificateTransfer $
--             newRejectCertificateTransferResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseSetLoggingOptions $
--             newSetLoggingOptionsResponse
--
--         , responseCreateMitigationAction $
--             newCreateMitigationActionResponse
--
--         , responseGetTopicRule $
--             newGetTopicRuleResponse
--
--         , responseDescribeThingType $
--             newDescribeThingTypeResponse
--
--         , responseListThingsInThingGroup $
--             newListThingsInThingGroupResponse
--
--         , responseDescribeScheduledAudit $
--             newDescribeScheduledAuditResponse
--
--         , responseListDomainConfigurations $
--             newListDomainConfigurationsResponse
--
--         , responseDeleteDomainConfiguration $
--             newDeleteDomainConfigurationResponse
--
--         , responseGetV2LoggingOptions $
--             newGetV2LoggingOptionsResponse
--
--         , responseCreateSecurityProfile $
--             newCreateSecurityProfileResponse
--
--         , responseDeleteTopicRule $
--             newDeleteTopicRuleResponse
--
--         , responseDeleteCACertificate $
--             newDeleteCACertificateResponse
--
--         , responseDeleteCustomMetric $
--             newDeleteCustomMetricResponse
--
--         , responseUpdateCustomMetric $
--             newUpdateCustomMetricResponse
--
--         , responseCancelAuditTask $
--             newCancelAuditTaskResponse
--
--         , responseListRoleAliases $
--             newListRoleAliasesResponse
--
--         , responseStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTaskResponse
--
--         , responseAttachSecurityProfile $
--             newAttachSecurityProfileResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseTransferCertificate $
--             newTransferCertificateResponse
--
--         , responseCreateKeysAndCertificate $
--             newCreateKeysAndCertificateResponse
--
--         , responseUpdateCACertificate $
--             newUpdateCACertificateResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseCreateRoleAlias $
--             newCreateRoleAliasResponse
--
--         , responseListThingsInBillingGroup $
--             newListThingsInBillingGroupResponse
--
--         , responseListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfileResponse
--
--         , responseListCustomMetrics $
--             newListCustomMetricsResponse
--
--         , responseDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersionResponse
--
--         , responseGetPercentiles $
--             newGetPercentilesResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseCreateCustomMetric $
--             newCreateCustomMetricResponse
--
--         , responseDisableTopicRule $
--             newDisableTopicRuleResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTaskResponse
--
--         , responseSetV2LoggingLevel $
--             newSetV2LoggingLevelResponse
--
--         , responseListJobExecutionsForThing $
--             newListJobExecutionsForThingResponse
--
--         , responseCreateThing $
--             newCreateThingResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplateResponse
--
--         , responseStartThingRegistrationTask $
--             newStartThingRegistrationTaskResponse
--
--         , responseSetDefaultAuthorizer $
--             newSetDefaultAuthorizerResponse
--
--         , responseDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplateResponse
--
--         , responseDescribeMitigationAction $
--             newDescribeMitigationActionResponse
--
--         , responseDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevelResponse
--
--         , responseDescribeJobExecution $
--             newDescribeJobExecutionResponse
--
--         , responseStopThingRegistrationTask $
--             newStopThingRegistrationTaskResponse
--
--         , responseCreateScheduledAudit $
--             newCreateScheduledAuditResponse
--
--         , responseGetIndexingConfiguration $
--             newGetIndexingConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListV2LoggingLevels $
--             newListV2LoggingLevelsResponse
--
--         , responseListProvisioningTemplates $
--             newListProvisioningTemplatesResponse
--
--         , responseListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutionsResponse
--
--         , responseDeleteAuditSuppression $
--             newDeleteAuditSuppressionResponse
--
--         , responseListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasksResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--         , responseDeleteRegistrationCode $
--             newDeleteRegistrationCodeResponse
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseDescribeDimension $
--             newDescribeDimensionResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfigurationResponse
--
--         , responseListThings $
--             newListThingsResponse
--
--         , responseSetV2LoggingOptions $
--             newSetV2LoggingOptionsResponse
--
--         , responseUpdateThing $
--             newUpdateThingResponse
--
--         , responseAddThingToThingGroup $
--             newAddThingToThingGroupResponse
--
--         , responseGetLoggingOptions $
--             newGetLoggingOptionsResponse
--
--         , responseUpdateAuditSuppression $
--             newUpdateAuditSuppressionResponse
--
--         , responseListScheduledAudits $
--             newListScheduledAuditsResponse
--
--         , responseAttachThingPrincipal $
--             newAttachThingPrincipalResponse
--
--         , responseDeleteThing $
--             newDeleteThingResponse
--
--         , responseListCertificatesByCA $
--             newListCertificatesByCAResponse
--
--         , responseListThingGroupsForThing $
--             newListThingGroupsForThingResponse
--
--         , responseUpdateBillingGroup $
--             newUpdateBillingGroupResponse
--
--         , responseDeleteBillingGroup $
--             newDeleteBillingGroupResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfigurationResponse
--
--         , responseDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTaskResponse
--
--         , responseDescribeCustomMetric $
--             newDescribeCustomMetricResponse
--
--         , responseDescribeCACertificate $
--             newDescribeCACertificateResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersionResponse
--
--         , responseDeleteOTAUpdate $
--             newDeleteOTAUpdateResponse
--
--         , responseRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCAResponse
--
--         , responseListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutionsResponse
--
--         , responseCreateDynamicThingGroup $
--             newCreateDynamicThingGroupResponse
--
--         , responseGetRegistrationCode $
--             newGetRegistrationCodeResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDetachSecurityProfile $
--             newDetachSecurityProfileResponse
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responseRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroupResponse
--
--         , responseGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummariesResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersionResponse
--
--         , responseListPrincipalThings $
--             newListPrincipalThingsResponse
--
--         , responseListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasksResponse
--
--         , responseDescribeRoleAlias $
--             newDescribeRoleAliasResponse
--
--         , responseCreateTopicRuleDestination $
--             newCreateTopicRuleDestinationResponse
--
--         , responseCreateOTAUpdate $
--             newCreateOTAUpdateResponse
--
--         , responseDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroupResponse
--
--         , responseUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroupResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseListThingPrincipals $
--             newListThingPrincipalsResponse
--
--         , responseDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizerResponse
--
--         , responseCreateThingGroup $
--             newCreateThingGroupResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
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
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseUpdateSecurityProfile $
--             newUpdateSecurityProfileResponse
--
--         , responseListActiveViolations $
--             newListActiveViolationsResponse
--
--         , responseDescribeAuthorizer $
--             newDescribeAuthorizerResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfigurationResponse
--
--         , responseDeprecateThingType $
--             newDeprecateThingTypeResponse
--
--         , responseDeleteDimension $
--             newDeleteDimensionResponse
--
--         , responseDescribeBillingGroup $
--             newDescribeBillingGroupResponse
--
--         , responseUpdateDimension $
--             newUpdateDimensionResponse
--
--         , responseConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestinationResponse
--
--         , responseDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTaskResponse
--
--         , responseListThingRegistrationTasks $
--             newListThingRegistrationTasksResponse
--
--         , responseListDimensions $
--             newListDimensionsResponse
--
--         , responseDescribeAuditSuppression $
--             newDescribeAuditSuppressionResponse
--
--         , responseListAuditFindings $
--             newListAuditFindingsResponse
--
--         , responseDescribeThing $
--             newDescribeThingResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseDetachThingPrincipal $
--             newDetachThingPrincipalResponse
--
--         , responseStartOnDemandAuditTask $
--             newStartOnDemandAuditTaskResponse
--
--         , responseListAttachedPolicies $
--             newListAttachedPoliciesResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListCACertificates $
--             newListCACertificatesResponse
--
--         , responseEnableTopicRule $
--             newEnableTopicRuleResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseRegisterCACertificate $
--             newRegisterCACertificateResponse
--
--         , responseListSecurityProfilesForTarget $
--             newListSecurityProfilesForTargetResponse
--
--         , responseUpdateEventConfigurations $
--             newUpdateEventConfigurationsResponse
--
--         , responseGetJobDocument $
--             newGetJobDocumentResponse
--
--         , responseListTopicRules $
--             newListTopicRulesResponse
--
--         , responseDescribeThingGroup $
--             newDescribeThingGroupResponse
--
--         , responseAcceptCertificateTransfer $
--             newAcceptCertificateTransferResponse
--
--         , responseUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThingResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseReplaceTopicRule $
--             newReplaceTopicRuleResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseClearDefaultAuthorizer $
--             newClearDefaultAuthorizerResponse
--
--         , responseCreateTopicRule $
--             newCreateTopicRuleResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseUpdateRoleAlias $
--             newUpdateRoleAliasResponse
--
--         , responseDeleteRoleAlias $
--             newDeleteRoleAliasResponse
--
--         , responseGetStatistics $
--             newGetStatisticsResponse
--
--         , responseAssociateTargetsWithJob $
--             newAssociateTargetsWithJobResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseCancelJobExecution $
--             newCancelJobExecutionResponse
--
--         , responseCancelCertificateTransfer $
--             newCancelCertificateTransferResponse
--
--         , responseCreateThingType $
--             newCreateThingTypeResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseSearchIndex $
--             newSearchIndexResponse
--
--         , responseDescribeSecurityProfile $
--             newDescribeSecurityProfileResponse
--
--         , responseListJobExecutionsForJob $
--             newListJobExecutionsForJobResponse
--
--         , responseCreateBillingGroup $
--             newCreateBillingGroupResponse
--
--         , responseCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTaskResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroupResponse
--
--         , responseListAuthorizers $
--             newListAuthorizersResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseCreateAuditSuppression $
--             newCreateAuditSuppressionResponse
--
--         , responseCreateProvisioningTemplate $
--             newCreateProvisioningTemplateResponse
--
--         , responseGetTopicRuleDestination $
--             newGetTopicRuleDestinationResponse
--
--         , responseDescribeAuditTask $
--             newDescribeAuditTaskResponse
--
--         , responseDescribeDomainConfiguration $
--             newDescribeDomainConfigurationResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListAuditSuppressions $
--             newListAuditSuppressionsResponse
--
--         , responseCreateCertificateFromCsr $
--             newCreateCertificateFromCsrResponse
--
--         , responseGetOTAUpdate $
--             newGetOTAUpdateResponse
--
--         , responseGetEffectivePolicies $
--             newGetEffectivePoliciesResponse
--
--         , responseUpdateScheduledAudit $
--             newUpdateScheduledAuditResponse
--
--         , responseDescribeAuditFinding $
--             newDescribeAuditFindingResponse
--
--         , responseDeleteScheduledAudit $
--             newDeleteScheduledAuditResponse
--
--         , responseListBillingGroups $
--             newListBillingGroupsResponse
--
--         , responseTestAuthorization $
--             newTestAuthorizationResponse
--
--         , responseListThingTypes $
--             newListThingTypesResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseDeleteThingType $
--             newDeleteThingTypeResponse
--
--         , responseRegisterThing $
--             newRegisterThingResponse
--
--         , responseListOutgoingCertificates $
--             newListOutgoingCertificatesResponse
--
--         , responseDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestinationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTopicRuleDestinations $
--             newListTopicRuleDestinationsResponse
--
--         , responseCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTaskResponse
--
--         , responseAddThingToBillingGroup $
--             newAddThingToBillingGroupResponse
--
--         , responseDeleteThingGroup $
--             newDeleteThingGroupResponse
--
--         , responseDescribeEventConfigurations $
--             newDescribeEventConfigurationsResponse
--
--         , responseUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestinationResponse
--
--         , responseListOTAUpdates $
--             newListOTAUpdatesResponse
--
--         , responseListThingGroups $
--             newListThingGroupsResponse
--
--         , responseListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersionsResponse
--
--         , responseUpdateThingGroup $
--             newUpdateThingGroupResponse
--
--           ]
--     ]

-- Requests

requestListThingRegistrationTaskReports :: ListThingRegistrationTaskReports -> TestTree
requestListThingRegistrationTaskReports =
  req
    "ListThingRegistrationTaskReports"
    "fixture/ListThingRegistrationTaskReports.yaml"

requestCreateProvisioningClaim :: CreateProvisioningClaim -> TestTree
requestCreateProvisioningClaim =
  req
    "CreateProvisioningClaim"
    "fixture/CreateProvisioningClaim.yaml"

requestUpdateIndexingConfiguration :: UpdateIndexingConfiguration -> TestTree
requestUpdateIndexingConfiguration =
  req
    "UpdateIndexingConfiguration"
    "fixture/UpdateIndexingConfiguration.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestDeleteJobExecution :: DeleteJobExecution -> TestTree
requestDeleteJobExecution =
  req
    "DeleteJobExecution"
    "fixture/DeleteJobExecution.yaml"

requestListMitigationActions :: ListMitigationActions -> TestTree
requestListMitigationActions =
  req
    "ListMitigationActions"
    "fixture/ListMitigationActions.yaml"

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

requestListViolationEvents :: ListViolationEvents -> TestTree
requestListViolationEvents =
  req
    "ListViolationEvents"
    "fixture/ListViolationEvents.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate =
  req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestDeleteMitigationAction :: DeleteMitigationAction -> TestTree
requestDeleteMitigationAction =
  req
    "DeleteMitigationAction"
    "fixture/DeleteMitigationAction.yaml"

requestUpdateMitigationAction :: UpdateMitigationAction -> TestTree
requestUpdateMitigationAction =
  req
    "UpdateMitigationAction"
    "fixture/UpdateMitigationAction.yaml"

requestDescribeProvisioningTemplate :: DescribeProvisioningTemplate -> TestTree
requestDescribeProvisioningTemplate =
  req
    "DescribeProvisioningTemplate"
    "fixture/DescribeProvisioningTemplate.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreateDimension :: CreateDimension -> TestTree
requestCreateDimension =
  req
    "CreateDimension"
    "fixture/CreateDimension.yaml"

requestUpdateDomainConfiguration :: UpdateDomainConfiguration -> TestTree
requestUpdateDomainConfiguration =
  req
    "UpdateDomainConfiguration"
    "fixture/UpdateDomainConfiguration.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestListAuditTasks :: ListAuditTasks -> TestTree
requestListAuditTasks =
  req
    "ListAuditTasks"
    "fixture/ListAuditTasks.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer =
  req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions =
  req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestCreateMitigationAction :: CreateMitigationAction -> TestTree
requestCreateMitigationAction =
  req
    "CreateMitigationAction"
    "fixture/CreateMitigationAction.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule =
  req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestDescribeThingType :: DescribeThingType -> TestTree
requestDescribeThingType =
  req
    "DescribeThingType"
    "fixture/DescribeThingType.yaml"

requestListThingsInThingGroup :: ListThingsInThingGroup -> TestTree
requestListThingsInThingGroup =
  req
    "ListThingsInThingGroup"
    "fixture/ListThingsInThingGroup.yaml"

requestDescribeScheduledAudit :: DescribeScheduledAudit -> TestTree
requestDescribeScheduledAudit =
  req
    "DescribeScheduledAudit"
    "fixture/DescribeScheduledAudit.yaml"

requestListDomainConfigurations :: ListDomainConfigurations -> TestTree
requestListDomainConfigurations =
  req
    "ListDomainConfigurations"
    "fixture/ListDomainConfigurations.yaml"

requestDeleteDomainConfiguration :: DeleteDomainConfiguration -> TestTree
requestDeleteDomainConfiguration =
  req
    "DeleteDomainConfiguration"
    "fixture/DeleteDomainConfiguration.yaml"

requestGetV2LoggingOptions :: GetV2LoggingOptions -> TestTree
requestGetV2LoggingOptions =
  req
    "GetV2LoggingOptions"
    "fixture/GetV2LoggingOptions.yaml"

requestCreateSecurityProfile :: CreateSecurityProfile -> TestTree
requestCreateSecurityProfile =
  req
    "CreateSecurityProfile"
    "fixture/CreateSecurityProfile.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule =
  req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

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

requestUpdateCustomMetric :: UpdateCustomMetric -> TestTree
requestUpdateCustomMetric =
  req
    "UpdateCustomMetric"
    "fixture/UpdateCustomMetric.yaml"

requestCancelAuditTask :: CancelAuditTask -> TestTree
requestCancelAuditTask =
  req
    "CancelAuditTask"
    "fixture/CancelAuditTask.yaml"

requestListRoleAliases :: ListRoleAliases -> TestTree
requestListRoleAliases =
  req
    "ListRoleAliases"
    "fixture/ListRoleAliases.yaml"

requestStartAuditMitigationActionsTask :: StartAuditMitigationActionsTask -> TestTree
requestStartAuditMitigationActionsTask =
  req
    "StartAuditMitigationActionsTask"
    "fixture/StartAuditMitigationActionsTask.yaml"

requestAttachSecurityProfile :: AttachSecurityProfile -> TestTree
requestAttachSecurityProfile =
  req
    "AttachSecurityProfile"
    "fixture/AttachSecurityProfile.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate =
  req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

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

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestCreateRoleAlias :: CreateRoleAlias -> TestTree
requestCreateRoleAlias =
  req
    "CreateRoleAlias"
    "fixture/CreateRoleAlias.yaml"

requestListThingsInBillingGroup :: ListThingsInBillingGroup -> TestTree
requestListThingsInBillingGroup =
  req
    "ListThingsInBillingGroup"
    "fixture/ListThingsInBillingGroup.yaml"

requestListTargetsForSecurityProfile :: ListTargetsForSecurityProfile -> TestTree
requestListTargetsForSecurityProfile =
  req
    "ListTargetsForSecurityProfile"
    "fixture/ListTargetsForSecurityProfile.yaml"

requestListCustomMetrics :: ListCustomMetrics -> TestTree
requestListCustomMetrics =
  req
    "ListCustomMetrics"
    "fixture/ListCustomMetrics.yaml"

requestDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersion -> TestTree
requestDescribeProvisioningTemplateVersion =
  req
    "DescribeProvisioningTemplateVersion"
    "fixture/DescribeProvisioningTemplateVersion.yaml"

requestGetPercentiles :: GetPercentiles -> TestTree
requestGetPercentiles =
  req
    "GetPercentiles"
    "fixture/GetPercentiles.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

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

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule =
  req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTask -> TestTree
requestDescribeAuditMitigationActionsTask =
  req
    "DescribeAuditMitigationActionsTask"
    "fixture/DescribeAuditMitigationActionsTask.yaml"

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

requestCreateThing :: CreateThing -> TestTree
requestCreateThing =
  req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestUpdateProvisioningTemplate :: UpdateProvisioningTemplate -> TestTree
requestUpdateProvisioningTemplate =
  req
    "UpdateProvisioningTemplate"
    "fixture/UpdateProvisioningTemplate.yaml"

requestStartThingRegistrationTask :: StartThingRegistrationTask -> TestTree
requestStartThingRegistrationTask =
  req
    "StartThingRegistrationTask"
    "fixture/StartThingRegistrationTask.yaml"

requestSetDefaultAuthorizer :: SetDefaultAuthorizer -> TestTree
requestSetDefaultAuthorizer =
  req
    "SetDefaultAuthorizer"
    "fixture/SetDefaultAuthorizer.yaml"

requestDeleteProvisioningTemplate :: DeleteProvisioningTemplate -> TestTree
requestDeleteProvisioningTemplate =
  req
    "DeleteProvisioningTemplate"
    "fixture/DeleteProvisioningTemplate.yaml"

requestDescribeMitigationAction :: DescribeMitigationAction -> TestTree
requestDescribeMitigationAction =
  req
    "DescribeMitigationAction"
    "fixture/DescribeMitigationAction.yaml"

requestDeleteV2LoggingLevel :: DeleteV2LoggingLevel -> TestTree
requestDeleteV2LoggingLevel =
  req
    "DeleteV2LoggingLevel"
    "fixture/DeleteV2LoggingLevel.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution =
  req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

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

requestGetIndexingConfiguration :: GetIndexingConfiguration -> TestTree
requestGetIndexingConfiguration =
  req
    "GetIndexingConfiguration"
    "fixture/GetIndexingConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListV2LoggingLevels :: ListV2LoggingLevels -> TestTree
requestListV2LoggingLevels =
  req
    "ListV2LoggingLevels"
    "fixture/ListV2LoggingLevels.yaml"

requestListProvisioningTemplates :: ListProvisioningTemplates -> TestTree
requestListProvisioningTemplates =
  req
    "ListProvisioningTemplates"
    "fixture/ListProvisioningTemplates.yaml"

requestListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutions -> TestTree
requestListAuditMitigationActionsExecutions =
  req
    "ListAuditMitigationActionsExecutions"
    "fixture/ListAuditMitigationActionsExecutions.yaml"

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

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream =
  req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode =
  req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestDescribeDimension :: DescribeDimension -> TestTree
requestDescribeDimension =
  req
    "DescribeDimension"
    "fixture/DescribeDimension.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestDeleteAccountAuditConfiguration :: DeleteAccountAuditConfiguration -> TestTree
requestDeleteAccountAuditConfiguration =
  req
    "DeleteAccountAuditConfiguration"
    "fixture/DeleteAccountAuditConfiguration.yaml"

requestListThings :: ListThings -> TestTree
requestListThings =
  req
    "ListThings"
    "fixture/ListThings.yaml"

requestSetV2LoggingOptions :: SetV2LoggingOptions -> TestTree
requestSetV2LoggingOptions =
  req
    "SetV2LoggingOptions"
    "fixture/SetV2LoggingOptions.yaml"

requestUpdateThing :: UpdateThing -> TestTree
requestUpdateThing =
  req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

requestAddThingToThingGroup :: AddThingToThingGroup -> TestTree
requestAddThingToThingGroup =
  req
    "AddThingToThingGroup"
    "fixture/AddThingToThingGroup.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions =
  req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestUpdateAuditSuppression :: UpdateAuditSuppression -> TestTree
requestUpdateAuditSuppression =
  req
    "UpdateAuditSuppression"
    "fixture/UpdateAuditSuppression.yaml"

requestListScheduledAudits :: ListScheduledAudits -> TestTree
requestListScheduledAudits =
  req
    "ListScheduledAudits"
    "fixture/ListScheduledAudits.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal =
  req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing =
  req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA =
  req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestListThingGroupsForThing :: ListThingGroupsForThing -> TestTree
requestListThingGroupsForThing =
  req
    "ListThingGroupsForThing"
    "fixture/ListThingGroupsForThing.yaml"

requestUpdateBillingGroup :: UpdateBillingGroup -> TestTree
requestUpdateBillingGroup =
  req
    "UpdateBillingGroup"
    "fixture/UpdateBillingGroup.yaml"

requestDeleteBillingGroup :: DeleteBillingGroup -> TestTree
requestDeleteBillingGroup =
  req
    "DeleteBillingGroup"
    "fixture/DeleteBillingGroup.yaml"

requestUpdateAccountAuditConfiguration :: UpdateAccountAuditConfiguration -> TestTree
requestUpdateAccountAuditConfiguration =
  req
    "UpdateAccountAuditConfiguration"
    "fixture/UpdateAccountAuditConfiguration.yaml"

requestDescribeThingRegistrationTask :: DescribeThingRegistrationTask -> TestTree
requestDescribeThingRegistrationTask =
  req
    "DescribeThingRegistrationTask"
    "fixture/DescribeThingRegistrationTask.yaml"

requestDescribeCustomMetric :: DescribeCustomMetric -> TestTree
requestDescribeCustomMetric =
  req
    "DescribeCustomMetric"
    "fixture/DescribeCustomMetric.yaml"

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

requestDeleteOTAUpdate :: DeleteOTAUpdate -> TestTree
requestDeleteOTAUpdate =
  req
    "DeleteOTAUpdate"
    "fixture/DeleteOTAUpdate.yaml"

requestRegisterCertificateWithoutCA :: RegisterCertificateWithoutCA -> TestTree
requestRegisterCertificateWithoutCA =
  req
    "RegisterCertificateWithoutCA"
    "fixture/RegisterCertificateWithoutCA.yaml"

requestListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutions -> TestTree
requestListDetectMitigationActionsExecutions =
  req
    "ListDetectMitigationActionsExecutions"
    "fixture/ListDetectMitigationActionsExecutions.yaml"

requestCreateDynamicThingGroup :: CreateDynamicThingGroup -> TestTree
requestCreateDynamicThingGroup =
  req
    "CreateDynamicThingGroup"
    "fixture/CreateDynamicThingGroup.yaml"

requestGetRegistrationCode :: GetRegistrationCode -> TestTree
requestGetRegistrationCode =
  req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDetachSecurityProfile :: DetachSecurityProfile -> TestTree
requestDetachSecurityProfile =
  req
    "DetachSecurityProfile"
    "fixture/DetachSecurityProfile.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

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

requestCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersion -> TestTree
requestCreateProvisioningTemplateVersion =
  req
    "CreateProvisioningTemplateVersion"
    "fixture/CreateProvisioningTemplateVersion.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings =
  req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasks -> TestTree
requestListAuditMitigationActionsTasks =
  req
    "ListAuditMitigationActionsTasks"
    "fixture/ListAuditMitigationActionsTasks.yaml"

requestDescribeRoleAlias :: DescribeRoleAlias -> TestTree
requestDescribeRoleAlias =
  req
    "DescribeRoleAlias"
    "fixture/DescribeRoleAlias.yaml"

requestCreateTopicRuleDestination :: CreateTopicRuleDestination -> TestTree
requestCreateTopicRuleDestination =
  req
    "CreateTopicRuleDestination"
    "fixture/CreateTopicRuleDestination.yaml"

requestCreateOTAUpdate :: CreateOTAUpdate -> TestTree
requestCreateOTAUpdate =
  req
    "CreateOTAUpdate"
    "fixture/CreateOTAUpdate.yaml"

requestDeleteDynamicThingGroup :: DeleteDynamicThingGroup -> TestTree
requestDeleteDynamicThingGroup =
  req
    "DeleteDynamicThingGroup"
    "fixture/DeleteDynamicThingGroup.yaml"

requestUpdateDynamicThingGroup :: UpdateDynamicThingGroup -> TestTree
requestUpdateDynamicThingGroup =
  req
    "UpdateDynamicThingGroup"
    "fixture/UpdateDynamicThingGroup.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals =
  req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestDescribeDefaultAuthorizer :: DescribeDefaultAuthorizer -> TestTree
requestDescribeDefaultAuthorizer =
  req
    "DescribeDefaultAuthorizer"
    "fixture/DescribeDefaultAuthorizer.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup =
  req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

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

requestUpdateSecurityProfile :: UpdateSecurityProfile -> TestTree
requestUpdateSecurityProfile =
  req
    "UpdateSecurityProfile"
    "fixture/UpdateSecurityProfile.yaml"

requestListActiveViolations :: ListActiveViolations -> TestTree
requestListActiveViolations =
  req
    "ListActiveViolations"
    "fixture/ListActiveViolations.yaml"

requestDescribeAuthorizer :: DescribeAuthorizer -> TestTree
requestDescribeAuthorizer =
  req
    "DescribeAuthorizer"
    "fixture/DescribeAuthorizer.yaml"

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

requestDeleteDimension :: DeleteDimension -> TestTree
requestDeleteDimension =
  req
    "DeleteDimension"
    "fixture/DeleteDimension.yaml"

requestDescribeBillingGroup :: DescribeBillingGroup -> TestTree
requestDescribeBillingGroup =
  req
    "DescribeBillingGroup"
    "fixture/DescribeBillingGroup.yaml"

requestUpdateDimension :: UpdateDimension -> TestTree
requestUpdateDimension =
  req
    "UpdateDimension"
    "fixture/UpdateDimension.yaml"

requestConfirmTopicRuleDestination :: ConfirmTopicRuleDestination -> TestTree
requestConfirmTopicRuleDestination =
  req
    "ConfirmTopicRuleDestination"
    "fixture/ConfirmTopicRuleDestination.yaml"

requestDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTask -> TestTree
requestDescribeDetectMitigationActionsTask =
  req
    "DescribeDetectMitigationActionsTask"
    "fixture/DescribeDetectMitigationActionsTask.yaml"

requestListThingRegistrationTasks :: ListThingRegistrationTasks -> TestTree
requestListThingRegistrationTasks =
  req
    "ListThingRegistrationTasks"
    "fixture/ListThingRegistrationTasks.yaml"

requestListDimensions :: ListDimensions -> TestTree
requestListDimensions =
  req
    "ListDimensions"
    "fixture/ListDimensions.yaml"

requestDescribeAuditSuppression :: DescribeAuditSuppression -> TestTree
requestDescribeAuditSuppression =
  req
    "DescribeAuditSuppression"
    "fixture/DescribeAuditSuppression.yaml"

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

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal =
  req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

requestStartOnDemandAuditTask :: StartOnDemandAuditTask -> TestTree
requestStartOnDemandAuditTask =
  req
    "StartOnDemandAuditTask"
    "fixture/StartOnDemandAuditTask.yaml"

requestListAttachedPolicies :: ListAttachedPolicies -> TestTree
requestListAttachedPolicies =
  req
    "ListAttachedPolicies"
    "fixture/ListAttachedPolicies.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates =
  req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule =
  req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate =
  req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

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

requestGetJobDocument :: GetJobDocument -> TestTree
requestGetJobDocument =
  req
    "GetJobDocument"
    "fixture/GetJobDocument.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules =
  req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestDescribeThingGroup :: DescribeThingGroup -> TestTree
requestDescribeThingGroup =
  req
    "DescribeThingGroup"
    "fixture/DescribeThingGroup.yaml"

requestAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
requestAcceptCertificateTransfer =
  req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

requestUpdateThingGroupsForThing :: UpdateThingGroupsForThing -> TestTree
requestUpdateThingGroupsForThing =
  req
    "UpdateThingGroupsForThing"
    "fixture/UpdateThingGroupsForThing.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule =
  req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

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

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestUpdateRoleAlias :: UpdateRoleAlias -> TestTree
requestUpdateRoleAlias =
  req
    "UpdateRoleAlias"
    "fixture/UpdateRoleAlias.yaml"

requestDeleteRoleAlias :: DeleteRoleAlias -> TestTree
requestDeleteRoleAlias =
  req
    "DeleteRoleAlias"
    "fixture/DeleteRoleAlias.yaml"

requestGetStatistics :: GetStatistics -> TestTree
requestGetStatistics =
  req
    "GetStatistics"
    "fixture/GetStatistics.yaml"

requestAssociateTargetsWithJob :: AssociateTargetsWithJob -> TestTree
requestAssociateTargetsWithJob =
  req
    "AssociateTargetsWithJob"
    "fixture/AssociateTargetsWithJob.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestCancelJobExecution :: CancelJobExecution -> TestTree
requestCancelJobExecution =
  req
    "CancelJobExecution"
    "fixture/CancelJobExecution.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer =
  req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

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

requestSearchIndex :: SearchIndex -> TestTree
requestSearchIndex =
  req
    "SearchIndex"
    "fixture/SearchIndex.yaml"

requestDescribeSecurityProfile :: DescribeSecurityProfile -> TestTree
requestDescribeSecurityProfile =
  req
    "DescribeSecurityProfile"
    "fixture/DescribeSecurityProfile.yaml"

requestListJobExecutionsForJob :: ListJobExecutionsForJob -> TestTree
requestListJobExecutionsForJob =
  req
    "ListJobExecutionsForJob"
    "fixture/ListJobExecutionsForJob.yaml"

requestCreateBillingGroup :: CreateBillingGroup -> TestTree
requestCreateBillingGroup =
  req
    "CreateBillingGroup"
    "fixture/CreateBillingGroup.yaml"

requestCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTask -> TestTree
requestCancelAuditMitigationActionsTask =
  req
    "CancelAuditMitigationActionsTask"
    "fixture/CancelAuditMitigationActionsTask.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestRemoveThingFromBillingGroup :: RemoveThingFromBillingGroup -> TestTree
requestRemoveThingFromBillingGroup =
  req
    "RemoveThingFromBillingGroup"
    "fixture/RemoveThingFromBillingGroup.yaml"

requestListAuthorizers :: ListAuthorizers -> TestTree
requestListAuthorizers =
  req
    "ListAuthorizers"
    "fixture/ListAuthorizers.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateAuditSuppression :: CreateAuditSuppression -> TestTree
requestCreateAuditSuppression =
  req
    "CreateAuditSuppression"
    "fixture/CreateAuditSuppression.yaml"

requestCreateProvisioningTemplate :: CreateProvisioningTemplate -> TestTree
requestCreateProvisioningTemplate =
  req
    "CreateProvisioningTemplate"
    "fixture/CreateProvisioningTemplate.yaml"

requestGetTopicRuleDestination :: GetTopicRuleDestination -> TestTree
requestGetTopicRuleDestination =
  req
    "GetTopicRuleDestination"
    "fixture/GetTopicRuleDestination.yaml"

requestDescribeAuditTask :: DescribeAuditTask -> TestTree
requestDescribeAuditTask =
  req
    "DescribeAuditTask"
    "fixture/DescribeAuditTask.yaml"

requestDescribeDomainConfiguration :: DescribeDomainConfiguration -> TestTree
requestDescribeDomainConfiguration =
  req
    "DescribeDomainConfiguration"
    "fixture/DescribeDomainConfiguration.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestListAuditSuppressions :: ListAuditSuppressions -> TestTree
requestListAuditSuppressions =
  req
    "ListAuditSuppressions"
    "fixture/ListAuditSuppressions.yaml"

requestCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
requestCreateCertificateFromCsr =
  req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

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

requestUpdateScheduledAudit :: UpdateScheduledAudit -> TestTree
requestUpdateScheduledAudit =
  req
    "UpdateScheduledAudit"
    "fixture/UpdateScheduledAudit.yaml"

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

requestListBillingGroups :: ListBillingGroups -> TestTree
requestListBillingGroups =
  req
    "ListBillingGroups"
    "fixture/ListBillingGroups.yaml"

requestTestAuthorization :: TestAuthorization -> TestTree
requestTestAuthorization =
  req
    "TestAuthorization"
    "fixture/TestAuthorization.yaml"

requestListThingTypes :: ListThingTypes -> TestTree
requestListThingTypes =
  req
    "ListThingTypes"
    "fixture/ListThingTypes.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices =
  req
    "ListIndices"
    "fixture/ListIndices.yaml"

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

requestListOutgoingCertificates :: ListOutgoingCertificates -> TestTree
requestListOutgoingCertificates =
  req
    "ListOutgoingCertificates"
    "fixture/ListOutgoingCertificates.yaml"

requestDeleteTopicRuleDestination :: DeleteTopicRuleDestination -> TestTree
requestDeleteTopicRuleDestination =
  req
    "DeleteTopicRuleDestination"
    "fixture/DeleteTopicRuleDestination.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTopicRuleDestinations :: ListTopicRuleDestinations -> TestTree
requestListTopicRuleDestinations =
  req
    "ListTopicRuleDestinations"
    "fixture/ListTopicRuleDestinations.yaml"

requestCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTask -> TestTree
requestCancelDetectMitigationActionsTask =
  req
    "CancelDetectMitigationActionsTask"
    "fixture/CancelDetectMitigationActionsTask.yaml"

requestAddThingToBillingGroup :: AddThingToBillingGroup -> TestTree
requestAddThingToBillingGroup =
  req
    "AddThingToBillingGroup"
    "fixture/AddThingToBillingGroup.yaml"

requestDeleteThingGroup :: DeleteThingGroup -> TestTree
requestDeleteThingGroup =
  req
    "DeleteThingGroup"
    "fixture/DeleteThingGroup.yaml"

requestDescribeEventConfigurations :: DescribeEventConfigurations -> TestTree
requestDescribeEventConfigurations =
  req
    "DescribeEventConfigurations"
    "fixture/DescribeEventConfigurations.yaml"

requestUpdateTopicRuleDestination :: UpdateTopicRuleDestination -> TestTree
requestUpdateTopicRuleDestination =
  req
    "UpdateTopicRuleDestination"
    "fixture/UpdateTopicRuleDestination.yaml"

requestListOTAUpdates :: ListOTAUpdates -> TestTree
requestListOTAUpdates =
  req
    "ListOTAUpdates"
    "fixture/ListOTAUpdates.yaml"

requestListThingGroups :: ListThingGroups -> TestTree
requestListThingGroups =
  req
    "ListThingGroups"
    "fixture/ListThingGroups.yaml"

requestListProvisioningTemplateVersions :: ListProvisioningTemplateVersions -> TestTree
requestListProvisioningTemplateVersions =
  req
    "ListProvisioningTemplateVersions"
    "fixture/ListProvisioningTemplateVersions.yaml"

requestUpdateThingGroup :: UpdateThingGroup -> TestTree
requestUpdateThingGroup =
  req
    "UpdateThingGroup"
    "fixture/UpdateThingGroup.yaml"

-- Responses

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingRegistrationTaskReports)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningClaim)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIndexingConfiguration)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificate)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfiles)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJobExecution)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMitigationActions)

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

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListViolationEvents)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCertificate)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMitigationAction)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMitigationAction)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningTemplate)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDimension)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainConfiguration)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditTasks)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy RejectCertificateTransfer)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoggingOptions)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMitigationAction)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicRule)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingType)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingsInThingGroup)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledAudit)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainConfigurations)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainConfiguration)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetV2LoggingOptions)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityProfile)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTopicRule)

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

responseUpdateCustomMetric :: UpdateCustomMetricResponse -> TestTree
responseUpdateCustomMetric =
  res
    "UpdateCustomMetricResponse"
    "fixture/UpdateCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomMetric)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelAuditTask)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoleAliases)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartAuditMitigationActionsTask)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AttachSecurityProfile)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJob)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy TransferCertificate)

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

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJob)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoleAlias)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingsInBillingGroup)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForSecurityProfile)

responseListCustomMetrics :: ListCustomMetricsResponse -> TestTree
responseListCustomMetrics =
  res
    "ListCustomMetricsResponse"
    "fixture/ListCustomMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCustomMetrics)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningTemplateVersion)

responseGetPercentiles :: GetPercentilesResponse -> TestTree
responseGetPercentiles =
  res
    "GetPercentilesResponse"
    "fixture/GetPercentilesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPercentiles)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicyVersion)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoint)

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

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DisableTopicRule)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditMitigationActionsTask)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    defaultService
    (Proxy :: Proxy SetV2LoggingLevel)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobExecutionsForThing)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateThing)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificate)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisioningTemplate)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartThingRegistrationTask)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultAuthorizer)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisioningTemplate)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMitigationAction)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteV2LoggingLevel)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobExecution)

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

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetIndexingConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListV2LoggingLevels)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningTemplates)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditMitigationActionsExecutions)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuditSuppression)

responseListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasksResponse -> TestTree
responseListDetectMitigationActionsTasks =
  res
    "ListDetectMitigationActionsTasksResponse"
    "fixture/ListDetectMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListDetectMitigationActionsTasks)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStream)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegistrationCode)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuthorizer)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDimension)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStream)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountAuditConfiguration)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThings)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy SetV2LoggingOptions)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThing)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddThingToThingGroup)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggingOptions)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuditSuppression)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    defaultService
    (Proxy :: Proxy ListScheduledAudits)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    defaultService
    (Proxy :: Proxy AttachThingPrincipal)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThing)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    defaultService
    (Proxy :: Proxy ListCertificatesByCA)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingGroupsForThing)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBillingGroup)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBillingGroup)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountAuditConfiguration)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingRegistrationTask)

responseDescribeCustomMetric :: DescribeCustomMetricResponse -> TestTree
responseDescribeCustomMetric =
  res
    "DescribeCustomMetricResponse"
    "fixture/DescribeCustomMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomMetric)

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

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOTAUpdate)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCertificateWithoutCA)

responseListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutionsResponse -> TestTree
responseListDetectMitigationActionsExecutions =
  res
    "ListDetectMitigationActionsExecutionsResponse"
    "fixture/ListDetectMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDetectMitigationActionsExecutions)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDynamicThingGroup)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegistrationCode)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJob)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DetachSecurityProfile)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeAuthorizer)

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

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningTemplateVersion)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrincipalThings)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditMitigationActionsTasks)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRoleAlias)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTopicRuleDestination)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOTAUpdate)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDynamicThingGroup)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDynamicThingGroup)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachPolicy)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingPrincipals)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDefaultAuthorizer)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateThingGroup)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCertificate)

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

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityProfile)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActiveViolations)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuthorizer)

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

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDimension)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBillingGroup)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDimension)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmTopicRuleDestination)

responseDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTaskResponse -> TestTree
responseDescribeDetectMitigationActionsTask =
  res
    "DescribeDetectMitigationActionsTaskResponse"
    "fixture/DescribeDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDetectMitigationActionsTask)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingRegistrationTasks)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDimensions)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditSuppression)

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

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStream)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    defaultService
    (Proxy :: Proxy DetachThingPrincipal)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartOnDemandAuditTask)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedPolicies)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCACertificates)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy EnableTopicRule)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCACertificate)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfilesForTarget)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEventConfigurations)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobDocument)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicRules)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeThingGroup)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptCertificateTransfer)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingGroupsForThing)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForPolicy)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceTopicRule)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIndex)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicyVersion)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachPolicy)

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

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoleAlias)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoleAlias)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetStatistics)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTargetsWithJob)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyVersions)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJobExecution)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCertificateTransfer)

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

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    defaultService
    (Proxy :: Proxy SearchIndex)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityProfile)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobExecutionsForJob)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBillingGroup)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelAuditMitigationActionsTask)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStream)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveThingFromBillingGroup)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuthorizers)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuditSuppression)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningTemplate)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicRuleDestination)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuditTask)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomainConfiguration)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreams)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAuditSuppressions)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr =
  res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificateFromCsr)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy GetOTAUpdate)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEffectivePolicies)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScheduledAudit)

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

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBillingGroups)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy TestAuthorization)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingTypes)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIndices)

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

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOutgoingCertificates)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTopicRuleDestination)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopicRuleDestinations)

responseCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTaskResponse -> TestTree
responseCancelDetectMitigationActionsTask =
  res
    "CancelDetectMitigationActionsTaskResponse"
    "fixture/CancelDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelDetectMitigationActionsTask)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddThingToBillingGroup)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThingGroup)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventConfigurations)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTopicRuleDestination)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOTAUpdates)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListThingGroups)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningTemplateVersions)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingGroup)
