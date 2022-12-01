{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoT
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoT where

import Amazonka.IoT
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoT.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptCertificateTransfer $
--             newAcceptCertificateTransfer
--
--         , requestAddThingToBillingGroup $
--             newAddThingToBillingGroup
--
--         , requestAddThingToThingGroup $
--             newAddThingToThingGroup
--
--         , requestAssociateTargetsWithJob $
--             newAssociateTargetsWithJob
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestAttachSecurityProfile $
--             newAttachSecurityProfile
--
--         , requestAttachThingPrincipal $
--             newAttachThingPrincipal
--
--         , requestCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTask
--
--         , requestCancelAuditTask $
--             newCancelAuditTask
--
--         , requestCancelCertificateTransfer $
--             newCancelCertificateTransfer
--
--         , requestCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTask
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestCancelJobExecution $
--             newCancelJobExecution
--
--         , requestClearDefaultAuthorizer $
--             newClearDefaultAuthorizer
--
--         , requestConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestination
--
--         , requestCreateAuditSuppression $
--             newCreateAuditSuppression
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestCreateBillingGroup $
--             newCreateBillingGroup
--
--         , requestCreateCertificateFromCsr $
--             newCreateCertificateFromCsr
--
--         , requestCreateCustomMetric $
--             newCreateCustomMetric
--
--         , requestCreateDimension $
--             newCreateDimension
--
--         , requestCreateDomainConfiguration $
--             newCreateDomainConfiguration
--
--         , requestCreateDynamicThingGroup $
--             newCreateDynamicThingGroup
--
--         , requestCreateFleetMetric $
--             newCreateFleetMetric
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestCreateKeysAndCertificate $
--             newCreateKeysAndCertificate
--
--         , requestCreateMitigationAction $
--             newCreateMitigationAction
--
--         , requestCreateOTAUpdate $
--             newCreateOTAUpdate
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestCreateProvisioningClaim $
--             newCreateProvisioningClaim
--
--         , requestCreateProvisioningTemplate $
--             newCreateProvisioningTemplate
--
--         , requestCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersion
--
--         , requestCreateRoleAlias $
--             newCreateRoleAlias
--
--         , requestCreateScheduledAudit $
--             newCreateScheduledAudit
--
--         , requestCreateSecurityProfile $
--             newCreateSecurityProfile
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestCreateThing $
--             newCreateThing
--
--         , requestCreateThingGroup $
--             newCreateThingGroup
--
--         , requestCreateThingType $
--             newCreateThingType
--
--         , requestCreateTopicRule $
--             newCreateTopicRule
--
--         , requestCreateTopicRuleDestination $
--             newCreateTopicRuleDestination
--
--         , requestDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfiguration
--
--         , requestDeleteAuditSuppression $
--             newDeleteAuditSuppression
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestDeleteBillingGroup $
--             newDeleteBillingGroup
--
--         , requestDeleteCACertificate $
--             newDeleteCACertificate
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDeleteCustomMetric $
--             newDeleteCustomMetric
--
--         , requestDeleteDimension $
--             newDeleteDimension
--
--         , requestDeleteDomainConfiguration $
--             newDeleteDomainConfiguration
--
--         , requestDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroup
--
--         , requestDeleteFleetMetric $
--             newDeleteFleetMetric
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteJobExecution $
--             newDeleteJobExecution
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestDeleteMitigationAction $
--             newDeleteMitigationAction
--
--         , requestDeleteOTAUpdate $
--             newDeleteOTAUpdate
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplate
--
--         , requestDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersion
--
--         , requestDeleteRegistrationCode $
--             newDeleteRegistrationCode
--
--         , requestDeleteRoleAlias $
--             newDeleteRoleAlias
--
--         , requestDeleteScheduledAudit $
--             newDeleteScheduledAudit
--
--         , requestDeleteSecurityProfile $
--             newDeleteSecurityProfile
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestDeleteThing $
--             newDeleteThing
--
--         , requestDeleteThingGroup $
--             newDeleteThingGroup
--
--         , requestDeleteThingType $
--             newDeleteThingType
--
--         , requestDeleteTopicRule $
--             newDeleteTopicRule
--
--         , requestDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestination
--
--         , requestDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevel
--
--         , requestDeprecateThingType $
--             newDeprecateThingType
--
--         , requestDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfiguration
--
--         , requestDescribeAuditFinding $
--             newDescribeAuditFinding
--
--         , requestDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTask
--
--         , requestDescribeAuditSuppression $
--             newDescribeAuditSuppression
--
--         , requestDescribeAuditTask $
--             newDescribeAuditTask
--
--         , requestDescribeAuthorizer $
--             newDescribeAuthorizer
--
--         , requestDescribeBillingGroup $
--             newDescribeBillingGroup
--
--         , requestDescribeCACertificate $
--             newDescribeCACertificate
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestDescribeCustomMetric $
--             newDescribeCustomMetric
--
--         , requestDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizer
--
--         , requestDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTask
--
--         , requestDescribeDimension $
--             newDescribeDimension
--
--         , requestDescribeDomainConfiguration $
--             newDescribeDomainConfiguration
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDescribeEventConfigurations $
--             newDescribeEventConfigurations
--
--         , requestDescribeFleetMetric $
--             newDescribeFleetMetric
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDescribeJobExecution $
--             newDescribeJobExecution
--
--         , requestDescribeJobTemplate $
--             newDescribeJobTemplate
--
--         , requestDescribeManagedJobTemplate $
--             newDescribeManagedJobTemplate
--
--         , requestDescribeMitigationAction $
--             newDescribeMitigationAction
--
--         , requestDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplate
--
--         , requestDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersion
--
--         , requestDescribeRoleAlias $
--             newDescribeRoleAlias
--
--         , requestDescribeScheduledAudit $
--             newDescribeScheduledAudit
--
--         , requestDescribeSecurityProfile $
--             newDescribeSecurityProfile
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestDescribeThing $
--             newDescribeThing
--
--         , requestDescribeThingGroup $
--             newDescribeThingGroup
--
--         , requestDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTask
--
--         , requestDescribeThingType $
--             newDescribeThingType
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDetachSecurityProfile $
--             newDetachSecurityProfile
--
--         , requestDetachThingPrincipal $
--             newDetachThingPrincipal
--
--         , requestDisableTopicRule $
--             newDisableTopicRule
--
--         , requestEnableTopicRule $
--             newEnableTopicRule
--
--         , requestGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummaries
--
--         , requestGetBucketsAggregation $
--             newGetBucketsAggregation
--
--         , requestGetCardinality $
--             newGetCardinality
--
--         , requestGetEffectivePolicies $
--             newGetEffectivePolicies
--
--         , requestGetIndexingConfiguration $
--             newGetIndexingConfiguration
--
--         , requestGetJobDocument $
--             newGetJobDocument
--
--         , requestGetLoggingOptions $
--             newGetLoggingOptions
--
--         , requestGetOTAUpdate $
--             newGetOTAUpdate
--
--         , requestGetPercentiles $
--             newGetPercentiles
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestGetRegistrationCode $
--             newGetRegistrationCode
--
--         , requestGetStatistics $
--             newGetStatistics
--
--         , requestGetTopicRule $
--             newGetTopicRule
--
--         , requestGetTopicRuleDestination $
--             newGetTopicRuleDestination
--
--         , requestGetV2LoggingOptions $
--             newGetV2LoggingOptions
--
--         , requestListActiveViolations $
--             newListActiveViolations
--
--         , requestListAttachedPolicies $
--             newListAttachedPolicies
--
--         , requestListAuditFindings $
--             newListAuditFindings
--
--         , requestListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutions
--
--         , requestListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasks
--
--         , requestListAuditSuppressions $
--             newListAuditSuppressions
--
--         , requestListAuditTasks $
--             newListAuditTasks
--
--         , requestListAuthorizers $
--             newListAuthorizers
--
--         , requestListBillingGroups $
--             newListBillingGroups
--
--         , requestListCACertificates $
--             newListCACertificates
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestListCertificatesByCA $
--             newListCertificatesByCA
--
--         , requestListCustomMetrics $
--             newListCustomMetrics
--
--         , requestListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutions
--
--         , requestListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasks
--
--         , requestListDimensions $
--             newListDimensions
--
--         , requestListDomainConfigurations $
--             newListDomainConfigurations
--
--         , requestListFleetMetrics $
--             newListFleetMetrics
--
--         , requestListIndices $
--             newListIndices
--
--         , requestListJobExecutionsForJob $
--             newListJobExecutionsForJob
--
--         , requestListJobExecutionsForThing $
--             newListJobExecutionsForThing
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListManagedJobTemplates $
--             newListManagedJobTemplates
--
--         , requestListMetricValues $
--             newListMetricValues
--
--         , requestListMitigationActions $
--             newListMitigationActions
--
--         , requestListOTAUpdates $
--             newListOTAUpdates
--
--         , requestListOutgoingCertificates $
--             newListOutgoingCertificates
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestListPrincipalThings $
--             newListPrincipalThings
--
--         , requestListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersions
--
--         , requestListProvisioningTemplates $
--             newListProvisioningTemplates
--
--         , requestListRelatedResourcesForAuditFinding $
--             newListRelatedResourcesForAuditFinding
--
--         , requestListRoleAliases $
--             newListRoleAliases
--
--         , requestListScheduledAudits $
--             newListScheduledAudits
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestListSecurityProfilesForTarget $
--             newListSecurityProfilesForTarget
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfile
--
--         , requestListThingGroups $
--             newListThingGroups
--
--         , requestListThingGroupsForThing $
--             newListThingGroupsForThing
--
--         , requestListThingPrincipals $
--             newListThingPrincipals
--
--         , requestListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReports
--
--         , requestListThingRegistrationTasks $
--             newListThingRegistrationTasks
--
--         , requestListThingTypes $
--             newListThingTypes
--
--         , requestListThings $
--             newListThings
--
--         , requestListThingsInBillingGroup $
--             newListThingsInBillingGroup
--
--         , requestListThingsInThingGroup $
--             newListThingsInThingGroup
--
--         , requestListTopicRuleDestinations $
--             newListTopicRuleDestinations
--
--         , requestListTopicRules $
--             newListTopicRules
--
--         , requestListV2LoggingLevels $
--             newListV2LoggingLevels
--
--         , requestListViolationEvents $
--             newListViolationEvents
--
--         , requestPutVerificationStateOnViolation $
--             newPutVerificationStateOnViolation
--
--         , requestRegisterCACertificate $
--             newRegisterCACertificate
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCA
--
--         , requestRegisterThing $
--             newRegisterThing
--
--         , requestRejectCertificateTransfer $
--             newRejectCertificateTransfer
--
--         , requestRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroup
--
--         , requestRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroup
--
--         , requestReplaceTopicRule $
--             newReplaceTopicRule
--
--         , requestSearchIndex $
--             newSearchIndex
--
--         , requestSetDefaultAuthorizer $
--             newSetDefaultAuthorizer
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestSetLoggingOptions $
--             newSetLoggingOptions
--
--         , requestSetV2LoggingLevel $
--             newSetV2LoggingLevel
--
--         , requestSetV2LoggingOptions $
--             newSetV2LoggingOptions
--
--         , requestStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTask
--
--         , requestStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTask
--
--         , requestStartOnDemandAuditTask $
--             newStartOnDemandAuditTask
--
--         , requestStartThingRegistrationTask $
--             newStartThingRegistrationTask
--
--         , requestStopThingRegistrationTask $
--             newStopThingRegistrationTask
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestAuthorization $
--             newTestAuthorization
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestTransferCertificate $
--             newTransferCertificate
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfiguration
--
--         , requestUpdateAuditSuppression $
--             newUpdateAuditSuppression
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestUpdateBillingGroup $
--             newUpdateBillingGroup
--
--         , requestUpdateCACertificate $
--             newUpdateCACertificate
--
--         , requestUpdateCertificate $
--             newUpdateCertificate
--
--         , requestUpdateCustomMetric $
--             newUpdateCustomMetric
--
--         , requestUpdateDimension $
--             newUpdateDimension
--
--         , requestUpdateDomainConfiguration $
--             newUpdateDomainConfiguration
--
--         , requestUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroup
--
--         , requestUpdateEventConfigurations $
--             newUpdateEventConfigurations
--
--         , requestUpdateFleetMetric $
--             newUpdateFleetMetric
--
--         , requestUpdateIndexingConfiguration $
--             newUpdateIndexingConfiguration
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestUpdateMitigationAction $
--             newUpdateMitigationAction
--
--         , requestUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplate
--
--         , requestUpdateRoleAlias $
--             newUpdateRoleAlias
--
--         , requestUpdateScheduledAudit $
--             newUpdateScheduledAudit
--
--         , requestUpdateSecurityProfile $
--             newUpdateSecurityProfile
--
--         , requestUpdateStream $
--             newUpdateStream
--
--         , requestUpdateThing $
--             newUpdateThing
--
--         , requestUpdateThingGroup $
--             newUpdateThingGroup
--
--         , requestUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThing
--
--         , requestUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestination
--
--         , requestValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviors
--
--           ]

--     , testGroup "response"
--         [ responseAcceptCertificateTransfer $
--             newAcceptCertificateTransferResponse
--
--         , responseAddThingToBillingGroup $
--             newAddThingToBillingGroupResponse
--
--         , responseAddThingToThingGroup $
--             newAddThingToThingGroupResponse
--
--         , responseAssociateTargetsWithJob $
--             newAssociateTargetsWithJobResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseAttachSecurityProfile $
--             newAttachSecurityProfileResponse
--
--         , responseAttachThingPrincipal $
--             newAttachThingPrincipalResponse
--
--         , responseCancelAuditMitigationActionsTask $
--             newCancelAuditMitigationActionsTaskResponse
--
--         , responseCancelAuditTask $
--             newCancelAuditTaskResponse
--
--         , responseCancelCertificateTransfer $
--             newCancelCertificateTransferResponse
--
--         , responseCancelDetectMitigationActionsTask $
--             newCancelDetectMitigationActionsTaskResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseCancelJobExecution $
--             newCancelJobExecutionResponse
--
--         , responseClearDefaultAuthorizer $
--             newClearDefaultAuthorizerResponse
--
--         , responseConfirmTopicRuleDestination $
--             newConfirmTopicRuleDestinationResponse
--
--         , responseCreateAuditSuppression $
--             newCreateAuditSuppressionResponse
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseCreateBillingGroup $
--             newCreateBillingGroupResponse
--
--         , responseCreateCertificateFromCsr $
--             newCreateCertificateFromCsrResponse
--
--         , responseCreateCustomMetric $
--             newCreateCustomMetricResponse
--
--         , responseCreateDimension $
--             newCreateDimensionResponse
--
--         , responseCreateDomainConfiguration $
--             newCreateDomainConfigurationResponse
--
--         , responseCreateDynamicThingGroup $
--             newCreateDynamicThingGroupResponse
--
--         , responseCreateFleetMetric $
--             newCreateFleetMetricResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseCreateKeysAndCertificate $
--             newCreateKeysAndCertificateResponse
--
--         , responseCreateMitigationAction $
--             newCreateMitigationActionResponse
--
--         , responseCreateOTAUpdate $
--             newCreateOTAUpdateResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseCreateProvisioningClaim $
--             newCreateProvisioningClaimResponse
--
--         , responseCreateProvisioningTemplate $
--             newCreateProvisioningTemplateResponse
--
--         , responseCreateProvisioningTemplateVersion $
--             newCreateProvisioningTemplateVersionResponse
--
--         , responseCreateRoleAlias $
--             newCreateRoleAliasResponse
--
--         , responseCreateScheduledAudit $
--             newCreateScheduledAuditResponse
--
--         , responseCreateSecurityProfile $
--             newCreateSecurityProfileResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseCreateThing $
--             newCreateThingResponse
--
--         , responseCreateThingGroup $
--             newCreateThingGroupResponse
--
--         , responseCreateThingType $
--             newCreateThingTypeResponse
--
--         , responseCreateTopicRule $
--             newCreateTopicRuleResponse
--
--         , responseCreateTopicRuleDestination $
--             newCreateTopicRuleDestinationResponse
--
--         , responseDeleteAccountAuditConfiguration $
--             newDeleteAccountAuditConfigurationResponse
--
--         , responseDeleteAuditSuppression $
--             newDeleteAuditSuppressionResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseDeleteBillingGroup $
--             newDeleteBillingGroupResponse
--
--         , responseDeleteCACertificate $
--             newDeleteCACertificateResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDeleteCustomMetric $
--             newDeleteCustomMetricResponse
--
--         , responseDeleteDimension $
--             newDeleteDimensionResponse
--
--         , responseDeleteDomainConfiguration $
--             newDeleteDomainConfigurationResponse
--
--         , responseDeleteDynamicThingGroup $
--             newDeleteDynamicThingGroupResponse
--
--         , responseDeleteFleetMetric $
--             newDeleteFleetMetricResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteJobExecution $
--             newDeleteJobExecutionResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseDeleteMitigationAction $
--             newDeleteMitigationActionResponse
--
--         , responseDeleteOTAUpdate $
--             newDeleteOTAUpdateResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseDeleteProvisioningTemplate $
--             newDeleteProvisioningTemplateResponse
--
--         , responseDeleteProvisioningTemplateVersion $
--             newDeleteProvisioningTemplateVersionResponse
--
--         , responseDeleteRegistrationCode $
--             newDeleteRegistrationCodeResponse
--
--         , responseDeleteRoleAlias $
--             newDeleteRoleAliasResponse
--
--         , responseDeleteScheduledAudit $
--             newDeleteScheduledAuditResponse
--
--         , responseDeleteSecurityProfile $
--             newDeleteSecurityProfileResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseDeleteThing $
--             newDeleteThingResponse
--
--         , responseDeleteThingGroup $
--             newDeleteThingGroupResponse
--
--         , responseDeleteThingType $
--             newDeleteThingTypeResponse
--
--         , responseDeleteTopicRule $
--             newDeleteTopicRuleResponse
--
--         , responseDeleteTopicRuleDestination $
--             newDeleteTopicRuleDestinationResponse
--
--         , responseDeleteV2LoggingLevel $
--             newDeleteV2LoggingLevelResponse
--
--         , responseDeprecateThingType $
--             newDeprecateThingTypeResponse
--
--         , responseDescribeAccountAuditConfiguration $
--             newDescribeAccountAuditConfigurationResponse
--
--         , responseDescribeAuditFinding $
--             newDescribeAuditFindingResponse
--
--         , responseDescribeAuditMitigationActionsTask $
--             newDescribeAuditMitigationActionsTaskResponse
--
--         , responseDescribeAuditSuppression $
--             newDescribeAuditSuppressionResponse
--
--         , responseDescribeAuditTask $
--             newDescribeAuditTaskResponse
--
--         , responseDescribeAuthorizer $
--             newDescribeAuthorizerResponse
--
--         , responseDescribeBillingGroup $
--             newDescribeBillingGroupResponse
--
--         , responseDescribeCACertificate $
--             newDescribeCACertificateResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseDescribeCustomMetric $
--             newDescribeCustomMetricResponse
--
--         , responseDescribeDefaultAuthorizer $
--             newDescribeDefaultAuthorizerResponse
--
--         , responseDescribeDetectMitigationActionsTask $
--             newDescribeDetectMitigationActionsTaskResponse
--
--         , responseDescribeDimension $
--             newDescribeDimensionResponse
--
--         , responseDescribeDomainConfiguration $
--             newDescribeDomainConfigurationResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDescribeEventConfigurations $
--             newDescribeEventConfigurationsResponse
--
--         , responseDescribeFleetMetric $
--             newDescribeFleetMetricResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDescribeJobExecution $
--             newDescribeJobExecutionResponse
--
--         , responseDescribeJobTemplate $
--             newDescribeJobTemplateResponse
--
--         , responseDescribeManagedJobTemplate $
--             newDescribeManagedJobTemplateResponse
--
--         , responseDescribeMitigationAction $
--             newDescribeMitigationActionResponse
--
--         , responseDescribeProvisioningTemplate $
--             newDescribeProvisioningTemplateResponse
--
--         , responseDescribeProvisioningTemplateVersion $
--             newDescribeProvisioningTemplateVersionResponse
--
--         , responseDescribeRoleAlias $
--             newDescribeRoleAliasResponse
--
--         , responseDescribeScheduledAudit $
--             newDescribeScheduledAuditResponse
--
--         , responseDescribeSecurityProfile $
--             newDescribeSecurityProfileResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseDescribeThing $
--             newDescribeThingResponse
--
--         , responseDescribeThingGroup $
--             newDescribeThingGroupResponse
--
--         , responseDescribeThingRegistrationTask $
--             newDescribeThingRegistrationTaskResponse
--
--         , responseDescribeThingType $
--             newDescribeThingTypeResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDetachSecurityProfile $
--             newDetachSecurityProfileResponse
--
--         , responseDetachThingPrincipal $
--             newDetachThingPrincipalResponse
--
--         , responseDisableTopicRule $
--             newDisableTopicRuleResponse
--
--         , responseEnableTopicRule $
--             newEnableTopicRuleResponse
--
--         , responseGetBehaviorModelTrainingSummaries $
--             newGetBehaviorModelTrainingSummariesResponse
--
--         , responseGetBucketsAggregation $
--             newGetBucketsAggregationResponse
--
--         , responseGetCardinality $
--             newGetCardinalityResponse
--
--         , responseGetEffectivePolicies $
--             newGetEffectivePoliciesResponse
--
--         , responseGetIndexingConfiguration $
--             newGetIndexingConfigurationResponse
--
--         , responseGetJobDocument $
--             newGetJobDocumentResponse
--
--         , responseGetLoggingOptions $
--             newGetLoggingOptionsResponse
--
--         , responseGetOTAUpdate $
--             newGetOTAUpdateResponse
--
--         , responseGetPercentiles $
--             newGetPercentilesResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseGetRegistrationCode $
--             newGetRegistrationCodeResponse
--
--         , responseGetStatistics $
--             newGetStatisticsResponse
--
--         , responseGetTopicRule $
--             newGetTopicRuleResponse
--
--         , responseGetTopicRuleDestination $
--             newGetTopicRuleDestinationResponse
--
--         , responseGetV2LoggingOptions $
--             newGetV2LoggingOptionsResponse
--
--         , responseListActiveViolations $
--             newListActiveViolationsResponse
--
--         , responseListAttachedPolicies $
--             newListAttachedPoliciesResponse
--
--         , responseListAuditFindings $
--             newListAuditFindingsResponse
--
--         , responseListAuditMitigationActionsExecutions $
--             newListAuditMitigationActionsExecutionsResponse
--
--         , responseListAuditMitigationActionsTasks $
--             newListAuditMitigationActionsTasksResponse
--
--         , responseListAuditSuppressions $
--             newListAuditSuppressionsResponse
--
--         , responseListAuditTasks $
--             newListAuditTasksResponse
--
--         , responseListAuthorizers $
--             newListAuthorizersResponse
--
--         , responseListBillingGroups $
--             newListBillingGroupsResponse
--
--         , responseListCACertificates $
--             newListCACertificatesResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseListCertificatesByCA $
--             newListCertificatesByCAResponse
--
--         , responseListCustomMetrics $
--             newListCustomMetricsResponse
--
--         , responseListDetectMitigationActionsExecutions $
--             newListDetectMitigationActionsExecutionsResponse
--
--         , responseListDetectMitigationActionsTasks $
--             newListDetectMitigationActionsTasksResponse
--
--         , responseListDimensions $
--             newListDimensionsResponse
--
--         , responseListDomainConfigurations $
--             newListDomainConfigurationsResponse
--
--         , responseListFleetMetrics $
--             newListFleetMetricsResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseListJobExecutionsForJob $
--             newListJobExecutionsForJobResponse
--
--         , responseListJobExecutionsForThing $
--             newListJobExecutionsForThingResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListManagedJobTemplates $
--             newListManagedJobTemplatesResponse
--
--         , responseListMetricValues $
--             newListMetricValuesResponse
--
--         , responseListMitigationActions $
--             newListMitigationActionsResponse
--
--         , responseListOTAUpdates $
--             newListOTAUpdatesResponse
--
--         , responseListOutgoingCertificates $
--             newListOutgoingCertificatesResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseListPrincipalThings $
--             newListPrincipalThingsResponse
--
--         , responseListProvisioningTemplateVersions $
--             newListProvisioningTemplateVersionsResponse
--
--         , responseListProvisioningTemplates $
--             newListProvisioningTemplatesResponse
--
--         , responseListRelatedResourcesForAuditFinding $
--             newListRelatedResourcesForAuditFindingResponse
--
--         , responseListRoleAliases $
--             newListRoleAliasesResponse
--
--         , responseListScheduledAudits $
--             newListScheduledAuditsResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseListSecurityProfilesForTarget $
--             newListSecurityProfilesForTargetResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseListTargetsForSecurityProfile $
--             newListTargetsForSecurityProfileResponse
--
--         , responseListThingGroups $
--             newListThingGroupsResponse
--
--         , responseListThingGroupsForThing $
--             newListThingGroupsForThingResponse
--
--         , responseListThingPrincipals $
--             newListThingPrincipalsResponse
--
--         , responseListThingRegistrationTaskReports $
--             newListThingRegistrationTaskReportsResponse
--
--         , responseListThingRegistrationTasks $
--             newListThingRegistrationTasksResponse
--
--         , responseListThingTypes $
--             newListThingTypesResponse
--
--         , responseListThings $
--             newListThingsResponse
--
--         , responseListThingsInBillingGroup $
--             newListThingsInBillingGroupResponse
--
--         , responseListThingsInThingGroup $
--             newListThingsInThingGroupResponse
--
--         , responseListTopicRuleDestinations $
--             newListTopicRuleDestinationsResponse
--
--         , responseListTopicRules $
--             newListTopicRulesResponse
--
--         , responseListV2LoggingLevels $
--             newListV2LoggingLevelsResponse
--
--         , responseListViolationEvents $
--             newListViolationEventsResponse
--
--         , responsePutVerificationStateOnViolation $
--             newPutVerificationStateOnViolationResponse
--
--         , responseRegisterCACertificate $
--             newRegisterCACertificateResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseRegisterCertificateWithoutCA $
--             newRegisterCertificateWithoutCAResponse
--
--         , responseRegisterThing $
--             newRegisterThingResponse
--
--         , responseRejectCertificateTransfer $
--             newRejectCertificateTransferResponse
--
--         , responseRemoveThingFromBillingGroup $
--             newRemoveThingFromBillingGroupResponse
--
--         , responseRemoveThingFromThingGroup $
--             newRemoveThingFromThingGroupResponse
--
--         , responseReplaceTopicRule $
--             newReplaceTopicRuleResponse
--
--         , responseSearchIndex $
--             newSearchIndexResponse
--
--         , responseSetDefaultAuthorizer $
--             newSetDefaultAuthorizerResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseSetLoggingOptions $
--             newSetLoggingOptionsResponse
--
--         , responseSetV2LoggingLevel $
--             newSetV2LoggingLevelResponse
--
--         , responseSetV2LoggingOptions $
--             newSetV2LoggingOptionsResponse
--
--         , responseStartAuditMitigationActionsTask $
--             newStartAuditMitigationActionsTaskResponse
--
--         , responseStartDetectMitigationActionsTask $
--             newStartDetectMitigationActionsTaskResponse
--
--         , responseStartOnDemandAuditTask $
--             newStartOnDemandAuditTaskResponse
--
--         , responseStartThingRegistrationTask $
--             newStartThingRegistrationTaskResponse
--
--         , responseStopThingRegistrationTask $
--             newStopThingRegistrationTaskResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestAuthorization $
--             newTestAuthorizationResponse
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responseTransferCertificate $
--             newTransferCertificateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccountAuditConfiguration $
--             newUpdateAccountAuditConfigurationResponse
--
--         , responseUpdateAuditSuppression $
--             newUpdateAuditSuppressionResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseUpdateBillingGroup $
--             newUpdateBillingGroupResponse
--
--         , responseUpdateCACertificate $
--             newUpdateCACertificateResponse
--
--         , responseUpdateCertificate $
--             newUpdateCertificateResponse
--
--         , responseUpdateCustomMetric $
--             newUpdateCustomMetricResponse
--
--         , responseUpdateDimension $
--             newUpdateDimensionResponse
--
--         , responseUpdateDomainConfiguration $
--             newUpdateDomainConfigurationResponse
--
--         , responseUpdateDynamicThingGroup $
--             newUpdateDynamicThingGroupResponse
--
--         , responseUpdateEventConfigurations $
--             newUpdateEventConfigurationsResponse
--
--         , responseUpdateFleetMetric $
--             newUpdateFleetMetricResponse
--
--         , responseUpdateIndexingConfiguration $
--             newUpdateIndexingConfigurationResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseUpdateMitigationAction $
--             newUpdateMitigationActionResponse
--
--         , responseUpdateProvisioningTemplate $
--             newUpdateProvisioningTemplateResponse
--
--         , responseUpdateRoleAlias $
--             newUpdateRoleAliasResponse
--
--         , responseUpdateScheduledAudit $
--             newUpdateScheduledAuditResponse
--
--         , responseUpdateSecurityProfile $
--             newUpdateSecurityProfileResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--         , responseUpdateThing $
--             newUpdateThingResponse
--
--         , responseUpdateThingGroup $
--             newUpdateThingGroupResponse
--
--         , responseUpdateThingGroupsForThing $
--             newUpdateThingGroupsForThingResponse
--
--         , responseUpdateTopicRuleDestination $
--             newUpdateTopicRuleDestinationResponse
--
--         , responseValidateSecurityProfileBehaviors $
--             newValidateSecurityProfileBehaviorsResponse
--
--           ]
--     ]

-- Requests

requestAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
requestAcceptCertificateTransfer =
  req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

requestAddThingToBillingGroup :: AddThingToBillingGroup -> TestTree
requestAddThingToBillingGroup =
  req
    "AddThingToBillingGroup"
    "fixture/AddThingToBillingGroup.yaml"

requestAddThingToThingGroup :: AddThingToThingGroup -> TestTree
requestAddThingToThingGroup =
  req
    "AddThingToThingGroup"
    "fixture/AddThingToThingGroup.yaml"

requestAssociateTargetsWithJob :: AssociateTargetsWithJob -> TestTree
requestAssociateTargetsWithJob =
  req
    "AssociateTargetsWithJob"
    "fixture/AssociateTargetsWithJob.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestAttachSecurityProfile :: AttachSecurityProfile -> TestTree
requestAttachSecurityProfile =
  req
    "AttachSecurityProfile"
    "fixture/AttachSecurityProfile.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal =
  req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTask -> TestTree
requestCancelAuditMitigationActionsTask =
  req
    "CancelAuditMitigationActionsTask"
    "fixture/CancelAuditMitigationActionsTask.yaml"

requestCancelAuditTask :: CancelAuditTask -> TestTree
requestCancelAuditTask =
  req
    "CancelAuditTask"
    "fixture/CancelAuditTask.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer =
  req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

requestCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTask -> TestTree
requestCancelDetectMitigationActionsTask =
  req
    "CancelDetectMitigationActionsTask"
    "fixture/CancelDetectMitigationActionsTask.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCancelJobExecution :: CancelJobExecution -> TestTree
requestCancelJobExecution =
  req
    "CancelJobExecution"
    "fixture/CancelJobExecution.yaml"

requestClearDefaultAuthorizer :: ClearDefaultAuthorizer -> TestTree
requestClearDefaultAuthorizer =
  req
    "ClearDefaultAuthorizer"
    "fixture/ClearDefaultAuthorizer.yaml"

requestConfirmTopicRuleDestination :: ConfirmTopicRuleDestination -> TestTree
requestConfirmTopicRuleDestination =
  req
    "ConfirmTopicRuleDestination"
    "fixture/ConfirmTopicRuleDestination.yaml"

requestCreateAuditSuppression :: CreateAuditSuppression -> TestTree
requestCreateAuditSuppression =
  req
    "CreateAuditSuppression"
    "fixture/CreateAuditSuppression.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestCreateBillingGroup :: CreateBillingGroup -> TestTree
requestCreateBillingGroup =
  req
    "CreateBillingGroup"
    "fixture/CreateBillingGroup.yaml"

requestCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
requestCreateCertificateFromCsr =
  req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

requestCreateCustomMetric :: CreateCustomMetric -> TestTree
requestCreateCustomMetric =
  req
    "CreateCustomMetric"
    "fixture/CreateCustomMetric.yaml"

requestCreateDimension :: CreateDimension -> TestTree
requestCreateDimension =
  req
    "CreateDimension"
    "fixture/CreateDimension.yaml"

requestCreateDomainConfiguration :: CreateDomainConfiguration -> TestTree
requestCreateDomainConfiguration =
  req
    "CreateDomainConfiguration"
    "fixture/CreateDomainConfiguration.yaml"

requestCreateDynamicThingGroup :: CreateDynamicThingGroup -> TestTree
requestCreateDynamicThingGroup =
  req
    "CreateDynamicThingGroup"
    "fixture/CreateDynamicThingGroup.yaml"

requestCreateFleetMetric :: CreateFleetMetric -> TestTree
requestCreateFleetMetric =
  req
    "CreateFleetMetric"
    "fixture/CreateFleetMetric.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

requestCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
requestCreateKeysAndCertificate =
  req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

requestCreateMitigationAction :: CreateMitigationAction -> TestTree
requestCreateMitigationAction =
  req
    "CreateMitigationAction"
    "fixture/CreateMitigationAction.yaml"

requestCreateOTAUpdate :: CreateOTAUpdate -> TestTree
requestCreateOTAUpdate =
  req
    "CreateOTAUpdate"
    "fixture/CreateOTAUpdate.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestCreateProvisioningClaim :: CreateProvisioningClaim -> TestTree
requestCreateProvisioningClaim =
  req
    "CreateProvisioningClaim"
    "fixture/CreateProvisioningClaim.yaml"

requestCreateProvisioningTemplate :: CreateProvisioningTemplate -> TestTree
requestCreateProvisioningTemplate =
  req
    "CreateProvisioningTemplate"
    "fixture/CreateProvisioningTemplate.yaml"

requestCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersion -> TestTree
requestCreateProvisioningTemplateVersion =
  req
    "CreateProvisioningTemplateVersion"
    "fixture/CreateProvisioningTemplateVersion.yaml"

requestCreateRoleAlias :: CreateRoleAlias -> TestTree
requestCreateRoleAlias =
  req
    "CreateRoleAlias"
    "fixture/CreateRoleAlias.yaml"

requestCreateScheduledAudit :: CreateScheduledAudit -> TestTree
requestCreateScheduledAudit =
  req
    "CreateScheduledAudit"
    "fixture/CreateScheduledAudit.yaml"

requestCreateSecurityProfile :: CreateSecurityProfile -> TestTree
requestCreateSecurityProfile =
  req
    "CreateSecurityProfile"
    "fixture/CreateSecurityProfile.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestCreateThing :: CreateThing -> TestTree
requestCreateThing =
  req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestCreateThingGroup :: CreateThingGroup -> TestTree
requestCreateThingGroup =
  req
    "CreateThingGroup"
    "fixture/CreateThingGroup.yaml"

requestCreateThingType :: CreateThingType -> TestTree
requestCreateThingType =
  req
    "CreateThingType"
    "fixture/CreateThingType.yaml"

requestCreateTopicRule :: CreateTopicRule -> TestTree
requestCreateTopicRule =
  req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

requestCreateTopicRuleDestination :: CreateTopicRuleDestination -> TestTree
requestCreateTopicRuleDestination =
  req
    "CreateTopicRuleDestination"
    "fixture/CreateTopicRuleDestination.yaml"

requestDeleteAccountAuditConfiguration :: DeleteAccountAuditConfiguration -> TestTree
requestDeleteAccountAuditConfiguration =
  req
    "DeleteAccountAuditConfiguration"
    "fixture/DeleteAccountAuditConfiguration.yaml"

requestDeleteAuditSuppression :: DeleteAuditSuppression -> TestTree
requestDeleteAuditSuppression =
  req
    "DeleteAuditSuppression"
    "fixture/DeleteAuditSuppression.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestDeleteBillingGroup :: DeleteBillingGroup -> TestTree
requestDeleteBillingGroup =
  req
    "DeleteBillingGroup"
    "fixture/DeleteBillingGroup.yaml"

requestDeleteCACertificate :: DeleteCACertificate -> TestTree
requestDeleteCACertificate =
  req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDeleteCustomMetric :: DeleteCustomMetric -> TestTree
requestDeleteCustomMetric =
  req
    "DeleteCustomMetric"
    "fixture/DeleteCustomMetric.yaml"

requestDeleteDimension :: DeleteDimension -> TestTree
requestDeleteDimension =
  req
    "DeleteDimension"
    "fixture/DeleteDimension.yaml"

requestDeleteDomainConfiguration :: DeleteDomainConfiguration -> TestTree
requestDeleteDomainConfiguration =
  req
    "DeleteDomainConfiguration"
    "fixture/DeleteDomainConfiguration.yaml"

requestDeleteDynamicThingGroup :: DeleteDynamicThingGroup -> TestTree
requestDeleteDynamicThingGroup =
  req
    "DeleteDynamicThingGroup"
    "fixture/DeleteDynamicThingGroup.yaml"

requestDeleteFleetMetric :: DeleteFleetMetric -> TestTree
requestDeleteFleetMetric =
  req
    "DeleteFleetMetric"
    "fixture/DeleteFleetMetric.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteJobExecution :: DeleteJobExecution -> TestTree
requestDeleteJobExecution =
  req
    "DeleteJobExecution"
    "fixture/DeleteJobExecution.yaml"

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

requestDeleteMitigationAction :: DeleteMitigationAction -> TestTree
requestDeleteMitigationAction =
  req
    "DeleteMitigationAction"
    "fixture/DeleteMitigationAction.yaml"

requestDeleteOTAUpdate :: DeleteOTAUpdate -> TestTree
requestDeleteOTAUpdate =
  req
    "DeleteOTAUpdate"
    "fixture/DeleteOTAUpdate.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDeleteProvisioningTemplate :: DeleteProvisioningTemplate -> TestTree
requestDeleteProvisioningTemplate =
  req
    "DeleteProvisioningTemplate"
    "fixture/DeleteProvisioningTemplate.yaml"

requestDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersion -> TestTree
requestDeleteProvisioningTemplateVersion =
  req
    "DeleteProvisioningTemplateVersion"
    "fixture/DeleteProvisioningTemplateVersion.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode =
  req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestDeleteRoleAlias :: DeleteRoleAlias -> TestTree
requestDeleteRoleAlias =
  req
    "DeleteRoleAlias"
    "fixture/DeleteRoleAlias.yaml"

requestDeleteScheduledAudit :: DeleteScheduledAudit -> TestTree
requestDeleteScheduledAudit =
  req
    "DeleteScheduledAudit"
    "fixture/DeleteScheduledAudit.yaml"

requestDeleteSecurityProfile :: DeleteSecurityProfile -> TestTree
requestDeleteSecurityProfile =
  req
    "DeleteSecurityProfile"
    "fixture/DeleteSecurityProfile.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing =
  req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestDeleteThingGroup :: DeleteThingGroup -> TestTree
requestDeleteThingGroup =
  req
    "DeleteThingGroup"
    "fixture/DeleteThingGroup.yaml"

requestDeleteThingType :: DeleteThingType -> TestTree
requestDeleteThingType =
  req
    "DeleteThingType"
    "fixture/DeleteThingType.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule =
  req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

requestDeleteTopicRuleDestination :: DeleteTopicRuleDestination -> TestTree
requestDeleteTopicRuleDestination =
  req
    "DeleteTopicRuleDestination"
    "fixture/DeleteTopicRuleDestination.yaml"

requestDeleteV2LoggingLevel :: DeleteV2LoggingLevel -> TestTree
requestDeleteV2LoggingLevel =
  req
    "DeleteV2LoggingLevel"
    "fixture/DeleteV2LoggingLevel.yaml"

requestDeprecateThingType :: DeprecateThingType -> TestTree
requestDeprecateThingType =
  req
    "DeprecateThingType"
    "fixture/DeprecateThingType.yaml"

requestDescribeAccountAuditConfiguration :: DescribeAccountAuditConfiguration -> TestTree
requestDescribeAccountAuditConfiguration =
  req
    "DescribeAccountAuditConfiguration"
    "fixture/DescribeAccountAuditConfiguration.yaml"

requestDescribeAuditFinding :: DescribeAuditFinding -> TestTree
requestDescribeAuditFinding =
  req
    "DescribeAuditFinding"
    "fixture/DescribeAuditFinding.yaml"

requestDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTask -> TestTree
requestDescribeAuditMitigationActionsTask =
  req
    "DescribeAuditMitigationActionsTask"
    "fixture/DescribeAuditMitigationActionsTask.yaml"

requestDescribeAuditSuppression :: DescribeAuditSuppression -> TestTree
requestDescribeAuditSuppression =
  req
    "DescribeAuditSuppression"
    "fixture/DescribeAuditSuppression.yaml"

requestDescribeAuditTask :: DescribeAuditTask -> TestTree
requestDescribeAuditTask =
  req
    "DescribeAuditTask"
    "fixture/DescribeAuditTask.yaml"

requestDescribeAuthorizer :: DescribeAuthorizer -> TestTree
requestDescribeAuthorizer =
  req
    "DescribeAuthorizer"
    "fixture/DescribeAuthorizer.yaml"

requestDescribeBillingGroup :: DescribeBillingGroup -> TestTree
requestDescribeBillingGroup =
  req
    "DescribeBillingGroup"
    "fixture/DescribeBillingGroup.yaml"

requestDescribeCACertificate :: DescribeCACertificate -> TestTree
requestDescribeCACertificate =
  req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestDescribeCustomMetric :: DescribeCustomMetric -> TestTree
requestDescribeCustomMetric =
  req
    "DescribeCustomMetric"
    "fixture/DescribeCustomMetric.yaml"

requestDescribeDefaultAuthorizer :: DescribeDefaultAuthorizer -> TestTree
requestDescribeDefaultAuthorizer =
  req
    "DescribeDefaultAuthorizer"
    "fixture/DescribeDefaultAuthorizer.yaml"

requestDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTask -> TestTree
requestDescribeDetectMitigationActionsTask =
  req
    "DescribeDetectMitigationActionsTask"
    "fixture/DescribeDetectMitigationActionsTask.yaml"

requestDescribeDimension :: DescribeDimension -> TestTree
requestDescribeDimension =
  req
    "DescribeDimension"
    "fixture/DescribeDimension.yaml"

requestDescribeDomainConfiguration :: DescribeDomainConfiguration -> TestTree
requestDescribeDomainConfiguration =
  req
    "DescribeDomainConfiguration"
    "fixture/DescribeDomainConfiguration.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestDescribeEventConfigurations :: DescribeEventConfigurations -> TestTree
requestDescribeEventConfigurations =
  req
    "DescribeEventConfigurations"
    "fixture/DescribeEventConfigurations.yaml"

requestDescribeFleetMetric :: DescribeFleetMetric -> TestTree
requestDescribeFleetMetric =
  req
    "DescribeFleetMetric"
    "fixture/DescribeFleetMetric.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeJobExecution :: DescribeJobExecution -> TestTree
requestDescribeJobExecution =
  req
    "DescribeJobExecution"
    "fixture/DescribeJobExecution.yaml"

requestDescribeJobTemplate :: DescribeJobTemplate -> TestTree
requestDescribeJobTemplate =
  req
    "DescribeJobTemplate"
    "fixture/DescribeJobTemplate.yaml"

requestDescribeManagedJobTemplate :: DescribeManagedJobTemplate -> TestTree
requestDescribeManagedJobTemplate =
  req
    "DescribeManagedJobTemplate"
    "fixture/DescribeManagedJobTemplate.yaml"

requestDescribeMitigationAction :: DescribeMitigationAction -> TestTree
requestDescribeMitigationAction =
  req
    "DescribeMitigationAction"
    "fixture/DescribeMitigationAction.yaml"

requestDescribeProvisioningTemplate :: DescribeProvisioningTemplate -> TestTree
requestDescribeProvisioningTemplate =
  req
    "DescribeProvisioningTemplate"
    "fixture/DescribeProvisioningTemplate.yaml"

requestDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersion -> TestTree
requestDescribeProvisioningTemplateVersion =
  req
    "DescribeProvisioningTemplateVersion"
    "fixture/DescribeProvisioningTemplateVersion.yaml"

requestDescribeRoleAlias :: DescribeRoleAlias -> TestTree
requestDescribeRoleAlias =
  req
    "DescribeRoleAlias"
    "fixture/DescribeRoleAlias.yaml"

requestDescribeScheduledAudit :: DescribeScheduledAudit -> TestTree
requestDescribeScheduledAudit =
  req
    "DescribeScheduledAudit"
    "fixture/DescribeScheduledAudit.yaml"

requestDescribeSecurityProfile :: DescribeSecurityProfile -> TestTree
requestDescribeSecurityProfile =
  req
    "DescribeSecurityProfile"
    "fixture/DescribeSecurityProfile.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestDescribeThing :: DescribeThing -> TestTree
requestDescribeThing =
  req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

requestDescribeThingGroup :: DescribeThingGroup -> TestTree
requestDescribeThingGroup =
  req
    "DescribeThingGroup"
    "fixture/DescribeThingGroup.yaml"

requestDescribeThingRegistrationTask :: DescribeThingRegistrationTask -> TestTree
requestDescribeThingRegistrationTask =
  req
    "DescribeThingRegistrationTask"
    "fixture/DescribeThingRegistrationTask.yaml"

requestDescribeThingType :: DescribeThingType -> TestTree
requestDescribeThingType =
  req
    "DescribeThingType"
    "fixture/DescribeThingType.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDetachSecurityProfile :: DetachSecurityProfile -> TestTree
requestDetachSecurityProfile =
  req
    "DetachSecurityProfile"
    "fixture/DetachSecurityProfile.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal =
  req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule =
  req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule =
  req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummaries -> TestTree
requestGetBehaviorModelTrainingSummaries =
  req
    "GetBehaviorModelTrainingSummaries"
    "fixture/GetBehaviorModelTrainingSummaries.yaml"

requestGetBucketsAggregation :: GetBucketsAggregation -> TestTree
requestGetBucketsAggregation =
  req
    "GetBucketsAggregation"
    "fixture/GetBucketsAggregation.yaml"

requestGetCardinality :: GetCardinality -> TestTree
requestGetCardinality =
  req
    "GetCardinality"
    "fixture/GetCardinality.yaml"

requestGetEffectivePolicies :: GetEffectivePolicies -> TestTree
requestGetEffectivePolicies =
  req
    "GetEffectivePolicies"
    "fixture/GetEffectivePolicies.yaml"

requestGetIndexingConfiguration :: GetIndexingConfiguration -> TestTree
requestGetIndexingConfiguration =
  req
    "GetIndexingConfiguration"
    "fixture/GetIndexingConfiguration.yaml"

requestGetJobDocument :: GetJobDocument -> TestTree
requestGetJobDocument =
  req
    "GetJobDocument"
    "fixture/GetJobDocument.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions =
  req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestGetOTAUpdate :: GetOTAUpdate -> TestTree
requestGetOTAUpdate =
  req
    "GetOTAUpdate"
    "fixture/GetOTAUpdate.yaml"

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

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion =
  req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestGetRegistrationCode :: GetRegistrationCode -> TestTree
requestGetRegistrationCode =
  req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

requestGetStatistics :: GetStatistics -> TestTree
requestGetStatistics =
  req
    "GetStatistics"
    "fixture/GetStatistics.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule =
  req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestGetTopicRuleDestination :: GetTopicRuleDestination -> TestTree
requestGetTopicRuleDestination =
  req
    "GetTopicRuleDestination"
    "fixture/GetTopicRuleDestination.yaml"

requestGetV2LoggingOptions :: GetV2LoggingOptions -> TestTree
requestGetV2LoggingOptions =
  req
    "GetV2LoggingOptions"
    "fixture/GetV2LoggingOptions.yaml"

requestListActiveViolations :: ListActiveViolations -> TestTree
requestListActiveViolations =
  req
    "ListActiveViolations"
    "fixture/ListActiveViolations.yaml"

requestListAttachedPolicies :: ListAttachedPolicies -> TestTree
requestListAttachedPolicies =
  req
    "ListAttachedPolicies"
    "fixture/ListAttachedPolicies.yaml"

requestListAuditFindings :: ListAuditFindings -> TestTree
requestListAuditFindings =
  req
    "ListAuditFindings"
    "fixture/ListAuditFindings.yaml"

requestListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutions -> TestTree
requestListAuditMitigationActionsExecutions =
  req
    "ListAuditMitigationActionsExecutions"
    "fixture/ListAuditMitigationActionsExecutions.yaml"

requestListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasks -> TestTree
requestListAuditMitigationActionsTasks =
  req
    "ListAuditMitigationActionsTasks"
    "fixture/ListAuditMitigationActionsTasks.yaml"

requestListAuditSuppressions :: ListAuditSuppressions -> TestTree
requestListAuditSuppressions =
  req
    "ListAuditSuppressions"
    "fixture/ListAuditSuppressions.yaml"

requestListAuditTasks :: ListAuditTasks -> TestTree
requestListAuditTasks =
  req
    "ListAuditTasks"
    "fixture/ListAuditTasks.yaml"

requestListAuthorizers :: ListAuthorizers -> TestTree
requestListAuthorizers =
  req
    "ListAuthorizers"
    "fixture/ListAuthorizers.yaml"

requestListBillingGroups :: ListBillingGroups -> TestTree
requestListBillingGroups =
  req
    "ListBillingGroups"
    "fixture/ListBillingGroups.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates =
  req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA =
  req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestListCustomMetrics :: ListCustomMetrics -> TestTree
requestListCustomMetrics =
  req
    "ListCustomMetrics"
    "fixture/ListCustomMetrics.yaml"

requestListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutions -> TestTree
requestListDetectMitigationActionsExecutions =
  req
    "ListDetectMitigationActionsExecutions"
    "fixture/ListDetectMitigationActionsExecutions.yaml"

requestListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasks -> TestTree
requestListDetectMitigationActionsTasks =
  req
    "ListDetectMitigationActionsTasks"
    "fixture/ListDetectMitigationActionsTasks.yaml"

requestListDimensions :: ListDimensions -> TestTree
requestListDimensions =
  req
    "ListDimensions"
    "fixture/ListDimensions.yaml"

requestListDomainConfigurations :: ListDomainConfigurations -> TestTree
requestListDomainConfigurations =
  req
    "ListDomainConfigurations"
    "fixture/ListDomainConfigurations.yaml"

requestListFleetMetrics :: ListFleetMetrics -> TestTree
requestListFleetMetrics =
  req
    "ListFleetMetrics"
    "fixture/ListFleetMetrics.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices =
  req
    "ListIndices"
    "fixture/ListIndices.yaml"

requestListJobExecutionsForJob :: ListJobExecutionsForJob -> TestTree
requestListJobExecutionsForJob =
  req
    "ListJobExecutionsForJob"
    "fixture/ListJobExecutionsForJob.yaml"

requestListJobExecutionsForThing :: ListJobExecutionsForThing -> TestTree
requestListJobExecutionsForThing =
  req
    "ListJobExecutionsForThing"
    "fixture/ListJobExecutionsForThing.yaml"

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListManagedJobTemplates :: ListManagedJobTemplates -> TestTree
requestListManagedJobTemplates =
  req
    "ListManagedJobTemplates"
    "fixture/ListManagedJobTemplates.yaml"

requestListMetricValues :: ListMetricValues -> TestTree
requestListMetricValues =
  req
    "ListMetricValues"
    "fixture/ListMetricValues.yaml"

requestListMitigationActions :: ListMitigationActions -> TestTree
requestListMitigationActions =
  req
    "ListMitigationActions"
    "fixture/ListMitigationActions.yaml"

requestListOTAUpdates :: ListOTAUpdates -> TestTree
requestListOTAUpdates =
  req
    "ListOTAUpdates"
    "fixture/ListOTAUpdates.yaml"

requestListOutgoingCertificates :: ListOutgoingCertificates -> TestTree
requestListOutgoingCertificates =
  req
    "ListOutgoingCertificates"
    "fixture/ListOutgoingCertificates.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings =
  req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestListProvisioningTemplateVersions :: ListProvisioningTemplateVersions -> TestTree
requestListProvisioningTemplateVersions =
  req
    "ListProvisioningTemplateVersions"
    "fixture/ListProvisioningTemplateVersions.yaml"

requestListProvisioningTemplates :: ListProvisioningTemplates -> TestTree
requestListProvisioningTemplates =
  req
    "ListProvisioningTemplates"
    "fixture/ListProvisioningTemplates.yaml"

requestListRelatedResourcesForAuditFinding :: ListRelatedResourcesForAuditFinding -> TestTree
requestListRelatedResourcesForAuditFinding =
  req
    "ListRelatedResourcesForAuditFinding"
    "fixture/ListRelatedResourcesForAuditFinding.yaml"

requestListRoleAliases :: ListRoleAliases -> TestTree
requestListRoleAliases =
  req
    "ListRoleAliases"
    "fixture/ListRoleAliases.yaml"

requestListScheduledAudits :: ListScheduledAudits -> TestTree
requestListScheduledAudits =
  req
    "ListScheduledAudits"
    "fixture/ListScheduledAudits.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestListSecurityProfilesForTarget :: ListSecurityProfilesForTarget -> TestTree
requestListSecurityProfilesForTarget =
  req
    "ListSecurityProfilesForTarget"
    "fixture/ListSecurityProfilesForTarget.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestListTargetsForSecurityProfile :: ListTargetsForSecurityProfile -> TestTree
requestListTargetsForSecurityProfile =
  req
    "ListTargetsForSecurityProfile"
    "fixture/ListTargetsForSecurityProfile.yaml"

requestListThingGroups :: ListThingGroups -> TestTree
requestListThingGroups =
  req
    "ListThingGroups"
    "fixture/ListThingGroups.yaml"

requestListThingGroupsForThing :: ListThingGroupsForThing -> TestTree
requestListThingGroupsForThing =
  req
    "ListThingGroupsForThing"
    "fixture/ListThingGroupsForThing.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals =
  req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestListThingRegistrationTaskReports :: ListThingRegistrationTaskReports -> TestTree
requestListThingRegistrationTaskReports =
  req
    "ListThingRegistrationTaskReports"
    "fixture/ListThingRegistrationTaskReports.yaml"

requestListThingRegistrationTasks :: ListThingRegistrationTasks -> TestTree
requestListThingRegistrationTasks =
  req
    "ListThingRegistrationTasks"
    "fixture/ListThingRegistrationTasks.yaml"

requestListThingTypes :: ListThingTypes -> TestTree
requestListThingTypes =
  req
    "ListThingTypes"
    "fixture/ListThingTypes.yaml"

requestListThings :: ListThings -> TestTree
requestListThings =
  req
    "ListThings"
    "fixture/ListThings.yaml"

requestListThingsInBillingGroup :: ListThingsInBillingGroup -> TestTree
requestListThingsInBillingGroup =
  req
    "ListThingsInBillingGroup"
    "fixture/ListThingsInBillingGroup.yaml"

requestListThingsInThingGroup :: ListThingsInThingGroup -> TestTree
requestListThingsInThingGroup =
  req
    "ListThingsInThingGroup"
    "fixture/ListThingsInThingGroup.yaml"

requestListTopicRuleDestinations :: ListTopicRuleDestinations -> TestTree
requestListTopicRuleDestinations =
  req
    "ListTopicRuleDestinations"
    "fixture/ListTopicRuleDestinations.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules =
  req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestListV2LoggingLevels :: ListV2LoggingLevels -> TestTree
requestListV2LoggingLevels =
  req
    "ListV2LoggingLevels"
    "fixture/ListV2LoggingLevels.yaml"

requestListViolationEvents :: ListViolationEvents -> TestTree
requestListViolationEvents =
  req
    "ListViolationEvents"
    "fixture/ListViolationEvents.yaml"

requestPutVerificationStateOnViolation :: PutVerificationStateOnViolation -> TestTree
requestPutVerificationStateOnViolation =
  req
    "PutVerificationStateOnViolation"
    "fixture/PutVerificationStateOnViolation.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate =
  req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestRegisterCertificateWithoutCA :: RegisterCertificateWithoutCA -> TestTree
requestRegisterCertificateWithoutCA =
  req
    "RegisterCertificateWithoutCA"
    "fixture/RegisterCertificateWithoutCA.yaml"

requestRegisterThing :: RegisterThing -> TestTree
requestRegisterThing =
  req
    "RegisterThing"
    "fixture/RegisterThing.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer =
  req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestRemoveThingFromBillingGroup :: RemoveThingFromBillingGroup -> TestTree
requestRemoveThingFromBillingGroup =
  req
    "RemoveThingFromBillingGroup"
    "fixture/RemoveThingFromBillingGroup.yaml"

requestRemoveThingFromThingGroup :: RemoveThingFromThingGroup -> TestTree
requestRemoveThingFromThingGroup =
  req
    "RemoveThingFromThingGroup"
    "fixture/RemoveThingFromThingGroup.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule =
  req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestSearchIndex :: SearchIndex -> TestTree
requestSearchIndex =
  req
    "SearchIndex"
    "fixture/SearchIndex.yaml"

requestSetDefaultAuthorizer :: SetDefaultAuthorizer -> TestTree
requestSetDefaultAuthorizer =
  req
    "SetDefaultAuthorizer"
    "fixture/SetDefaultAuthorizer.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions =
  req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestSetV2LoggingLevel :: SetV2LoggingLevel -> TestTree
requestSetV2LoggingLevel =
  req
    "SetV2LoggingLevel"
    "fixture/SetV2LoggingLevel.yaml"

requestSetV2LoggingOptions :: SetV2LoggingOptions -> TestTree
requestSetV2LoggingOptions =
  req
    "SetV2LoggingOptions"
    "fixture/SetV2LoggingOptions.yaml"

requestStartAuditMitigationActionsTask :: StartAuditMitigationActionsTask -> TestTree
requestStartAuditMitigationActionsTask =
  req
    "StartAuditMitigationActionsTask"
    "fixture/StartAuditMitigationActionsTask.yaml"

requestStartDetectMitigationActionsTask :: StartDetectMitigationActionsTask -> TestTree
requestStartDetectMitigationActionsTask =
  req
    "StartDetectMitigationActionsTask"
    "fixture/StartDetectMitigationActionsTask.yaml"

requestStartOnDemandAuditTask :: StartOnDemandAuditTask -> TestTree
requestStartOnDemandAuditTask =
  req
    "StartOnDemandAuditTask"
    "fixture/StartOnDemandAuditTask.yaml"

requestStartThingRegistrationTask :: StartThingRegistrationTask -> TestTree
requestStartThingRegistrationTask =
  req
    "StartThingRegistrationTask"
    "fixture/StartThingRegistrationTask.yaml"

requestStopThingRegistrationTask :: StopThingRegistrationTask -> TestTree
requestStopThingRegistrationTask =
  req
    "StopThingRegistrationTask"
    "fixture/StopThingRegistrationTask.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestAuthorization :: TestAuthorization -> TestTree
requestTestAuthorization =
  req
    "TestAuthorization"
    "fixture/TestAuthorization.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate =
  req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccountAuditConfiguration :: UpdateAccountAuditConfiguration -> TestTree
requestUpdateAccountAuditConfiguration =
  req
    "UpdateAccountAuditConfiguration"
    "fixture/UpdateAccountAuditConfiguration.yaml"

requestUpdateAuditSuppression :: UpdateAuditSuppression -> TestTree
requestUpdateAuditSuppression =
  req
    "UpdateAuditSuppression"
    "fixture/UpdateAuditSuppression.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestUpdateBillingGroup :: UpdateBillingGroup -> TestTree
requestUpdateBillingGroup =
  req
    "UpdateBillingGroup"
    "fixture/UpdateBillingGroup.yaml"

requestUpdateCACertificate :: UpdateCACertificate -> TestTree
requestUpdateCACertificate =
  req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate =
  req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestUpdateCustomMetric :: UpdateCustomMetric -> TestTree
requestUpdateCustomMetric =
  req
    "UpdateCustomMetric"
    "fixture/UpdateCustomMetric.yaml"

requestUpdateDimension :: UpdateDimension -> TestTree
requestUpdateDimension =
  req
    "UpdateDimension"
    "fixture/UpdateDimension.yaml"

requestUpdateDomainConfiguration :: UpdateDomainConfiguration -> TestTree
requestUpdateDomainConfiguration =
  req
    "UpdateDomainConfiguration"
    "fixture/UpdateDomainConfiguration.yaml"

requestUpdateDynamicThingGroup :: UpdateDynamicThingGroup -> TestTree
requestUpdateDynamicThingGroup =
  req
    "UpdateDynamicThingGroup"
    "fixture/UpdateDynamicThingGroup.yaml"

requestUpdateEventConfigurations :: UpdateEventConfigurations -> TestTree
requestUpdateEventConfigurations =
  req
    "UpdateEventConfigurations"
    "fixture/UpdateEventConfigurations.yaml"

requestUpdateFleetMetric :: UpdateFleetMetric -> TestTree
requestUpdateFleetMetric =
  req
    "UpdateFleetMetric"
    "fixture/UpdateFleetMetric.yaml"

requestUpdateIndexingConfiguration :: UpdateIndexingConfiguration -> TestTree
requestUpdateIndexingConfiguration =
  req
    "UpdateIndexingConfiguration"
    "fixture/UpdateIndexingConfiguration.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestUpdateMitigationAction :: UpdateMitigationAction -> TestTree
requestUpdateMitigationAction =
  req
    "UpdateMitigationAction"
    "fixture/UpdateMitigationAction.yaml"

requestUpdateProvisioningTemplate :: UpdateProvisioningTemplate -> TestTree
requestUpdateProvisioningTemplate =
  req
    "UpdateProvisioningTemplate"
    "fixture/UpdateProvisioningTemplate.yaml"

requestUpdateRoleAlias :: UpdateRoleAlias -> TestTree
requestUpdateRoleAlias =
  req
    "UpdateRoleAlias"
    "fixture/UpdateRoleAlias.yaml"

requestUpdateScheduledAudit :: UpdateScheduledAudit -> TestTree
requestUpdateScheduledAudit =
  req
    "UpdateScheduledAudit"
    "fixture/UpdateScheduledAudit.yaml"

requestUpdateSecurityProfile :: UpdateSecurityProfile -> TestTree
requestUpdateSecurityProfile =
  req
    "UpdateSecurityProfile"
    "fixture/UpdateSecurityProfile.yaml"

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

requestUpdateThingGroup :: UpdateThingGroup -> TestTree
requestUpdateThingGroup =
  req
    "UpdateThingGroup"
    "fixture/UpdateThingGroup.yaml"

requestUpdateThingGroupsForThing :: UpdateThingGroupsForThing -> TestTree
requestUpdateThingGroupsForThing =
  req
    "UpdateThingGroupsForThing"
    "fixture/UpdateThingGroupsForThing.yaml"

requestUpdateTopicRuleDestination :: UpdateTopicRuleDestination -> TestTree
requestUpdateTopicRuleDestination =
  req
    "UpdateTopicRuleDestination"
    "fixture/UpdateTopicRuleDestination.yaml"

requestValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviors -> TestTree
requestValidateSecurityProfileBehaviors =
  req
    "ValidateSecurityProfileBehaviors"
    "fixture/ValidateSecurityProfileBehaviors.yaml"

-- Responses

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer =
  res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptCertificateTransfer)

responseAddThingToBillingGroup :: AddThingToBillingGroupResponse -> TestTree
responseAddThingToBillingGroup =
  res
    "AddThingToBillingGroupResponse"
    "fixture/AddThingToBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddThingToBillingGroup)

responseAddThingToThingGroup :: AddThingToThingGroupResponse -> TestTree
responseAddThingToThingGroup =
  res
    "AddThingToThingGroupResponse"
    "fixture/AddThingToThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddThingToThingGroup)

responseAssociateTargetsWithJob :: AssociateTargetsWithJobResponse -> TestTree
responseAssociateTargetsWithJob =
  res
    "AssociateTargetsWithJobResponse"
    "fixture/AssociateTargetsWithJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTargetsWithJob)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseAttachSecurityProfile :: AttachSecurityProfileResponse -> TestTree
responseAttachSecurityProfile =
  res
    "AttachSecurityProfileResponse"
    "fixture/AttachSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachSecurityProfile)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal =
  res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachThingPrincipal)

responseCancelAuditMitigationActionsTask :: CancelAuditMitigationActionsTaskResponse -> TestTree
responseCancelAuditMitigationActionsTask =
  res
    "CancelAuditMitigationActionsTaskResponse"
    "fixture/CancelAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelAuditMitigationActionsTask)

responseCancelAuditTask :: CancelAuditTaskResponse -> TestTree
responseCancelAuditTask =
  res
    "CancelAuditTaskResponse"
    "fixture/CancelAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelAuditTask)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer =
  res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCertificateTransfer)

responseCancelDetectMitigationActionsTask :: CancelDetectMitigationActionsTaskResponse -> TestTree
responseCancelDetectMitigationActionsTask =
  res
    "CancelDetectMitigationActionsTaskResponse"
    "fixture/CancelDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDetectMitigationActionsTask)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCancelJobExecution :: CancelJobExecutionResponse -> TestTree
responseCancelJobExecution =
  res
    "CancelJobExecutionResponse"
    "fixture/CancelJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJobExecution)

responseClearDefaultAuthorizer :: ClearDefaultAuthorizerResponse -> TestTree
responseClearDefaultAuthorizer =
  res
    "ClearDefaultAuthorizerResponse"
    "fixture/ClearDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClearDefaultAuthorizer)

responseConfirmTopicRuleDestination :: ConfirmTopicRuleDestinationResponse -> TestTree
responseConfirmTopicRuleDestination =
  res
    "ConfirmTopicRuleDestinationResponse"
    "fixture/ConfirmTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmTopicRuleDestination)

responseCreateAuditSuppression :: CreateAuditSuppressionResponse -> TestTree
responseCreateAuditSuppression =
  res
    "CreateAuditSuppressionResponse"
    "fixture/CreateAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuditSuppression)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBillingGroup)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr =
  res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateFromCsr)

responseCreateCustomMetric :: CreateCustomMetricResponse -> TestTree
responseCreateCustomMetric =
  res
    "CreateCustomMetricResponse"
    "fixture/CreateCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomMetric)

responseCreateDimension :: CreateDimensionResponse -> TestTree
responseCreateDimension =
  res
    "CreateDimensionResponse"
    "fixture/CreateDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDimension)

responseCreateDomainConfiguration :: CreateDomainConfigurationResponse -> TestTree
responseCreateDomainConfiguration =
  res
    "CreateDomainConfigurationResponse"
    "fixture/CreateDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainConfiguration)

responseCreateDynamicThingGroup :: CreateDynamicThingGroupResponse -> TestTree
responseCreateDynamicThingGroup =
  res
    "CreateDynamicThingGroupResponse"
    "fixture/CreateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDynamicThingGroup)

responseCreateFleetMetric :: CreateFleetMetricResponse -> TestTree
responseCreateFleetMetric =
  res
    "CreateFleetMetricResponse"
    "fixture/CreateFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleetMetric)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobTemplate)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate =
  res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeysAndCertificate)

responseCreateMitigationAction :: CreateMitigationActionResponse -> TestTree
responseCreateMitigationAction =
  res
    "CreateMitigationActionResponse"
    "fixture/CreateMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMitigationAction)

responseCreateOTAUpdate :: CreateOTAUpdateResponse -> TestTree
responseCreateOTAUpdate =
  res
    "CreateOTAUpdateResponse"
    "fixture/CreateOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOTAUpdate)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicyVersion)

responseCreateProvisioningClaim :: CreateProvisioningClaimResponse -> TestTree
responseCreateProvisioningClaim =
  res
    "CreateProvisioningClaimResponse"
    "fixture/CreateProvisioningClaimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningClaim)

responseCreateProvisioningTemplate :: CreateProvisioningTemplateResponse -> TestTree
responseCreateProvisioningTemplate =
  res
    "CreateProvisioningTemplateResponse"
    "fixture/CreateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningTemplate)

responseCreateProvisioningTemplateVersion :: CreateProvisioningTemplateVersionResponse -> TestTree
responseCreateProvisioningTemplateVersion =
  res
    "CreateProvisioningTemplateVersionResponse"
    "fixture/CreateProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningTemplateVersion)

responseCreateRoleAlias :: CreateRoleAliasResponse -> TestTree
responseCreateRoleAlias =
  res
    "CreateRoleAliasResponse"
    "fixture/CreateRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoleAlias)

responseCreateScheduledAudit :: CreateScheduledAuditResponse -> TestTree
responseCreateScheduledAudit =
  res
    "CreateScheduledAuditResponse"
    "fixture/CreateScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduledAudit)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityProfile)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStream)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing =
  res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThing)

responseCreateThingGroup :: CreateThingGroupResponse -> TestTree
responseCreateThingGroup =
  res
    "CreateThingGroupResponse"
    "fixture/CreateThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThingGroup)

responseCreateThingType :: CreateThingTypeResponse -> TestTree
responseCreateThingType =
  res
    "CreateThingTypeResponse"
    "fixture/CreateThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThingType)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule =
  res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopicRule)

responseCreateTopicRuleDestination :: CreateTopicRuleDestinationResponse -> TestTree
responseCreateTopicRuleDestination =
  res
    "CreateTopicRuleDestinationResponse"
    "fixture/CreateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopicRuleDestination)

responseDeleteAccountAuditConfiguration :: DeleteAccountAuditConfigurationResponse -> TestTree
responseDeleteAccountAuditConfiguration =
  res
    "DeleteAccountAuditConfigurationResponse"
    "fixture/DeleteAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAuditConfiguration)

responseDeleteAuditSuppression :: DeleteAuditSuppressionResponse -> TestTree
responseDeleteAuditSuppression =
  res
    "DeleteAuditSuppressionResponse"
    "fixture/DeleteAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuditSuppression)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthorizer)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBillingGroup)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate =
  res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCACertificate)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseDeleteCustomMetric :: DeleteCustomMetricResponse -> TestTree
responseDeleteCustomMetric =
  res
    "DeleteCustomMetricResponse"
    "fixture/DeleteCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomMetric)

responseDeleteDimension :: DeleteDimensionResponse -> TestTree
responseDeleteDimension =
  res
    "DeleteDimensionResponse"
    "fixture/DeleteDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDimension)

responseDeleteDomainConfiguration :: DeleteDomainConfigurationResponse -> TestTree
responseDeleteDomainConfiguration =
  res
    "DeleteDomainConfigurationResponse"
    "fixture/DeleteDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainConfiguration)

responseDeleteDynamicThingGroup :: DeleteDynamicThingGroupResponse -> TestTree
responseDeleteDynamicThingGroup =
  res
    "DeleteDynamicThingGroupResponse"
    "fixture/DeleteDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDynamicThingGroup)

responseDeleteFleetMetric :: DeleteFleetMetricResponse -> TestTree
responseDeleteFleetMetric =
  res
    "DeleteFleetMetricResponse"
    "fixture/DeleteFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetMetric)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseDeleteJobExecution :: DeleteJobExecutionResponse -> TestTree
responseDeleteJobExecution =
  res
    "DeleteJobExecutionResponse"
    "fixture/DeleteJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobExecution)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobTemplate)

responseDeleteMitigationAction :: DeleteMitigationActionResponse -> TestTree
responseDeleteMitigationAction =
  res
    "DeleteMitigationActionResponse"
    "fixture/DeleteMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMitigationAction)

responseDeleteOTAUpdate :: DeleteOTAUpdateResponse -> TestTree
responseDeleteOTAUpdate =
  res
    "DeleteOTAUpdateResponse"
    "fixture/DeleteOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOTAUpdate)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicyVersion)

responseDeleteProvisioningTemplate :: DeleteProvisioningTemplateResponse -> TestTree
responseDeleteProvisioningTemplate =
  res
    "DeleteProvisioningTemplateResponse"
    "fixture/DeleteProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningTemplate)

responseDeleteProvisioningTemplateVersion :: DeleteProvisioningTemplateVersionResponse -> TestTree
responseDeleteProvisioningTemplateVersion =
  res
    "DeleteProvisioningTemplateVersionResponse"
    "fixture/DeleteProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningTemplateVersion)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode =
  res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistrationCode)

responseDeleteRoleAlias :: DeleteRoleAliasResponse -> TestTree
responseDeleteRoleAlias =
  res
    "DeleteRoleAliasResponse"
    "fixture/DeleteRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoleAlias)

responseDeleteScheduledAudit :: DeleteScheduledAuditResponse -> TestTree
responseDeleteScheduledAudit =
  res
    "DeleteScheduledAuditResponse"
    "fixture/DeleteScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAudit)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityProfile)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStream)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing =
  res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThing)

responseDeleteThingGroup :: DeleteThingGroupResponse -> TestTree
responseDeleteThingGroup =
  res
    "DeleteThingGroupResponse"
    "fixture/DeleteThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingGroup)

responseDeleteThingType :: DeleteThingTypeResponse -> TestTree
responseDeleteThingType =
  res
    "DeleteThingTypeResponse"
    "fixture/DeleteThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingType)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule =
  res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopicRule)

responseDeleteTopicRuleDestination :: DeleteTopicRuleDestinationResponse -> TestTree
responseDeleteTopicRuleDestination =
  res
    "DeleteTopicRuleDestinationResponse"
    "fixture/DeleteTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopicRuleDestination)

responseDeleteV2LoggingLevel :: DeleteV2LoggingLevelResponse -> TestTree
responseDeleteV2LoggingLevel =
  res
    "DeleteV2LoggingLevelResponse"
    "fixture/DeleteV2LoggingLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteV2LoggingLevel)

responseDeprecateThingType :: DeprecateThingTypeResponse -> TestTree
responseDeprecateThingType =
  res
    "DeprecateThingTypeResponse"
    "fixture/DeprecateThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateThingType)

responseDescribeAccountAuditConfiguration :: DescribeAccountAuditConfigurationResponse -> TestTree
responseDescribeAccountAuditConfiguration =
  res
    "DescribeAccountAuditConfigurationResponse"
    "fixture/DescribeAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAuditConfiguration)

responseDescribeAuditFinding :: DescribeAuditFindingResponse -> TestTree
responseDescribeAuditFinding =
  res
    "DescribeAuditFindingResponse"
    "fixture/DescribeAuditFindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditFinding)

responseDescribeAuditMitigationActionsTask :: DescribeAuditMitigationActionsTaskResponse -> TestTree
responseDescribeAuditMitigationActionsTask =
  res
    "DescribeAuditMitigationActionsTaskResponse"
    "fixture/DescribeAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditMitigationActionsTask)

responseDescribeAuditSuppression :: DescribeAuditSuppressionResponse -> TestTree
responseDescribeAuditSuppression =
  res
    "DescribeAuditSuppressionResponse"
    "fixture/DescribeAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditSuppression)

responseDescribeAuditTask :: DescribeAuditTaskResponse -> TestTree
responseDescribeAuditTask =
  res
    "DescribeAuditTaskResponse"
    "fixture/DescribeAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditTask)

responseDescribeAuthorizer :: DescribeAuthorizerResponse -> TestTree
responseDescribeAuthorizer =
  res
    "DescribeAuthorizerResponse"
    "fixture/DescribeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuthorizer)

responseDescribeBillingGroup :: DescribeBillingGroupResponse -> TestTree
responseDescribeBillingGroup =
  res
    "DescribeBillingGroupResponse"
    "fixture/DescribeBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBillingGroup)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate =
  res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCACertificate)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseDescribeCustomMetric :: DescribeCustomMetricResponse -> TestTree
responseDescribeCustomMetric =
  res
    "DescribeCustomMetricResponse"
    "fixture/DescribeCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomMetric)

responseDescribeDefaultAuthorizer :: DescribeDefaultAuthorizerResponse -> TestTree
responseDescribeDefaultAuthorizer =
  res
    "DescribeDefaultAuthorizerResponse"
    "fixture/DescribeDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultAuthorizer)

responseDescribeDetectMitigationActionsTask :: DescribeDetectMitigationActionsTaskResponse -> TestTree
responseDescribeDetectMitigationActionsTask =
  res
    "DescribeDetectMitigationActionsTaskResponse"
    "fixture/DescribeDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectMitigationActionsTask)

responseDescribeDimension :: DescribeDimensionResponse -> TestTree
responseDescribeDimension =
  res
    "DescribeDimensionResponse"
    "fixture/DescribeDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDimension)

responseDescribeDomainConfiguration :: DescribeDomainConfigurationResponse -> TestTree
responseDescribeDomainConfiguration =
  res
    "DescribeDomainConfigurationResponse"
    "fixture/DescribeDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainConfiguration)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseDescribeEventConfigurations :: DescribeEventConfigurationsResponse -> TestTree
responseDescribeEventConfigurations =
  res
    "DescribeEventConfigurationsResponse"
    "fixture/DescribeEventConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventConfigurations)

responseDescribeFleetMetric :: DescribeFleetMetricResponse -> TestTree
responseDescribeFleetMetric =
  res
    "DescribeFleetMetricResponse"
    "fixture/DescribeFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetMetric)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndex)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseDescribeJobExecution :: DescribeJobExecutionResponse -> TestTree
responseDescribeJobExecution =
  res
    "DescribeJobExecutionResponse"
    "fixture/DescribeJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobExecution)

responseDescribeJobTemplate :: DescribeJobTemplateResponse -> TestTree
responseDescribeJobTemplate =
  res
    "DescribeJobTemplateResponse"
    "fixture/DescribeJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobTemplate)

responseDescribeManagedJobTemplate :: DescribeManagedJobTemplateResponse -> TestTree
responseDescribeManagedJobTemplate =
  res
    "DescribeManagedJobTemplateResponse"
    "fixture/DescribeManagedJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedJobTemplate)

responseDescribeMitigationAction :: DescribeMitigationActionResponse -> TestTree
responseDescribeMitigationAction =
  res
    "DescribeMitigationActionResponse"
    "fixture/DescribeMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMitigationAction)

responseDescribeProvisioningTemplate :: DescribeProvisioningTemplateResponse -> TestTree
responseDescribeProvisioningTemplate =
  res
    "DescribeProvisioningTemplateResponse"
    "fixture/DescribeProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningTemplate)

responseDescribeProvisioningTemplateVersion :: DescribeProvisioningTemplateVersionResponse -> TestTree
responseDescribeProvisioningTemplateVersion =
  res
    "DescribeProvisioningTemplateVersionResponse"
    "fixture/DescribeProvisioningTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningTemplateVersion)

responseDescribeRoleAlias :: DescribeRoleAliasResponse -> TestTree
responseDescribeRoleAlias =
  res
    "DescribeRoleAliasResponse"
    "fixture/DescribeRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoleAlias)

responseDescribeScheduledAudit :: DescribeScheduledAuditResponse -> TestTree
responseDescribeScheduledAudit =
  res
    "DescribeScheduledAuditResponse"
    "fixture/DescribeScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledAudit)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityProfile)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing =
  res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThing)

responseDescribeThingGroup :: DescribeThingGroupResponse -> TestTree
responseDescribeThingGroup =
  res
    "DescribeThingGroupResponse"
    "fixture/DescribeThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingGroup)

responseDescribeThingRegistrationTask :: DescribeThingRegistrationTaskResponse -> TestTree
responseDescribeThingRegistrationTask =
  res
    "DescribeThingRegistrationTaskResponse"
    "fixture/DescribeThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingRegistrationTask)

responseDescribeThingType :: DescribeThingTypeResponse -> TestTree
responseDescribeThingType =
  res
    "DescribeThingTypeResponse"
    "fixture/DescribeThingTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThingType)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseDetachSecurityProfile :: DetachSecurityProfileResponse -> TestTree
responseDetachSecurityProfile =
  res
    "DetachSecurityProfileResponse"
    "fixture/DetachSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachSecurityProfile)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal =
  res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachThingPrincipal)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule =
  res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableTopicRule)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule =
  res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableTopicRule)

responseGetBehaviorModelTrainingSummaries :: GetBehaviorModelTrainingSummariesResponse -> TestTree
responseGetBehaviorModelTrainingSummaries =
  res
    "GetBehaviorModelTrainingSummariesResponse"
    "fixture/GetBehaviorModelTrainingSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBehaviorModelTrainingSummaries)

responseGetBucketsAggregation :: GetBucketsAggregationResponse -> TestTree
responseGetBucketsAggregation =
  res
    "GetBucketsAggregationResponse"
    "fixture/GetBucketsAggregationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketsAggregation)

responseGetCardinality :: GetCardinalityResponse -> TestTree
responseGetCardinality =
  res
    "GetCardinalityResponse"
    "fixture/GetCardinalityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCardinality)

responseGetEffectivePolicies :: GetEffectivePoliciesResponse -> TestTree
responseGetEffectivePolicies =
  res
    "GetEffectivePoliciesResponse"
    "fixture/GetEffectivePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEffectivePolicies)

responseGetIndexingConfiguration :: GetIndexingConfigurationResponse -> TestTree
responseGetIndexingConfiguration =
  res
    "GetIndexingConfigurationResponse"
    "fixture/GetIndexingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIndexingConfiguration)

responseGetJobDocument :: GetJobDocumentResponse -> TestTree
responseGetJobDocument =
  res
    "GetJobDocumentResponse"
    "fixture/GetJobDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobDocument)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingOptions)

responseGetOTAUpdate :: GetOTAUpdateResponse -> TestTree
responseGetOTAUpdate =
  res
    "GetOTAUpdateResponse"
    "fixture/GetOTAUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOTAUpdate)

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

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicyVersion)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode =
  res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistrationCode)

responseGetStatistics :: GetStatisticsResponse -> TestTree
responseGetStatistics =
  res
    "GetStatisticsResponse"
    "fixture/GetStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStatistics)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule =
  res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicRule)

responseGetTopicRuleDestination :: GetTopicRuleDestinationResponse -> TestTree
responseGetTopicRuleDestination =
  res
    "GetTopicRuleDestinationResponse"
    "fixture/GetTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicRuleDestination)

responseGetV2LoggingOptions :: GetV2LoggingOptionsResponse -> TestTree
responseGetV2LoggingOptions =
  res
    "GetV2LoggingOptionsResponse"
    "fixture/GetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetV2LoggingOptions)

responseListActiveViolations :: ListActiveViolationsResponse -> TestTree
responseListActiveViolations =
  res
    "ListActiveViolationsResponse"
    "fixture/ListActiveViolationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActiveViolations)

responseListAttachedPolicies :: ListAttachedPoliciesResponse -> TestTree
responseListAttachedPolicies =
  res
    "ListAttachedPoliciesResponse"
    "fixture/ListAttachedPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedPolicies)

responseListAuditFindings :: ListAuditFindingsResponse -> TestTree
responseListAuditFindings =
  res
    "ListAuditFindingsResponse"
    "fixture/ListAuditFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditFindings)

responseListAuditMitigationActionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> TestTree
responseListAuditMitigationActionsExecutions =
  res
    "ListAuditMitigationActionsExecutionsResponse"
    "fixture/ListAuditMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditMitigationActionsExecutions)

responseListAuditMitigationActionsTasks :: ListAuditMitigationActionsTasksResponse -> TestTree
responseListAuditMitigationActionsTasks =
  res
    "ListAuditMitigationActionsTasksResponse"
    "fixture/ListAuditMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditMitigationActionsTasks)

responseListAuditSuppressions :: ListAuditSuppressionsResponse -> TestTree
responseListAuditSuppressions =
  res
    "ListAuditSuppressionsResponse"
    "fixture/ListAuditSuppressionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditSuppressions)

responseListAuditTasks :: ListAuditTasksResponse -> TestTree
responseListAuditTasks =
  res
    "ListAuditTasksResponse"
    "fixture/ListAuditTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuditTasks)

responseListAuthorizers :: ListAuthorizersResponse -> TestTree
responseListAuthorizers =
  res
    "ListAuthorizersResponse"
    "fixture/ListAuthorizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAuthorizers)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBillingGroups)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates =
  res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCACertificates)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA =
  res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificatesByCA)

responseListCustomMetrics :: ListCustomMetricsResponse -> TestTree
responseListCustomMetrics =
  res
    "ListCustomMetricsResponse"
    "fixture/ListCustomMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomMetrics)

responseListDetectMitigationActionsExecutions :: ListDetectMitigationActionsExecutionsResponse -> TestTree
responseListDetectMitigationActionsExecutions =
  res
    "ListDetectMitigationActionsExecutionsResponse"
    "fixture/ListDetectMitigationActionsExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectMitigationActionsExecutions)

responseListDetectMitigationActionsTasks :: ListDetectMitigationActionsTasksResponse -> TestTree
responseListDetectMitigationActionsTasks =
  res
    "ListDetectMitigationActionsTasksResponse"
    "fixture/ListDetectMitigationActionsTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectMitigationActionsTasks)

responseListDimensions :: ListDimensionsResponse -> TestTree
responseListDimensions =
  res
    "ListDimensionsResponse"
    "fixture/ListDimensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDimensions)

responseListDomainConfigurations :: ListDomainConfigurationsResponse -> TestTree
responseListDomainConfigurations =
  res
    "ListDomainConfigurationsResponse"
    "fixture/ListDomainConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainConfigurations)

responseListFleetMetrics :: ListFleetMetricsResponse -> TestTree
responseListFleetMetrics =
  res
    "ListFleetMetricsResponse"
    "fixture/ListFleetMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleetMetrics)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndices)

responseListJobExecutionsForJob :: ListJobExecutionsForJobResponse -> TestTree
responseListJobExecutionsForJob =
  res
    "ListJobExecutionsForJobResponse"
    "fixture/ListJobExecutionsForJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobExecutionsForJob)

responseListJobExecutionsForThing :: ListJobExecutionsForThingResponse -> TestTree
responseListJobExecutionsForThing =
  res
    "ListJobExecutionsForThingResponse"
    "fixture/ListJobExecutionsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobExecutionsForThing)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobTemplates)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListManagedJobTemplates :: ListManagedJobTemplatesResponse -> TestTree
responseListManagedJobTemplates =
  res
    "ListManagedJobTemplatesResponse"
    "fixture/ListManagedJobTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedJobTemplates)

responseListMetricValues :: ListMetricValuesResponse -> TestTree
responseListMetricValues =
  res
    "ListMetricValuesResponse"
    "fixture/ListMetricValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricValues)

responseListMitigationActions :: ListMitigationActionsResponse -> TestTree
responseListMitigationActions =
  res
    "ListMitigationActionsResponse"
    "fixture/ListMitigationActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMitigationActions)

responseListOTAUpdates :: ListOTAUpdatesResponse -> TestTree
responseListOTAUpdates =
  res
    "ListOTAUpdatesResponse"
    "fixture/ListOTAUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOTAUpdates)

responseListOutgoingCertificates :: ListOutgoingCertificatesResponse -> TestTree
responseListOutgoingCertificates =
  res
    "ListOutgoingCertificatesResponse"
    "fixture/ListOutgoingCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOutgoingCertificates)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyVersions)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings =
  res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipalThings)

responseListProvisioningTemplateVersions :: ListProvisioningTemplateVersionsResponse -> TestTree
responseListProvisioningTemplateVersions =
  res
    "ListProvisioningTemplateVersionsResponse"
    "fixture/ListProvisioningTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningTemplateVersions)

responseListProvisioningTemplates :: ListProvisioningTemplatesResponse -> TestTree
responseListProvisioningTemplates =
  res
    "ListProvisioningTemplatesResponse"
    "fixture/ListProvisioningTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningTemplates)

responseListRelatedResourcesForAuditFinding :: ListRelatedResourcesForAuditFindingResponse -> TestTree
responseListRelatedResourcesForAuditFinding =
  res
    "ListRelatedResourcesForAuditFindingResponse"
    "fixture/ListRelatedResourcesForAuditFindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRelatedResourcesForAuditFinding)

responseListRoleAliases :: ListRoleAliasesResponse -> TestTree
responseListRoleAliases =
  res
    "ListRoleAliasesResponse"
    "fixture/ListRoleAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoleAliases)

responseListScheduledAudits :: ListScheduledAuditsResponse -> TestTree
responseListScheduledAudits =
  res
    "ListScheduledAuditsResponse"
    "fixture/ListScheduledAuditsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScheduledAudits)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfiles)

responseListSecurityProfilesForTarget :: ListSecurityProfilesForTargetResponse -> TestTree
responseListSecurityProfilesForTarget =
  res
    "ListSecurityProfilesForTargetResponse"
    "fixture/ListSecurityProfilesForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfilesForTarget)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForPolicy)

responseListTargetsForSecurityProfile :: ListTargetsForSecurityProfileResponse -> TestTree
responseListTargetsForSecurityProfile =
  res
    "ListTargetsForSecurityProfileResponse"
    "fixture/ListTargetsForSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForSecurityProfile)

responseListThingGroups :: ListThingGroupsResponse -> TestTree
responseListThingGroups =
  res
    "ListThingGroupsResponse"
    "fixture/ListThingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingGroups)

responseListThingGroupsForThing :: ListThingGroupsForThingResponse -> TestTree
responseListThingGroupsForThing =
  res
    "ListThingGroupsForThingResponse"
    "fixture/ListThingGroupsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingGroupsForThing)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals =
  res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingPrincipals)

responseListThingRegistrationTaskReports :: ListThingRegistrationTaskReportsResponse -> TestTree
responseListThingRegistrationTaskReports =
  res
    "ListThingRegistrationTaskReportsResponse"
    "fixture/ListThingRegistrationTaskReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingRegistrationTaskReports)

responseListThingRegistrationTasks :: ListThingRegistrationTasksResponse -> TestTree
responseListThingRegistrationTasks =
  res
    "ListThingRegistrationTasksResponse"
    "fixture/ListThingRegistrationTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingRegistrationTasks)

responseListThingTypes :: ListThingTypesResponse -> TestTree
responseListThingTypes =
  res
    "ListThingTypesResponse"
    "fixture/ListThingTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingTypes)

responseListThings :: ListThingsResponse -> TestTree
responseListThings =
  res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThings)

responseListThingsInBillingGroup :: ListThingsInBillingGroupResponse -> TestTree
responseListThingsInBillingGroup =
  res
    "ListThingsInBillingGroupResponse"
    "fixture/ListThingsInBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingsInBillingGroup)

responseListThingsInThingGroup :: ListThingsInThingGroupResponse -> TestTree
responseListThingsInThingGroup =
  res
    "ListThingsInThingGroupResponse"
    "fixture/ListThingsInThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThingsInThingGroup)

responseListTopicRuleDestinations :: ListTopicRuleDestinationsResponse -> TestTree
responseListTopicRuleDestinations =
  res
    "ListTopicRuleDestinationsResponse"
    "fixture/ListTopicRuleDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicRuleDestinations)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules =
  res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicRules)

responseListV2LoggingLevels :: ListV2LoggingLevelsResponse -> TestTree
responseListV2LoggingLevels =
  res
    "ListV2LoggingLevelsResponse"
    "fixture/ListV2LoggingLevelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListV2LoggingLevels)

responseListViolationEvents :: ListViolationEventsResponse -> TestTree
responseListViolationEvents =
  res
    "ListViolationEventsResponse"
    "fixture/ListViolationEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListViolationEvents)

responsePutVerificationStateOnViolation :: PutVerificationStateOnViolationResponse -> TestTree
responsePutVerificationStateOnViolation =
  res
    "PutVerificationStateOnViolationResponse"
    "fixture/PutVerificationStateOnViolationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVerificationStateOnViolation)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate =
  res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCACertificate)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificate)

responseRegisterCertificateWithoutCA :: RegisterCertificateWithoutCAResponse -> TestTree
responseRegisterCertificateWithoutCA =
  res
    "RegisterCertificateWithoutCAResponse"
    "fixture/RegisterCertificateWithoutCAResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificateWithoutCA)

responseRegisterThing :: RegisterThingResponse -> TestTree
responseRegisterThing =
  res
    "RegisterThingResponse"
    "fixture/RegisterThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterThing)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer =
  res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectCertificateTransfer)

responseRemoveThingFromBillingGroup :: RemoveThingFromBillingGroupResponse -> TestTree
responseRemoveThingFromBillingGroup =
  res
    "RemoveThingFromBillingGroupResponse"
    "fixture/RemoveThingFromBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveThingFromBillingGroup)

responseRemoveThingFromThingGroup :: RemoveThingFromThingGroupResponse -> TestTree
responseRemoveThingFromThingGroup =
  res
    "RemoveThingFromThingGroupResponse"
    "fixture/RemoveThingFromThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveThingFromThingGroup)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule =
  res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplaceTopicRule)

responseSearchIndex :: SearchIndexResponse -> TestTree
responseSearchIndex =
  res
    "SearchIndexResponse"
    "fixture/SearchIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchIndex)

responseSetDefaultAuthorizer :: SetDefaultAuthorizerResponse -> TestTree
responseSetDefaultAuthorizer =
  res
    "SetDefaultAuthorizerResponse"
    "fixture/SetDefaultAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultAuthorizer)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultPolicyVersion)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions =
  res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoggingOptions)

responseSetV2LoggingLevel :: SetV2LoggingLevelResponse -> TestTree
responseSetV2LoggingLevel =
  res
    "SetV2LoggingLevelResponse"
    "fixture/SetV2LoggingLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetV2LoggingLevel)

responseSetV2LoggingOptions :: SetV2LoggingOptionsResponse -> TestTree
responseSetV2LoggingOptions =
  res
    "SetV2LoggingOptionsResponse"
    "fixture/SetV2LoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetV2LoggingOptions)

responseStartAuditMitigationActionsTask :: StartAuditMitigationActionsTaskResponse -> TestTree
responseStartAuditMitigationActionsTask =
  res
    "StartAuditMitigationActionsTaskResponse"
    "fixture/StartAuditMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAuditMitigationActionsTask)

responseStartDetectMitigationActionsTask :: StartDetectMitigationActionsTaskResponse -> TestTree
responseStartDetectMitigationActionsTask =
  res
    "StartDetectMitigationActionsTaskResponse"
    "fixture/StartDetectMitigationActionsTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDetectMitigationActionsTask)

responseStartOnDemandAuditTask :: StartOnDemandAuditTaskResponse -> TestTree
responseStartOnDemandAuditTask =
  res
    "StartOnDemandAuditTaskResponse"
    "fixture/StartOnDemandAuditTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandAuditTask)

responseStartThingRegistrationTask :: StartThingRegistrationTaskResponse -> TestTree
responseStartThingRegistrationTask =
  res
    "StartThingRegistrationTaskResponse"
    "fixture/StartThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartThingRegistrationTask)

responseStopThingRegistrationTask :: StopThingRegistrationTaskResponse -> TestTree
responseStopThingRegistrationTask =
  res
    "StopThingRegistrationTaskResponse"
    "fixture/StopThingRegistrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopThingRegistrationTask)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestAuthorization :: TestAuthorizationResponse -> TestTree
responseTestAuthorization =
  res
    "TestAuthorizationResponse"
    "fixture/TestAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestAuthorization)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeAuthorizer)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate =
  res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferCertificate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccountAuditConfiguration :: UpdateAccountAuditConfigurationResponse -> TestTree
responseUpdateAccountAuditConfiguration =
  res
    "UpdateAccountAuditConfigurationResponse"
    "fixture/UpdateAccountAuditConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountAuditConfiguration)

responseUpdateAuditSuppression :: UpdateAuditSuppressionResponse -> TestTree
responseUpdateAuditSuppression =
  res
    "UpdateAuditSuppressionResponse"
    "fixture/UpdateAuditSuppressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuditSuppression)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthorizer)

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBillingGroup)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate =
  res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCACertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificate)

responseUpdateCustomMetric :: UpdateCustomMetricResponse -> TestTree
responseUpdateCustomMetric =
  res
    "UpdateCustomMetricResponse"
    "fixture/UpdateCustomMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomMetric)

responseUpdateDimension :: UpdateDimensionResponse -> TestTree
responseUpdateDimension =
  res
    "UpdateDimensionResponse"
    "fixture/UpdateDimensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDimension)

responseUpdateDomainConfiguration :: UpdateDomainConfigurationResponse -> TestTree
responseUpdateDomainConfiguration =
  res
    "UpdateDomainConfigurationResponse"
    "fixture/UpdateDomainConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainConfiguration)

responseUpdateDynamicThingGroup :: UpdateDynamicThingGroupResponse -> TestTree
responseUpdateDynamicThingGroup =
  res
    "UpdateDynamicThingGroupResponse"
    "fixture/UpdateDynamicThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDynamicThingGroup)

responseUpdateEventConfigurations :: UpdateEventConfigurationsResponse -> TestTree
responseUpdateEventConfigurations =
  res
    "UpdateEventConfigurationsResponse"
    "fixture/UpdateEventConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventConfigurations)

responseUpdateFleetMetric :: UpdateFleetMetricResponse -> TestTree
responseUpdateFleetMetric =
  res
    "UpdateFleetMetricResponse"
    "fixture/UpdateFleetMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetMetric)

responseUpdateIndexingConfiguration :: UpdateIndexingConfigurationResponse -> TestTree
responseUpdateIndexingConfiguration =
  res
    "UpdateIndexingConfigurationResponse"
    "fixture/UpdateIndexingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIndexingConfiguration)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJob)

responseUpdateMitigationAction :: UpdateMitigationActionResponse -> TestTree
responseUpdateMitigationAction =
  res
    "UpdateMitigationActionResponse"
    "fixture/UpdateMitigationActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMitigationAction)

responseUpdateProvisioningTemplate :: UpdateProvisioningTemplateResponse -> TestTree
responseUpdateProvisioningTemplate =
  res
    "UpdateProvisioningTemplateResponse"
    "fixture/UpdateProvisioningTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisioningTemplate)

responseUpdateRoleAlias :: UpdateRoleAliasResponse -> TestTree
responseUpdateRoleAlias =
  res
    "UpdateRoleAliasResponse"
    "fixture/UpdateRoleAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoleAlias)

responseUpdateScheduledAudit :: UpdateScheduledAuditResponse -> TestTree
responseUpdateScheduledAudit =
  res
    "UpdateScheduledAuditResponse"
    "fixture/UpdateScheduledAuditResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScheduledAudit)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityProfile)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStream)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing =
  res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThing)

responseUpdateThingGroup :: UpdateThingGroupResponse -> TestTree
responseUpdateThingGroup =
  res
    "UpdateThingGroupResponse"
    "fixture/UpdateThingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingGroup)

responseUpdateThingGroupsForThing :: UpdateThingGroupsForThingResponse -> TestTree
responseUpdateThingGroupsForThing =
  res
    "UpdateThingGroupsForThingResponse"
    "fixture/UpdateThingGroupsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingGroupsForThing)

responseUpdateTopicRuleDestination :: UpdateTopicRuleDestinationResponse -> TestTree
responseUpdateTopicRuleDestination =
  res
    "UpdateTopicRuleDestinationResponse"
    "fixture/UpdateTopicRuleDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTopicRuleDestination)

responseValidateSecurityProfileBehaviors :: ValidateSecurityProfileBehaviorsResponse -> TestTree
responseValidateSecurityProfileBehaviors =
  res
    "ValidateSecurityProfileBehaviorsResponse"
    "fixture/ValidateSecurityProfileBehaviorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateSecurityProfileBehaviors)
