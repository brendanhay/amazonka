{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Inspector
--
-- Amazon Inspector enables you to analyze the behavior of your AWS
-- resources and to identify potential security issues. For more
-- information, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_introduction.html Amazon Inspector User Guide>.
module Network.AWS.Inspector
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PreviewGenerationInProgressException
    _PreviewGenerationInProgressException,

    -- ** ServiceTemporarilyUnavailableException
    _ServiceTemporarilyUnavailableException,

    -- ** UnsupportedFeatureException
    _UnsupportedFeatureException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidCrossAccountRoleException
    _InvalidCrossAccountRoleException,

    -- ** AssessmentRunInProgressException
    _AssessmentRunInProgressException,

    -- ** AgentsAlreadyRunningAssessmentException
    _AgentsAlreadyRunningAssessmentException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NoSuchEntityException
    _NoSuchEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartAssessmentRun
    StartAssessmentRun (StartAssessmentRun'),
    newStartAssessmentRun,
    StartAssessmentRunResponse (StartAssessmentRunResponse'),
    newStartAssessmentRunResponse,

    -- ** DeleteAssessmentTemplate
    DeleteAssessmentTemplate (DeleteAssessmentTemplate'),
    newDeleteAssessmentTemplate,
    DeleteAssessmentTemplateResponse (DeleteAssessmentTemplateResponse'),
    newDeleteAssessmentTemplateResponse,

    -- ** SubscribeToEvent
    SubscribeToEvent (SubscribeToEvent'),
    newSubscribeToEvent,
    SubscribeToEventResponse (SubscribeToEventResponse'),
    newSubscribeToEventResponse,

    -- ** StopAssessmentRun
    StopAssessmentRun (StopAssessmentRun'),
    newStopAssessmentRun,
    StopAssessmentRunResponse (StopAssessmentRunResponse'),
    newStopAssessmentRunResponse,

    -- ** GetTelemetryMetadata
    GetTelemetryMetadata (GetTelemetryMetadata'),
    newGetTelemetryMetadata,
    GetTelemetryMetadataResponse (GetTelemetryMetadataResponse'),
    newGetTelemetryMetadataResponse,

    -- ** ListFindings (Paginated)
    ListFindings (ListFindings'),
    newListFindings,
    ListFindingsResponse (ListFindingsResponse'),
    newListFindingsResponse,

    -- ** DescribeAssessmentTargets
    DescribeAssessmentTargets (DescribeAssessmentTargets'),
    newDescribeAssessmentTargets,
    DescribeAssessmentTargetsResponse (DescribeAssessmentTargetsResponse'),
    newDescribeAssessmentTargetsResponse,

    -- ** UpdateAssessmentTarget
    UpdateAssessmentTarget (UpdateAssessmentTarget'),
    newUpdateAssessmentTarget,
    UpdateAssessmentTargetResponse (UpdateAssessmentTargetResponse'),
    newUpdateAssessmentTargetResponse,

    -- ** ListAssessmentTargets (Paginated)
    ListAssessmentTargets (ListAssessmentTargets'),
    newListAssessmentTargets,
    ListAssessmentTargetsResponse (ListAssessmentTargetsResponse'),
    newListAssessmentTargetsResponse,

    -- ** ListAssessmentRuns (Paginated)
    ListAssessmentRuns (ListAssessmentRuns'),
    newListAssessmentRuns,
    ListAssessmentRunsResponse (ListAssessmentRunsResponse'),
    newListAssessmentRunsResponse,

    -- ** DeleteAssessmentTarget
    DeleteAssessmentTarget (DeleteAssessmentTarget'),
    newDeleteAssessmentTarget,
    DeleteAssessmentTargetResponse (DeleteAssessmentTargetResponse'),
    newDeleteAssessmentTargetResponse,

    -- ** AddAttributesToFindings
    AddAttributesToFindings (AddAttributesToFindings'),
    newAddAttributesToFindings,
    AddAttributesToFindingsResponse (AddAttributesToFindingsResponse'),
    newAddAttributesToFindingsResponse,

    -- ** CreateAssessmentTarget
    CreateAssessmentTarget (CreateAssessmentTarget'),
    newCreateAssessmentTarget,
    CreateAssessmentTargetResponse (CreateAssessmentTargetResponse'),
    newCreateAssessmentTargetResponse,

    -- ** GetExclusionsPreview
    GetExclusionsPreview (GetExclusionsPreview'),
    newGetExclusionsPreview,
    GetExclusionsPreviewResponse (GetExclusionsPreviewResponse'),
    newGetExclusionsPreviewResponse,

    -- ** DescribeResourceGroups
    DescribeResourceGroups (DescribeResourceGroups'),
    newDescribeResourceGroups,
    DescribeResourceGroupsResponse (DescribeResourceGroupsResponse'),
    newDescribeResourceGroupsResponse,

    -- ** PreviewAgents (Paginated)
    PreviewAgents (PreviewAgents'),
    newPreviewAgents,
    PreviewAgentsResponse (PreviewAgentsResponse'),
    newPreviewAgentsResponse,

    -- ** ListExclusions (Paginated)
    ListExclusions (ListExclusions'),
    newListExclusions,
    ListExclusionsResponse (ListExclusionsResponse'),
    newListExclusionsResponse,

    -- ** CreateAssessmentTemplate
    CreateAssessmentTemplate (CreateAssessmentTemplate'),
    newCreateAssessmentTemplate,
    CreateAssessmentTemplateResponse (CreateAssessmentTemplateResponse'),
    newCreateAssessmentTemplateResponse,

    -- ** DescribeCrossAccountAccessRole
    DescribeCrossAccountAccessRole (DescribeCrossAccountAccessRole'),
    newDescribeCrossAccountAccessRole,
    DescribeCrossAccountAccessRoleResponse (DescribeCrossAccountAccessRoleResponse'),
    newDescribeCrossAccountAccessRoleResponse,

    -- ** SetTagsForResource
    SetTagsForResource (SetTagsForResource'),
    newSetTagsForResource,
    SetTagsForResourceResponse (SetTagsForResourceResponse'),
    newSetTagsForResourceResponse,

    -- ** DescribeExclusions
    DescribeExclusions (DescribeExclusions'),
    newDescribeExclusions,
    DescribeExclusionsResponse (DescribeExclusionsResponse'),
    newDescribeExclusionsResponse,

    -- ** ListAssessmentTemplates (Paginated)
    ListAssessmentTemplates (ListAssessmentTemplates'),
    newListAssessmentTemplates,
    ListAssessmentTemplatesResponse (ListAssessmentTemplatesResponse'),
    newListAssessmentTemplatesResponse,

    -- ** ListAssessmentRunAgents (Paginated)
    ListAssessmentRunAgents (ListAssessmentRunAgents'),
    newListAssessmentRunAgents,
    ListAssessmentRunAgentsResponse (ListAssessmentRunAgentsResponse'),
    newListAssessmentRunAgentsResponse,

    -- ** DescribeAssessmentRuns
    DescribeAssessmentRuns (DescribeAssessmentRuns'),
    newDescribeAssessmentRuns,
    DescribeAssessmentRunsResponse (DescribeAssessmentRunsResponse'),
    newDescribeAssessmentRunsResponse,

    -- ** DescribeRulesPackages
    DescribeRulesPackages (DescribeRulesPackages'),
    newDescribeRulesPackages,
    DescribeRulesPackagesResponse (DescribeRulesPackagesResponse'),
    newDescribeRulesPackagesResponse,

    -- ** CreateExclusionsPreview
    CreateExclusionsPreview (CreateExclusionsPreview'),
    newCreateExclusionsPreview,
    CreateExclusionsPreviewResponse (CreateExclusionsPreviewResponse'),
    newCreateExclusionsPreviewResponse,

    -- ** CreateResourceGroup
    CreateResourceGroup (CreateResourceGroup'),
    newCreateResourceGroup,
    CreateResourceGroupResponse (CreateResourceGroupResponse'),
    newCreateResourceGroupResponse,

    -- ** UnsubscribeFromEvent
    UnsubscribeFromEvent (UnsubscribeFromEvent'),
    newUnsubscribeFromEvent,
    UnsubscribeFromEventResponse (UnsubscribeFromEventResponse'),
    newUnsubscribeFromEventResponse,

    -- ** RemoveAttributesFromFindings
    RemoveAttributesFromFindings (RemoveAttributesFromFindings'),
    newRemoveAttributesFromFindings,
    RemoveAttributesFromFindingsResponse (RemoveAttributesFromFindingsResponse'),
    newRemoveAttributesFromFindingsResponse,

    -- ** DeleteAssessmentRun
    DeleteAssessmentRun (DeleteAssessmentRun'),
    newDeleteAssessmentRun,
    DeleteAssessmentRunResponse (DeleteAssessmentRunResponse'),
    newDeleteAssessmentRunResponse,

    -- ** RegisterCrossAccountAccessRole
    RegisterCrossAccountAccessRole (RegisterCrossAccountAccessRole'),
    newRegisterCrossAccountAccessRole,
    RegisterCrossAccountAccessRoleResponse (RegisterCrossAccountAccessRoleResponse'),
    newRegisterCrossAccountAccessRoleResponse,

    -- ** ListEventSubscriptions (Paginated)
    ListEventSubscriptions (ListEventSubscriptions'),
    newListEventSubscriptions,
    ListEventSubscriptionsResponse (ListEventSubscriptionsResponse'),
    newListEventSubscriptionsResponse,

    -- ** GetAssessmentReport
    GetAssessmentReport (GetAssessmentReport'),
    newGetAssessmentReport,
    GetAssessmentReportResponse (GetAssessmentReportResponse'),
    newGetAssessmentReportResponse,

    -- ** ListRulesPackages (Paginated)
    ListRulesPackages (ListRulesPackages'),
    newListRulesPackages,
    ListRulesPackagesResponse (ListRulesPackagesResponse'),
    newListRulesPackagesResponse,

    -- ** DescribeFindings
    DescribeFindings (DescribeFindings'),
    newDescribeFindings,
    DescribeFindingsResponse (DescribeFindingsResponse'),
    newDescribeFindingsResponse,

    -- ** DescribeAssessmentTemplates
    DescribeAssessmentTemplates (DescribeAssessmentTemplates'),
    newDescribeAssessmentTemplates,
    DescribeAssessmentTemplatesResponse (DescribeAssessmentTemplatesResponse'),
    newDescribeAssessmentTemplatesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** AgentHealth
    AgentHealth (..),

    -- ** AgentHealthCode
    AgentHealthCode (..),

    -- ** AssessmentRunNotificationSnsStatusCode
    AssessmentRunNotificationSnsStatusCode (..),

    -- ** AssessmentRunState
    AssessmentRunState (..),

    -- ** AssetType
    AssetType (..),

    -- ** FailedItemErrorCode
    FailedItemErrorCode (..),

    -- ** InspectorEvent
    InspectorEvent (..),

    -- ** Locale
    Locale (..),

    -- ** PreviewStatus
    PreviewStatus (..),

    -- ** ReportFileFormat
    ReportFileFormat (..),

    -- ** ReportStatus
    ReportStatus (..),

    -- ** ReportType
    ReportType (..),

    -- ** ScopeType
    ScopeType (..),

    -- ** Severity
    Severity (..),

    -- ** StopAction
    StopAction (..),

    -- ** AgentFilter
    AgentFilter (AgentFilter'),
    newAgentFilter,

    -- ** AgentPreview
    AgentPreview (AgentPreview'),
    newAgentPreview,

    -- ** AssessmentRun
    AssessmentRun (AssessmentRun'),
    newAssessmentRun,

    -- ** AssessmentRunAgent
    AssessmentRunAgent (AssessmentRunAgent'),
    newAssessmentRunAgent,

    -- ** AssessmentRunFilter
    AssessmentRunFilter (AssessmentRunFilter'),
    newAssessmentRunFilter,

    -- ** AssessmentRunNotification
    AssessmentRunNotification (AssessmentRunNotification'),
    newAssessmentRunNotification,

    -- ** AssessmentRunStateChange
    AssessmentRunStateChange (AssessmentRunStateChange'),
    newAssessmentRunStateChange,

    -- ** AssessmentTarget
    AssessmentTarget (AssessmentTarget'),
    newAssessmentTarget,

    -- ** AssessmentTargetFilter
    AssessmentTargetFilter (AssessmentTargetFilter'),
    newAssessmentTargetFilter,

    -- ** AssessmentTemplate
    AssessmentTemplate (AssessmentTemplate'),
    newAssessmentTemplate,

    -- ** AssessmentTemplateFilter
    AssessmentTemplateFilter (AssessmentTemplateFilter'),
    newAssessmentTemplateFilter,

    -- ** AssetAttributes
    AssetAttributes (AssetAttributes'),
    newAssetAttributes,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** DurationRange
    DurationRange (DurationRange'),
    newDurationRange,

    -- ** EventSubscription
    EventSubscription (EventSubscription'),
    newEventSubscription,

    -- ** Exclusion
    Exclusion (Exclusion'),
    newExclusion,

    -- ** ExclusionPreview
    ExclusionPreview (ExclusionPreview'),
    newExclusionPreview,

    -- ** FailedItemDetails
    FailedItemDetails (FailedItemDetails'),
    newFailedItemDetails,

    -- ** Finding
    Finding (Finding'),
    newFinding,

    -- ** FindingFilter
    FindingFilter (FindingFilter'),
    newFindingFilter,

    -- ** InspectorServiceAttributes
    InspectorServiceAttributes (InspectorServiceAttributes'),
    newInspectorServiceAttributes,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** PrivateIp
    PrivateIp (PrivateIp'),
    newPrivateIp,

    -- ** ResourceGroup
    ResourceGroup (ResourceGroup'),
    newResourceGroup,

    -- ** ResourceGroupTag
    ResourceGroupTag (ResourceGroupTag'),
    newResourceGroupTag,

    -- ** RulesPackage
    RulesPackage (RulesPackage'),
    newRulesPackage,

    -- ** Scope
    Scope (Scope'),
    newScope,

    -- ** SecurityGroup
    SecurityGroup (SecurityGroup'),
    newSecurityGroup,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TelemetryMetadata
    TelemetryMetadata (TelemetryMetadata'),
    newTelemetryMetadata,

    -- ** TimestampRange
    TimestampRange (TimestampRange'),
    newTimestampRange,
  )
where

import Network.AWS.Inspector.AddAttributesToFindings
import Network.AWS.Inspector.CreateAssessmentTarget
import Network.AWS.Inspector.CreateAssessmentTemplate
import Network.AWS.Inspector.CreateExclusionsPreview
import Network.AWS.Inspector.CreateResourceGroup
import Network.AWS.Inspector.DeleteAssessmentRun
import Network.AWS.Inspector.DeleteAssessmentTarget
import Network.AWS.Inspector.DeleteAssessmentTemplate
import Network.AWS.Inspector.DescribeAssessmentRuns
import Network.AWS.Inspector.DescribeAssessmentTargets
import Network.AWS.Inspector.DescribeAssessmentTemplates
import Network.AWS.Inspector.DescribeCrossAccountAccessRole
import Network.AWS.Inspector.DescribeExclusions
import Network.AWS.Inspector.DescribeFindings
import Network.AWS.Inspector.DescribeResourceGroups
import Network.AWS.Inspector.DescribeRulesPackages
import Network.AWS.Inspector.GetAssessmentReport
import Network.AWS.Inspector.GetExclusionsPreview
import Network.AWS.Inspector.GetTelemetryMetadata
import Network.AWS.Inspector.Lens
import Network.AWS.Inspector.ListAssessmentRunAgents
import Network.AWS.Inspector.ListAssessmentRuns
import Network.AWS.Inspector.ListAssessmentTargets
import Network.AWS.Inspector.ListAssessmentTemplates
import Network.AWS.Inspector.ListEventSubscriptions
import Network.AWS.Inspector.ListExclusions
import Network.AWS.Inspector.ListFindings
import Network.AWS.Inspector.ListRulesPackages
import Network.AWS.Inspector.ListTagsForResource
import Network.AWS.Inspector.PreviewAgents
import Network.AWS.Inspector.RegisterCrossAccountAccessRole
import Network.AWS.Inspector.RemoveAttributesFromFindings
import Network.AWS.Inspector.SetTagsForResource
import Network.AWS.Inspector.StartAssessmentRun
import Network.AWS.Inspector.StopAssessmentRun
import Network.AWS.Inspector.SubscribeToEvent
import Network.AWS.Inspector.Types
import Network.AWS.Inspector.UnsubscribeFromEvent
import Network.AWS.Inspector.UpdateAssessmentTarget
import Network.AWS.Inspector.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Inspector'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
