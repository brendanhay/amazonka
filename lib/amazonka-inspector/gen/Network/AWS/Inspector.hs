{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Inspector__ 
--
-- Amazon Inspector enables you to analyze the behavior of your AWS resources and to identify potential security issues. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_introduction.html Amazon Inspector User Guide> .
module Network.AWS.Inspector
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** AssessmentRunInProgressException
    , _AssessmentRunInProgressException

    -- ** NoSuchEntityException
    , _NoSuchEntityException

    -- ** UnsupportedFeatureException
    , _UnsupportedFeatureException

    -- ** PreviewGenerationInProgressException
    , _PreviewGenerationInProgressException

    -- ** AgentsAlreadyRunningAssessmentException
    , _AgentsAlreadyRunningAssessmentException

    -- ** InvalidCrossAccountRoleException
    , _InvalidCrossAccountRoleException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InternalException
    , _InternalException

    -- ** ServiceTemporarilyUnavailableException
    , _ServiceTemporarilyUnavailableException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetTelemetryMetadata 
    , module Network.AWS.Inspector.GetTelemetryMetadata

    -- ** ListFindings (Paginated)
    , module Network.AWS.Inspector.ListFindings

    -- ** ListAssessmentTemplates (Paginated)
    , module Network.AWS.Inspector.ListAssessmentTemplates

    -- ** SubscribeToEvent 
    , module Network.AWS.Inspector.SubscribeToEvent

    -- ** ListAssessmentRunAgents (Paginated)
    , module Network.AWS.Inspector.ListAssessmentRunAgents

    -- ** StartAssessmentRun 
    , module Network.AWS.Inspector.StartAssessmentRun

    -- ** DeleteAssessmentTemplate 
    , module Network.AWS.Inspector.DeleteAssessmentTemplate

    -- ** CreateAssessmentTemplate 
    , module Network.AWS.Inspector.CreateAssessmentTemplate

    -- ** DescribeExclusions 
    , module Network.AWS.Inspector.DescribeExclusions

    -- ** ListTagsForResource 
    , module Network.AWS.Inspector.ListTagsForResource

    -- ** SetTagsForResource 
    , module Network.AWS.Inspector.SetTagsForResource

    -- ** DescribeCrossAccountAccessRole 
    , module Network.AWS.Inspector.DescribeCrossAccountAccessRole

    -- ** DescribeAssessmentTemplates 
    , module Network.AWS.Inspector.DescribeAssessmentTemplates

    -- ** DescribeResourceGroups 
    , module Network.AWS.Inspector.DescribeResourceGroups

    -- ** CreateAssessmentTarget 
    , module Network.AWS.Inspector.CreateAssessmentTarget

    -- ** GetExclusionsPreview 
    , module Network.AWS.Inspector.GetExclusionsPreview

    -- ** ListEventSubscriptions (Paginated)
    , module Network.AWS.Inspector.ListEventSubscriptions

    -- ** RegisterCrossAccountAccessRole 
    , module Network.AWS.Inspector.RegisterCrossAccountAccessRole

    -- ** ListAssessmentTargets (Paginated)
    , module Network.AWS.Inspector.ListAssessmentTargets

    -- ** CreateExclusionsPreview 
    , module Network.AWS.Inspector.CreateExclusionsPreview

    -- ** CreateResourceGroup 
    , module Network.AWS.Inspector.CreateResourceGroup

    -- ** DescribeRulesPackages 
    , module Network.AWS.Inspector.DescribeRulesPackages

    -- ** StopAssessmentRun 
    , module Network.AWS.Inspector.StopAssessmentRun

    -- ** ListExclusions (Paginated)
    , module Network.AWS.Inspector.ListExclusions

    -- ** PreviewAgents (Paginated)
    , module Network.AWS.Inspector.PreviewAgents

    -- ** DescribeFindings 
    , module Network.AWS.Inspector.DescribeFindings

    -- ** AddAttributesToFindings 
    , module Network.AWS.Inspector.AddAttributesToFindings

    -- ** UpdateAssessmentTarget 
    , module Network.AWS.Inspector.UpdateAssessmentTarget

    -- ** DeleteAssessmentTarget 
    , module Network.AWS.Inspector.DeleteAssessmentTarget

    -- ** DeleteAssessmentRun 
    , module Network.AWS.Inspector.DeleteAssessmentRun

    -- ** ListAssessmentRuns (Paginated)
    , module Network.AWS.Inspector.ListAssessmentRuns

    -- ** GetAssessmentReport 
    , module Network.AWS.Inspector.GetAssessmentReport

    -- ** ListRulesPackages (Paginated)
    , module Network.AWS.Inspector.ListRulesPackages

    -- ** DescribeAssessmentRuns 
    , module Network.AWS.Inspector.DescribeAssessmentRuns

    -- ** UnsubscribeFromEvent 
    , module Network.AWS.Inspector.UnsubscribeFromEvent

    -- ** RemoveAttributesFromFindings 
    , module Network.AWS.Inspector.RemoveAttributesFromFindings

    -- ** DescribeAssessmentTargets 
    , module Network.AWS.Inspector.DescribeAssessmentTargets

    -- * Types

    -- ** Exclusion
    , Exclusion (..)
    , mkExclusion
    , eArn
    , eTitle
    , eDescription
    , eRecommendation
    , eScopes
    , eAttributes

    -- ** Attribute
    , Attribute (..)
    , mkAttribute
    , aKey
    , aValue

    -- ** PrivateIp
    , PrivateIp (..)
    , mkPrivateIp
    , piPrivateDnsName
    , piPrivateIpAddress

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** NamePattern
    , NamePattern (..)

    -- ** Hostname
    , Hostname (..)

    -- ** ResourceGroupTag
    , ResourceGroupTag (..)
    , mkResourceGroupTag
    , rgtKey
    , rgtValue

    -- ** AssessmentTargetFilter
    , AssessmentTargetFilter (..)
    , mkAssessmentTargetFilter
    , atfAssessmentTargetNamePattern

    -- ** AssessmentRun
    , AssessmentRun (..)
    , mkAssessmentRun
    , arArn
    , arName
    , arAssessmentTemplateArn
    , arState
    , arDurationInSeconds
    , arRulesPackageArns
    , arUserAttributesForFindings
    , arCreatedAt
    , arStateChangedAt
    , arDataCollected
    , arStateChanges
    , arNotifications
    , arFindingCounts
    , arCompletedAt
    , arStartedAt

    -- ** RulesPackage
    , RulesPackage (..)
    , mkRulesPackage
    , rpArn
    , rpName
    , rpVersion
    , rpProvider
    , rpDescription

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** ScopeType
    , ScopeType (..)

    -- ** AssessmentRunNotification
    , AssessmentRunNotification (..)
    , mkAssessmentRunNotification
    , arnDate
    , arnEvent
    , arnError
    , arnMessage
    , arnSnsPublishStatusCode
    , arnSnsTopicArn

    -- ** AutoScalingGroup
    , AutoScalingGroup (..)

    -- ** Arn
    , Arn (..)

    -- ** Text
    , Text (..)

    -- ** Finding
    , Finding (..)
    , mkFinding
    , fArn
    , fAttributes
    , fUserAttributes
    , fCreatedAt
    , fUpdatedAt
    , fAssetAttributes
    , fAssetType
    , fConfidence
    , fDescription
    , fId
    , fIndicatorOfCompromise
    , fNumericSeverity
    , fRecommendation
    , fSchemaVersion
    , fService
    , fServiceAttributes
    , fSeverity
    , fTitle

    -- ** AssessmentRunNotificationSnsStatusCode
    , AssessmentRunNotificationSnsStatusCode (..)

    -- ** AssessmentTargetName
    , AssessmentTargetName (..)

    -- ** OperatingSystem
    , OperatingSystem (..)

    -- ** Locale
    , Locale (..)

    -- ** AgentHealthCode
    , AgentHealthCode (..)

    -- ** AgentPreview
    , AgentPreview (..)
    , mkAgentPreview
    , apAgentId
    , apAgentHealth
    , apAgentVersion
    , apAutoScalingGroup
    , apHostname
    , apIpv4Address
    , apKernelVersion
    , apOperatingSystem

    -- ** Severity
    , Severity (..)

    -- ** NetworkInterface
    , NetworkInterface (..)
    , mkNetworkInterface
    , niIpv6Addresses
    , niNetworkInterfaceId
    , niPrivateDnsName
    , niPrivateIpAddress
    , niPrivateIpAddresses
    , niPublicDnsName
    , niPublicIp
    , niSecurityGroups
    , niSubnetId
    , niVpcId

    -- ** AgentVersion
    , AgentVersion (..)

    -- ** Url
    , Url (..)

    -- ** MessageType
    , MessageType (..)

    -- ** AssessmentTemplate
    , AssessmentTemplate (..)
    , mkAssessmentTemplate
    , atArn
    , atName
    , atAssessmentTargetArn
    , atDurationInSeconds
    , atRulesPackageArns
    , atUserAttributesForFindings
    , atAssessmentRunCount
    , atCreatedAt
    , atLastAssessmentRunArn

    -- ** ExclusionPreview
    , ExclusionPreview (..)
    , mkExclusionPreview
    , epTitle
    , epDescription
    , epRecommendation
    , epScopes
    , epAttributes

    -- ** AssessmentRunAgent
    , AssessmentRunAgent (..)
    , mkAssessmentRunAgent
    , araAgentId
    , araAssessmentRunArn
    , araAgentHealth
    , araAgentHealthCode
    , araTelemetryMetadata
    , araAgentHealthDetails
    , araAutoScalingGroup

    -- ** PreviewStatus
    , PreviewStatus (..)

    -- ** InspectorServiceAttributes
    , InspectorServiceAttributes (..)
    , mkInspectorServiceAttributes
    , isaSchemaVersion
    , isaAssessmentRunArn
    , isaRulesPackageArn

    -- ** RuleName
    , RuleName (..)

    -- ** SecurityGroup
    , SecurityGroup (..)
    , mkSecurityGroup
    , sgGroupId
    , sgGroupName

    -- ** KernelVersion
    , KernelVersion (..)

    -- ** AssessmentTemplateName
    , AssessmentTemplateName (..)

    -- ** AgentId
    , AgentId (..)

    -- ** FailedItemDetails
    , FailedItemDetails (..)
    , mkFailedItemDetails
    , fidFailureCode
    , fidRetryable

    -- ** InspectorEvent
    , InspectorEvent (..)

    -- ** AssetAttributes
    , AssetAttributes (..)
    , mkAssetAttributes
    , aaSchemaVersion
    , aaAgentId
    , aaAmiId
    , aaAutoScalingGroup
    , aaHostname
    , aaIpv4Addresses
    , aaNetworkInterfaces
    , aaTags

    -- ** AssessmentRunFilter
    , AssessmentRunFilter (..)
    , mkAssessmentRunFilter
    , arfCompletionTimeRange
    , arfDurationRange
    , arfNamePattern
    , arfRulesPackageArns
    , arfStartTimeRange
    , arfStateChangeTimeRange
    , arfStates

    -- ** EventSubscription
    , EventSubscription (..)
    , mkEventSubscription
    , esEvent
    , esSubscribedAt

    -- ** AssessmentTarget
    , AssessmentTarget (..)
    , mkAssessmentTarget
    , aArn
    , aName
    , aCreatedAt
    , aUpdatedAt
    , aResourceGroupArn

    -- ** TelemetryMetadata
    , TelemetryMetadata (..)
    , mkTelemetryMetadata
    , tmMessageType
    , tmCount
    , tmDataSize

    -- ** ReportStatus
    , ReportStatus (..)

    -- ** Version
    , Version (..)

    -- ** Scope
    , Scope (..)
    , mkScope
    , sKey
    , sValue

    -- ** FindingFilter
    , FindingFilter (..)
    , mkFindingFilter
    , ffAgentIds
    , ffAttributes
    , ffAutoScalingGroups
    , ffCreationTimeRange
    , ffRuleNames
    , ffRulesPackageArns
    , ffSeverities
    , ffUserAttributes

    -- ** AgentHealth
    , AgentHealth (..)

    -- ** ResourceGroup
    , ResourceGroup (..)
    , mkResourceGroup
    , rgArn
    , rgTags
    , rgCreatedAt

    -- ** Ipv4Address
    , Ipv4Address (..)

    -- ** AssetType
    , AssetType (..)

    -- ** AssessmentRunState
    , AssessmentRunState (..)

    -- ** AssessmentRunName
    , AssessmentRunName (..)

    -- ** AmiId
    , AmiId (..)

    -- ** AttributeKey
    , AttributeKey (..)

    -- ** Subscription
    , Subscription (..)
    , mkSubscription
    , sResourceArn
    , sTopicArn
    , sEventSubscriptions

    -- ** Message
    , Message (..)

    -- ** ReportFileFormat
    , ReportFileFormat (..)

    -- ** FailedItemErrorCode
    , FailedItemErrorCode (..)

    -- ** AssessmentTemplateFilter
    , AssessmentTemplateFilter (..)
    , mkAssessmentTemplateFilter
    , atfDurationRange
    , atfNamePattern
    , atfRulesPackageArns

    -- ** StopAction
    , StopAction (..)

    -- ** ReportType
    , ReportType (..)

    -- ** AssessmentRunStateChange
    , AssessmentRunStateChange (..)
    , mkAssessmentRunStateChange
    , arscStateChangedAt
    , arscState

    -- ** DurationRange
    , DurationRange (..)
    , mkDurationRange
    , drMaxSeconds
    , drMinSeconds

    -- ** AgentFilter
    , AgentFilter (..)
    , mkAgentFilter
    , afAgentHealths
    , afAgentHealthCodes

    -- ** TimestampRange
    , TimestampRange (..)
    , mkTimestampRange
    , trBeginDate
    , trEndDate

    -- ** NextToken
    , NextToken (..)

    -- ** Title
    , Title (..)

    -- ** Description
    , Description (..)

    -- ** Recommendation
    , Recommendation (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** AssessmentRunArn
    , AssessmentRunArn (..)

    -- ** PrivateDnsName
    , PrivateDnsName (..)

    -- ** PrivateIpAddress
    , PrivateIpAddress (..)

    -- ** RoleArn
    , RoleArn (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** Name
    , Name (..)

    -- ** AssessmentTemplateArn
    , AssessmentTemplateArn (..)

    -- ** Provider
    , Provider (..)

    -- ** ResourceGroupArn
    , ResourceGroupArn (..)

    -- ** PreviewToken
    , PreviewToken (..)

    -- ** SnsTopicArn
    , SnsTopicArn (..)

    -- ** Id
    , Id (..)

    -- ** Service
    , Service (..)

    -- ** AgentHealthDetails
    , AgentHealthDetails (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Waiters
import Network.AWS.Inspector.GetTelemetryMetadata
import Network.AWS.Inspector.ListFindings
import Network.AWS.Inspector.ListAssessmentTemplates
import Network.AWS.Inspector.SubscribeToEvent
import Network.AWS.Inspector.ListAssessmentRunAgents
import Network.AWS.Inspector.StartAssessmentRun
import Network.AWS.Inspector.DeleteAssessmentTemplate
import Network.AWS.Inspector.CreateAssessmentTemplate
import Network.AWS.Inspector.DescribeExclusions
import Network.AWS.Inspector.ListTagsForResource
import Network.AWS.Inspector.SetTagsForResource
import Network.AWS.Inspector.DescribeCrossAccountAccessRole
import Network.AWS.Inspector.DescribeAssessmentTemplates
import Network.AWS.Inspector.DescribeResourceGroups
import Network.AWS.Inspector.CreateAssessmentTarget
import Network.AWS.Inspector.GetExclusionsPreview
import Network.AWS.Inspector.ListEventSubscriptions
import Network.AWS.Inspector.RegisterCrossAccountAccessRole
import Network.AWS.Inspector.ListAssessmentTargets
import Network.AWS.Inspector.CreateExclusionsPreview
import Network.AWS.Inspector.CreateResourceGroup
import Network.AWS.Inspector.DescribeRulesPackages
import Network.AWS.Inspector.StopAssessmentRun
import Network.AWS.Inspector.ListExclusions
import Network.AWS.Inspector.PreviewAgents
import Network.AWS.Inspector.DescribeFindings
import Network.AWS.Inspector.AddAttributesToFindings
import Network.AWS.Inspector.UpdateAssessmentTarget
import Network.AWS.Inspector.DeleteAssessmentTarget
import Network.AWS.Inspector.DeleteAssessmentRun
import Network.AWS.Inspector.ListAssessmentRuns
import Network.AWS.Inspector.GetAssessmentReport
import Network.AWS.Inspector.ListRulesPackages
import Network.AWS.Inspector.DescribeAssessmentRuns
import Network.AWS.Inspector.UnsubscribeFromEvent
import Network.AWS.Inspector.RemoveAttributesFromFindings
import Network.AWS.Inspector.DescribeAssessmentTargets
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Inspector'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
