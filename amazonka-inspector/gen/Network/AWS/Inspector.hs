{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Inspector
--
-- Amazon Inspector enables you to analyze the behavior of the applications
-- you run in AWS and to identify potential security issues. For more
-- information, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_introduction.html Amazon Inspector User Guide>.
module Network.AWS.Inspector
    (
    -- * Service Configuration
      inspector

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** NoSuchEntityException
    , _NoSuchEntityException

    -- ** OperationInProgressException
    , _OperationInProgressException

    -- ** InvalidCrossAccountRoleException
    , _InvalidCrossAccountRoleException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InternalException
    , _InternalException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListFindings
    , module Network.AWS.Inspector.ListFindings

    -- ** DescribeRun
    , module Network.AWS.Inspector.DescribeRun

    -- ** DescribeApplication
    , module Network.AWS.Inspector.DescribeApplication

    -- ** RunAssessment
    , module Network.AWS.Inspector.RunAssessment

    -- ** ListTagsForResource
    , module Network.AWS.Inspector.ListTagsForResource

    -- ** SetTagsForResource
    , module Network.AWS.Inspector.SetTagsForResource

    -- ** DescribeCrossAccountAccessRole
    , module Network.AWS.Inspector.DescribeCrossAccountAccessRole

    -- ** ListAttachedAssessments
    , module Network.AWS.Inspector.ListAttachedAssessments

    -- ** DescribeFinding
    , module Network.AWS.Inspector.DescribeFinding

    -- ** DeleteRun
    , module Network.AWS.Inspector.DeleteRun

    -- ** ListRuns
    , module Network.AWS.Inspector.ListRuns

    -- ** DeleteApplication
    , module Network.AWS.Inspector.DeleteApplication

    -- ** UpdateApplication
    , module Network.AWS.Inspector.UpdateApplication

    -- ** StartDataCollection
    , module Network.AWS.Inspector.StartDataCollection

    -- ** LocalizeText
    , module Network.AWS.Inspector.LocalizeText

    -- ** RegisterCrossAccountAccessRole
    , module Network.AWS.Inspector.RegisterCrossAccountAccessRole

    -- ** CreateApplication
    , module Network.AWS.Inspector.CreateApplication

    -- ** CreateResourceGroup
    , module Network.AWS.Inspector.CreateResourceGroup

    -- ** ListAttachedRulesPackages
    , module Network.AWS.Inspector.ListAttachedRulesPackages

    -- ** DeleteAssessment
    , module Network.AWS.Inspector.DeleteAssessment

    -- ** UpdateAssessment
    , module Network.AWS.Inspector.UpdateAssessment

    -- ** GetAssessmentTelemetry
    , module Network.AWS.Inspector.GetAssessmentTelemetry

    -- ** ListAssessments
    , module Network.AWS.Inspector.ListAssessments

    -- ** DescribeRulesPackage
    , module Network.AWS.Inspector.DescribeRulesPackage

    -- ** CreateAssessment
    , module Network.AWS.Inspector.CreateAssessment

    -- ** DetachAssessmentAndRulesPackage
    , module Network.AWS.Inspector.DetachAssessmentAndRulesPackage

    -- ** DescribeResourceGroup
    , module Network.AWS.Inspector.DescribeResourceGroup

    -- ** ListApplications
    , module Network.AWS.Inspector.ListApplications

    -- ** DescribeAssessment
    , module Network.AWS.Inspector.DescribeAssessment

    -- ** AddAttributesToFindings
    , module Network.AWS.Inspector.AddAttributesToFindings

    -- ** StopDataCollection
    , module Network.AWS.Inspector.StopDataCollection

    -- ** PreviewAgentsForResourceGroup
    , module Network.AWS.Inspector.PreviewAgentsForResourceGroup

    -- ** ListAssessmentAgents
    , module Network.AWS.Inspector.ListAssessmentAgents

    -- ** ListRulesPackages
    , module Network.AWS.Inspector.ListRulesPackages

    -- ** RemoveAttributesFromFindings
    , module Network.AWS.Inspector.RemoveAttributesFromFindings

    -- ** AttachAssessmentAndRulesPackage
    , module Network.AWS.Inspector.AttachAssessmentAndRulesPackage

    -- * Types

    -- ** Agent
    , Agent
    , agent
    , aTelemetry
    , aAutoScalingGroup
    , aAgentHealthCode
    , aAssessmentARN
    , aAgentId
    , aAccountId
    , aAgentHealthDetails
    , aAgentHealth

    -- ** AgentPreview
    , AgentPreview
    , agentPreview
    , apAutoScalingGroup
    , apAgentId

    -- ** AgentsFilter
    , AgentsFilter
    , agentsFilter
    , afAgentHealthList

    -- ** Application
    , Application
    , application
    , aApplicationARN
    , aResourceGroupARN
    , aApplicationName

    -- ** ApplicationsFilter
    , ApplicationsFilter
    , applicationsFilter
    , afApplicationNamePatterns

    -- ** Assessment
    , Assessment
    , assessment
    , assDataCollected
    , assApplicationARN
    , assStartTime
    , assAssessmentARN
    , assUserAttributesForFindings
    , assFailureMessage
    , assAssessmentState
    , assEndTime
    , assDurationInSeconds
    , assAssessmentName

    -- ** AssessmentsFilter
    , AssessmentsFilter
    , assessmentsFilter
    , afDataCollected
    , afAssessmentStates
    , afStartTimeRange
    , afAssessmentNamePatterns
    , afEndTimeRange
    , afDurationRange

    -- ** Attribute
    , Attribute
    , attribute
    , aValue
    , aKey

    -- ** DurationRange
    , DurationRange
    , durationRange
    , drMaximum
    , drMinimum

    -- ** Finding
    , Finding
    , finding
    , fAutoScalingGroup
    , fFinding
    , fSeverity
    , fUserAttributes
    , fRuleName
    , fAgentId
    , fRunARN
    , fAttributes
    , fRulesPackageARN
    , fFindingARN
    , fDescription
    , fRecommendation

    -- ** FindingsFilter
    , FindingsFilter
    , findingsFilter
    , ffRuleNames
    , ffUserAttributes
    , ffRulesPackageARNs
    , ffAttributes
    , ffSeverities

    -- ** LocalizedText
    , LocalizedText
    , localizedText
    , ltKey
    , ltParameters

    -- ** LocalizedTextKey
    , LocalizedTextKey
    , localizedTextKey
    , ltkFacility
    , ltkId

    -- ** MessageTypeTelemetry
    , MessageTypeTelemetry
    , messageTypeTelemetry
    , mttDataSize
    , mttMessageType
    , mttCount

    -- ** Parameter
    , Parameter
    , parameter
    , pValue
    , pName

    -- ** ResourceGroup
    , ResourceGroup
    , resourceGroup
    , rgResourceGroupTags
    , rgResourceGroupARN

    -- ** RulesPackage
    , RulesPackage
    , rulesPackage
    , rpVersion
    , rpRulesPackageARN
    , rpRulesPackageName
    , rpDescription
    , rpProvider

    -- ** Run
    , Run
    , run
    , runCreationTime
    , runRulesPackages
    , runAssessmentARN
    , runRunState
    , runRunName
    , runCompletionTime
    , runRunARN

    -- ** RunsFilter
    , RunsFilter
    , runsFilter
    , rfCreationTime
    , rfRulesPackages
    , rfRunStates
    , rfRunNamePatterns
    , rfCompletionTime

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** Telemetry
    , Telemetry
    , telemetry
    , tStatus
    , tMessageTypeTelemetries

    -- ** TimestampRange
    , TimestampRange
    , timestampRange
    , trMaximum
    , trMinimum
    ) where

import           Network.AWS.Inspector.AddAttributesToFindings
import           Network.AWS.Inspector.AttachAssessmentAndRulesPackage
import           Network.AWS.Inspector.CreateApplication
import           Network.AWS.Inspector.CreateAssessment
import           Network.AWS.Inspector.CreateResourceGroup
import           Network.AWS.Inspector.DeleteApplication
import           Network.AWS.Inspector.DeleteAssessment
import           Network.AWS.Inspector.DeleteRun
import           Network.AWS.Inspector.DescribeApplication
import           Network.AWS.Inspector.DescribeAssessment
import           Network.AWS.Inspector.DescribeCrossAccountAccessRole
import           Network.AWS.Inspector.DescribeFinding
import           Network.AWS.Inspector.DescribeResourceGroup
import           Network.AWS.Inspector.DescribeRulesPackage
import           Network.AWS.Inspector.DescribeRun
import           Network.AWS.Inspector.DetachAssessmentAndRulesPackage
import           Network.AWS.Inspector.GetAssessmentTelemetry
import           Network.AWS.Inspector.ListApplications
import           Network.AWS.Inspector.ListAssessmentAgents
import           Network.AWS.Inspector.ListAssessments
import           Network.AWS.Inspector.ListAttachedAssessments
import           Network.AWS.Inspector.ListAttachedRulesPackages
import           Network.AWS.Inspector.ListFindings
import           Network.AWS.Inspector.ListRulesPackages
import           Network.AWS.Inspector.ListRuns
import           Network.AWS.Inspector.ListTagsForResource
import           Network.AWS.Inspector.LocalizeText
import           Network.AWS.Inspector.PreviewAgentsForResourceGroup
import           Network.AWS.Inspector.RegisterCrossAccountAccessRole
import           Network.AWS.Inspector.RemoveAttributesFromFindings
import           Network.AWS.Inspector.RunAssessment
import           Network.AWS.Inspector.SetTagsForResource
import           Network.AWS.Inspector.StartDataCollection
import           Network.AWS.Inspector.StopDataCollection
import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.UpdateApplication
import           Network.AWS.Inspector.UpdateAssessment
import           Network.AWS.Inspector.Waiters

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
