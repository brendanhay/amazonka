-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _OutOfCapacityException
    , _InvalidFleetStatusException
    , _InvalidRequestException
    , _ConflictException
    , _TaggingFailedException
    , _TerminalRoutingStrategyException
    , _NotFoundException
    , _GameSessionFullException
    , _UnsupportedRegionException
    , _InvalidGameSessionStatusException
    , _InternalServiceException
    , _IdempotentParameterMismatchException
    , _UnauthorizedException
    , _FleetCapacityExceededException
    , _LimitExceededException

    -- * GameSessionQueueArn
    , GameSessionQueueArn (..)

    -- * InstanceId
    , InstanceId (..)

    -- * MatchmakingConfiguration
    , MatchmakingConfiguration (..)
    , mkMatchmakingConfiguration
    , mcAcceptanceRequired
    , mcAcceptanceTimeoutSeconds
    , mcAdditionalPlayerCount
    , mcBackfillMode
    , mcConfigurationArn
    , mcCreationTime
    , mcCustomEventData
    , mcDescription
    , mcFlexMatchMode
    , mcGameProperties
    , mcGameSessionData
    , mcGameSessionQueueArns
    , mcName
    , mcNotificationTarget
    , mcRequestTimeoutSeconds
    , mcRuleSetArn
    , mcRuleSetName

    -- * IamRoleArn
    , IamRoleArn (..)

    -- * Event
    , Event (..)
    , mkEvent
    , eEventCode
    , eEventId
    , eEventTime
    , eMessage
    , ePreSignedLogUrl
    , eResourceId

    -- * BackfillMode
    , BackfillMode (..)

    -- * RoutingStrategyType
    , RoutingStrategyType (..)

    -- * Script
    , Script (..)
    , mkScript
    , sCreationTime
    , sName
    , sScriptArn
    , sScriptId
    , sSizeOnDisk
    , sStorageLocation
    , sVersion

    -- * IdStringModel
    , IdStringModel (..)

    -- * IpAddress
    , IpAddress (..)

    -- * NonZeroAndMaxString
    , NonZeroAndMaxString (..)

    -- * AttributeValue
    , AttributeValue (..)
    , mkAttributeValue
    , avN
    , avS
    , avSDM
    , avSL

    -- * MatchedPlayerSession
    , MatchedPlayerSession (..)
    , mkMatchedPlayerSession
    , mpsPlayerId
    , mpsPlayerSessionId

    -- * GameServerInstanceId
    , GameServerInstanceId (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * PlacedPlayerSession
    , PlacedPlayerSession (..)
    , mkPlacedPlayerSession
    , ppsPlayerId
    , ppsPlayerSessionId

    -- * LaunchTemplateName
    , LaunchTemplateName (..)

    -- * FleetStatus
    , FleetStatus (..)

    -- * GamePropertyValue
    , GamePropertyValue (..)

    -- * GameServerGroupAction
    , GameServerGroupAction (..)

    -- * ScalingAdjustmentType
    , ScalingAdjustmentType (..)

    -- * StringModel
    , StringModel (..)

    -- * PlayerSession
    , PlayerSession (..)
    , mkPlayerSession
    , psCreationTime
    , psDnsName
    , psFleetArn
    , psFleetId
    , psGameSessionId
    , psIpAddress
    , psPlayerData
    , psPlayerId
    , psPlayerSessionId
    , psPort
    , psStatus
    , psTerminationTime

    -- * GameSessionQueueDestination
    , GameSessionQueueDestination (..)
    , mkGameSessionQueueDestination
    , gsqdDestinationArn

    -- * GameServerClaimStatus
    , GameServerClaimStatus (..)

    -- * WeightedCapacity
    , WeightedCapacity (..)

    -- * ScalingPolicy
    , ScalingPolicy (..)
    , mkScalingPolicy
    , spComparisonOperator
    , spEvaluationPeriods
    , spFleetId
    , spMetricName
    , spName
    , spPolicyType
    , spScalingAdjustment
    , spScalingAdjustmentType
    , spStatus
    , spTargetConfiguration
    , spThreshold

    -- * GameServerUtilizationStatus
    , GameServerUtilizationStatus (..)

    -- * CertificateType
    , CertificateType (..)

    -- * DesiredPlayerSession
    , DesiredPlayerSession (..)
    , mkDesiredPlayerSession
    , dpsPlayerData
    , dpsPlayerId

    -- * MatchmakingRuleSetArn
    , MatchmakingRuleSetArn (..)

    -- * EC2InstanceType
    , EC2InstanceType (..)

    -- * OperatingSystem
    , OperatingSystem (..)

    -- * GameSessionConnectionInfo
    , GameSessionConnectionInfo (..)
    , mkGameSessionConnectionInfo
    , gsciDnsName
    , gsciGameSessionArn
    , gsciIpAddress
    , gsciMatchedPlayerSessions
    , gsciPort

    -- * MatchmakingConfigurationStatus
    , MatchmakingConfigurationStatus (..)

    -- * LaunchTemplateId
    , LaunchTemplateId (..)

    -- * BalancingStrategy
    , BalancingStrategy (..)

    -- * PolicyType
    , PolicyType (..)

    -- * MatchmakingRuleSet
    , MatchmakingRuleSet (..)
    , mkMatchmakingRuleSet
    , mrsRuleSetBody
    , mrsCreationTime
    , mrsRuleSetArn
    , mrsRuleSetName

    -- * MatchmakingTicket
    , MatchmakingTicket (..)
    , mkMatchmakingTicket
    , mtConfigurationArn
    , mtConfigurationName
    , mtEndTime
    , mtEstimatedWaitTime
    , mtGameSessionConnectionInfo
    , mtPlayers
    , mtStartTime
    , mtStatus
    , mtStatusMessage
    , mtStatusReason
    , mtTicketId

    -- * GameSessionStatusReason
    , GameSessionStatusReason (..)

    -- * GameServer
    , GameServer (..)
    , mkGameServer
    , gsClaimStatus
    , gsConnectionInfo
    , gsGameServerData
    , gsGameServerGroupArn
    , gsGameServerGroupName
    , gsGameServerId
    , gsInstanceId
    , gsLastClaimTime
    , gsLastHealthCheckTime
    , gsRegistrationTime
    , gsUtilizationStatus

    -- * MetricName
    , MetricName (..)

    -- * GameServerGroupName
    , GameServerGroupName (..)

    -- * BuildId
    , BuildId (..)

    -- * PlayerLatency
    , PlayerLatency (..)
    , mkPlayerLatency
    , plLatencyInMilliseconds
    , plPlayerId
    , plRegionIdentifier

    -- * FleetIdOrArn
    , FleetIdOrArn (..)

    -- * GameServerData
    , GameServerData (..)

    -- * AliasId
    , AliasId (..)

    -- * MatchmakerData
    , MatchmakerData (..)

    -- * FleetArn
    , FleetArn (..)

    -- * GameServerGroupDeleteOption
    , GameServerGroupDeleteOption (..)

    -- * FleetType
    , FleetType (..)

    -- * Alias
    , Alias (..)
    , mkAlias
    , aAliasArn
    , aAliasId
    , aCreationTime
    , aDescription
    , aLastUpdatedTime
    , aName
    , aRoutingStrategy

    -- * GameSessionPlacementState
    , GameSessionPlacementState (..)

    -- * VpcSubnet
    , VpcSubnet (..)

    -- * GameServerInstanceStatus
    , GameServerInstanceStatus (..)

    -- * TargetTrackingConfiguration
    , TargetTrackingConfiguration (..)
    , mkTargetTrackingConfiguration
    , ttcTargetValue

    -- * Build
    , Build (..)
    , mkBuild
    , bBuildArn
    , bBuildId
    , bCreationTime
    , bName
    , bOperatingSystem
    , bSizeOnDisk
    , bStatus
    , bVersion

    -- * GameServerId
    , GameServerId (..)

    -- * GamePropertyKey
    , GamePropertyKey (..)

    -- * SnsArnStringModel
    , SnsArnStringModel (..)

    -- * ScalingStatusType
    , ScalingStatusType (..)

    -- * PlayerSessionStatus
    , PlayerSessionStatus (..)

    -- * InstanceCredentials
    , InstanceCredentials (..)
    , mkInstanceCredentials
    , icSecret
    , icUserName

    -- * AcceptanceType
    , AcceptanceType (..)

    -- * GameSessionPlacement
    , GameSessionPlacement (..)
    , mkGameSessionPlacement
    , gspDnsName
    , gspEndTime
    , gspGameProperties
    , gspGameSessionArn
    , gspGameSessionData
    , gspGameSessionId
    , gspGameSessionName
    , gspGameSessionQueueName
    , gspGameSessionRegion
    , gspIpAddress
    , gspMatchmakerData
    , gspMaximumPlayerSessionCount
    , gspPlacedPlayerSessions
    , gspPlacementId
    , gspPlayerLatencies
    , gspPort
    , gspStartTime
    , gspStatus

    -- * FreeText
    , FreeText (..)

    -- * GameSessionDetail
    , GameSessionDetail (..)
    , mkGameSessionDetail
    , gsdGameSession
    , gsdProtectionPolicy

    -- * RuntimeConfiguration
    , RuntimeConfiguration (..)
    , mkRuntimeConfiguration
    , rcGameSessionActivationTimeoutSeconds
    , rcMaxConcurrentGameSessionActivations
    , rcServerProcesses

    -- * PlayerSessionCreationPolicy
    , PlayerSessionCreationPolicy (..)

    -- * GameServerGroup
    , GameServerGroup (..)
    , mkGameServerGroup
    , gsgAutoScalingGroupArn
    , gsgBalancingStrategy
    , gsgCreationTime
    , gsgGameServerGroupArn
    , gsgGameServerGroupName
    , gsgGameServerProtectionPolicy
    , gsgInstanceDefinitions
    , gsgLastUpdatedTime
    , gsgRoleArn
    , gsgStatus
    , gsgStatusReason
    , gsgSuspendedActions

    -- * FleetUtilization
    , FleetUtilization (..)
    , mkFleetUtilization
    , fuActiveGameSessionCount
    , fuActiveServerProcessCount
    , fuCurrentPlayerSessionCount
    , fuFleetId
    , fuMaximumPlayerSessionCount

    -- * RuleSetBody
    , RuleSetBody (..)

    -- * InstanceDefinition
    , InstanceDefinition (..)
    , mkInstanceDefinition
    , idInstanceType
    , idWeightedCapacity

    -- * VpcPeeringAuthorization
    , VpcPeeringAuthorization (..)
    , mkVpcPeeringAuthorization
    , vpaCreationTime
    , vpaExpirationTime
    , vpaGameLiftAwsAccountId
    , vpaPeerVpcAwsAccountId
    , vpaPeerVpcId

    -- * SortOrder
    , SortOrder (..)

    -- * GameSessionStatus
    , GameSessionStatus (..)

    -- * NonEmptyString
    , NonEmptyString (..)

    -- * FlexMatchMode
    , FlexMatchMode (..)

    -- * ArnStringModel
    , ArnStringModel (..)

    -- * LaunchTemplateVersion
    , LaunchTemplateVersion (..)

    -- * MatchmakingRuleSetName
    , MatchmakingRuleSetName (..)

    -- * RoutingStrategy
    , RoutingStrategy (..)
    , mkRoutingStrategy
    , rsFleetId
    , rsMessage
    , rsType

    -- * IpProtocol
    , IpProtocol (..)

    -- * CustomEventData
    , CustomEventData (..)

    -- * ScriptId
    , ScriptId (..)

    -- * ScriptIdOrArn
    , ScriptIdOrArn (..)

    -- * IpPermission
    , IpPermission (..)
    , mkIpPermission
    , ipFromPort
    , ipToPort
    , ipIpRange
    , ipProtocol

    -- * FleetCapacity
    , FleetCapacity (..)
    , mkFleetCapacity
    , fcFleetId
    , fcInstanceCounts
    , fcInstanceType

    -- * BuildStatus
    , BuildStatus (..)

    -- * ScriptArn
    , ScriptArn (..)

    -- * GameServerInstance
    , GameServerInstance (..)
    , mkGameServerInstance
    , gsiGameServerGroupArn
    , gsiGameServerGroupName
    , gsiInstanceId
    , gsiInstanceStatus

    -- * NonBlankAndLengthConstraintString
    , NonBlankAndLengthConstraintString (..)

    -- * EC2InstanceCounts
    , EC2InstanceCounts (..)
    , mkEC2InstanceCounts
    , ecicACTIVE
    , ecicDESIRED
    , ecicIDLE
    , ecicMAXIMUM
    , ecicMINIMUM
    , ecicPENDING
    , ecicTERMINATING

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * CertificateConfiguration
    , CertificateConfiguration (..)
    , mkCertificateConfiguration
    , ccCertificateType

    -- * AwsCredentials
    , AwsCredentials (..)
    , mkAwsCredentials
    , acAccessKeyId
    , acSecretAccessKey
    , acSessionToken

    -- * InstanceAccess
    , InstanceAccess (..)
    , mkInstanceAccess
    , iaCredentials
    , iaFleetId
    , iaInstanceId
    , iaIpAddress
    , iaOperatingSystem

    -- * VpcPeeringConnectionStatus
    , VpcPeeringConnectionStatus (..)
    , mkVpcPeeringConnectionStatus
    , vpcsCode
    , vpcsMessage

    -- * TagKey
    , TagKey (..)

    -- * S3Location
    , S3Location (..)
    , mkS3Location
    , slBucket
    , slKey
    , slObjectVersion
    , slRoleArn

    -- * GameServerGroupInstanceType
    , GameServerGroupInstanceType (..)

    -- * AutoScalingGroupArn
    , AutoScalingGroupArn (..)

    -- * EC2InstanceLimit
    , EC2InstanceLimit (..)
    , mkEC2InstanceLimit
    , ecilCurrentInstances
    , ecilEC2InstanceType
    , ecilInstanceLimit

    -- * LaunchTemplateSpecification
    , LaunchTemplateSpecification (..)
    , mkLaunchTemplateSpecification
    , ltsLaunchTemplateId
    , ltsLaunchTemplateName
    , ltsVersion

    -- * GameSession
    , GameSession (..)
    , mkGameSession
    , gsCreationTime
    , gsCreatorId
    , gsCurrentPlayerSessionCount
    , gsDnsName
    , gsFleetArn
    , gsFleetId
    , gsGameProperties
    , gsGameSessionData
    , gsGameSessionId
    , gsIpAddress
    , gsMatchmakerData
    , gsMaximumPlayerSessionCount
    , gsName
    , gsPlayerSessionCreationPolicy
    , gsPort
    , gsStatus
    , gsStatusReason
    , gsTerminationTime

    -- * PlayerSessionId
    , PlayerSessionId (..)

    -- * FleetAction
    , FleetAction (..)

    -- * GameSessionData
    , GameSessionData (..)

    -- * GameServerProtectionPolicy
    , GameServerProtectionPolicy (..)

    -- * GameServerGroupStatus
    , GameServerGroupStatus (..)

    -- * BuildArn
    , BuildArn (..)

    -- * ProtectionPolicy
    , ProtectionPolicy (..)

    -- * BuildIdOrArn
    , BuildIdOrArn (..)

    -- * MatchmakingIdStringModel
    , MatchmakingIdStringModel (..)

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * Player
    , Player (..)
    , mkPlayer
    , pLatencyInMs
    , pPlayerAttributes
    , pPlayerId
    , pTeam

    -- * AliasArn
    , AliasArn (..)

    -- * MetricGroup
    , MetricGroup (..)

    -- * FleetId
    , FleetId (..)

    -- * PlayerLatencyPolicy
    , PlayerLatencyPolicy (..)
    , mkPlayerLatencyPolicy
    , plpMaximumIndividualPlayerLatencyMilliseconds
    , plpPolicyDurationSeconds

    -- * AliasIdOrArn
    , AliasIdOrArn (..)

    -- * PlayerData
    , PlayerData (..)

    -- * MatchmakingConfigurationName
    , MatchmakingConfigurationName (..)

    -- * GameServerGroupNameOrArn
    , GameServerGroupNameOrArn (..)

    -- * DnsName
    , DnsName (..)

    -- * ServerProcess
    , ServerProcess (..)
    , mkServerProcess
    , spLaunchPath
    , spConcurrentExecutions
    , spParameters

    -- * GameServerGroupAutoScalingPolicy
    , GameServerGroupAutoScalingPolicy (..)
    , mkGameServerGroupAutoScalingPolicy
    , gsgaspTargetTrackingConfiguration
    , gsgaspEstimatedInstanceWarmup

    -- * GameSessionQueueNameOrArn
    , GameSessionQueueNameOrArn (..)

    -- * GameSessionQueueName
    , GameSessionQueueName (..)

    -- * ResourceCreationLimitPolicy
    , ResourceCreationLimitPolicy (..)
    , mkResourceCreationLimitPolicy
    , rclpNewGameSessionsPerCreator
    , rclpPolicyPeriodInMinutes

    -- * EventCode
    , EventCode (..)

    -- * VpcPeeringConnection
    , VpcPeeringConnection (..)
    , mkVpcPeeringConnection
    , vpcFleetArn
    , vpcFleetId
    , vpcGameLiftVpcId
    , vpcIpV4CidrBlock
    , vpcPeerVpcId
    , vpcStatus
    , vpcVpcPeeringConnectionId

    -- * FleetAttributes
    , FleetAttributes (..)
    , mkFleetAttributes
    , faBuildArn
    , faBuildId
    , faCertificateConfiguration
    , faCreationTime
    , faDescription
    , faFleetArn
    , faFleetId
    , faFleetType
    , faInstanceRoleArn
    , faInstanceType
    , faLogPaths
    , faMetricGroups
    , faName
    , faNewGameSessionProtectionPolicy
    , faOperatingSystem
    , faResourceCreationLimitPolicy
    , faScriptArn
    , faScriptId
    , faServerLaunchParameters
    , faServerLaunchPath
    , faStatus
    , faStoppedActions
    , faTerminationTime

    -- * GameServerGroupArn
    , GameServerGroupArn (..)

    -- * GameSessionQueue
    , GameSessionQueue (..)
    , mkGameSessionQueue
    , gsqDestinations
    , gsqGameSessionQueueArn
    , gsqName
    , gsqPlayerLatencyPolicies
    , gsqTimeoutInSeconds

    -- * GameServerHealthCheck
    , GameServerHealthCheck (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iCreationTime
    , iDnsName
    , iFleetId
    , iInstanceId
    , iIpAddress
    , iOperatingSystem
    , iStatus
    , iType

    -- * TargetConfiguration
    , TargetConfiguration (..)
    , mkTargetConfiguration
    , tcTargetValue

    -- * ComparisonOperatorType
    , ComparisonOperatorType (..)

    -- * GameProperty
    , GameProperty (..)
    , mkGameProperty
    , gpKey
    , gpValue

    -- * Message
    , Message (..)

    -- * Name
    , Name (..)

    -- * Description
    , Description (..)

    -- * InstanceRoleArn
    , InstanceRoleArn (..)

    -- * PeerVpcAwsAccountId
    , PeerVpcAwsAccountId (..)

    -- * PeerVpcId
    , PeerVpcId (..)

    -- * ServerLaunchParameters
    , ServerLaunchParameters (..)

    -- * ServerLaunchPath
    , ServerLaunchPath (..)

    -- * NextToken
    , NextToken (..)

    -- * RuleSetName
    , RuleSetName (..)

    -- * ConfigurationArn
    , ConfigurationArn (..)

    -- * NotificationTarget
    , NotificationTarget (..)

    -- * RuleSetArn
    , RuleSetArn (..)

    -- * GameSessionId
    , GameSessionId (..)

    -- * EventId
    , EventId (..)

    -- * PreSignedLogUrl
    , PreSignedLogUrl (..)

    -- * ResourceId
    , ResourceId (..)

    -- * Version
    , Version (..)

    -- * GameSessionName
    , GameSessionName (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ConfigurationName
    , ConfigurationName (..)

    -- * TicketId
    , TicketId (..)

    -- * DestinationArn
    , DestinationArn (..)

    -- * GameSessionArn
    , GameSessionArn (..)

    -- * ConnectionInfo
    , ConnectionInfo (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * Secret
    , Secret (..)

    -- * UserName
    , UserName (..)

    -- * IpRange
    , IpRange (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.GameLift.Types.GameSessionQueueArn
  
import Network.AWS.GameLift.Types.InstanceId
  
import Network.AWS.GameLift.Types.MatchmakingConfiguration
  
  
import Network.AWS.GameLift.Types.IamRoleArn
  
import Network.AWS.GameLift.Types.Event
  
import Network.AWS.GameLift.Types.BackfillMode
  
import Network.AWS.GameLift.Types.RoutingStrategyType
  
import Network.AWS.GameLift.Types.Script
  
import Network.AWS.GameLift.Types.IdStringModel
  
import Network.AWS.GameLift.Types.IpAddress
  
  
import Network.AWS.GameLift.Types.NonZeroAndMaxString
  
import Network.AWS.GameLift.Types.AttributeValue
  
import Network.AWS.GameLift.Types.MatchedPlayerSession
  
import Network.AWS.GameLift.Types.GameServerInstanceId
  
import Network.AWS.GameLift.Types.Tag
  
import Network.AWS.GameLift.Types.PlacedPlayerSession
  
import Network.AWS.GameLift.Types.LaunchTemplateName
  
import Network.AWS.GameLift.Types.FleetStatus
  
import Network.AWS.GameLift.Types.GamePropertyValue
  
import Network.AWS.GameLift.Types.GameServerGroupAction
  
import Network.AWS.GameLift.Types.ScalingAdjustmentType
  
import Network.AWS.GameLift.Types.StringModel
  
import Network.AWS.GameLift.Types.PlayerSession
  
import Network.AWS.GameLift.Types.GameSessionQueueDestination
  
import Network.AWS.GameLift.Types.GameServerClaimStatus
  
import Network.AWS.GameLift.Types.WeightedCapacity
  
import Network.AWS.GameLift.Types.ScalingPolicy
  
import Network.AWS.GameLift.Types.GameServerUtilizationStatus
  
import Network.AWS.GameLift.Types.CertificateType
  
import Network.AWS.GameLift.Types.DesiredPlayerSession
  
import Network.AWS.GameLift.Types.MatchmakingRuleSetArn
  
import Network.AWS.GameLift.Types.EC2InstanceType
  
import Network.AWS.GameLift.Types.OperatingSystem
  
import Network.AWS.GameLift.Types.GameSessionConnectionInfo
  
import Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
  
import Network.AWS.GameLift.Types.LaunchTemplateId
  
import Network.AWS.GameLift.Types.BalancingStrategy
  
import Network.AWS.GameLift.Types.PolicyType
  
import Network.AWS.GameLift.Types.MatchmakingRuleSet
  
import Network.AWS.GameLift.Types.MatchmakingTicket
  
import Network.AWS.GameLift.Types.GameSessionStatusReason
  
import Network.AWS.GameLift.Types.GameServer
  
import Network.AWS.GameLift.Types.MetricName
  
import Network.AWS.GameLift.Types.GameServerGroupName
  
import Network.AWS.GameLift.Types.BuildId
  
import Network.AWS.GameLift.Types.PlayerLatency
  
import Network.AWS.GameLift.Types.FleetIdOrArn
  
import Network.AWS.GameLift.Types.GameServerData
  
import Network.AWS.GameLift.Types.AliasId
  
import Network.AWS.GameLift.Types.MatchmakerData
  
import Network.AWS.GameLift.Types.FleetArn
  
import Network.AWS.GameLift.Types.GameServerGroupDeleteOption
  
import Network.AWS.GameLift.Types.FleetType
  
import Network.AWS.GameLift.Types.Alias
  
import Network.AWS.GameLift.Types.GameSessionPlacementState
  
import Network.AWS.GameLift.Types.VpcSubnet
  
import Network.AWS.GameLift.Types.GameServerInstanceStatus
  
import Network.AWS.GameLift.Types.TargetTrackingConfiguration
  
import Network.AWS.GameLift.Types.Build
  
import Network.AWS.GameLift.Types.GameServerId
  
import Network.AWS.GameLift.Types.GamePropertyKey
  
import Network.AWS.GameLift.Types.SnsArnStringModel
  
import Network.AWS.GameLift.Types.ScalingStatusType
  
import Network.AWS.GameLift.Types.PlayerSessionStatus
  
  
import Network.AWS.GameLift.Types.InstanceCredentials
  
import Network.AWS.GameLift.Types.AcceptanceType
  
import Network.AWS.GameLift.Types.GameSessionPlacement
  
  
import Network.AWS.GameLift.Types.FreeText
  
import Network.AWS.GameLift.Types.GameSessionDetail
  
  
import Network.AWS.GameLift.Types.RuntimeConfiguration
  
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
  
import Network.AWS.GameLift.Types.GameServerGroup
  
import Network.AWS.GameLift.Types.FleetUtilization
  
import Network.AWS.GameLift.Types.RuleSetBody
  
import Network.AWS.GameLift.Types.InstanceDefinition
  
  
import Network.AWS.GameLift.Types.VpcPeeringAuthorization
  
import Network.AWS.GameLift.Types.SortOrder
  
import Network.AWS.GameLift.Types.GameSessionStatus
  
  
import Network.AWS.GameLift.Types.NonEmptyString
  
import Network.AWS.GameLift.Types.FlexMatchMode
  
import Network.AWS.GameLift.Types.ArnStringModel
  
import Network.AWS.GameLift.Types.LaunchTemplateVersion
  
import Network.AWS.GameLift.Types.MatchmakingRuleSetName
  
import Network.AWS.GameLift.Types.RoutingStrategy
  
import Network.AWS.GameLift.Types.IpProtocol
  
import Network.AWS.GameLift.Types.CustomEventData
  
import Network.AWS.GameLift.Types.ScriptId
  
import Network.AWS.GameLift.Types.ScriptIdOrArn
  
import Network.AWS.GameLift.Types.IpPermission
  
import Network.AWS.GameLift.Types.FleetCapacity
  
import Network.AWS.GameLift.Types.BuildStatus
  
import Network.AWS.GameLift.Types.ScriptArn
  
  
import Network.AWS.GameLift.Types.GameServerInstance
  
import Network.AWS.GameLift.Types.NonBlankAndLengthConstraintString
  
  
import Network.AWS.GameLift.Types.EC2InstanceCounts
  
import Network.AWS.GameLift.Types.InstanceStatus
  
import Network.AWS.GameLift.Types.CertificateConfiguration
  
import Network.AWS.GameLift.Types.AwsCredentials
  
import Network.AWS.GameLift.Types.InstanceAccess
  
import Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
  
  
import Network.AWS.GameLift.Types.TagKey
  
import Network.AWS.GameLift.Types.S3Location
  
import Network.AWS.GameLift.Types.GameServerGroupInstanceType
  
import Network.AWS.GameLift.Types.AutoScalingGroupArn
  
import Network.AWS.GameLift.Types.EC2InstanceLimit
  
import Network.AWS.GameLift.Types.LaunchTemplateSpecification
  
import Network.AWS.GameLift.Types.GameSession
  
import Network.AWS.GameLift.Types.PlayerSessionId
  
import Network.AWS.GameLift.Types.FleetAction
  
import Network.AWS.GameLift.Types.GameSessionData
  
import Network.AWS.GameLift.Types.GameServerProtectionPolicy
  
import Network.AWS.GameLift.Types.GameServerGroupStatus
  
import Network.AWS.GameLift.Types.BuildArn
  
import Network.AWS.GameLift.Types.ProtectionPolicy
  
import Network.AWS.GameLift.Types.BuildIdOrArn
  
  
import Network.AWS.GameLift.Types.MatchmakingIdStringModel
  
import Network.AWS.GameLift.Types.AmazonResourceName
  
import Network.AWS.GameLift.Types.Player
  
  
import Network.AWS.GameLift.Types.AliasArn
  
import Network.AWS.GameLift.Types.MetricGroup
  
import Network.AWS.GameLift.Types.FleetId
  
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
  
import Network.AWS.GameLift.Types.AliasIdOrArn
  
import Network.AWS.GameLift.Types.PlayerData
  
import Network.AWS.GameLift.Types.MatchmakingConfigurationName
  
import Network.AWS.GameLift.Types.GameServerGroupNameOrArn
  
import Network.AWS.GameLift.Types.DnsName
  
import Network.AWS.GameLift.Types.ServerProcess
  
  
import Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
  
import Network.AWS.GameLift.Types.GameSessionQueueNameOrArn
  
import Network.AWS.GameLift.Types.GameSessionQueueName
  
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
  
import Network.AWS.GameLift.Types.EventCode
  
import Network.AWS.GameLift.Types.VpcPeeringConnection
  
import Network.AWS.GameLift.Types.FleetAttributes
  
import Network.AWS.GameLift.Types.GameServerGroupArn
  
import Network.AWS.GameLift.Types.GameSessionQueue
  
import Network.AWS.GameLift.Types.GameServerHealthCheck
  
import Network.AWS.GameLift.Types.Instance
  
import Network.AWS.GameLift.Types.TargetConfiguration
  
  
import Network.AWS.GameLift.Types.ComparisonOperatorType
  
import Network.AWS.GameLift.Types.GameProperty
  
import Network.AWS.GameLift.Types.Message
  
import Network.AWS.GameLift.Types.Name
  
import Network.AWS.GameLift.Types.Description
  
import Network.AWS.GameLift.Types.InstanceRoleArn
  
import Network.AWS.GameLift.Types.PeerVpcAwsAccountId
  
import Network.AWS.GameLift.Types.PeerVpcId
  
import Network.AWS.GameLift.Types.ServerLaunchParameters
  
import Network.AWS.GameLift.Types.ServerLaunchPath
  
import Network.AWS.GameLift.Types.NextToken
  
import Network.AWS.GameLift.Types.RuleSetName
  
import Network.AWS.GameLift.Types.ConfigurationArn
  
import Network.AWS.GameLift.Types.NotificationTarget
  
import Network.AWS.GameLift.Types.RuleSetArn
  
import Network.AWS.GameLift.Types.GameSessionId
  
import Network.AWS.GameLift.Types.EventId
  
import Network.AWS.GameLift.Types.PreSignedLogUrl
  
import Network.AWS.GameLift.Types.ResourceId
  
import Network.AWS.GameLift.Types.Version
  
import Network.AWS.GameLift.Types.GameSessionName
  
import Network.AWS.GameLift.Types.Key
  
import Network.AWS.GameLift.Types.Value
  
import Network.AWS.GameLift.Types.ConfigurationName
  
import Network.AWS.GameLift.Types.TicketId
  
import Network.AWS.GameLift.Types.DestinationArn
  
import Network.AWS.GameLift.Types.GameSessionArn
  
import Network.AWS.GameLift.Types.ConnectionInfo
  
import Network.AWS.GameLift.Types.ResourceARN
  
import Network.AWS.GameLift.Types.Secret
  
import Network.AWS.GameLift.Types.UserName
  
import Network.AWS.GameLift.Types.IpRange
  

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "GameLift",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "gamelift",
                 Core._svcVersion = "2015-10-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "GameLift",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The specified game server group has no available game servers to fulfill a @ClaimGameServer@ request. Clients can retry such requests immediately or after a waiting period. 
_OutOfCapacityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OutOfCapacityException
  = Core._MatchServiceError mkServiceConfig "OutOfCapacityException"
{-# INLINEABLE _OutOfCapacityException #-}
{-# DEPRECATED _OutOfCapacityException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation would cause a conflict with the current state of a resource associated with the request and/or the fleet. Resolve the conflict before retrying.
_InvalidFleetStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFleetStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidFleetStatusException"
{-# INLINEABLE _InvalidFleetStatusException #-}
{-# DEPRECATED _InvalidFleetStatusException "Use generic-lens or generic-optics instead"  #-}

-- | One or more parameter values in the request are invalid. Correct the invalid parameter values before retrying.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The requested tagging operation did not succeed. This may be due to invalid tag format or the maximum tag limit may have been exceeded. Resolve the issue before retrying. 
_TaggingFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaggingFailedException
  = Core._MatchServiceError mkServiceConfig "TaggingFailedException"
{-# INLINEABLE _TaggingFailedException #-}
{-# DEPRECATED _TaggingFailedException "Use generic-lens or generic-optics instead"  #-}

-- | The service is unable to resolve the routing for a particular alias because it has a terminal 'RoutingStrategy' associated with it. The message returned in this exception is the message defined in the routing strategy itself. Such requests should only be retried if the routing strategy for the specified alias is modified. 
_TerminalRoutingStrategyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TerminalRoutingStrategyException
  = Core._MatchServiceError mkServiceConfig
      "TerminalRoutingStrategyException"
{-# INLINEABLE _TerminalRoutingStrategyException #-}
{-# DEPRECATED _TerminalRoutingStrategyException "Use generic-lens or generic-optics instead"  #-}

-- | A service resource associated with the request could not be found. Clients should not retry such requests.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The game instance is currently full and cannot allow the requested player(s) to join. Clients can retry such requests immediately or after a waiting period.
_GameSessionFullException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GameSessionFullException
  = Core._MatchServiceError mkServiceConfig
      "GameSessionFullException"
{-# INLINEABLE _GameSessionFullException #-}
{-# DEPRECATED _GameSessionFullException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation is not supported in the Region specified.
_UnsupportedRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedRegionException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedRegionException"
{-# INLINEABLE _UnsupportedRegionException #-}
{-# DEPRECATED _UnsupportedRegionException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation would cause a conflict with the current state of a resource associated with the request and/or the game instance. Resolve the conflict before retrying.
_InvalidGameSessionStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGameSessionStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidGameSessionStatusException"
{-# INLINEABLE _InvalidGameSessionStatusException #-}
{-# DEPRECATED _InvalidGameSessionStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The service encountered an unrecoverable internal failure while processing the request. Clients can retry such requests immediately or after a waiting period.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | A game session with this custom ID string already exists in this fleet. Resolve this conflict before retrying this request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException
  = Core._MatchServiceError mkServiceConfig
      "IdempotentParameterMismatchException"
{-# INLINEABLE _IdempotentParameterMismatchException #-}
{-# DEPRECATED _IdempotentParameterMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The client failed authentication. Clients should not retry such requests.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException
  = Core._MatchServiceError mkServiceConfig "UnauthorizedException"
{-# INLINEABLE _UnauthorizedException #-}
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified fleet has no available instances to fulfill a @CreateGameSession@ request. Clients can retry such requests immediately or after a waiting period.
_FleetCapacityExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FleetCapacityExceededException
  = Core._MatchServiceError mkServiceConfig
      "FleetCapacityExceededException"
{-# INLINEABLE _FleetCapacityExceededException #-}
{-# DEPRECATED _FleetCapacityExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation would cause the resource to exceed the allowed service limit. Resolve the issue before retrying.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
