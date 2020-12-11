-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types
  ( -- * Service configuration
    cloudWatchEventsService,

    -- * Errors

    -- * ArchiveState
    ArchiveState (..),

    -- * AssignPublicIP
    AssignPublicIP (..),

    -- * EventSourceState
    EventSourceState (..),

    -- * LaunchType
    LaunchType (..),

    -- * ReplayState
    ReplayState (..),

    -- * RuleState
    RuleState (..),

    -- * AWSVPCConfiguration
    AWSVPCConfiguration (..),
    mkAWSVPCConfiguration,
    avcSecurityGroups,
    avcAssignPublicIP,
    avcSubnets,

    -- * Archive
    Archive (..),
    mkArchive,
    aCreationTime,
    aSizeBytes,
    aEventSourceARN,
    aState,
    aEventCount,
    aArchiveName,
    aRetentionDays,
    aStateReason,

    -- * BatchArrayProperties
    BatchArrayProperties (..),
    mkBatchArrayProperties,
    bapSize,

    -- * BatchParameters
    BatchParameters (..),
    mkBatchParameters,
    bpRetryStrategy,
    bpArrayProperties,
    bpJobDefinition,
    bpJobName,

    -- * BatchRetryStrategy
    BatchRetryStrategy (..),
    mkBatchRetryStrategy,
    brsAttempts,

    -- * Condition
    Condition (..),
    mkCondition,
    cType,
    cKey,
    cValue,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    mkDeadLetterConfig,
    dlcARN,

    -- * EcsParameters
    EcsParameters (..),
    mkEcsParameters,
    epGroup,
    epPlatformVersion,
    epLaunchType,
    epTaskCount,
    epNetworkConfiguration,
    epTaskDefinitionARN,

    -- * EventBus
    EventBus (..),
    mkEventBus,
    ebARN,
    ebName,
    ebPolicy,

    -- * EventSource
    EventSource (..),
    mkEventSource,
    esCreationTime,
    esState,
    esARN,
    esCreatedBy,
    esName,
    esExpirationTime,

    -- * HTTPParameters
    HTTPParameters (..),
    mkHTTPParameters,
    httppPathParameterValues,
    httppQueryStringParameters,
    httppHeaderParameters,

    -- * InputTransformer
    InputTransformer (..),
    mkInputTransformer,
    itInputPathsMap,
    itInputTemplate,

    -- * KinesisParameters
    KinesisParameters (..),
    mkKinesisParameters,
    kpPartitionKeyPath,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    mkNetworkConfiguration,
    ncAwsvpcConfiguration,

    -- * PartnerEventSource
    PartnerEventSource (..),
    mkPartnerEventSource,
    pesARN,
    pesName,

    -- * PartnerEventSourceAccount
    PartnerEventSourceAccount (..),
    mkPartnerEventSourceAccount,
    pesaCreationTime,
    pesaState,
    pesaAccount,
    pesaExpirationTime,

    -- * PutEventsRequestEntry
    PutEventsRequestEntry (..),
    mkPutEventsRequestEntry,
    pereTime,
    pereDetailType,
    pereResources,
    pereEventBusName,
    pereSource,
    pereDetail,

    -- * PutEventsResultEntry
    PutEventsResultEntry (..),
    mkPutEventsResultEntry,
    pereErrorCode,
    pereErrorMessage,
    pereEventId,

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry (..),
    mkPutPartnerEventsRequestEntry,
    ppereTime,
    ppereDetailType,
    ppereResources,
    ppereSource,
    ppereDetail,

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry (..),
    mkPutPartnerEventsResultEntry,
    ppereErrorCode,
    ppereErrorMessage,
    ppereEventId,

    -- * PutTargetsResultEntry
    PutTargetsResultEntry (..),
    mkPutTargetsResultEntry,
    ptreTargetId,
    ptreErrorCode,
    ptreErrorMessage,

    -- * RedshiftDataParameters
    RedshiftDataParameters (..),
    mkRedshiftDataParameters,
    rdpDBUser,
    rdpSecretManagerARN,
    rdpStatementName,
    rdpWithEvent,
    rdpDatabase,
    rdpSql,

    -- * RemoveTargetsResultEntry
    RemoveTargetsResultEntry (..),
    mkRemoveTargetsResultEntry,
    rtreTargetId,
    rtreErrorCode,
    rtreErrorMessage,

    -- * Replay
    Replay (..),
    mkReplay,
    repEventSourceARN,
    repState,
    repEventEndTime,
    repReplayStartTime,
    repReplayEndTime,
    repEventLastReplayedTime,
    repEventStartTime,
    repReplayName,
    repStateReason,

    -- * ReplayDestination
    ReplayDestination (..),
    mkReplayDestination,
    rdFilterARNs,
    rdARN,

    -- * RetryPolicy
    RetryPolicy (..),
    mkRetryPolicy,
    rpMaximumEventAgeInSeconds,
    rpMaximumRetryAttempts,

    -- * Rule
    Rule (..),
    mkRule,
    rEventPattern,
    rState,
    rARN,
    rEventBusName,
    rScheduleExpression,
    rName,
    rDescription,
    rManagedBy,
    rRoleARN,

    -- * RunCommandParameters
    RunCommandParameters (..),
    mkRunCommandParameters,
    rcpRunCommandTargets,

    -- * RunCommandTarget
    RunCommandTarget (..),
    mkRunCommandTarget,
    rctKey,
    rctValues,

    -- * SqsParameters
    SqsParameters (..),
    mkSqsParameters,
    spMessageGroupId,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Target
    Target (..),
    mkTarget,
    tRunCommandParameters,
    tHTTPParameters,
    tKinesisParameters,
    tInputTransformer,
    tDeadLetterConfig,
    tSqsParameters,
    tInput,
    tBatchParameters,
    tRedshiftDataParameters,
    tEcsParameters,
    tRetryPolicy,
    tInputPath,
    tRoleARN,
    tId,
    tARN,
  )
where

import Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration
import Network.AWS.CloudWatchEvents.Types.Archive
import Network.AWS.CloudWatchEvents.Types.ArchiveState
import Network.AWS.CloudWatchEvents.Types.AssignPublicIP
import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import Network.AWS.CloudWatchEvents.Types.Condition
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.EventBus
import Network.AWS.CloudWatchEvents.Types.EventSource
import Network.AWS.CloudWatchEvents.Types.EventSourceState
import Network.AWS.CloudWatchEvents.Types.HTTPParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import Network.AWS.CloudWatchEvents.Types.PartnerEventSource
import Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
import Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.Replay
import Network.AWS.CloudWatchEvents.Types.ReplayDestination
import Network.AWS.CloudWatchEvents.Types.ReplayState
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.Rule
import Network.AWS.CloudWatchEvents.Types.RuleState
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import Network.AWS.CloudWatchEvents.Types.Tag
import Network.AWS.CloudWatchEvents.Types.Target
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
cloudWatchEventsService :: Lude.Service
cloudWatchEventsService =
  Lude.Service
    { Lude._svcAbbrev = "CloudWatchEvents",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "events",
      Lude._svcVersion = "2015-10-07",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudWatchEventsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudWatchEvents",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
