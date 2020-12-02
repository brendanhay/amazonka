{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types
  ( -- * Service Configuration
    cloudWatchEvents,

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
    AWSVPCConfiguration,
    awsVPCConfiguration,
    avcSecurityGroups,
    avcAssignPublicIP,
    avcSubnets,

    -- * Archive
    Archive,
    archive,
    aCreationTime,
    aSizeBytes,
    aEventSourceARN,
    aState,
    aEventCount,
    aArchiveName,
    aRetentionDays,
    aStateReason,

    -- * BatchArrayProperties
    BatchArrayProperties,
    batchArrayProperties,
    bapSize,

    -- * BatchParameters
    BatchParameters,
    batchParameters,
    bpRetryStrategy,
    bpArrayProperties,
    bpJobDefinition,
    bpJobName,

    -- * BatchRetryStrategy
    BatchRetryStrategy,
    batchRetryStrategy,
    brsAttempts,

    -- * Condition
    Condition,
    condition,
    cType,
    cKey,
    cValue,

    -- * DeadLetterConfig
    DeadLetterConfig,
    deadLetterConfig,
    dlcARN,

    -- * EcsParameters
    EcsParameters,
    ecsParameters,
    epGroup,
    epPlatformVersion,
    epLaunchType,
    epTaskCount,
    epNetworkConfiguration,
    epTaskDefinitionARN,

    -- * EventBus
    EventBus,
    eventBus,
    ebARN,
    ebName,
    ebPolicy,

    -- * EventSource
    EventSource,
    eventSource,
    esCreationTime,
    esState,
    esARN,
    esCreatedBy,
    esName,
    esExpirationTime,

    -- * HTTPParameters
    HTTPParameters,
    hTTPParameters,
    httppPathParameterValues,
    httppQueryStringParameters,
    httppHeaderParameters,

    -- * InputTransformer
    InputTransformer,
    inputTransformer,
    itInputPathsMap,
    itInputTemplate,

    -- * KinesisParameters
    KinesisParameters,
    kinesisParameters,
    kpPartitionKeyPath,

    -- * NetworkConfiguration
    NetworkConfiguration,
    networkConfiguration,
    ncAwsvpcConfiguration,

    -- * PartnerEventSource
    PartnerEventSource,
    partnerEventSource,
    pesARN,
    pesName,

    -- * PartnerEventSourceAccount
    PartnerEventSourceAccount,
    partnerEventSourceAccount,
    pesaCreationTime,
    pesaState,
    pesaAccount,
    pesaExpirationTime,

    -- * PutEventsRequestEntry
    PutEventsRequestEntry,
    putEventsRequestEntry,
    pereTime,
    pereDetailType,
    pereResources,
    pereEventBusName,
    pereSource,
    pereDetail,

    -- * PutEventsResultEntry
    PutEventsResultEntry,
    putEventsResultEntry,
    pereErrorCode,
    pereErrorMessage,
    pereEventId,

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry,
    putPartnerEventsRequestEntry,
    ppereTime,
    ppereDetailType,
    ppereResources,
    ppereSource,
    ppereDetail,

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry,
    putPartnerEventsResultEntry,
    ppereErrorCode,
    ppereErrorMessage,
    ppereEventId,

    -- * PutTargetsResultEntry
    PutTargetsResultEntry,
    putTargetsResultEntry,
    ptreTargetId,
    ptreErrorCode,
    ptreErrorMessage,

    -- * RedshiftDataParameters
    RedshiftDataParameters,
    redshiftDataParameters,
    rdpDBUser,
    rdpSecretManagerARN,
    rdpStatementName,
    rdpWithEvent,
    rdpDatabase,
    rdpSql,

    -- * RemoveTargetsResultEntry
    RemoveTargetsResultEntry,
    removeTargetsResultEntry,
    rtreTargetId,
    rtreErrorCode,
    rtreErrorMessage,

    -- * Replay
    Replay,
    replay,
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
    ReplayDestination,
    replayDestination,
    rdFilterARNs,
    rdARN,

    -- * RetryPolicy
    RetryPolicy,
    retryPolicy,
    rpMaximumEventAgeInSeconds,
    rpMaximumRetryAttempts,

    -- * Rule
    Rule,
    rule,
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
    RunCommandParameters,
    runCommandParameters,
    rcpRunCommandTargets,

    -- * RunCommandTarget
    RunCommandTarget,
    runCommandTarget,
    rctKey,
    rctValues,

    -- * SqsParameters
    SqsParameters,
    sqsParameters,
    spMessageGroupId,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Target
    Target,
    target,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
cloudWatchEvents :: Service
cloudWatchEvents =
  Service
    { _svcAbbrev = "CloudWatchEvents",
      _svcSigner = v4,
      _svcPrefix = "events",
      _svcVersion = "2015-10-07",
      _svcEndpoint = defaultEndpoint cloudWatchEvents,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CloudWatchEvents",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
