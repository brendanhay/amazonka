{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types
  ( -- * Service Configuration
    lexModels,

    -- * Errors

    -- * ChannelStatus
    ChannelStatus (..),

    -- * ChannelType
    ChannelType (..),

    -- * ContentType
    ContentType (..),

    -- * Destination
    Destination (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * ExportType
    ExportType (..),

    -- * FulfillmentActivityType
    FulfillmentActivityType (..),

    -- * ImportStatus
    ImportStatus (..),

    -- * LexStatus
    LexStatus (..),

    -- * Locale
    Locale (..),

    -- * LogType
    LogType (..),

    -- * MergeStrategy
    MergeStrategy (..),

    -- * ObfuscationSetting
    ObfuscationSetting (..),

    -- * ProcessBehavior
    ProcessBehavior (..),

    -- * ResourceType
    ResourceType (..),

    -- * SlotConstraint
    SlotConstraint (..),

    -- * SlotValueSelectionStrategy
    SlotValueSelectionStrategy (..),

    -- * StatusType
    StatusType (..),

    -- * BotAliasMetadata
    BotAliasMetadata,
    botAliasMetadata,
    bamChecksum,
    bamBotVersion,
    bamBotName,
    bamCreatedDate,
    bamName,
    bamConversationLogs,
    bamLastUpdatedDate,
    bamDescription,

    -- * BotChannelAssociation
    BotChannelAssociation,
    botChannelAssociation,
    bcaFailureReason,
    bcaStatus,
    bcaBotAlias,
    bcaBotName,
    bcaBotConfiguration,
    bcaCreatedDate,
    bcaName,
    bcaType,
    bcaDescription,

    -- * BotMetadata
    BotMetadata,
    botMetadata,
    bmStatus,
    bmCreatedDate,
    bmName,
    bmVersion,
    bmLastUpdatedDate,
    bmDescription,

    -- * BuiltinIntentMetadata
    BuiltinIntentMetadata,
    builtinIntentMetadata,
    bimSignature,
    bimSupportedLocales,

    -- * BuiltinIntentSlot
    BuiltinIntentSlot,
    builtinIntentSlot,
    bisName,

    -- * BuiltinSlotTypeMetadata
    BuiltinSlotTypeMetadata,
    builtinSlotTypeMetadata,
    bstmSignature,
    bstmSupportedLocales,

    -- * CodeHook
    CodeHook,
    codeHook,
    chUri,
    chMessageVersion,

    -- * ConversationLogsRequest
    ConversationLogsRequest,
    conversationLogsRequest,
    clrLogSettings,
    clrIamRoleARN,

    -- * ConversationLogsResponse
    ConversationLogsResponse,
    conversationLogsResponse,
    clIamRoleARN,
    clLogSettings,

    -- * EnumerationValue
    EnumerationValue,
    enumerationValue,
    evSynonyms,
    evValue,

    -- * FollowUpPrompt
    FollowUpPrompt,
    followUpPrompt,
    fupPrompt,
    fupRejectionStatement,

    -- * FulfillmentActivity
    FulfillmentActivity,
    fulfillmentActivity,
    faCodeHook,
    faType,

    -- * InputContext
    InputContext,
    inputContext,
    icName,

    -- * Intent
    Intent,
    intent,
    iIntentName,
    iIntentVersion,

    -- * IntentMetadata
    IntentMetadata,
    intentMetadata,
    imCreatedDate,
    imName,
    imVersion,
    imLastUpdatedDate,
    imDescription,

    -- * KendraConfiguration
    KendraConfiguration,
    kendraConfiguration,
    kcQueryFilterString,
    kcKendraIndex,
    kcRole,

    -- * LogSettingsRequest
    LogSettingsRequest,
    logSettingsRequest,
    lsrKmsKeyARN,
    lsrLogType,
    lsrDestination,
    lsrResourceARN,

    -- * LogSettingsResponse
    LogSettingsResponse,
    logSettingsResponse,
    lsDestination,
    lsKmsKeyARN,
    lsLogType,
    lsResourceARN,
    lsResourcePrefix,

    -- * Message
    Message,
    message,
    mGroupNumber,
    mContentType,
    mContent,

    -- * OutputContext
    OutputContext,
    outputContext,
    ocName,
    ocTimeToLiveInSeconds,
    ocTurnsToLive,

    -- * Prompt
    Prompt,
    prompt,
    pResponseCard,
    pMessages,
    pMaxAttempts,

    -- * Slot
    Slot,
    slot,
    sSlotType,
    sValueElicitationPrompt,
    sResponseCard,
    sPriority,
    sObfuscationSetting,
    sDefaultValueSpec,
    sSlotTypeVersion,
    sSampleUtterances,
    sDescription,
    sName,
    sSlotConstraint,

    -- * SlotDefaultValue
    SlotDefaultValue,
    slotDefaultValue,
    sdvDefaultValue,

    -- * SlotDefaultValueSpec
    SlotDefaultValueSpec,
    slotDefaultValueSpec,
    sdvsDefaultValueList,

    -- * SlotTypeConfiguration
    SlotTypeConfiguration,
    slotTypeConfiguration,
    stcRegexConfiguration,

    -- * SlotTypeMetadata
    SlotTypeMetadata,
    slotTypeMetadata,
    stmCreatedDate,
    stmName,
    stmVersion,
    stmLastUpdatedDate,
    stmDescription,

    -- * SlotTypeRegexConfiguration
    SlotTypeRegexConfiguration,
    slotTypeRegexConfiguration,
    strcPattern,

    -- * Statement
    Statement,
    statement,
    staResponseCard,
    staMessages,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * UtteranceData
    UtteranceData,
    utteranceData,
    udFirstUtteredDate,
    udCount,
    udUtteranceString,
    udLastUtteredDate,
    udDistinctUsers,

    -- * UtteranceList
    UtteranceList,
    utteranceList,
    ulBotVersion,
    ulUtterances,
  )
where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.BotAliasMetadata
import Network.AWS.LexModels.Types.BotChannelAssociation
import Network.AWS.LexModels.Types.BotMetadata
import Network.AWS.LexModels.Types.BuiltinIntentMetadata
import Network.AWS.LexModels.Types.BuiltinIntentSlot
import Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
import Network.AWS.LexModels.Types.ChannelStatus
import Network.AWS.LexModels.Types.ChannelType
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.ContentType
import Network.AWS.LexModels.Types.ConversationLogsRequest
import Network.AWS.LexModels.Types.ConversationLogsResponse
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.EnumerationValue
import Network.AWS.LexModels.Types.ExportStatus
import Network.AWS.LexModels.Types.ExportType
import Network.AWS.LexModels.Types.FollowUpPrompt
import Network.AWS.LexModels.Types.FulfillmentActivity
import Network.AWS.LexModels.Types.FulfillmentActivityType
import Network.AWS.LexModels.Types.ImportStatus
import Network.AWS.LexModels.Types.InputContext
import Network.AWS.LexModels.Types.Intent
import Network.AWS.LexModels.Types.IntentMetadata
import Network.AWS.LexModels.Types.KendraConfiguration
import Network.AWS.LexModels.Types.LexStatus
import Network.AWS.LexModels.Types.Locale
import Network.AWS.LexModels.Types.LogSettingsRequest
import Network.AWS.LexModels.Types.LogSettingsResponse
import Network.AWS.LexModels.Types.LogType
import Network.AWS.LexModels.Types.MergeStrategy
import Network.AWS.LexModels.Types.Message
import Network.AWS.LexModels.Types.ObfuscationSetting
import Network.AWS.LexModels.Types.OutputContext
import Network.AWS.LexModels.Types.ProcessBehavior
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.ResourceType
import Network.AWS.LexModels.Types.Slot
import Network.AWS.LexModels.Types.SlotConstraint
import Network.AWS.LexModels.Types.SlotDefaultValue
import Network.AWS.LexModels.Types.SlotDefaultValueSpec
import Network.AWS.LexModels.Types.SlotTypeConfiguration
import Network.AWS.LexModels.Types.SlotTypeMetadata
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
import Network.AWS.LexModels.Types.SlotValueSelectionStrategy
import Network.AWS.LexModels.Types.Statement
import Network.AWS.LexModels.Types.StatusType
import Network.AWS.LexModels.Types.Tag
import Network.AWS.LexModels.Types.UtteranceData
import Network.AWS.LexModels.Types.UtteranceList
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
lexModels :: Service
lexModels =
  Service
    { _svcAbbrev = "LexModels",
      _svcSigner = v4,
      _svcPrefix = "models.lex",
      _svcVersion = "2017-04-19",
      _svcEndpoint = defaultEndpoint lexModels,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "LexModels",
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
