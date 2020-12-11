-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types
  ( -- * Service configuration
    lexModelsService,

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
    BotAliasMetadata (..),
    mkBotAliasMetadata,
    bamChecksum,
    bamBotVersion,
    bamBotName,
    bamCreatedDate,
    bamName,
    bamConversationLogs,
    bamLastUpdatedDate,
    bamDescription,

    -- * BotChannelAssociation
    BotChannelAssociation (..),
    mkBotChannelAssociation,
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
    BotMetadata (..),
    mkBotMetadata,
    bmStatus,
    bmCreatedDate,
    bmName,
    bmVersion,
    bmLastUpdatedDate,
    bmDescription,

    -- * BuiltinIntentMetadata
    BuiltinIntentMetadata (..),
    mkBuiltinIntentMetadata,
    bimSignature,
    bimSupportedLocales,

    -- * BuiltinIntentSlot
    BuiltinIntentSlot (..),
    mkBuiltinIntentSlot,
    bisName,

    -- * BuiltinSlotTypeMetadata
    BuiltinSlotTypeMetadata (..),
    mkBuiltinSlotTypeMetadata,
    bstmSignature,
    bstmSupportedLocales,

    -- * CodeHook
    CodeHook (..),
    mkCodeHook,
    chUri,
    chMessageVersion,

    -- * ConversationLogsRequest
    ConversationLogsRequest (..),
    mkConversationLogsRequest,
    clrLogSettings,
    clrIamRoleARN,

    -- * ConversationLogsResponse
    ConversationLogsResponse (..),
    mkConversationLogsResponse,
    clIamRoleARN,
    clLogSettings,

    -- * EnumerationValue
    EnumerationValue (..),
    mkEnumerationValue,
    evSynonyms,
    evValue,

    -- * FollowUpPrompt
    FollowUpPrompt (..),
    mkFollowUpPrompt,
    fupPrompt,
    fupRejectionStatement,

    -- * FulfillmentActivity
    FulfillmentActivity (..),
    mkFulfillmentActivity,
    faCodeHook,
    faType,

    -- * InputContext
    InputContext (..),
    mkInputContext,
    icName,

    -- * Intent
    Intent (..),
    mkIntent,
    iIntentName,
    iIntentVersion,

    -- * IntentMetadata
    IntentMetadata (..),
    mkIntentMetadata,
    imCreatedDate,
    imName,
    imVersion,
    imLastUpdatedDate,
    imDescription,

    -- * KendraConfiguration
    KendraConfiguration (..),
    mkKendraConfiguration,
    kcQueryFilterString,
    kcKendraIndex,
    kcRole,

    -- * LogSettingsRequest
    LogSettingsRequest (..),
    mkLogSettingsRequest,
    lsrKmsKeyARN,
    lsrLogType,
    lsrDestination,
    lsrResourceARN,

    -- * LogSettingsResponse
    LogSettingsResponse (..),
    mkLogSettingsResponse,
    lsDestination,
    lsKmsKeyARN,
    lsLogType,
    lsResourceARN,
    lsResourcePrefix,

    -- * Message
    Message (..),
    mkMessage,
    mGroupNumber,
    mContentType,
    mContent,

    -- * OutputContext
    OutputContext (..),
    mkOutputContext,
    ocName,
    ocTimeToLiveInSeconds,
    ocTurnsToLive,

    -- * Prompt
    Prompt (..),
    mkPrompt,
    pResponseCard,
    pMessages,
    pMaxAttempts,

    -- * Slot
    Slot (..),
    mkSlot,
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
    SlotDefaultValue (..),
    mkSlotDefaultValue,
    sdvDefaultValue,

    -- * SlotDefaultValueSpec
    SlotDefaultValueSpec (..),
    mkSlotDefaultValueSpec,
    sdvsDefaultValueList,

    -- * SlotTypeConfiguration
    SlotTypeConfiguration (..),
    mkSlotTypeConfiguration,
    stcRegexConfiguration,

    -- * SlotTypeMetadata
    SlotTypeMetadata (..),
    mkSlotTypeMetadata,
    stmCreatedDate,
    stmName,
    stmVersion,
    stmLastUpdatedDate,
    stmDescription,

    -- * SlotTypeRegexConfiguration
    SlotTypeRegexConfiguration (..),
    mkSlotTypeRegexConfiguration,
    strcPattern,

    -- * Statement
    Statement (..),
    mkStatement,
    staResponseCard,
    staMessages,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * UtteranceData
    UtteranceData (..),
    mkUtteranceData,
    udFirstUtteredDate,
    udCount,
    udUtteranceString,
    udLastUtteredDate,
    udDistinctUsers,

    -- * UtteranceList
    UtteranceList (..),
    mkUtteranceList,
    ulBotVersion,
    ulUtterances,
  )
where

import qualified Network.AWS.Lens as Lens
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
lexModelsService :: Lude.Service
lexModelsService =
  Lude.Service
    { Lude._svcAbbrev = "LexModels",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "models.lex",
      Lude._svcVersion = "2017-04-19",
      Lude._svcEndpoint = Lude.defaultEndpoint lexModelsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "LexModels",
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
