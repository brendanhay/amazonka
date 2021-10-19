{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _PreconditionFailedException,
    _ConflictException,
    _NotFoundException,
    _InternalFailureException,
    _BadRequestException,
    _LimitExceededException,
    _ResourceInUseException,

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

    -- * MigrationAlertType
    MigrationAlertType (..),

    -- * MigrationSortAttribute
    MigrationSortAttribute (..),

    -- * MigrationStatus
    MigrationStatus (..),

    -- * MigrationStrategy
    MigrationStrategy (..),

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

    -- * SortOrder
    SortOrder (..),

    -- * StatusType
    StatusType (..),

    -- * BotAliasMetadata
    BotAliasMetadata (..),
    newBotAliasMetadata,
    botAliasMetadata_checksum,
    botAliasMetadata_botVersion,
    botAliasMetadata_botName,
    botAliasMetadata_createdDate,
    botAliasMetadata_name,
    botAliasMetadata_conversationLogs,
    botAliasMetadata_lastUpdatedDate,
    botAliasMetadata_description,

    -- * BotChannelAssociation
    BotChannelAssociation (..),
    newBotChannelAssociation,
    botChannelAssociation_failureReason,
    botChannelAssociation_status,
    botChannelAssociation_botAlias,
    botChannelAssociation_botName,
    botChannelAssociation_botConfiguration,
    botChannelAssociation_createdDate,
    botChannelAssociation_name,
    botChannelAssociation_type,
    botChannelAssociation_description,

    -- * BotMetadata
    BotMetadata (..),
    newBotMetadata,
    botMetadata_status,
    botMetadata_createdDate,
    botMetadata_name,
    botMetadata_version,
    botMetadata_lastUpdatedDate,
    botMetadata_description,

    -- * BuiltinIntentMetadata
    BuiltinIntentMetadata (..),
    newBuiltinIntentMetadata,
    builtinIntentMetadata_signature,
    builtinIntentMetadata_supportedLocales,

    -- * BuiltinIntentSlot
    BuiltinIntentSlot (..),
    newBuiltinIntentSlot,
    builtinIntentSlot_name,

    -- * BuiltinSlotTypeMetadata
    BuiltinSlotTypeMetadata (..),
    newBuiltinSlotTypeMetadata,
    builtinSlotTypeMetadata_signature,
    builtinSlotTypeMetadata_supportedLocales,

    -- * CodeHook
    CodeHook (..),
    newCodeHook,
    codeHook_uri,
    codeHook_messageVersion,

    -- * ConversationLogsRequest
    ConversationLogsRequest (..),
    newConversationLogsRequest,
    conversationLogsRequest_logSettings,
    conversationLogsRequest_iamRoleArn,

    -- * ConversationLogsResponse
    ConversationLogsResponse (..),
    newConversationLogsResponse,
    conversationLogsResponse_iamRoleArn,
    conversationLogsResponse_logSettings,

    -- * EnumerationValue
    EnumerationValue (..),
    newEnumerationValue,
    enumerationValue_synonyms,
    enumerationValue_value,

    -- * FollowUpPrompt
    FollowUpPrompt (..),
    newFollowUpPrompt,
    followUpPrompt_prompt,
    followUpPrompt_rejectionStatement,

    -- * FulfillmentActivity
    FulfillmentActivity (..),
    newFulfillmentActivity,
    fulfillmentActivity_codeHook,
    fulfillmentActivity_type,

    -- * InputContext
    InputContext (..),
    newInputContext,
    inputContext_name,

    -- * Intent
    Intent (..),
    newIntent,
    intent_intentName,
    intent_intentVersion,

    -- * IntentMetadata
    IntentMetadata (..),
    newIntentMetadata,
    intentMetadata_createdDate,
    intentMetadata_name,
    intentMetadata_version,
    intentMetadata_lastUpdatedDate,
    intentMetadata_description,

    -- * KendraConfiguration
    KendraConfiguration (..),
    newKendraConfiguration,
    kendraConfiguration_queryFilterString,
    kendraConfiguration_kendraIndex,
    kendraConfiguration_role,

    -- * LogSettingsRequest
    LogSettingsRequest (..),
    newLogSettingsRequest,
    logSettingsRequest_kmsKeyArn,
    logSettingsRequest_logType,
    logSettingsRequest_destination,
    logSettingsRequest_resourceArn,

    -- * LogSettingsResponse
    LogSettingsResponse (..),
    newLogSettingsResponse,
    logSettingsResponse_destination,
    logSettingsResponse_kmsKeyArn,
    logSettingsResponse_logType,
    logSettingsResponse_resourceArn,
    logSettingsResponse_resourcePrefix,

    -- * Message
    Message (..),
    newMessage,
    message_groupNumber,
    message_contentType,
    message_content,

    -- * MigrationAlert
    MigrationAlert (..),
    newMigrationAlert,
    migrationAlert_referenceURLs,
    migrationAlert_details,
    migrationAlert_type,
    migrationAlert_message,

    -- * MigrationSummary
    MigrationSummary (..),
    newMigrationSummary,
    migrationSummary_v1BotVersion,
    migrationSummary_migrationStrategy,
    migrationSummary_migrationTimestamp,
    migrationSummary_migrationStatus,
    migrationSummary_v2BotId,
    migrationSummary_v1BotLocale,
    migrationSummary_v1BotName,
    migrationSummary_v2BotRole,
    migrationSummary_migrationId,

    -- * OutputContext
    OutputContext (..),
    newOutputContext,
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- * Prompt
    Prompt (..),
    newPrompt,
    prompt_responseCard,
    prompt_messages,
    prompt_maxAttempts,

    -- * Slot
    Slot (..),
    newSlot,
    slot_slotType,
    slot_valueElicitationPrompt,
    slot_responseCard,
    slot_priority,
    slot_obfuscationSetting,
    slot_defaultValueSpec,
    slot_slotTypeVersion,
    slot_sampleUtterances,
    slot_description,
    slot_name,
    slot_slotConstraint,

    -- * SlotDefaultValue
    SlotDefaultValue (..),
    newSlotDefaultValue,
    slotDefaultValue_defaultValue,

    -- * SlotDefaultValueSpec
    SlotDefaultValueSpec (..),
    newSlotDefaultValueSpec,
    slotDefaultValueSpec_defaultValueList,

    -- * SlotTypeConfiguration
    SlotTypeConfiguration (..),
    newSlotTypeConfiguration,
    slotTypeConfiguration_regexConfiguration,

    -- * SlotTypeMetadata
    SlotTypeMetadata (..),
    newSlotTypeMetadata,
    slotTypeMetadata_createdDate,
    slotTypeMetadata_name,
    slotTypeMetadata_version,
    slotTypeMetadata_lastUpdatedDate,
    slotTypeMetadata_description,

    -- * SlotTypeRegexConfiguration
    SlotTypeRegexConfiguration (..),
    newSlotTypeRegexConfiguration,
    slotTypeRegexConfiguration_pattern,

    -- * Statement
    Statement (..),
    newStatement,
    statement_responseCard,
    statement_messages,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UtteranceData
    UtteranceData (..),
    newUtteranceData,
    utteranceData_firstUtteredDate,
    utteranceData_count,
    utteranceData_utteranceString,
    utteranceData_lastUtteredDate,
    utteranceData_distinctUsers,

    -- * UtteranceList
    UtteranceList (..),
    newUtteranceList,
    utteranceList_botVersion,
    utteranceList_utterances,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.LexModels.Types.MigrationAlert
import Network.AWS.LexModels.Types.MigrationAlertType
import Network.AWS.LexModels.Types.MigrationSortAttribute
import Network.AWS.LexModels.Types.MigrationStatus
import Network.AWS.LexModels.Types.MigrationStrategy
import Network.AWS.LexModels.Types.MigrationSummary
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
import Network.AWS.LexModels.Types.SortOrder
import Network.AWS.LexModels.Types.Statement
import Network.AWS.LexModels.Types.StatusType
import Network.AWS.LexModels.Types.Tag
import Network.AWS.LexModels.Types.UtteranceData
import Network.AWS.LexModels.Types.UtteranceList
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LexModels",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "models.lex",
      Core._serviceSigningName = "lex",
      Core._serviceVersion = "2017-04-19",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "LexModels",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Your IAM user or role does not have permission to call the Amazon Lex V2
-- APIs required to migrate your bot.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The checksum of the resource that you are trying to change does not
-- match the checksum in the request. Check the resource\'s checksum and
-- try again.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource specified in the request was not found. Check the resource
-- and try again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | An internal Amazon Lex error occurred. Try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The request is not well formed. For example, a value is invalid or a
-- required field is missing. Check the field values, and try again.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource that you are attempting to delete is referred to by another
-- resource. Use this information to remove references to the resource that
-- you are trying to delete.
--
-- The body of the exception contains a JSON object that describes the
-- resource.
--
-- @{ \"resourceType\": BOT | BOTALIAS | BOTCHANNEL | INTENT,@
--
-- @\"resourceReference\": {@
--
-- @\"name\": string, \"version\": string } }@
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400
