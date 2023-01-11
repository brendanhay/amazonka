{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexModels.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Lens
  ( -- * Operations

    -- ** CreateBotVersion
    createBotVersion_checksum,
    createBotVersion_name,
    createBotVersionResponse_abortStatement,
    createBotVersionResponse_checksum,
    createBotVersionResponse_childDirected,
    createBotVersionResponse_clarificationPrompt,
    createBotVersionResponse_createdDate,
    createBotVersionResponse_description,
    createBotVersionResponse_detectSentiment,
    createBotVersionResponse_enableModelImprovements,
    createBotVersionResponse_failureReason,
    createBotVersionResponse_idleSessionTTLInSeconds,
    createBotVersionResponse_intents,
    createBotVersionResponse_lastUpdatedDate,
    createBotVersionResponse_locale,
    createBotVersionResponse_name,
    createBotVersionResponse_status,
    createBotVersionResponse_version,
    createBotVersionResponse_voiceId,
    createBotVersionResponse_httpStatus,

    -- ** CreateIntentVersion
    createIntentVersion_checksum,
    createIntentVersion_name,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_description,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_name,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_version,
    createIntentVersionResponse_httpStatus,

    -- ** CreateSlotTypeVersion
    createSlotTypeVersion_checksum,
    createSlotTypeVersion_name,
    createSlotTypeVersionResponse_checksum,
    createSlotTypeVersionResponse_createdDate,
    createSlotTypeVersionResponse_description,
    createSlotTypeVersionResponse_enumerationValues,
    createSlotTypeVersionResponse_lastUpdatedDate,
    createSlotTypeVersionResponse_name,
    createSlotTypeVersionResponse_parentSlotTypeSignature,
    createSlotTypeVersionResponse_slotTypeConfigurations,
    createSlotTypeVersionResponse_valueSelectionStrategy,
    createSlotTypeVersionResponse_version,
    createSlotTypeVersionResponse_httpStatus,

    -- ** DeleteBot
    deleteBot_name,

    -- ** DeleteBotAlias
    deleteBotAlias_name,
    deleteBotAlias_botName,

    -- ** DeleteBotChannelAssociation
    deleteBotChannelAssociation_name,
    deleteBotChannelAssociation_botName,
    deleteBotChannelAssociation_botAlias,

    -- ** DeleteBotVersion
    deleteBotVersion_name,
    deleteBotVersion_version,

    -- ** DeleteIntent
    deleteIntent_name,

    -- ** DeleteIntentVersion
    deleteIntentVersion_name,
    deleteIntentVersion_version,

    -- ** DeleteSlotType
    deleteSlotType_name,

    -- ** DeleteSlotTypeVersion
    deleteSlotTypeVersion_name,
    deleteSlotTypeVersion_version,

    -- ** DeleteUtterances
    deleteUtterances_botName,
    deleteUtterances_userId,

    -- ** GetBot
    getBot_name,
    getBot_versionOrAlias,
    getBotResponse_abortStatement,
    getBotResponse_checksum,
    getBotResponse_childDirected,
    getBotResponse_clarificationPrompt,
    getBotResponse_createdDate,
    getBotResponse_description,
    getBotResponse_detectSentiment,
    getBotResponse_enableModelImprovements,
    getBotResponse_failureReason,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_intents,
    getBotResponse_lastUpdatedDate,
    getBotResponse_locale,
    getBotResponse_name,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_status,
    getBotResponse_version,
    getBotResponse_voiceId,
    getBotResponse_httpStatus,

    -- ** GetBotAlias
    getBotAlias_name,
    getBotAlias_botName,
    getBotAliasResponse_botName,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_checksum,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_createdDate,
    getBotAliasResponse_description,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_name,
    getBotAliasResponse_httpStatus,

    -- ** GetBotAliases
    getBotAliases_maxResults,
    getBotAliases_nameContains,
    getBotAliases_nextToken,
    getBotAliases_botName,
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_httpStatus,

    -- ** GetBotChannelAssociation
    getBotChannelAssociation_name,
    getBotChannelAssociation_botName,
    getBotChannelAssociation_botAlias,
    getBotChannelAssociationResponse_botAlias,
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_httpStatus,

    -- ** GetBotChannelAssociations
    getBotChannelAssociations_maxResults,
    getBotChannelAssociations_nameContains,
    getBotChannelAssociations_nextToken,
    getBotChannelAssociations_botName,
    getBotChannelAssociations_botAlias,
    getBotChannelAssociationsResponse_botChannelAssociations,
    getBotChannelAssociationsResponse_nextToken,
    getBotChannelAssociationsResponse_httpStatus,

    -- ** GetBotVersions
    getBotVersions_maxResults,
    getBotVersions_nextToken,
    getBotVersions_name,
    getBotVersionsResponse_bots,
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_httpStatus,

    -- ** GetBots
    getBots_maxResults,
    getBots_nameContains,
    getBots_nextToken,
    getBotsResponse_bots,
    getBotsResponse_nextToken,
    getBotsResponse_httpStatus,

    -- ** GetBuiltinIntent
    getBuiltinIntent_signature,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_httpStatus,

    -- ** GetBuiltinIntents
    getBuiltinIntents_locale,
    getBuiltinIntents_maxResults,
    getBuiltinIntents_nextToken,
    getBuiltinIntents_signatureContains,
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_httpStatus,

    -- ** GetBuiltinSlotTypes
    getBuiltinSlotTypes_locale,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_signatureContains,
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_httpStatus,

    -- ** GetExport
    getExport_name,
    getExport_version,
    getExport_resourceType,
    getExport_exportType,
    getExportResponse_exportStatus,
    getExportResponse_exportType,
    getExportResponse_failureReason,
    getExportResponse_name,
    getExportResponse_resourceType,
    getExportResponse_url,
    getExportResponse_version,
    getExportResponse_httpStatus,

    -- ** GetImport
    getImport_importId,
    getImportResponse_createdDate,
    getImportResponse_failureReason,
    getImportResponse_importId,
    getImportResponse_importStatus,
    getImportResponse_mergeStrategy,
    getImportResponse_name,
    getImportResponse_resourceType,
    getImportResponse_httpStatus,

    -- ** GetIntent
    getIntent_name,
    getIntent_version,
    getIntentResponse_checksum,
    getIntentResponse_conclusionStatement,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_createdDate,
    getIntentResponse_description,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_followUpPrompt,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_inputContexts,
    getIntentResponse_kendraConfiguration,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_name,
    getIntentResponse_outputContexts,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_rejectionStatement,
    getIntentResponse_sampleUtterances,
    getIntentResponse_slots,
    getIntentResponse_version,
    getIntentResponse_httpStatus,

    -- ** GetIntentVersions
    getIntentVersions_maxResults,
    getIntentVersions_nextToken,
    getIntentVersions_name,
    getIntentVersionsResponse_intents,
    getIntentVersionsResponse_nextToken,
    getIntentVersionsResponse_httpStatus,

    -- ** GetIntents
    getIntents_maxResults,
    getIntents_nameContains,
    getIntents_nextToken,
    getIntentsResponse_intents,
    getIntentsResponse_nextToken,
    getIntentsResponse_httpStatus,

    -- ** GetMigration
    getMigration_migrationId,
    getMigrationResponse_alerts,
    getMigrationResponse_migrationId,
    getMigrationResponse_migrationStatus,
    getMigrationResponse_migrationStrategy,
    getMigrationResponse_migrationTimestamp,
    getMigrationResponse_v1BotLocale,
    getMigrationResponse_v1BotName,
    getMigrationResponse_v1BotVersion,
    getMigrationResponse_v2BotId,
    getMigrationResponse_v2BotRole,
    getMigrationResponse_httpStatus,

    -- ** GetMigrations
    getMigrations_maxResults,
    getMigrations_migrationStatusEquals,
    getMigrations_nextToken,
    getMigrations_sortByAttribute,
    getMigrations_sortByOrder,
    getMigrations_v1BotNameContains,
    getMigrationsResponse_migrationSummaries,
    getMigrationsResponse_nextToken,
    getMigrationsResponse_httpStatus,

    -- ** GetSlotType
    getSlotType_name,
    getSlotType_version,
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_description,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_name,
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_version,
    getSlotTypeResponse_httpStatus,

    -- ** GetSlotTypeVersions
    getSlotTypeVersions_maxResults,
    getSlotTypeVersions_nextToken,
    getSlotTypeVersions_name,
    getSlotTypeVersionsResponse_nextToken,
    getSlotTypeVersionsResponse_slotTypes,
    getSlotTypeVersionsResponse_httpStatus,

    -- ** GetSlotTypes
    getSlotTypes_maxResults,
    getSlotTypes_nameContains,
    getSlotTypes_nextToken,
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_httpStatus,

    -- ** GetUtterancesView
    getUtterancesView_botName,
    getUtterancesView_botVersions,
    getUtterancesView_statusType,
    getUtterancesViewResponse_botName,
    getUtterancesViewResponse_utterances,
    getUtterancesViewResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutBot
    putBot_abortStatement,
    putBot_checksum,
    putBot_clarificationPrompt,
    putBot_createVersion,
    putBot_description,
    putBot_detectSentiment,
    putBot_enableModelImprovements,
    putBot_idleSessionTTLInSeconds,
    putBot_intents,
    putBot_nluIntentConfidenceThreshold,
    putBot_processBehavior,
    putBot_tags,
    putBot_voiceId,
    putBot_name,
    putBot_locale,
    putBot_childDirected,
    putBotResponse_abortStatement,
    putBotResponse_checksum,
    putBotResponse_childDirected,
    putBotResponse_clarificationPrompt,
    putBotResponse_createVersion,
    putBotResponse_createdDate,
    putBotResponse_description,
    putBotResponse_detectSentiment,
    putBotResponse_enableModelImprovements,
    putBotResponse_failureReason,
    putBotResponse_idleSessionTTLInSeconds,
    putBotResponse_intents,
    putBotResponse_lastUpdatedDate,
    putBotResponse_locale,
    putBotResponse_name,
    putBotResponse_nluIntentConfidenceThreshold,
    putBotResponse_status,
    putBotResponse_tags,
    putBotResponse_version,
    putBotResponse_voiceId,
    putBotResponse_httpStatus,

    -- ** PutBotAlias
    putBotAlias_checksum,
    putBotAlias_conversationLogs,
    putBotAlias_description,
    putBotAlias_tags,
    putBotAlias_name,
    putBotAlias_botVersion,
    putBotAlias_botName,
    putBotAliasResponse_botName,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_checksum,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_createdDate,
    putBotAliasResponse_description,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_name,
    putBotAliasResponse_tags,
    putBotAliasResponse_httpStatus,

    -- ** PutIntent
    putIntent_checksum,
    putIntent_conclusionStatement,
    putIntent_confirmationPrompt,
    putIntent_createVersion,
    putIntent_description,
    putIntent_dialogCodeHook,
    putIntent_followUpPrompt,
    putIntent_fulfillmentActivity,
    putIntent_inputContexts,
    putIntent_kendraConfiguration,
    putIntent_outputContexts,
    putIntent_parentIntentSignature,
    putIntent_rejectionStatement,
    putIntent_sampleUtterances,
    putIntent_slots,
    putIntent_name,
    putIntentResponse_checksum,
    putIntentResponse_conclusionStatement,
    putIntentResponse_confirmationPrompt,
    putIntentResponse_createVersion,
    putIntentResponse_createdDate,
    putIntentResponse_description,
    putIntentResponse_dialogCodeHook,
    putIntentResponse_followUpPrompt,
    putIntentResponse_fulfillmentActivity,
    putIntentResponse_inputContexts,
    putIntentResponse_kendraConfiguration,
    putIntentResponse_lastUpdatedDate,
    putIntentResponse_name,
    putIntentResponse_outputContexts,
    putIntentResponse_parentIntentSignature,
    putIntentResponse_rejectionStatement,
    putIntentResponse_sampleUtterances,
    putIntentResponse_slots,
    putIntentResponse_version,
    putIntentResponse_httpStatus,

    -- ** PutSlotType
    putSlotType_checksum,
    putSlotType_createVersion,
    putSlotType_description,
    putSlotType_enumerationValues,
    putSlotType_parentSlotTypeSignature,
    putSlotType_slotTypeConfigurations,
    putSlotType_valueSelectionStrategy,
    putSlotType_name,
    putSlotTypeResponse_checksum,
    putSlotTypeResponse_createVersion,
    putSlotTypeResponse_createdDate,
    putSlotTypeResponse_description,
    putSlotTypeResponse_enumerationValues,
    putSlotTypeResponse_lastUpdatedDate,
    putSlotTypeResponse_name,
    putSlotTypeResponse_parentSlotTypeSignature,
    putSlotTypeResponse_slotTypeConfigurations,
    putSlotTypeResponse_valueSelectionStrategy,
    putSlotTypeResponse_version,
    putSlotTypeResponse_httpStatus,

    -- ** StartImport
    startImport_tags,
    startImport_payload,
    startImport_resourceType,
    startImport_mergeStrategy,
    startImportResponse_createdDate,
    startImportResponse_importId,
    startImportResponse_importStatus,
    startImportResponse_mergeStrategy,
    startImportResponse_name,
    startImportResponse_resourceType,
    startImportResponse_tags,
    startImportResponse_httpStatus,

    -- ** StartMigration
    startMigration_v1BotName,
    startMigration_v1BotVersion,
    startMigration_v2BotName,
    startMigration_v2BotRole,
    startMigration_migrationStrategy,
    startMigrationResponse_migrationId,
    startMigrationResponse_migrationStrategy,
    startMigrationResponse_migrationTimestamp,
    startMigrationResponse_v1BotLocale,
    startMigrationResponse_v1BotName,
    startMigrationResponse_v1BotVersion,
    startMigrationResponse_v2BotId,
    startMigrationResponse_v2BotRole,
    startMigrationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** BotAliasMetadata
    botAliasMetadata_botName,
    botAliasMetadata_botVersion,
    botAliasMetadata_checksum,
    botAliasMetadata_conversationLogs,
    botAliasMetadata_createdDate,
    botAliasMetadata_description,
    botAliasMetadata_lastUpdatedDate,
    botAliasMetadata_name,

    -- ** BotChannelAssociation
    botChannelAssociation_botAlias,
    botChannelAssociation_botConfiguration,
    botChannelAssociation_botName,
    botChannelAssociation_createdDate,
    botChannelAssociation_description,
    botChannelAssociation_failureReason,
    botChannelAssociation_name,
    botChannelAssociation_status,
    botChannelAssociation_type,

    -- ** BotMetadata
    botMetadata_createdDate,
    botMetadata_description,
    botMetadata_lastUpdatedDate,
    botMetadata_name,
    botMetadata_status,
    botMetadata_version,

    -- ** BuiltinIntentMetadata
    builtinIntentMetadata_signature,
    builtinIntentMetadata_supportedLocales,

    -- ** BuiltinIntentSlot
    builtinIntentSlot_name,

    -- ** BuiltinSlotTypeMetadata
    builtinSlotTypeMetadata_signature,
    builtinSlotTypeMetadata_supportedLocales,

    -- ** CodeHook
    codeHook_uri,
    codeHook_messageVersion,

    -- ** ConversationLogsRequest
    conversationLogsRequest_logSettings,
    conversationLogsRequest_iamRoleArn,

    -- ** ConversationLogsResponse
    conversationLogsResponse_iamRoleArn,
    conversationLogsResponse_logSettings,

    -- ** EnumerationValue
    enumerationValue_synonyms,
    enumerationValue_value,

    -- ** FollowUpPrompt
    followUpPrompt_prompt,
    followUpPrompt_rejectionStatement,

    -- ** FulfillmentActivity
    fulfillmentActivity_codeHook,
    fulfillmentActivity_type,

    -- ** InputContext
    inputContext_name,

    -- ** Intent
    intent_intentName,
    intent_intentVersion,

    -- ** IntentMetadata
    intentMetadata_createdDate,
    intentMetadata_description,
    intentMetadata_lastUpdatedDate,
    intentMetadata_name,
    intentMetadata_version,

    -- ** KendraConfiguration
    kendraConfiguration_queryFilterString,
    kendraConfiguration_kendraIndex,
    kendraConfiguration_role,

    -- ** LogSettingsRequest
    logSettingsRequest_kmsKeyArn,
    logSettingsRequest_logType,
    logSettingsRequest_destination,
    logSettingsRequest_resourceArn,

    -- ** LogSettingsResponse
    logSettingsResponse_destination,
    logSettingsResponse_kmsKeyArn,
    logSettingsResponse_logType,
    logSettingsResponse_resourceArn,
    logSettingsResponse_resourcePrefix,

    -- ** Message
    message_groupNumber,
    message_contentType,
    message_content,

    -- ** MigrationAlert
    migrationAlert_details,
    migrationAlert_message,
    migrationAlert_referenceURLs,
    migrationAlert_type,

    -- ** MigrationSummary
    migrationSummary_migrationId,
    migrationSummary_migrationStatus,
    migrationSummary_migrationStrategy,
    migrationSummary_migrationTimestamp,
    migrationSummary_v1BotLocale,
    migrationSummary_v1BotName,
    migrationSummary_v1BotVersion,
    migrationSummary_v2BotId,
    migrationSummary_v2BotRole,

    -- ** OutputContext
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- ** Prompt
    prompt_responseCard,
    prompt_messages,
    prompt_maxAttempts,

    -- ** Slot
    slot_defaultValueSpec,
    slot_description,
    slot_obfuscationSetting,
    slot_priority,
    slot_responseCard,
    slot_sampleUtterances,
    slot_slotType,
    slot_slotTypeVersion,
    slot_valueElicitationPrompt,
    slot_name,
    slot_slotConstraint,

    -- ** SlotDefaultValue
    slotDefaultValue_defaultValue,

    -- ** SlotDefaultValueSpec
    slotDefaultValueSpec_defaultValueList,

    -- ** SlotTypeConfiguration
    slotTypeConfiguration_regexConfiguration,

    -- ** SlotTypeMetadata
    slotTypeMetadata_createdDate,
    slotTypeMetadata_description,
    slotTypeMetadata_lastUpdatedDate,
    slotTypeMetadata_name,
    slotTypeMetadata_version,

    -- ** SlotTypeRegexConfiguration
    slotTypeRegexConfiguration_pattern,

    -- ** Statement
    statement_responseCard,
    statement_messages,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UtteranceData
    utteranceData_count,
    utteranceData_distinctUsers,
    utteranceData_firstUtteredDate,
    utteranceData_lastUtteredDate,
    utteranceData_utteranceString,

    -- ** UtteranceList
    utteranceList_botVersion,
    utteranceList_utterances,
  )
where

import Amazonka.LexModels.CreateBotVersion
import Amazonka.LexModels.CreateIntentVersion
import Amazonka.LexModels.CreateSlotTypeVersion
import Amazonka.LexModels.DeleteBot
import Amazonka.LexModels.DeleteBotAlias
import Amazonka.LexModels.DeleteBotChannelAssociation
import Amazonka.LexModels.DeleteBotVersion
import Amazonka.LexModels.DeleteIntent
import Amazonka.LexModels.DeleteIntentVersion
import Amazonka.LexModels.DeleteSlotType
import Amazonka.LexModels.DeleteSlotTypeVersion
import Amazonka.LexModels.DeleteUtterances
import Amazonka.LexModels.GetBot
import Amazonka.LexModels.GetBotAlias
import Amazonka.LexModels.GetBotAliases
import Amazonka.LexModels.GetBotChannelAssociation
import Amazonka.LexModels.GetBotChannelAssociations
import Amazonka.LexModels.GetBotVersions
import Amazonka.LexModels.GetBots
import Amazonka.LexModels.GetBuiltinIntent
import Amazonka.LexModels.GetBuiltinIntents
import Amazonka.LexModels.GetBuiltinSlotTypes
import Amazonka.LexModels.GetExport
import Amazonka.LexModels.GetImport
import Amazonka.LexModels.GetIntent
import Amazonka.LexModels.GetIntentVersions
import Amazonka.LexModels.GetIntents
import Amazonka.LexModels.GetMigration
import Amazonka.LexModels.GetMigrations
import Amazonka.LexModels.GetSlotType
import Amazonka.LexModels.GetSlotTypeVersions
import Amazonka.LexModels.GetSlotTypes
import Amazonka.LexModels.GetUtterancesView
import Amazonka.LexModels.ListTagsForResource
import Amazonka.LexModels.PutBot
import Amazonka.LexModels.PutBotAlias
import Amazonka.LexModels.PutIntent
import Amazonka.LexModels.PutSlotType
import Amazonka.LexModels.StartImport
import Amazonka.LexModels.StartMigration
import Amazonka.LexModels.TagResource
import Amazonka.LexModels.Types.BotAliasMetadata
import Amazonka.LexModels.Types.BotChannelAssociation
import Amazonka.LexModels.Types.BotMetadata
import Amazonka.LexModels.Types.BuiltinIntentMetadata
import Amazonka.LexModels.Types.BuiltinIntentSlot
import Amazonka.LexModels.Types.BuiltinSlotTypeMetadata
import Amazonka.LexModels.Types.CodeHook
import Amazonka.LexModels.Types.ConversationLogsRequest
import Amazonka.LexModels.Types.ConversationLogsResponse
import Amazonka.LexModels.Types.EnumerationValue
import Amazonka.LexModels.Types.FollowUpPrompt
import Amazonka.LexModels.Types.FulfillmentActivity
import Amazonka.LexModels.Types.InputContext
import Amazonka.LexModels.Types.Intent
import Amazonka.LexModels.Types.IntentMetadata
import Amazonka.LexModels.Types.KendraConfiguration
import Amazonka.LexModels.Types.LogSettingsRequest
import Amazonka.LexModels.Types.LogSettingsResponse
import Amazonka.LexModels.Types.Message
import Amazonka.LexModels.Types.MigrationAlert
import Amazonka.LexModels.Types.MigrationSummary
import Amazonka.LexModels.Types.OutputContext
import Amazonka.LexModels.Types.Prompt
import Amazonka.LexModels.Types.Slot
import Amazonka.LexModels.Types.SlotDefaultValue
import Amazonka.LexModels.Types.SlotDefaultValueSpec
import Amazonka.LexModels.Types.SlotTypeConfiguration
import Amazonka.LexModels.Types.SlotTypeMetadata
import Amazonka.LexModels.Types.SlotTypeRegexConfiguration
import Amazonka.LexModels.Types.Statement
import Amazonka.LexModels.Types.Tag
import Amazonka.LexModels.Types.UtteranceData
import Amazonka.LexModels.Types.UtteranceList
import Amazonka.LexModels.UntagResource
