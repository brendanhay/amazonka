{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexModels.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Lens
  ( -- * Operations

    -- ** CreateBotVersion
    createBotVersion_checksum,
    createBotVersion_name,
    createBotVersionResponse_detectSentiment,
    createBotVersionResponse_voiceId,
    createBotVersionResponse_name,
    createBotVersionResponse_locale,
    createBotVersionResponse_lastUpdatedDate,
    createBotVersionResponse_intents,
    createBotVersionResponse_status,
    createBotVersionResponse_description,
    createBotVersionResponse_idleSessionTTLInSeconds,
    createBotVersionResponse_enableModelImprovements,
    createBotVersionResponse_checksum,
    createBotVersionResponse_childDirected,
    createBotVersionResponse_abortStatement,
    createBotVersionResponse_clarificationPrompt,
    createBotVersionResponse_createdDate,
    createBotVersionResponse_failureReason,
    createBotVersionResponse_version,
    createBotVersionResponse_httpStatus,

    -- ** CreateIntentVersion
    createIntentVersion_checksum,
    createIntentVersion_name,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_name,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_description,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_version,
    createIntentVersionResponse_httpStatus,

    -- ** CreateSlotTypeVersion
    createSlotTypeVersion_checksum,
    createSlotTypeVersion_name,
    createSlotTypeVersionResponse_name,
    createSlotTypeVersionResponse_valueSelectionStrategy,
    createSlotTypeVersionResponse_lastUpdatedDate,
    createSlotTypeVersionResponse_description,
    createSlotTypeVersionResponse_checksum,
    createSlotTypeVersionResponse_createdDate,
    createSlotTypeVersionResponse_enumerationValues,
    createSlotTypeVersionResponse_slotTypeConfigurations,
    createSlotTypeVersionResponse_parentSlotTypeSignature,
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
    getBotResponse_detectSentiment,
    getBotResponse_voiceId,
    getBotResponse_name,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_locale,
    getBotResponse_lastUpdatedDate,
    getBotResponse_intents,
    getBotResponse_status,
    getBotResponse_description,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_enableModelImprovements,
    getBotResponse_checksum,
    getBotResponse_childDirected,
    getBotResponse_abortStatement,
    getBotResponse_clarificationPrompt,
    getBotResponse_createdDate,
    getBotResponse_failureReason,
    getBotResponse_version,
    getBotResponse_httpStatus,

    -- ** GetBotAlias
    getBotAlias_name,
    getBotAlias_botName,
    getBotAliasResponse_name,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_description,
    getBotAliasResponse_checksum,
    getBotAliasResponse_botName,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_createdDate,
    getBotAliasResponse_httpStatus,

    -- ** GetBotAliases
    getBotAliases_nextToken,
    getBotAliases_nameContains,
    getBotAliases_maxResults,
    getBotAliases_botName,
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_httpStatus,

    -- ** GetBotChannelAssociation
    getBotChannelAssociation_name,
    getBotChannelAssociation_botName,
    getBotChannelAssociation_botAlias,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_botAlias,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_httpStatus,

    -- ** GetBotChannelAssociations
    getBotChannelAssociations_nextToken,
    getBotChannelAssociations_nameContains,
    getBotChannelAssociations_maxResults,
    getBotChannelAssociations_botName,
    getBotChannelAssociations_botAlias,
    getBotChannelAssociationsResponse_nextToken,
    getBotChannelAssociationsResponse_botChannelAssociations,
    getBotChannelAssociationsResponse_httpStatus,

    -- ** GetBotVersions
    getBotVersions_nextToken,
    getBotVersions_maxResults,
    getBotVersions_name,
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_bots,
    getBotVersionsResponse_httpStatus,

    -- ** GetBots
    getBots_nextToken,
    getBots_nameContains,
    getBots_maxResults,
    getBotsResponse_nextToken,
    getBotsResponse_bots,
    getBotsResponse_httpStatus,

    -- ** GetBuiltinIntent
    getBuiltinIntent_signature,
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_httpStatus,

    -- ** GetBuiltinIntents
    getBuiltinIntents_nextToken,
    getBuiltinIntents_locale,
    getBuiltinIntents_maxResults,
    getBuiltinIntents_signatureContains,
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_httpStatus,

    -- ** GetBuiltinSlotTypes
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_locale,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypes_signatureContains,
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_httpStatus,

    -- ** GetExport
    getExport_name,
    getExport_version,
    getExport_resourceType,
    getExport_exportType,
    getExportResponse_resourceType,
    getExportResponse_name,
    getExportResponse_exportType,
    getExportResponse_url,
    getExportResponse_exportStatus,
    getExportResponse_failureReason,
    getExportResponse_version,
    getExportResponse_httpStatus,

    -- ** GetImport
    getImport_importId,
    getImportResponse_resourceType,
    getImportResponse_name,
    getImportResponse_importId,
    getImportResponse_createdDate,
    getImportResponse_importStatus,
    getImportResponse_failureReason,
    getImportResponse_mergeStrategy,
    getImportResponse_httpStatus,

    -- ** GetIntent
    getIntent_name,
    getIntent_version,
    getIntentResponse_sampleUtterances,
    getIntentResponse_kendraConfiguration,
    getIntentResponse_name,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_outputContexts,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_followUpPrompt,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_description,
    getIntentResponse_rejectionStatement,
    getIntentResponse_checksum,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_createdDate,
    getIntentResponse_slots,
    getIntentResponse_inputContexts,
    getIntentResponse_conclusionStatement,
    getIntentResponse_version,
    getIntentResponse_httpStatus,

    -- ** GetIntentVersions
    getIntentVersions_nextToken,
    getIntentVersions_maxResults,
    getIntentVersions_name,
    getIntentVersionsResponse_nextToken,
    getIntentVersionsResponse_intents,
    getIntentVersionsResponse_httpStatus,

    -- ** GetIntents
    getIntents_nextToken,
    getIntents_nameContains,
    getIntents_maxResults,
    getIntentsResponse_nextToken,
    getIntentsResponse_intents,
    getIntentsResponse_httpStatus,

    -- ** GetMigration
    getMigration_migrationId,
    getMigrationResponse_v2BotRole,
    getMigrationResponse_alerts,
    getMigrationResponse_migrationStatus,
    getMigrationResponse_v1BotLocale,
    getMigrationResponse_v1BotVersion,
    getMigrationResponse_migrationStrategy,
    getMigrationResponse_v2BotId,
    getMigrationResponse_v1BotName,
    getMigrationResponse_migrationTimestamp,
    getMigrationResponse_migrationId,
    getMigrationResponse_httpStatus,

    -- ** GetMigrations
    getMigrations_nextToken,
    getMigrations_sortByAttribute,
    getMigrations_v1BotNameContains,
    getMigrations_sortByOrder,
    getMigrations_maxResults,
    getMigrations_migrationStatusEquals,
    getMigrationsResponse_nextToken,
    getMigrationsResponse_migrationSummaries,
    getMigrationsResponse_httpStatus,

    -- ** GetSlotType
    getSlotType_name,
    getSlotType_version,
    getSlotTypeResponse_name,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_description,
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_version,
    getSlotTypeResponse_httpStatus,

    -- ** GetSlotTypeVersions
    getSlotTypeVersions_nextToken,
    getSlotTypeVersions_maxResults,
    getSlotTypeVersions_name,
    getSlotTypeVersionsResponse_nextToken,
    getSlotTypeVersionsResponse_slotTypes,
    getSlotTypeVersionsResponse_httpStatus,

    -- ** GetSlotTypes
    getSlotTypes_nextToken,
    getSlotTypes_nameContains,
    getSlotTypes_maxResults,
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_httpStatus,

    -- ** GetUtterancesView
    getUtterancesView_botName,
    getUtterancesView_botVersions,
    getUtterancesView_statusType,
    getUtterancesViewResponse_utterances,
    getUtterancesViewResponse_botName,
    getUtterancesViewResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutBot
    putBot_tags,
    putBot_detectSentiment,
    putBot_voiceId,
    putBot_nluIntentConfidenceThreshold,
    putBot_intents,
    putBot_description,
    putBot_idleSessionTTLInSeconds,
    putBot_enableModelImprovements,
    putBot_checksum,
    putBot_createVersion,
    putBot_abortStatement,
    putBot_clarificationPrompt,
    putBot_processBehavior,
    putBot_name,
    putBot_locale,
    putBot_childDirected,
    putBotResponse_tags,
    putBotResponse_detectSentiment,
    putBotResponse_voiceId,
    putBotResponse_name,
    putBotResponse_nluIntentConfidenceThreshold,
    putBotResponse_locale,
    putBotResponse_lastUpdatedDate,
    putBotResponse_intents,
    putBotResponse_status,
    putBotResponse_description,
    putBotResponse_idleSessionTTLInSeconds,
    putBotResponse_enableModelImprovements,
    putBotResponse_checksum,
    putBotResponse_childDirected,
    putBotResponse_createVersion,
    putBotResponse_abortStatement,
    putBotResponse_clarificationPrompt,
    putBotResponse_createdDate,
    putBotResponse_failureReason,
    putBotResponse_version,
    putBotResponse_httpStatus,

    -- ** PutBotAlias
    putBotAlias_tags,
    putBotAlias_description,
    putBotAlias_checksum,
    putBotAlias_conversationLogs,
    putBotAlias_name,
    putBotAlias_botVersion,
    putBotAlias_botName,
    putBotAliasResponse_tags,
    putBotAliasResponse_name,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_description,
    putBotAliasResponse_checksum,
    putBotAliasResponse_botName,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_createdDate,
    putBotAliasResponse_httpStatus,

    -- ** PutIntent
    putIntent_sampleUtterances,
    putIntent_kendraConfiguration,
    putIntent_dialogCodeHook,
    putIntent_outputContexts,
    putIntent_fulfillmentActivity,
    putIntent_followUpPrompt,
    putIntent_parentIntentSignature,
    putIntent_description,
    putIntent_rejectionStatement,
    putIntent_checksum,
    putIntent_createVersion,
    putIntent_confirmationPrompt,
    putIntent_slots,
    putIntent_inputContexts,
    putIntent_conclusionStatement,
    putIntent_name,
    putIntentResponse_sampleUtterances,
    putIntentResponse_kendraConfiguration,
    putIntentResponse_name,
    putIntentResponse_dialogCodeHook,
    putIntentResponse_outputContexts,
    putIntentResponse_fulfillmentActivity,
    putIntentResponse_lastUpdatedDate,
    putIntentResponse_followUpPrompt,
    putIntentResponse_parentIntentSignature,
    putIntentResponse_description,
    putIntentResponse_rejectionStatement,
    putIntentResponse_checksum,
    putIntentResponse_createVersion,
    putIntentResponse_confirmationPrompt,
    putIntentResponse_createdDate,
    putIntentResponse_slots,
    putIntentResponse_inputContexts,
    putIntentResponse_conclusionStatement,
    putIntentResponse_version,
    putIntentResponse_httpStatus,

    -- ** PutSlotType
    putSlotType_valueSelectionStrategy,
    putSlotType_description,
    putSlotType_checksum,
    putSlotType_createVersion,
    putSlotType_enumerationValues,
    putSlotType_slotTypeConfigurations,
    putSlotType_parentSlotTypeSignature,
    putSlotType_name,
    putSlotTypeResponse_name,
    putSlotTypeResponse_valueSelectionStrategy,
    putSlotTypeResponse_lastUpdatedDate,
    putSlotTypeResponse_description,
    putSlotTypeResponse_checksum,
    putSlotTypeResponse_createVersion,
    putSlotTypeResponse_createdDate,
    putSlotTypeResponse_enumerationValues,
    putSlotTypeResponse_slotTypeConfigurations,
    putSlotTypeResponse_parentSlotTypeSignature,
    putSlotTypeResponse_version,
    putSlotTypeResponse_httpStatus,

    -- ** StartImport
    startImport_tags,
    startImport_payload,
    startImport_resourceType,
    startImport_mergeStrategy,
    startImportResponse_tags,
    startImportResponse_resourceType,
    startImportResponse_name,
    startImportResponse_importId,
    startImportResponse_createdDate,
    startImportResponse_importStatus,
    startImportResponse_mergeStrategy,
    startImportResponse_httpStatus,

    -- ** StartMigration
    startMigration_v1BotName,
    startMigration_v1BotVersion,
    startMigration_v2BotName,
    startMigration_v2BotRole,
    startMigration_migrationStrategy,
    startMigrationResponse_v2BotRole,
    startMigrationResponse_v1BotLocale,
    startMigrationResponse_v1BotVersion,
    startMigrationResponse_migrationStrategy,
    startMigrationResponse_v2BotId,
    startMigrationResponse_v1BotName,
    startMigrationResponse_migrationTimestamp,
    startMigrationResponse_migrationId,
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
    botAliasMetadata_name,
    botAliasMetadata_botVersion,
    botAliasMetadata_lastUpdatedDate,
    botAliasMetadata_description,
    botAliasMetadata_checksum,
    botAliasMetadata_botName,
    botAliasMetadata_conversationLogs,
    botAliasMetadata_createdDate,

    -- ** BotChannelAssociation
    botChannelAssociation_name,
    botChannelAssociation_type,
    botChannelAssociation_botConfiguration,
    botChannelAssociation_status,
    botChannelAssociation_description,
    botChannelAssociation_botName,
    botChannelAssociation_createdDate,
    botChannelAssociation_botAlias,
    botChannelAssociation_failureReason,

    -- ** BotMetadata
    botMetadata_name,
    botMetadata_lastUpdatedDate,
    botMetadata_status,
    botMetadata_description,
    botMetadata_createdDate,
    botMetadata_version,

    -- ** BuiltinIntentMetadata
    builtinIntentMetadata_supportedLocales,
    builtinIntentMetadata_signature,

    -- ** BuiltinIntentSlot
    builtinIntentSlot_name,

    -- ** BuiltinSlotTypeMetadata
    builtinSlotTypeMetadata_supportedLocales,
    builtinSlotTypeMetadata_signature,

    -- ** CodeHook
    codeHook_uri,
    codeHook_messageVersion,

    -- ** ConversationLogsRequest
    conversationLogsRequest_logSettings,
    conversationLogsRequest_iamRoleArn,

    -- ** ConversationLogsResponse
    conversationLogsResponse_logSettings,
    conversationLogsResponse_iamRoleArn,

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
    intentMetadata_name,
    intentMetadata_lastUpdatedDate,
    intentMetadata_description,
    intentMetadata_createdDate,
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
    logSettingsResponse_logType,
    logSettingsResponse_resourcePrefix,
    logSettingsResponse_kmsKeyArn,
    logSettingsResponse_resourceArn,

    -- ** Message
    message_groupNumber,
    message_contentType,
    message_content,

    -- ** MigrationAlert
    migrationAlert_message,
    migrationAlert_type,
    migrationAlert_referenceURLs,
    migrationAlert_details,

    -- ** MigrationSummary
    migrationSummary_v2BotRole,
    migrationSummary_migrationStatus,
    migrationSummary_v1BotLocale,
    migrationSummary_v1BotVersion,
    migrationSummary_migrationStrategy,
    migrationSummary_v2BotId,
    migrationSummary_v1BotName,
    migrationSummary_migrationTimestamp,
    migrationSummary_migrationId,

    -- ** OutputContext
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- ** Prompt
    prompt_responseCard,
    prompt_messages,
    prompt_maxAttempts,

    -- ** Slot
    slot_sampleUtterances,
    slot_slotType,
    slot_defaultValueSpec,
    slot_description,
    slot_responseCard,
    slot_priority,
    slot_obfuscationSetting,
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
    slotTypeMetadata_name,
    slotTypeMetadata_lastUpdatedDate,
    slotTypeMetadata_description,
    slotTypeMetadata_createdDate,
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
    utteranceData_firstUtteredDate,
    utteranceData_count,
    utteranceData_utteranceString,
    utteranceData_distinctUsers,
    utteranceData_lastUtteredDate,

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
