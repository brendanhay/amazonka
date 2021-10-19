{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Lens
  ( -- * Operations

    -- ** StartMigration
    startMigration_v1BotName,
    startMigration_v1BotVersion,
    startMigration_v2BotName,
    startMigration_v2BotRole,
    startMigration_migrationStrategy,
    startMigrationResponse_v1BotVersion,
    startMigrationResponse_migrationStrategy,
    startMigrationResponse_migrationTimestamp,
    startMigrationResponse_v2BotId,
    startMigrationResponse_v1BotLocale,
    startMigrationResponse_v1BotName,
    startMigrationResponse_v2BotRole,
    startMigrationResponse_migrationId,
    startMigrationResponse_httpStatus,

    -- ** DeleteIntentVersion
    deleteIntentVersion_name,
    deleteIntentVersion_version,

    -- ** GetBotAliases
    getBotAliases_nameContains,
    getBotAliases_nextToken,
    getBotAliases_maxResults,
    getBotAliases_botName,
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_httpStatus,

    -- ** DeleteBotChannelAssociation
    deleteBotChannelAssociation_name,
    deleteBotChannelAssociation_botName,
    deleteBotChannelAssociation_botAlias,

    -- ** CreateSlotTypeVersion
    createSlotTypeVersion_checksum,
    createSlotTypeVersion_name,
    createSlotTypeVersionResponse_parentSlotTypeSignature,
    createSlotTypeVersionResponse_slotTypeConfigurations,
    createSlotTypeVersionResponse_checksum,
    createSlotTypeVersionResponse_valueSelectionStrategy,
    createSlotTypeVersionResponse_createdDate,
    createSlotTypeVersionResponse_name,
    createSlotTypeVersionResponse_version,
    createSlotTypeVersionResponse_lastUpdatedDate,
    createSlotTypeVersionResponse_description,
    createSlotTypeVersionResponse_enumerationValues,
    createSlotTypeVersionResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetIntent
    getIntent_name,
    getIntent_version,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_slots,
    getIntentResponse_rejectionStatement,
    getIntentResponse_checksum,
    getIntentResponse_conclusionStatement,
    getIntentResponse_sampleUtterances,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_createdDate,
    getIntentResponse_kendraConfiguration,
    getIntentResponse_name,
    getIntentResponse_version,
    getIntentResponse_inputContexts,
    getIntentResponse_followUpPrompt,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_outputContexts,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_description,
    getIntentResponse_httpStatus,

    -- ** PutIntent
    putIntent_fulfillmentActivity,
    putIntent_slots,
    putIntent_rejectionStatement,
    putIntent_checksum,
    putIntent_conclusionStatement,
    putIntent_sampleUtterances,
    putIntent_parentIntentSignature,
    putIntent_kendraConfiguration,
    putIntent_inputContexts,
    putIntent_followUpPrompt,
    putIntent_outputContexts,
    putIntent_confirmationPrompt,
    putIntent_createVersion,
    putIntent_dialogCodeHook,
    putIntent_description,
    putIntent_name,
    putIntentResponse_fulfillmentActivity,
    putIntentResponse_slots,
    putIntentResponse_rejectionStatement,
    putIntentResponse_checksum,
    putIntentResponse_conclusionStatement,
    putIntentResponse_sampleUtterances,
    putIntentResponse_parentIntentSignature,
    putIntentResponse_createdDate,
    putIntentResponse_kendraConfiguration,
    putIntentResponse_name,
    putIntentResponse_version,
    putIntentResponse_inputContexts,
    putIntentResponse_followUpPrompt,
    putIntentResponse_lastUpdatedDate,
    putIntentResponse_outputContexts,
    putIntentResponse_confirmationPrompt,
    putIntentResponse_createVersion,
    putIntentResponse_dialogCodeHook,
    putIntentResponse_description,
    putIntentResponse_httpStatus,

    -- ** DeleteIntent
    deleteIntent_name,

    -- ** GetBuiltinIntents
    getBuiltinIntents_locale,
    getBuiltinIntents_nextToken,
    getBuiltinIntents_signatureContains,
    getBuiltinIntents_maxResults,
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_httpStatus,

    -- ** PutBot
    putBot_abortStatement,
    putBot_intents,
    putBot_checksum,
    putBot_enableModelImprovements,
    putBot_nluIntentConfidenceThreshold,
    putBot_detectSentiment,
    putBot_processBehavior,
    putBot_idleSessionTTLInSeconds,
    putBot_clarificationPrompt,
    putBot_voiceId,
    putBot_createVersion,
    putBot_description,
    putBot_tags,
    putBot_name,
    putBot_locale,
    putBot_childDirected,
    putBotResponse_failureReason,
    putBotResponse_status,
    putBotResponse_abortStatement,
    putBotResponse_intents,
    putBotResponse_checksum,
    putBotResponse_enableModelImprovements,
    putBotResponse_nluIntentConfidenceThreshold,
    putBotResponse_detectSentiment,
    putBotResponse_locale,
    putBotResponse_createdDate,
    putBotResponse_name,
    putBotResponse_version,
    putBotResponse_idleSessionTTLInSeconds,
    putBotResponse_clarificationPrompt,
    putBotResponse_voiceId,
    putBotResponse_lastUpdatedDate,
    putBotResponse_createVersion,
    putBotResponse_childDirected,
    putBotResponse_description,
    putBotResponse_tags,
    putBotResponse_httpStatus,

    -- ** DeleteBot
    deleteBot_name,

    -- ** GetImport
    getImport_importId,
    getImportResponse_failureReason,
    getImportResponse_resourceType,
    getImportResponse_importId,
    getImportResponse_createdDate,
    getImportResponse_name,
    getImportResponse_mergeStrategy,
    getImportResponse_importStatus,
    getImportResponse_httpStatus,

    -- ** GetIntentVersions
    getIntentVersions_nextToken,
    getIntentVersions_maxResults,
    getIntentVersions_name,
    getIntentVersionsResponse_intents,
    getIntentVersionsResponse_nextToken,
    getIntentVersionsResponse_httpStatus,

    -- ** GetBuiltinIntent
    getBuiltinIntent_signature,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_httpStatus,

    -- ** PutBotAlias
    putBotAlias_checksum,
    putBotAlias_conversationLogs,
    putBotAlias_description,
    putBotAlias_tags,
    putBotAlias_name,
    putBotAlias_botVersion,
    putBotAlias_botName,
    putBotAliasResponse_checksum,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_botName,
    putBotAliasResponse_createdDate,
    putBotAliasResponse_name,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_description,
    putBotAliasResponse_tags,
    putBotAliasResponse_httpStatus,

    -- ** GetBotVersions
    getBotVersions_nextToken,
    getBotVersions_maxResults,
    getBotVersions_name,
    getBotVersionsResponse_bots,
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_httpStatus,

    -- ** GetBotChannelAssociations
    getBotChannelAssociations_nameContains,
    getBotChannelAssociations_nextToken,
    getBotChannelAssociations_maxResults,
    getBotChannelAssociations_botName,
    getBotChannelAssociations_botAlias,
    getBotChannelAssociationsResponse_botChannelAssociations,
    getBotChannelAssociationsResponse_nextToken,
    getBotChannelAssociationsResponse_httpStatus,

    -- ** DeleteBotAlias
    deleteBotAlias_name,
    deleteBotAlias_botName,

    -- ** GetSlotTypes
    getSlotTypes_nameContains,
    getSlotTypes_nextToken,
    getSlotTypes_maxResults,
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_httpStatus,

    -- ** GetMigrations
    getMigrations_sortByOrder,
    getMigrations_sortByAttribute,
    getMigrations_nextToken,
    getMigrations_migrationStatusEquals,
    getMigrations_v1BotNameContains,
    getMigrations_maxResults,
    getMigrationsResponse_migrationSummaries,
    getMigrationsResponse_nextToken,
    getMigrationsResponse_httpStatus,

    -- ** DeleteUtterances
    deleteUtterances_botName,
    deleteUtterances_userId,

    -- ** GetBots
    getBots_nameContains,
    getBots_nextToken,
    getBots_maxResults,
    getBotsResponse_bots,
    getBotsResponse_nextToken,
    getBotsResponse_httpStatus,

    -- ** GetBot
    getBot_name,
    getBot_versionOrAlias,
    getBotResponse_failureReason,
    getBotResponse_status,
    getBotResponse_abortStatement,
    getBotResponse_intents,
    getBotResponse_checksum,
    getBotResponse_enableModelImprovements,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_detectSentiment,
    getBotResponse_locale,
    getBotResponse_createdDate,
    getBotResponse_name,
    getBotResponse_version,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_clarificationPrompt,
    getBotResponse_voiceId,
    getBotResponse_lastUpdatedDate,
    getBotResponse_childDirected,
    getBotResponse_description,
    getBotResponse_httpStatus,

    -- ** CreateBotVersion
    createBotVersion_checksum,
    createBotVersion_name,
    createBotVersionResponse_failureReason,
    createBotVersionResponse_status,
    createBotVersionResponse_abortStatement,
    createBotVersionResponse_intents,
    createBotVersionResponse_checksum,
    createBotVersionResponse_enableModelImprovements,
    createBotVersionResponse_detectSentiment,
    createBotVersionResponse_locale,
    createBotVersionResponse_createdDate,
    createBotVersionResponse_name,
    createBotVersionResponse_version,
    createBotVersionResponse_idleSessionTTLInSeconds,
    createBotVersionResponse_clarificationPrompt,
    createBotVersionResponse_voiceId,
    createBotVersionResponse_lastUpdatedDate,
    createBotVersionResponse_childDirected,
    createBotVersionResponse_description,
    createBotVersionResponse_httpStatus,

    -- ** DeleteSlotTypeVersion
    deleteSlotTypeVersion_name,
    deleteSlotTypeVersion_version,

    -- ** DeleteBotVersion
    deleteBotVersion_name,
    deleteBotVersion_version,

    -- ** GetSlotType
    getSlotType_name,
    getSlotType_version,
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_name,
    getSlotTypeResponse_version,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_description,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_httpStatus,

    -- ** GetExport
    getExport_name,
    getExport_version,
    getExport_resourceType,
    getExport_exportType,
    getExportResponse_failureReason,
    getExportResponse_resourceType,
    getExportResponse_exportStatus,
    getExportResponse_url,
    getExportResponse_exportType,
    getExportResponse_name,
    getExportResponse_version,
    getExportResponse_httpStatus,

    -- ** GetMigration
    getMigration_migrationId,
    getMigrationResponse_v1BotVersion,
    getMigrationResponse_migrationStrategy,
    getMigrationResponse_migrationTimestamp,
    getMigrationResponse_alerts,
    getMigrationResponse_migrationStatus,
    getMigrationResponse_v2BotId,
    getMigrationResponse_v1BotLocale,
    getMigrationResponse_v1BotName,
    getMigrationResponse_v2BotRole,
    getMigrationResponse_migrationId,
    getMigrationResponse_httpStatus,

    -- ** CreateIntentVersion
    createIntentVersion_checksum,
    createIntentVersion_name,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_name,
    createIntentVersionResponse_version,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_description,
    createIntentVersionResponse_httpStatus,

    -- ** DeleteSlotType
    deleteSlotType_name,

    -- ** StartImport
    startImport_tags,
    startImport_payload,
    startImport_resourceType,
    startImport_mergeStrategy,
    startImportResponse_resourceType,
    startImportResponse_importId,
    startImportResponse_createdDate,
    startImportResponse_name,
    startImportResponse_mergeStrategy,
    startImportResponse_importStatus,
    startImportResponse_tags,
    startImportResponse_httpStatus,

    -- ** GetBotChannelAssociation
    getBotChannelAssociation_name,
    getBotChannelAssociation_botName,
    getBotChannelAssociation_botAlias,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_botAlias,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_httpStatus,

    -- ** PutSlotType
    putSlotType_parentSlotTypeSignature,
    putSlotType_slotTypeConfigurations,
    putSlotType_checksum,
    putSlotType_valueSelectionStrategy,
    putSlotType_createVersion,
    putSlotType_description,
    putSlotType_enumerationValues,
    putSlotType_name,
    putSlotTypeResponse_parentSlotTypeSignature,
    putSlotTypeResponse_slotTypeConfigurations,
    putSlotTypeResponse_checksum,
    putSlotTypeResponse_valueSelectionStrategy,
    putSlotTypeResponse_createdDate,
    putSlotTypeResponse_name,
    putSlotTypeResponse_version,
    putSlotTypeResponse_lastUpdatedDate,
    putSlotTypeResponse_createVersion,
    putSlotTypeResponse_description,
    putSlotTypeResponse_enumerationValues,
    putSlotTypeResponse_httpStatus,

    -- ** GetBuiltinSlotTypes
    getBuiltinSlotTypes_locale,
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_signatureContains,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetUtterancesView
    getUtterancesView_botName,
    getUtterancesView_botVersions,
    getUtterancesView_statusType,
    getUtterancesViewResponse_botName,
    getUtterancesViewResponse_utterances,
    getUtterancesViewResponse_httpStatus,

    -- ** GetSlotTypeVersions
    getSlotTypeVersions_nextToken,
    getSlotTypeVersions_maxResults,
    getSlotTypeVersions_name,
    getSlotTypeVersionsResponse_nextToken,
    getSlotTypeVersionsResponse_slotTypes,
    getSlotTypeVersionsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetIntents
    getIntents_nameContains,
    getIntents_nextToken,
    getIntents_maxResults,
    getIntentsResponse_intents,
    getIntentsResponse_nextToken,
    getIntentsResponse_httpStatus,

    -- ** GetBotAlias
    getBotAlias_name,
    getBotAlias_botName,
    getBotAliasResponse_checksum,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_botName,
    getBotAliasResponse_createdDate,
    getBotAliasResponse_name,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_description,
    getBotAliasResponse_httpStatus,

    -- * Types

    -- ** BotAliasMetadata
    botAliasMetadata_checksum,
    botAliasMetadata_botVersion,
    botAliasMetadata_botName,
    botAliasMetadata_createdDate,
    botAliasMetadata_name,
    botAliasMetadata_conversationLogs,
    botAliasMetadata_lastUpdatedDate,
    botAliasMetadata_description,

    -- ** BotChannelAssociation
    botChannelAssociation_failureReason,
    botChannelAssociation_status,
    botChannelAssociation_botAlias,
    botChannelAssociation_botName,
    botChannelAssociation_botConfiguration,
    botChannelAssociation_createdDate,
    botChannelAssociation_name,
    botChannelAssociation_type,
    botChannelAssociation_description,

    -- ** BotMetadata
    botMetadata_status,
    botMetadata_createdDate,
    botMetadata_name,
    botMetadata_version,
    botMetadata_lastUpdatedDate,
    botMetadata_description,

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
    intentMetadata_name,
    intentMetadata_version,
    intentMetadata_lastUpdatedDate,
    intentMetadata_description,

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
    migrationAlert_referenceURLs,
    migrationAlert_details,
    migrationAlert_type,
    migrationAlert_message,

    -- ** MigrationSummary
    migrationSummary_v1BotVersion,
    migrationSummary_migrationStrategy,
    migrationSummary_migrationTimestamp,
    migrationSummary_migrationStatus,
    migrationSummary_v2BotId,
    migrationSummary_v1BotLocale,
    migrationSummary_v1BotName,
    migrationSummary_v2BotRole,
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

    -- ** SlotDefaultValue
    slotDefaultValue_defaultValue,

    -- ** SlotDefaultValueSpec
    slotDefaultValueSpec_defaultValueList,

    -- ** SlotTypeConfiguration
    slotTypeConfiguration_regexConfiguration,

    -- ** SlotTypeMetadata
    slotTypeMetadata_createdDate,
    slotTypeMetadata_name,
    slotTypeMetadata_version,
    slotTypeMetadata_lastUpdatedDate,
    slotTypeMetadata_description,

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
    utteranceData_lastUtteredDate,
    utteranceData_distinctUsers,

    -- ** UtteranceList
    utteranceList_botVersion,
    utteranceList_utterances,
  )
where

import Network.AWS.LexModels.CreateBotVersion
import Network.AWS.LexModels.CreateIntentVersion
import Network.AWS.LexModels.CreateSlotTypeVersion
import Network.AWS.LexModels.DeleteBot
import Network.AWS.LexModels.DeleteBotAlias
import Network.AWS.LexModels.DeleteBotChannelAssociation
import Network.AWS.LexModels.DeleteBotVersion
import Network.AWS.LexModels.DeleteIntent
import Network.AWS.LexModels.DeleteIntentVersion
import Network.AWS.LexModels.DeleteSlotType
import Network.AWS.LexModels.DeleteSlotTypeVersion
import Network.AWS.LexModels.DeleteUtterances
import Network.AWS.LexModels.GetBot
import Network.AWS.LexModels.GetBotAlias
import Network.AWS.LexModels.GetBotAliases
import Network.AWS.LexModels.GetBotChannelAssociation
import Network.AWS.LexModels.GetBotChannelAssociations
import Network.AWS.LexModels.GetBotVersions
import Network.AWS.LexModels.GetBots
import Network.AWS.LexModels.GetBuiltinIntent
import Network.AWS.LexModels.GetBuiltinIntents
import Network.AWS.LexModels.GetBuiltinSlotTypes
import Network.AWS.LexModels.GetExport
import Network.AWS.LexModels.GetImport
import Network.AWS.LexModels.GetIntent
import Network.AWS.LexModels.GetIntentVersions
import Network.AWS.LexModels.GetIntents
import Network.AWS.LexModels.GetMigration
import Network.AWS.LexModels.GetMigrations
import Network.AWS.LexModels.GetSlotType
import Network.AWS.LexModels.GetSlotTypeVersions
import Network.AWS.LexModels.GetSlotTypes
import Network.AWS.LexModels.GetUtterancesView
import Network.AWS.LexModels.ListTagsForResource
import Network.AWS.LexModels.PutBot
import Network.AWS.LexModels.PutBotAlias
import Network.AWS.LexModels.PutIntent
import Network.AWS.LexModels.PutSlotType
import Network.AWS.LexModels.StartImport
import Network.AWS.LexModels.StartMigration
import Network.AWS.LexModels.TagResource
import Network.AWS.LexModels.Types.BotAliasMetadata
import Network.AWS.LexModels.Types.BotChannelAssociation
import Network.AWS.LexModels.Types.BotMetadata
import Network.AWS.LexModels.Types.BuiltinIntentMetadata
import Network.AWS.LexModels.Types.BuiltinIntentSlot
import Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.ConversationLogsRequest
import Network.AWS.LexModels.Types.ConversationLogsResponse
import Network.AWS.LexModels.Types.EnumerationValue
import Network.AWS.LexModels.Types.FollowUpPrompt
import Network.AWS.LexModels.Types.FulfillmentActivity
import Network.AWS.LexModels.Types.InputContext
import Network.AWS.LexModels.Types.Intent
import Network.AWS.LexModels.Types.IntentMetadata
import Network.AWS.LexModels.Types.KendraConfiguration
import Network.AWS.LexModels.Types.LogSettingsRequest
import Network.AWS.LexModels.Types.LogSettingsResponse
import Network.AWS.LexModels.Types.Message
import Network.AWS.LexModels.Types.MigrationAlert
import Network.AWS.LexModels.Types.MigrationSummary
import Network.AWS.LexModels.Types.OutputContext
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.Slot
import Network.AWS.LexModels.Types.SlotDefaultValue
import Network.AWS.LexModels.Types.SlotDefaultValueSpec
import Network.AWS.LexModels.Types.SlotTypeConfiguration
import Network.AWS.LexModels.Types.SlotTypeMetadata
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
import Network.AWS.LexModels.Types.Statement
import Network.AWS.LexModels.Types.Tag
import Network.AWS.LexModels.Types.UtteranceData
import Network.AWS.LexModels.Types.UtteranceList
import Network.AWS.LexModels.UntagResource
