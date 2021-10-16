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

    -- ** DeleteSlotTypeVersion
    deleteSlotTypeVersion_name,
    deleteSlotTypeVersion_version,

    -- ** StartMigration
    startMigration_v1BotName,
    startMigration_v1BotVersion,
    startMigration_v2BotName,
    startMigration_v2BotRole,
    startMigration_migrationStrategy,
    startMigrationResponse_v1BotVersion,
    startMigrationResponse_migrationId,
    startMigrationResponse_v1BotLocale,
    startMigrationResponse_migrationTimestamp,
    startMigrationResponse_migrationStrategy,
    startMigrationResponse_v2BotRole,
    startMigrationResponse_v1BotName,
    startMigrationResponse_v2BotId,
    startMigrationResponse_httpStatus,

    -- ** GetBots
    getBots_nameContains,
    getBots_nextToken,
    getBots_maxResults,
    getBotsResponse_nextToken,
    getBotsResponse_bots,
    getBotsResponse_httpStatus,

    -- ** GetBotAlias
    getBotAlias_name,
    getBotAlias_botName,
    getBotAliasResponse_createdDate,
    getBotAliasResponse_botName,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_name,
    getBotAliasResponse_description,
    getBotAliasResponse_checksum,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_httpStatus,

    -- ** GetSlotTypes
    getSlotTypes_nameContains,
    getSlotTypes_nextToken,
    getSlotTypes_maxResults,
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_httpStatus,

    -- ** DeleteUtterances
    deleteUtterances_botName,
    deleteUtterances_userId,

    -- ** GetBotChannelAssociations
    getBotChannelAssociations_nameContains,
    getBotChannelAssociations_nextToken,
    getBotChannelAssociations_maxResults,
    getBotChannelAssociations_botName,
    getBotChannelAssociations_botAlias,
    getBotChannelAssociationsResponse_nextToken,
    getBotChannelAssociationsResponse_botChannelAssociations,
    getBotChannelAssociationsResponse_httpStatus,

    -- ** PutBotAlias
    putBotAlias_tags,
    putBotAlias_description,
    putBotAlias_checksum,
    putBotAlias_conversationLogs,
    putBotAlias_name,
    putBotAlias_botVersion,
    putBotAlias_botName,
    putBotAliasResponse_createdDate,
    putBotAliasResponse_botName,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_name,
    putBotAliasResponse_tags,
    putBotAliasResponse_description,
    putBotAliasResponse_checksum,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_httpStatus,

    -- ** GetSlotTypeVersions
    getSlotTypeVersions_nextToken,
    getSlotTypeVersions_maxResults,
    getSlotTypeVersions_name,
    getSlotTypeVersionsResponse_slotTypes,
    getSlotTypeVersionsResponse_nextToken,
    getSlotTypeVersionsResponse_httpStatus,

    -- ** GetBuiltinIntent
    getBuiltinIntent_signature,
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetUtterancesView
    getUtterancesView_botName,
    getUtterancesView_botVersions,
    getUtterancesView_statusType,
    getUtterancesViewResponse_botName,
    getUtterancesViewResponse_utterances,
    getUtterancesViewResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetBuiltinSlotTypes
    getBuiltinSlotTypes_signatureContains,
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypes_locale,
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_httpStatus,

    -- ** PutBot
    putBot_abortStatement,
    putBot_processBehavior,
    putBot_voiceId,
    putBot_clarificationPrompt,
    putBot_nluIntentConfidenceThreshold,
    putBot_enableModelImprovements,
    putBot_idleSessionTTLInSeconds,
    putBot_intents,
    putBot_tags,
    putBot_description,
    putBot_createVersion,
    putBot_detectSentiment,
    putBot_checksum,
    putBot_name,
    putBot_locale,
    putBot_childDirected,
    putBotResponse_abortStatement,
    putBotResponse_createdDate,
    putBotResponse_status,
    putBotResponse_lastUpdatedDate,
    putBotResponse_voiceId,
    putBotResponse_clarificationPrompt,
    putBotResponse_nluIntentConfidenceThreshold,
    putBotResponse_locale,
    putBotResponse_enableModelImprovements,
    putBotResponse_version,
    putBotResponse_idleSessionTTLInSeconds,
    putBotResponse_name,
    putBotResponse_intents,
    putBotResponse_failureReason,
    putBotResponse_tags,
    putBotResponse_childDirected,
    putBotResponse_description,
    putBotResponse_createVersion,
    putBotResponse_detectSentiment,
    putBotResponse_checksum,
    putBotResponse_httpStatus,

    -- ** PutIntent
    putIntent_kendraConfiguration,
    putIntent_parentIntentSignature,
    putIntent_dialogCodeHook,
    putIntent_conclusionStatement,
    putIntent_rejectionStatement,
    putIntent_inputContexts,
    putIntent_slots,
    putIntent_fulfillmentActivity,
    putIntent_sampleUtterances,
    putIntent_description,
    putIntent_createVersion,
    putIntent_confirmationPrompt,
    putIntent_outputContexts,
    putIntent_followUpPrompt,
    putIntent_checksum,
    putIntent_name,
    putIntentResponse_kendraConfiguration,
    putIntentResponse_createdDate,
    putIntentResponse_parentIntentSignature,
    putIntentResponse_dialogCodeHook,
    putIntentResponse_conclusionStatement,
    putIntentResponse_lastUpdatedDate,
    putIntentResponse_rejectionStatement,
    putIntentResponse_inputContexts,
    putIntentResponse_version,
    putIntentResponse_slots,
    putIntentResponse_fulfillmentActivity,
    putIntentResponse_name,
    putIntentResponse_sampleUtterances,
    putIntentResponse_description,
    putIntentResponse_createVersion,
    putIntentResponse_confirmationPrompt,
    putIntentResponse_outputContexts,
    putIntentResponse_followUpPrompt,
    putIntentResponse_checksum,
    putIntentResponse_httpStatus,

    -- ** GetBotChannelAssociation
    getBotChannelAssociation_name,
    getBotChannelAssociation_botName,
    getBotChannelAssociation_botAlias,
    getBotChannelAssociationResponse_botAlias,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_httpStatus,

    -- ** DeleteSlotType
    deleteSlotType_name,

    -- ** GetExport
    getExport_name,
    getExport_version,
    getExport_resourceType,
    getExport_exportType,
    getExportResponse_exportStatus,
    getExportResponse_version,
    getExportResponse_resourceType,
    getExportResponse_name,
    getExportResponse_failureReason,
    getExportResponse_exportType,
    getExportResponse_url,
    getExportResponse_httpStatus,

    -- ** GetSlotType
    getSlotType_name,
    getSlotType_version,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_version,
    getSlotTypeResponse_name,
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_description,
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_httpStatus,

    -- ** CreateIntentVersion
    createIntentVersion_checksum,
    createIntentVersion_name,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_version,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_name,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_description,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_httpStatus,

    -- ** GetBot
    getBot_name,
    getBot_versionOrAlias,
    getBotResponse_abortStatement,
    getBotResponse_createdDate,
    getBotResponse_status,
    getBotResponse_lastUpdatedDate,
    getBotResponse_voiceId,
    getBotResponse_clarificationPrompt,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_locale,
    getBotResponse_enableModelImprovements,
    getBotResponse_version,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_name,
    getBotResponse_intents,
    getBotResponse_failureReason,
    getBotResponse_childDirected,
    getBotResponse_description,
    getBotResponse_detectSentiment,
    getBotResponse_checksum,
    getBotResponse_httpStatus,

    -- ** GetBotAliases
    getBotAliases_nameContains,
    getBotAliases_nextToken,
    getBotAliases_maxResults,
    getBotAliases_botName,
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_httpStatus,

    -- ** DeleteIntentVersion
    deleteIntentVersion_name,
    deleteIntentVersion_version,

    -- ** CreateBotVersion
    createBotVersion_checksum,
    createBotVersion_name,
    createBotVersionResponse_abortStatement,
    createBotVersionResponse_createdDate,
    createBotVersionResponse_status,
    createBotVersionResponse_lastUpdatedDate,
    createBotVersionResponse_voiceId,
    createBotVersionResponse_clarificationPrompt,
    createBotVersionResponse_locale,
    createBotVersionResponse_enableModelImprovements,
    createBotVersionResponse_version,
    createBotVersionResponse_idleSessionTTLInSeconds,
    createBotVersionResponse_name,
    createBotVersionResponse_intents,
    createBotVersionResponse_failureReason,
    createBotVersionResponse_childDirected,
    createBotVersionResponse_description,
    createBotVersionResponse_detectSentiment,
    createBotVersionResponse_checksum,
    createBotVersionResponse_httpStatus,

    -- ** GetIntents
    getIntents_nameContains,
    getIntents_nextToken,
    getIntents_maxResults,
    getIntentsResponse_nextToken,
    getIntentsResponse_intents,
    getIntentsResponse_httpStatus,

    -- ** GetMigrations
    getMigrations_migrationStatusEquals,
    getMigrations_nextToken,
    getMigrations_maxResults,
    getMigrations_sortByAttribute,
    getMigrations_v1BotNameContains,
    getMigrations_sortByOrder,
    getMigrationsResponse_nextToken,
    getMigrationsResponse_migrationSummaries,
    getMigrationsResponse_httpStatus,

    -- ** GetBotVersions
    getBotVersions_nextToken,
    getBotVersions_maxResults,
    getBotVersions_name,
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_bots,
    getBotVersionsResponse_httpStatus,

    -- ** DeleteBotAlias
    deleteBotAlias_name,
    deleteBotAlias_botName,

    -- ** GetIntentVersions
    getIntentVersions_nextToken,
    getIntentVersions_maxResults,
    getIntentVersions_name,
    getIntentVersionsResponse_nextToken,
    getIntentVersionsResponse_intents,
    getIntentVersionsResponse_httpStatus,

    -- ** GetImport
    getImport_importId,
    getImportResponse_createdDate,
    getImportResponse_mergeStrategy,
    getImportResponse_importId,
    getImportResponse_resourceType,
    getImportResponse_name,
    getImportResponse_failureReason,
    getImportResponse_importStatus,
    getImportResponse_httpStatus,

    -- ** GetBuiltinIntents
    getBuiltinIntents_signatureContains,
    getBuiltinIntents_nextToken,
    getBuiltinIntents_maxResults,
    getBuiltinIntents_locale,
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_httpStatus,

    -- ** DeleteBot
    deleteBot_name,

    -- ** StartImport
    startImport_tags,
    startImport_payload,
    startImport_resourceType,
    startImport_mergeStrategy,
    startImportResponse_createdDate,
    startImportResponse_mergeStrategy,
    startImportResponse_importId,
    startImportResponse_resourceType,
    startImportResponse_name,
    startImportResponse_importStatus,
    startImportResponse_tags,
    startImportResponse_httpStatus,

    -- ** PutSlotType
    putSlotType_slotTypeConfigurations,
    putSlotType_enumerationValues,
    putSlotType_valueSelectionStrategy,
    putSlotType_parentSlotTypeSignature,
    putSlotType_description,
    putSlotType_createVersion,
    putSlotType_checksum,
    putSlotType_name,
    putSlotTypeResponse_slotTypeConfigurations,
    putSlotTypeResponse_createdDate,
    putSlotTypeResponse_enumerationValues,
    putSlotTypeResponse_lastUpdatedDate,
    putSlotTypeResponse_valueSelectionStrategy,
    putSlotTypeResponse_version,
    putSlotTypeResponse_name,
    putSlotTypeResponse_parentSlotTypeSignature,
    putSlotTypeResponse_description,
    putSlotTypeResponse_createVersion,
    putSlotTypeResponse_checksum,
    putSlotTypeResponse_httpStatus,

    -- ** DeleteIntent
    deleteIntent_name,

    -- ** CreateSlotTypeVersion
    createSlotTypeVersion_checksum,
    createSlotTypeVersion_name,
    createSlotTypeVersionResponse_slotTypeConfigurations,
    createSlotTypeVersionResponse_createdDate,
    createSlotTypeVersionResponse_enumerationValues,
    createSlotTypeVersionResponse_lastUpdatedDate,
    createSlotTypeVersionResponse_valueSelectionStrategy,
    createSlotTypeVersionResponse_version,
    createSlotTypeVersionResponse_name,
    createSlotTypeVersionResponse_parentSlotTypeSignature,
    createSlotTypeVersionResponse_description,
    createSlotTypeVersionResponse_checksum,
    createSlotTypeVersionResponse_httpStatus,

    -- ** DeleteBotChannelAssociation
    deleteBotChannelAssociation_name,
    deleteBotChannelAssociation_botName,
    deleteBotChannelAssociation_botAlias,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetMigration
    getMigration_migrationId,
    getMigrationResponse_v1BotVersion,
    getMigrationResponse_migrationId,
    getMigrationResponse_v1BotLocale,
    getMigrationResponse_migrationTimestamp,
    getMigrationResponse_migrationStrategy,
    getMigrationResponse_v2BotRole,
    getMigrationResponse_v1BotName,
    getMigrationResponse_v2BotId,
    getMigrationResponse_migrationStatus,
    getMigrationResponse_alerts,
    getMigrationResponse_httpStatus,

    -- ** GetIntent
    getIntent_name,
    getIntent_version,
    getIntentResponse_kendraConfiguration,
    getIntentResponse_createdDate,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_conclusionStatement,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_rejectionStatement,
    getIntentResponse_inputContexts,
    getIntentResponse_version,
    getIntentResponse_slots,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_name,
    getIntentResponse_sampleUtterances,
    getIntentResponse_description,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_outputContexts,
    getIntentResponse_followUpPrompt,
    getIntentResponse_checksum,
    getIntentResponse_httpStatus,

    -- ** DeleteBotVersion
    deleteBotVersion_name,
    deleteBotVersion_version,

    -- * Types

    -- ** BotAliasMetadata
    botAliasMetadata_createdDate,
    botAliasMetadata_botName,
    botAliasMetadata_lastUpdatedDate,
    botAliasMetadata_botVersion,
    botAliasMetadata_name,
    botAliasMetadata_description,
    botAliasMetadata_checksum,
    botAliasMetadata_conversationLogs,

    -- ** BotChannelAssociation
    botChannelAssociation_botAlias,
    botChannelAssociation_createdDate,
    botChannelAssociation_status,
    botChannelAssociation_botConfiguration,
    botChannelAssociation_botName,
    botChannelAssociation_name,
    botChannelAssociation_failureReason,
    botChannelAssociation_description,
    botChannelAssociation_type,

    -- ** BotMetadata
    botMetadata_createdDate,
    botMetadata_status,
    botMetadata_lastUpdatedDate,
    botMetadata_version,
    botMetadata_name,
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
    intentMetadata_lastUpdatedDate,
    intentMetadata_version,
    intentMetadata_name,
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
    logSettingsResponse_resourceArn,
    logSettingsResponse_logType,
    logSettingsResponse_kmsKeyArn,
    logSettingsResponse_destination,
    logSettingsResponse_resourcePrefix,

    -- ** Message
    message_groupNumber,
    message_contentType,
    message_content,

    -- ** MigrationAlert
    migrationAlert_message,
    migrationAlert_details,
    migrationAlert_referenceURLs,
    migrationAlert_type,

    -- ** MigrationSummary
    migrationSummary_v1BotVersion,
    migrationSummary_migrationId,
    migrationSummary_v1BotLocale,
    migrationSummary_migrationTimestamp,
    migrationSummary_migrationStrategy,
    migrationSummary_v2BotRole,
    migrationSummary_v1BotName,
    migrationSummary_v2BotId,
    migrationSummary_migrationStatus,

    -- ** OutputContext
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- ** Prompt
    prompt_responseCard,
    prompt_messages,
    prompt_maxAttempts,

    -- ** Slot
    slot_responseCard,
    slot_slotType,
    slot_valueElicitationPrompt,
    slot_slotTypeVersion,
    slot_priority,
    slot_sampleUtterances,
    slot_description,
    slot_defaultValueSpec,
    slot_obfuscationSetting,
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
    slotTypeMetadata_lastUpdatedDate,
    slotTypeMetadata_version,
    slotTypeMetadata_name,
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
    utteranceData_utteranceString,
    utteranceData_distinctUsers,
    utteranceData_count,
    utteranceData_lastUtteredDate,
    utteranceData_firstUtteredDate,

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
