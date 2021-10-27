{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Models.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Lens
  ( -- * Operations

    -- ** DeleteImport
    deleteImport_importId,
    deleteImportResponse_importId,
    deleteImportResponse_importStatus,
    deleteImportResponse_httpStatus,

    -- ** DescribeBot
    describeBot_botId,
    describeBotResponse_botStatus,
    describeBotResponse_botName,
    describeBotResponse_lastUpdatedDateTime,
    describeBotResponse_dataPrivacy,
    describeBotResponse_botId,
    describeBotResponse_idleSessionTTLInSeconds,
    describeBotResponse_creationDateTime,
    describeBotResponse_description,
    describeBotResponse_roleArn,
    describeBotResponse_httpStatus,

    -- ** DescribeBotLocale
    describeBotLocale_botId,
    describeBotLocale_botVersion,
    describeBotLocale_localeId,
    describeBotLocaleResponse_slotTypesCount,
    describeBotLocaleResponse_lastBuildSubmittedDateTime,
    describeBotLocaleResponse_botLocaleHistoryEvents,
    describeBotLocaleResponse_botLocaleStatus,
    describeBotLocaleResponse_nluIntentConfidenceThreshold,
    describeBotLocaleResponse_voiceSettings,
    describeBotLocaleResponse_botVersion,
    describeBotLocaleResponse_lastUpdatedDateTime,
    describeBotLocaleResponse_botId,
    describeBotLocaleResponse_intentsCount,
    describeBotLocaleResponse_localeName,
    describeBotLocaleResponse_failureReasons,
    describeBotLocaleResponse_localeId,
    describeBotLocaleResponse_creationDateTime,
    describeBotLocaleResponse_description,
    describeBotLocaleResponse_httpStatus,

    -- ** ListBuiltInSlotTypes
    listBuiltInSlotTypes_nextToken,
    listBuiltInSlotTypes_maxResults,
    listBuiltInSlotTypes_sortBy,
    listBuiltInSlotTypes_localeId,
    listBuiltInSlotTypesResponse_builtInSlotTypeSummaries,
    listBuiltInSlotTypesResponse_nextToken,
    listBuiltInSlotTypesResponse_localeId,
    listBuiltInSlotTypesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListBotVersions
    listBotVersions_nextToken,
    listBotVersions_maxResults,
    listBotVersions_sortBy,
    listBotVersions_botId,
    listBotVersionsResponse_botVersionSummaries,
    listBotVersionsResponse_nextToken,
    listBotVersionsResponse_botId,
    listBotVersionsResponse_httpStatus,

    -- ** ListIntents
    listIntents_filters,
    listIntents_nextToken,
    listIntents_maxResults,
    listIntents_sortBy,
    listIntents_botId,
    listIntents_botVersion,
    listIntents_localeId,
    listIntentsResponse_botVersion,
    listIntentsResponse_nextToken,
    listIntentsResponse_botId,
    listIntentsResponse_localeId,
    listIntentsResponse_intentSummaries,
    listIntentsResponse_httpStatus,

    -- ** ListSlots
    listSlots_filters,
    listSlots_nextToken,
    listSlots_maxResults,
    listSlots_sortBy,
    listSlots_botId,
    listSlots_botVersion,
    listSlots_localeId,
    listSlots_intentId,
    listSlotsResponse_botVersion,
    listSlotsResponse_intentId,
    listSlotsResponse_nextToken,
    listSlotsResponse_botId,
    listSlotsResponse_localeId,
    listSlotsResponse_slotSummaries,
    listSlotsResponse_httpStatus,

    -- ** DeleteIntent
    deleteIntent_intentId,
    deleteIntent_botId,
    deleteIntent_botVersion,
    deleteIntent_localeId,

    -- ** DeleteSlot
    deleteSlot_slotId,
    deleteSlot_botId,
    deleteSlot_botVersion,
    deleteSlot_localeId,
    deleteSlot_intentId,

    -- ** UpdateIntent
    updateIntent_fulfillmentCodeHook,
    updateIntent_intentConfirmationSetting,
    updateIntent_slotPriorities,
    updateIntent_sampleUtterances,
    updateIntent_parentIntentSignature,
    updateIntent_kendraConfiguration,
    updateIntent_inputContexts,
    updateIntent_outputContexts,
    updateIntent_dialogCodeHook,
    updateIntent_description,
    updateIntent_intentClosingSetting,
    updateIntent_intentId,
    updateIntent_intentName,
    updateIntent_botId,
    updateIntent_botVersion,
    updateIntent_localeId,
    updateIntentResponse_fulfillmentCodeHook,
    updateIntentResponse_intentConfirmationSetting,
    updateIntentResponse_slotPriorities,
    updateIntentResponse_intentName,
    updateIntentResponse_botVersion,
    updateIntentResponse_lastUpdatedDateTime,
    updateIntentResponse_sampleUtterances,
    updateIntentResponse_intentId,
    updateIntentResponse_parentIntentSignature,
    updateIntentResponse_botId,
    updateIntentResponse_kendraConfiguration,
    updateIntentResponse_inputContexts,
    updateIntentResponse_localeId,
    updateIntentResponse_creationDateTime,
    updateIntentResponse_outputContexts,
    updateIntentResponse_dialogCodeHook,
    updateIntentResponse_description,
    updateIntentResponse_intentClosingSetting,
    updateIntentResponse_httpStatus,

    -- ** UpdateSlot
    updateSlot_obfuscationSetting,
    updateSlot_multipleValuesSetting,
    updateSlot_description,
    updateSlot_slotId,
    updateSlot_slotName,
    updateSlot_slotTypeId,
    updateSlot_valueElicitationSetting,
    updateSlot_botId,
    updateSlot_botVersion,
    updateSlot_localeId,
    updateSlot_intentId,
    updateSlotResponse_obfuscationSetting,
    updateSlotResponse_slotName,
    updateSlotResponse_botVersion,
    updateSlotResponse_valueElicitationSetting,
    updateSlotResponse_lastUpdatedDateTime,
    updateSlotResponse_multipleValuesSetting,
    updateSlotResponse_slotId,
    updateSlotResponse_intentId,
    updateSlotResponse_botId,
    updateSlotResponse_localeId,
    updateSlotResponse_creationDateTime,
    updateSlotResponse_description,
    updateSlotResponse_slotTypeId,
    updateSlotResponse_httpStatus,

    -- ** CreateSlot
    createSlot_obfuscationSetting,
    createSlot_multipleValuesSetting,
    createSlot_description,
    createSlot_slotName,
    createSlot_slotTypeId,
    createSlot_valueElicitationSetting,
    createSlot_botId,
    createSlot_botVersion,
    createSlot_localeId,
    createSlot_intentId,
    createSlotResponse_obfuscationSetting,
    createSlotResponse_slotName,
    createSlotResponse_botVersion,
    createSlotResponse_valueElicitationSetting,
    createSlotResponse_multipleValuesSetting,
    createSlotResponse_slotId,
    createSlotResponse_intentId,
    createSlotResponse_botId,
    createSlotResponse_localeId,
    createSlotResponse_creationDateTime,
    createSlotResponse_description,
    createSlotResponse_slotTypeId,
    createSlotResponse_httpStatus,

    -- ** ListBots
    listBots_filters,
    listBots_nextToken,
    listBots_maxResults,
    listBots_sortBy,
    listBotsResponse_botSummaries,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,

    -- ** DeleteBotLocale
    deleteBotLocale_botId,
    deleteBotLocale_botVersion,
    deleteBotLocale_localeId,
    deleteBotLocaleResponse_botLocaleStatus,
    deleteBotLocaleResponse_botVersion,
    deleteBotLocaleResponse_botId,
    deleteBotLocaleResponse_localeId,
    deleteBotLocaleResponse_httpStatus,

    -- ** UpdateBotLocale
    updateBotLocale_voiceSettings,
    updateBotLocale_description,
    updateBotLocale_botId,
    updateBotLocale_botVersion,
    updateBotLocale_localeId,
    updateBotLocale_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_botLocaleStatus,
    updateBotLocaleResponse_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_voiceSettings,
    updateBotLocaleResponse_botVersion,
    updateBotLocaleResponse_lastUpdatedDateTime,
    updateBotLocaleResponse_botId,
    updateBotLocaleResponse_localeName,
    updateBotLocaleResponse_failureReasons,
    updateBotLocaleResponse_localeId,
    updateBotLocaleResponse_creationDateTime,
    updateBotLocaleResponse_description,
    updateBotLocaleResponse_httpStatus,

    -- ** CreateIntent
    createIntent_fulfillmentCodeHook,
    createIntent_intentConfirmationSetting,
    createIntent_sampleUtterances,
    createIntent_parentIntentSignature,
    createIntent_kendraConfiguration,
    createIntent_inputContexts,
    createIntent_outputContexts,
    createIntent_dialogCodeHook,
    createIntent_description,
    createIntent_intentClosingSetting,
    createIntent_intentName,
    createIntent_botId,
    createIntent_botVersion,
    createIntent_localeId,
    createIntentResponse_fulfillmentCodeHook,
    createIntentResponse_intentConfirmationSetting,
    createIntentResponse_intentName,
    createIntentResponse_botVersion,
    createIntentResponse_sampleUtterances,
    createIntentResponse_intentId,
    createIntentResponse_parentIntentSignature,
    createIntentResponse_botId,
    createIntentResponse_kendraConfiguration,
    createIntentResponse_inputContexts,
    createIntentResponse_localeId,
    createIntentResponse_creationDateTime,
    createIntentResponse_outputContexts,
    createIntentResponse_dialogCodeHook,
    createIntentResponse_description,
    createIntentResponse_intentClosingSetting,
    createIntentResponse_httpStatus,

    -- ** DescribeImport
    describeImport_importId,
    describeImportResponse_resourceSpecification,
    describeImportResponse_importId,
    describeImportResponse_importedResourceId,
    describeImportResponse_lastUpdatedDateTime,
    describeImportResponse_importedResourceName,
    describeImportResponse_failureReasons,
    describeImportResponse_creationDateTime,
    describeImportResponse_mergeStrategy,
    describeImportResponse_importStatus,
    describeImportResponse_httpStatus,

    -- ** DeleteBot
    deleteBot_skipResourceInUseCheck,
    deleteBot_botId,
    deleteBotResponse_botStatus,
    deleteBotResponse_botId,
    deleteBotResponse_httpStatus,

    -- ** UpdateBot
    updateBot_description,
    updateBot_botId,
    updateBot_botName,
    updateBot_roleArn,
    updateBot_dataPrivacy,
    updateBot_idleSessionTTLInSeconds,
    updateBotResponse_botStatus,
    updateBotResponse_botName,
    updateBotResponse_lastUpdatedDateTime,
    updateBotResponse_dataPrivacy,
    updateBotResponse_botId,
    updateBotResponse_idleSessionTTLInSeconds,
    updateBotResponse_creationDateTime,
    updateBotResponse_description,
    updateBotResponse_roleArn,
    updateBotResponse_httpStatus,

    -- ** ListBotLocales
    listBotLocales_filters,
    listBotLocales_nextToken,
    listBotLocales_maxResults,
    listBotLocales_sortBy,
    listBotLocales_botId,
    listBotLocales_botVersion,
    listBotLocalesResponse_botLocaleSummaries,
    listBotLocalesResponse_botVersion,
    listBotLocalesResponse_nextToken,
    listBotLocalesResponse_botId,
    listBotLocalesResponse_httpStatus,

    -- ** CreateResourcePolicy
    createResourcePolicy_resourceArn,
    createResourcePolicy_policy,
    createResourcePolicyResponse_resourceArn,
    createResourcePolicyResponse_revisionId,
    createResourcePolicyResponse_httpStatus,

    -- ** DeleteBotAlias
    deleteBotAlias_skipResourceInUseCheck,
    deleteBotAlias_botAliasId,
    deleteBotAlias_botId,
    deleteBotAliasResponse_botAliasStatus,
    deleteBotAliasResponse_botId,
    deleteBotAliasResponse_botAliasId,
    deleteBotAliasResponse_httpStatus,

    -- ** UpdateBotAlias
    updateBotAlias_botAliasLocaleSettings,
    updateBotAlias_botVersion,
    updateBotAlias_conversationLogSettings,
    updateBotAlias_sentimentAnalysisSettings,
    updateBotAlias_description,
    updateBotAlias_botAliasId,
    updateBotAlias_botAliasName,
    updateBotAlias_botId,
    updateBotAliasResponse_botAliasLocaleSettings,
    updateBotAliasResponse_botAliasStatus,
    updateBotAliasResponse_botVersion,
    updateBotAliasResponse_lastUpdatedDateTime,
    updateBotAliasResponse_conversationLogSettings,
    updateBotAliasResponse_botId,
    updateBotAliasResponse_botAliasId,
    updateBotAliasResponse_creationDateTime,
    updateBotAliasResponse_sentimentAnalysisSettings,
    updateBotAliasResponse_botAliasName,
    updateBotAliasResponse_description,
    updateBotAliasResponse_httpStatus,

    -- ** DescribeBotVersion
    describeBotVersion_botId,
    describeBotVersion_botVersion,
    describeBotVersionResponse_botStatus,
    describeBotVersionResponse_botVersion,
    describeBotVersionResponse_botName,
    describeBotVersionResponse_dataPrivacy,
    describeBotVersionResponse_botId,
    describeBotVersionResponse_failureReasons,
    describeBotVersionResponse_idleSessionTTLInSeconds,
    describeBotVersionResponse_creationDateTime,
    describeBotVersionResponse_description,
    describeBotVersionResponse_roleArn,
    describeBotVersionResponse_httpStatus,

    -- ** DescribeSlot
    describeSlot_slotId,
    describeSlot_botId,
    describeSlot_botVersion,
    describeSlot_localeId,
    describeSlot_intentId,
    describeSlotResponse_obfuscationSetting,
    describeSlotResponse_slotName,
    describeSlotResponse_botVersion,
    describeSlotResponse_valueElicitationSetting,
    describeSlotResponse_lastUpdatedDateTime,
    describeSlotResponse_multipleValuesSetting,
    describeSlotResponse_slotId,
    describeSlotResponse_intentId,
    describeSlotResponse_botId,
    describeSlotResponse_localeId,
    describeSlotResponse_creationDateTime,
    describeSlotResponse_description,
    describeSlotResponse_slotTypeId,
    describeSlotResponse_httpStatus,

    -- ** DescribeIntent
    describeIntent_intentId,
    describeIntent_botId,
    describeIntent_botVersion,
    describeIntent_localeId,
    describeIntentResponse_fulfillmentCodeHook,
    describeIntentResponse_intentConfirmationSetting,
    describeIntentResponse_slotPriorities,
    describeIntentResponse_intentName,
    describeIntentResponse_botVersion,
    describeIntentResponse_lastUpdatedDateTime,
    describeIntentResponse_sampleUtterances,
    describeIntentResponse_intentId,
    describeIntentResponse_parentIntentSignature,
    describeIntentResponse_botId,
    describeIntentResponse_kendraConfiguration,
    describeIntentResponse_inputContexts,
    describeIntentResponse_localeId,
    describeIntentResponse_creationDateTime,
    describeIntentResponse_outputContexts,
    describeIntentResponse_dialogCodeHook,
    describeIntentResponse_description,
    describeIntentResponse_intentClosingSetting,
    describeIntentResponse_httpStatus,

    -- ** DeleteUtterances
    deleteUtterances_localeId,
    deleteUtterances_sessionId,
    deleteUtterances_botId,
    deleteUtterancesResponse_httpStatus,

    -- ** CreateUploadUrl
    createUploadUrlResponse_importId,
    createUploadUrlResponse_uploadUrl,
    createUploadUrlResponse_httpStatus,

    -- ** ListBuiltInIntents
    listBuiltInIntents_nextToken,
    listBuiltInIntents_maxResults,
    listBuiltInIntents_sortBy,
    listBuiltInIntents_localeId,
    listBuiltInIntentsResponse_nextToken,
    listBuiltInIntentsResponse_builtInIntentSummaries,
    listBuiltInIntentsResponse_localeId,
    listBuiltInIntentsResponse_httpStatus,

    -- ** ListImports
    listImports_filters,
    listImports_botVersion,
    listImports_nextToken,
    listImports_botId,
    listImports_maxResults,
    listImports_sortBy,
    listImportsResponse_botVersion,
    listImportsResponse_nextToken,
    listImportsResponse_botId,
    listImportsResponse_importSummaries,
    listImportsResponse_httpStatus,

    -- ** ListAggregatedUtterances
    listAggregatedUtterances_filters,
    listAggregatedUtterances_botVersion,
    listAggregatedUtterances_nextToken,
    listAggregatedUtterances_botAliasId,
    listAggregatedUtterances_maxResults,
    listAggregatedUtterances_sortBy,
    listAggregatedUtterances_botId,
    listAggregatedUtterances_localeId,
    listAggregatedUtterances_aggregationDuration,
    listAggregatedUtterancesResponse_aggregationWindowEndTime,
    listAggregatedUtterancesResponse_botVersion,
    listAggregatedUtterancesResponse_aggregationLastRefreshedDateTime,
    listAggregatedUtterancesResponse_nextToken,
    listAggregatedUtterancesResponse_botId,
    listAggregatedUtterancesResponse_aggregatedUtterancesSummaries,
    listAggregatedUtterancesResponse_aggregationDuration,
    listAggregatedUtterancesResponse_botAliasId,
    listAggregatedUtterancesResponse_localeId,
    listAggregatedUtterancesResponse_aggregationWindowStartTime,
    listAggregatedUtterancesResponse_httpStatus,

    -- ** CreateBotVersion
    createBotVersion_description,
    createBotVersion_botId,
    createBotVersion_botVersionLocaleSpecification,
    createBotVersionResponse_botStatus,
    createBotVersionResponse_botVersion,
    createBotVersionResponse_botId,
    createBotVersionResponse_creationDateTime,
    createBotVersionResponse_botVersionLocaleSpecification,
    createBotVersionResponse_description,
    createBotVersionResponse_httpStatus,

    -- ** BuildBotLocale
    buildBotLocale_botId,
    buildBotLocale_botVersion,
    buildBotLocale_localeId,
    buildBotLocaleResponse_lastBuildSubmittedDateTime,
    buildBotLocaleResponse_botLocaleStatus,
    buildBotLocaleResponse_botVersion,
    buildBotLocaleResponse_botId,
    buildBotLocaleResponse_localeId,
    buildBotLocaleResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_revisionId,
    describeResourcePolicyResponse_httpStatus,

    -- ** DeleteBotVersion
    deleteBotVersion_skipResourceInUseCheck,
    deleteBotVersion_botId,
    deleteBotVersion_botVersion,
    deleteBotVersionResponse_botStatus,
    deleteBotVersionResponse_botVersion,
    deleteBotVersionResponse_botId,
    deleteBotVersionResponse_httpStatus,

    -- ** DescribeBotAlias
    describeBotAlias_botAliasId,
    describeBotAlias_botId,
    describeBotAliasResponse_botAliasHistoryEvents,
    describeBotAliasResponse_botAliasLocaleSettings,
    describeBotAliasResponse_botAliasStatus,
    describeBotAliasResponse_botVersion,
    describeBotAliasResponse_lastUpdatedDateTime,
    describeBotAliasResponse_conversationLogSettings,
    describeBotAliasResponse_botId,
    describeBotAliasResponse_botAliasId,
    describeBotAliasResponse_creationDateTime,
    describeBotAliasResponse_sentimentAnalysisSettings,
    describeBotAliasResponse_botAliasName,
    describeBotAliasResponse_description,
    describeBotAliasResponse_httpStatus,

    -- ** UpdateSlotType
    updateSlotType_parentSlotTypeSignature,
    updateSlotType_slotTypeValues,
    updateSlotType_description,
    updateSlotType_slotTypeId,
    updateSlotType_slotTypeName,
    updateSlotType_valueSelectionSetting,
    updateSlotType_botId,
    updateSlotType_botVersion,
    updateSlotType_localeId,
    updateSlotTypeResponse_parentSlotTypeSignature,
    updateSlotTypeResponse_slotTypeValues,
    updateSlotTypeResponse_valueSelectionSetting,
    updateSlotTypeResponse_botVersion,
    updateSlotTypeResponse_lastUpdatedDateTime,
    updateSlotTypeResponse_botId,
    updateSlotTypeResponse_localeId,
    updateSlotTypeResponse_creationDateTime,
    updateSlotTypeResponse_slotTypeName,
    updateSlotTypeResponse_description,
    updateSlotTypeResponse_slotTypeId,
    updateSlotTypeResponse_httpStatus,

    -- ** DeleteSlotType
    deleteSlotType_skipResourceInUseCheck,
    deleteSlotType_slotTypeId,
    deleteSlotType_botId,
    deleteSlotType_botVersion,
    deleteSlotType_localeId,

    -- ** CreateBotLocale
    createBotLocale_voiceSettings,
    createBotLocale_description,
    createBotLocale_botId,
    createBotLocale_botVersion,
    createBotLocale_localeId,
    createBotLocale_nluIntentConfidenceThreshold,
    createBotLocaleResponse_botLocaleStatus,
    createBotLocaleResponse_nluIntentConfidenceThreshold,
    createBotLocaleResponse_voiceSettings,
    createBotLocaleResponse_botVersion,
    createBotLocaleResponse_botId,
    createBotLocaleResponse_localeName,
    createBotLocaleResponse_localeId,
    createBotLocaleResponse_creationDateTime,
    createBotLocaleResponse_description,
    createBotLocaleResponse_httpStatus,

    -- ** ListSlotTypes
    listSlotTypes_filters,
    listSlotTypes_nextToken,
    listSlotTypes_maxResults,
    listSlotTypes_sortBy,
    listSlotTypes_botId,
    listSlotTypes_botVersion,
    listSlotTypes_localeId,
    listSlotTypesResponse_botVersion,
    listSlotTypesResponse_slotTypeSummaries,
    listSlotTypesResponse_nextToken,
    listSlotTypesResponse_botId,
    listSlotTypesResponse_localeId,
    listSlotTypesResponse_httpStatus,

    -- ** DeleteExport
    deleteExport_exportId,
    deleteExportResponse_exportStatus,
    deleteExportResponse_exportId,
    deleteExportResponse_httpStatus,

    -- ** StartImport
    startImport_filePassword,
    startImport_importId,
    startImport_resourceSpecification,
    startImport_mergeStrategy,
    startImportResponse_resourceSpecification,
    startImportResponse_importId,
    startImportResponse_creationDateTime,
    startImportResponse_mergeStrategy,
    startImportResponse_importStatus,
    startImportResponse_httpStatus,

    -- ** UpdateExport
    updateExport_filePassword,
    updateExport_exportId,
    updateExportResponse_resourceSpecification,
    updateExportResponse_fileFormat,
    updateExportResponse_exportStatus,
    updateExportResponse_lastUpdatedDateTime,
    updateExportResponse_creationDateTime,
    updateExportResponse_exportId,
    updateExportResponse_httpStatus,

    -- ** CreateBot
    createBot_testBotAliasTags,
    createBot_botTags,
    createBot_description,
    createBot_botName,
    createBot_roleArn,
    createBot_dataPrivacy,
    createBot_idleSessionTTLInSeconds,
    createBotResponse_botStatus,
    createBotResponse_testBotAliasTags,
    createBotResponse_botName,
    createBotResponse_dataPrivacy,
    createBotResponse_botId,
    createBotResponse_idleSessionTTLInSeconds,
    createBotResponse_creationDateTime,
    createBotResponse_botTags,
    createBotResponse_description,
    createBotResponse_roleArn,
    createBotResponse_httpStatus,

    -- ** ListExports
    listExports_filters,
    listExports_botVersion,
    listExports_nextToken,
    listExports_botId,
    listExports_maxResults,
    listExports_sortBy,
    listExportsResponse_botVersion,
    listExportsResponse_exportSummaries,
    listExportsResponse_nextToken,
    listExportsResponse_botId,
    listExportsResponse_httpStatus,

    -- ** CreateSlotType
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeValues,
    createSlotType_description,
    createSlotType_slotTypeName,
    createSlotType_valueSelectionSetting,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_botId,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_description,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_httpStatus,

    -- ** CreateExport
    createExport_filePassword,
    createExport_resourceSpecification,
    createExport_fileFormat,
    createExportResponse_resourceSpecification,
    createExportResponse_fileFormat,
    createExportResponse_exportStatus,
    createExportResponse_creationDateTime,
    createExportResponse_exportId,
    createExportResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListBotAliases
    listBotAliases_nextToken,
    listBotAliases_maxResults,
    listBotAliases_botId,
    listBotAliasesResponse_botAliasSummaries,
    listBotAliasesResponse_nextToken,
    listBotAliasesResponse_botId,
    listBotAliasesResponse_httpStatus,

    -- ** CreateBotAlias
    createBotAlias_botAliasLocaleSettings,
    createBotAlias_botVersion,
    createBotAlias_conversationLogSettings,
    createBotAlias_sentimentAnalysisSettings,
    createBotAlias_description,
    createBotAlias_tags,
    createBotAlias_botAliasName,
    createBotAlias_botId,
    createBotAliasResponse_botAliasLocaleSettings,
    createBotAliasResponse_botAliasStatus,
    createBotAliasResponse_botVersion,
    createBotAliasResponse_conversationLogSettings,
    createBotAliasResponse_botId,
    createBotAliasResponse_botAliasId,
    createBotAliasResponse_creationDateTime,
    createBotAliasResponse_sentimentAnalysisSettings,
    createBotAliasResponse_botAliasName,
    createBotAliasResponse_description,
    createBotAliasResponse_tags,
    createBotAliasResponse_httpStatus,

    -- ** CreateResourcePolicyStatement
    createResourcePolicyStatement_expectedRevisionId,
    createResourcePolicyStatement_condition,
    createResourcePolicyStatement_resourceArn,
    createResourcePolicyStatement_statementId,
    createResourcePolicyStatement_effect,
    createResourcePolicyStatement_principal,
    createResourcePolicyStatement_action,
    createResourcePolicyStatementResponse_resourceArn,
    createResourcePolicyStatementResponse_revisionId,
    createResourcePolicyStatementResponse_httpStatus,

    -- ** UpdateResourcePolicy
    updateResourcePolicy_expectedRevisionId,
    updateResourcePolicy_resourceArn,
    updateResourcePolicy_policy,
    updateResourcePolicyResponse_resourceArn,
    updateResourcePolicyResponse_revisionId,
    updateResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_expectedRevisionId,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_resourceArn,
    deleteResourcePolicyResponse_revisionId,
    deleteResourcePolicyResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteResourcePolicyStatement
    deleteResourcePolicyStatement_expectedRevisionId,
    deleteResourcePolicyStatement_resourceArn,
    deleteResourcePolicyStatement_statementId,
    deleteResourcePolicyStatementResponse_resourceArn,
    deleteResourcePolicyStatementResponse_revisionId,
    deleteResourcePolicyStatementResponse_httpStatus,

    -- ** DescribeSlotType
    describeSlotType_slotTypeId,
    describeSlotType_botId,
    describeSlotType_botVersion,
    describeSlotType_localeId,
    describeSlotTypeResponse_parentSlotTypeSignature,
    describeSlotTypeResponse_slotTypeValues,
    describeSlotTypeResponse_valueSelectionSetting,
    describeSlotTypeResponse_botVersion,
    describeSlotTypeResponse_lastUpdatedDateTime,
    describeSlotTypeResponse_botId,
    describeSlotTypeResponse_localeId,
    describeSlotTypeResponse_creationDateTime,
    describeSlotTypeResponse_slotTypeName,
    describeSlotTypeResponse_description,
    describeSlotTypeResponse_slotTypeId,
    describeSlotTypeResponse_httpStatus,

    -- ** DescribeExport
    describeExport_exportId,
    describeExportResponse_resourceSpecification,
    describeExportResponse_fileFormat,
    describeExportResponse_exportStatus,
    describeExportResponse_lastUpdatedDateTime,
    describeExportResponse_downloadUrl,
    describeExportResponse_failureReasons,
    describeExportResponse_creationDateTime,
    describeExportResponse_exportId,
    describeExportResponse_httpStatus,

    -- * Types

    -- ** AggregatedUtterancesFilter
    aggregatedUtterancesFilter_name,
    aggregatedUtterancesFilter_values,
    aggregatedUtterancesFilter_operator,

    -- ** AggregatedUtterancesSortBy
    aggregatedUtterancesSortBy_attribute,
    aggregatedUtterancesSortBy_order,

    -- ** AggregatedUtterancesSummary
    aggregatedUtterancesSummary_utteranceFirstRecordedInAggregationDuration,
    aggregatedUtterancesSummary_utteranceLastRecordedInAggregationDuration,
    aggregatedUtterancesSummary_hitCount,
    aggregatedUtterancesSummary_missedCount,
    aggregatedUtterancesSummary_containsDataFromDeletedResources,
    aggregatedUtterancesSummary_utterance,

    -- ** AudioLogDestination
    audioLogDestination_s3Bucket,

    -- ** AudioLogSetting
    audioLogSetting_enabled,
    audioLogSetting_destination,

    -- ** BotAliasHistoryEvent
    botAliasHistoryEvent_endDate,
    botAliasHistoryEvent_botVersion,
    botAliasHistoryEvent_startDate,

    -- ** BotAliasLocaleSettings
    botAliasLocaleSettings_codeHookSpecification,
    botAliasLocaleSettings_enabled,

    -- ** BotAliasSummary
    botAliasSummary_botAliasStatus,
    botAliasSummary_botVersion,
    botAliasSummary_lastUpdatedDateTime,
    botAliasSummary_botAliasId,
    botAliasSummary_creationDateTime,
    botAliasSummary_botAliasName,
    botAliasSummary_description,

    -- ** BotExportSpecification
    botExportSpecification_botId,
    botExportSpecification_botVersion,

    -- ** BotFilter
    botFilter_name,
    botFilter_values,
    botFilter_operator,

    -- ** BotImportSpecification
    botImportSpecification_testBotAliasTags,
    botImportSpecification_idleSessionTTLInSeconds,
    botImportSpecification_botTags,
    botImportSpecification_botName,
    botImportSpecification_roleArn,
    botImportSpecification_dataPrivacy,

    -- ** BotLocaleExportSpecification
    botLocaleExportSpecification_botId,
    botLocaleExportSpecification_botVersion,
    botLocaleExportSpecification_localeId,

    -- ** BotLocaleFilter
    botLocaleFilter_name,
    botLocaleFilter_values,
    botLocaleFilter_operator,

    -- ** BotLocaleHistoryEvent
    botLocaleHistoryEvent_event,
    botLocaleHistoryEvent_eventDate,

    -- ** BotLocaleImportSpecification
    botLocaleImportSpecification_nluIntentConfidenceThreshold,
    botLocaleImportSpecification_voiceSettings,
    botLocaleImportSpecification_botId,
    botLocaleImportSpecification_botVersion,
    botLocaleImportSpecification_localeId,

    -- ** BotLocaleSortBy
    botLocaleSortBy_attribute,
    botLocaleSortBy_order,

    -- ** BotLocaleSummary
    botLocaleSummary_lastBuildSubmittedDateTime,
    botLocaleSummary_botLocaleStatus,
    botLocaleSummary_lastUpdatedDateTime,
    botLocaleSummary_localeName,
    botLocaleSummary_localeId,
    botLocaleSummary_description,

    -- ** BotSortBy
    botSortBy_attribute,
    botSortBy_order,

    -- ** BotSummary
    botSummary_botStatus,
    botSummary_botName,
    botSummary_lastUpdatedDateTime,
    botSummary_botId,
    botSummary_latestBotVersion,
    botSummary_description,

    -- ** BotVersionLocaleDetails
    botVersionLocaleDetails_sourceBotVersion,

    -- ** BotVersionSortBy
    botVersionSortBy_attribute,
    botVersionSortBy_order,

    -- ** BotVersionSummary
    botVersionSummary_botStatus,
    botVersionSummary_botVersion,
    botVersionSummary_botName,
    botVersionSummary_creationDateTime,
    botVersionSummary_description,

    -- ** BuiltInIntentSortBy
    builtInIntentSortBy_attribute,
    builtInIntentSortBy_order,

    -- ** BuiltInIntentSummary
    builtInIntentSummary_intentSignature,
    builtInIntentSummary_description,

    -- ** BuiltInSlotTypeSortBy
    builtInSlotTypeSortBy_attribute,
    builtInSlotTypeSortBy_order,

    -- ** BuiltInSlotTypeSummary
    builtInSlotTypeSummary_slotTypeSignature,
    builtInSlotTypeSummary_description,

    -- ** Button
    button_text,
    button_value,

    -- ** CloudWatchLogGroupLogDestination
    cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn,
    cloudWatchLogGroupLogDestination_logPrefix,

    -- ** CodeHookSpecification
    codeHookSpecification_lambdaCodeHook,

    -- ** ConversationLogSettings
    conversationLogSettings_audioLogSettings,
    conversationLogSettings_textLogSettings,

    -- ** CustomPayload
    customPayload_value,

    -- ** DataPrivacy
    dataPrivacy_childDirected,

    -- ** DialogCodeHookSettings
    dialogCodeHookSettings_enabled,

    -- ** ExportFilter
    exportFilter_name,
    exportFilter_values,
    exportFilter_operator,

    -- ** ExportResourceSpecification
    exportResourceSpecification_botExportSpecification,
    exportResourceSpecification_botLocaleExportSpecification,

    -- ** ExportSortBy
    exportSortBy_attribute,
    exportSortBy_order,

    -- ** ExportSummary
    exportSummary_resourceSpecification,
    exportSummary_fileFormat,
    exportSummary_exportStatus,
    exportSummary_lastUpdatedDateTime,
    exportSummary_creationDateTime,
    exportSummary_exportId,

    -- ** FulfillmentCodeHookSettings
    fulfillmentCodeHookSettings_postFulfillmentStatusSpecification,
    fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification,
    fulfillmentCodeHookSettings_enabled,

    -- ** FulfillmentStartResponseSpecification
    fulfillmentStartResponseSpecification_allowInterrupt,
    fulfillmentStartResponseSpecification_delayInSeconds,
    fulfillmentStartResponseSpecification_messageGroups,

    -- ** FulfillmentUpdateResponseSpecification
    fulfillmentUpdateResponseSpecification_allowInterrupt,
    fulfillmentUpdateResponseSpecification_frequencyInSeconds,
    fulfillmentUpdateResponseSpecification_messageGroups,

    -- ** FulfillmentUpdatesSpecification
    fulfillmentUpdatesSpecification_startResponse,
    fulfillmentUpdatesSpecification_timeoutInSeconds,
    fulfillmentUpdatesSpecification_updateResponse,
    fulfillmentUpdatesSpecification_active,

    -- ** ImageResponseCard
    imageResponseCard_buttons,
    imageResponseCard_subtitle,
    imageResponseCard_imageUrl,
    imageResponseCard_title,

    -- ** ImportFilter
    importFilter_name,
    importFilter_values,
    importFilter_operator,

    -- ** ImportResourceSpecification
    importResourceSpecification_botImportSpecification,
    importResourceSpecification_botLocaleImportSpecification,

    -- ** ImportSortBy
    importSortBy_attribute,
    importSortBy_order,

    -- ** ImportSummary
    importSummary_importId,
    importSummary_importedResourceId,
    importSummary_lastUpdatedDateTime,
    importSummary_importedResourceName,
    importSummary_creationDateTime,
    importSummary_mergeStrategy,
    importSummary_importStatus,

    -- ** InputContext
    inputContext_name,

    -- ** IntentClosingSetting
    intentClosingSetting_active,
    intentClosingSetting_closingResponse,

    -- ** IntentConfirmationSetting
    intentConfirmationSetting_active,
    intentConfirmationSetting_promptSpecification,
    intentConfirmationSetting_declinationResponse,

    -- ** IntentFilter
    intentFilter_name,
    intentFilter_values,
    intentFilter_operator,

    -- ** IntentSortBy
    intentSortBy_attribute,
    intentSortBy_order,

    -- ** IntentSummary
    intentSummary_intentName,
    intentSummary_lastUpdatedDateTime,
    intentSummary_intentId,
    intentSummary_parentIntentSignature,
    intentSummary_inputContexts,
    intentSummary_outputContexts,
    intentSummary_description,

    -- ** KendraConfiguration
    kendraConfiguration_queryFilterString,
    kendraConfiguration_queryFilterStringEnabled,
    kendraConfiguration_kendraIndex,

    -- ** LambdaCodeHook
    lambdaCodeHook_lambdaARN,
    lambdaCodeHook_codeHookInterfaceVersion,

    -- ** Message
    message_ssmlMessage,
    message_customPayload,
    message_imageResponseCard,
    message_plainTextMessage,

    -- ** MessageGroup
    messageGroup_variations,
    messageGroup_message,

    -- ** MultipleValuesSetting
    multipleValuesSetting_allowMultipleValues,

    -- ** ObfuscationSetting
    obfuscationSetting_obfuscationSettingType,

    -- ** OutputContext
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- ** PlainTextMessage
    plainTextMessage_value,

    -- ** PostFulfillmentStatusSpecification
    postFulfillmentStatusSpecification_successResponse,
    postFulfillmentStatusSpecification_timeoutResponse,
    postFulfillmentStatusSpecification_failureResponse,

    -- ** Principal
    principal_arn,
    principal_service,

    -- ** PromptSpecification
    promptSpecification_allowInterrupt,
    promptSpecification_messageGroups,
    promptSpecification_maxRetries,

    -- ** RelativeAggregationDuration
    relativeAggregationDuration_timeDimension,
    relativeAggregationDuration_timeValue,

    -- ** ResponseSpecification
    responseSpecification_allowInterrupt,
    responseSpecification_messageGroups,

    -- ** S3BucketLogDestination
    s3BucketLogDestination_kmsKeyArn,
    s3BucketLogDestination_s3BucketArn,
    s3BucketLogDestination_logPrefix,

    -- ** SSMLMessage
    sSMLMessage_value,

    -- ** SampleUtterance
    sampleUtterance_utterance,

    -- ** SampleValue
    sampleValue_value,

    -- ** SentimentAnalysisSettings
    sentimentAnalysisSettings_detectSentiment,

    -- ** SlotDefaultValue
    slotDefaultValue_defaultValue,

    -- ** SlotDefaultValueSpecification
    slotDefaultValueSpecification_defaultValueList,

    -- ** SlotFilter
    slotFilter_name,
    slotFilter_values,
    slotFilter_operator,

    -- ** SlotPriority
    slotPriority_priority,
    slotPriority_slotId,

    -- ** SlotSortBy
    slotSortBy_attribute,
    slotSortBy_order,

    -- ** SlotSummary
    slotSummary_slotName,
    slotSummary_lastUpdatedDateTime,
    slotSummary_slotConstraint,
    slotSummary_slotId,
    slotSummary_description,
    slotSummary_slotTypeId,
    slotSummary_valueElicitationPromptSpecification,

    -- ** SlotTypeFilter
    slotTypeFilter_name,
    slotTypeFilter_values,
    slotTypeFilter_operator,

    -- ** SlotTypeSortBy
    slotTypeSortBy_attribute,
    slotTypeSortBy_order,

    -- ** SlotTypeSummary
    slotTypeSummary_parentSlotTypeSignature,
    slotTypeSummary_lastUpdatedDateTime,
    slotTypeSummary_slotTypeName,
    slotTypeSummary_description,
    slotTypeSummary_slotTypeId,

    -- ** SlotTypeValue
    slotTypeValue_sampleValue,
    slotTypeValue_synonyms,

    -- ** SlotValueElicitationSetting
    slotValueElicitationSetting_waitAndContinueSpecification,
    slotValueElicitationSetting_defaultValueSpecification,
    slotValueElicitationSetting_sampleUtterances,
    slotValueElicitationSetting_promptSpecification,
    slotValueElicitationSetting_slotConstraint,

    -- ** SlotValueRegexFilter
    slotValueRegexFilter_pattern,

    -- ** SlotValueSelectionSetting
    slotValueSelectionSetting_regexFilter,
    slotValueSelectionSetting_resolutionStrategy,

    -- ** StillWaitingResponseSpecification
    stillWaitingResponseSpecification_allowInterrupt,
    stillWaitingResponseSpecification_messageGroups,
    stillWaitingResponseSpecification_frequencyInSeconds,
    stillWaitingResponseSpecification_timeoutInSeconds,

    -- ** TextLogDestination
    textLogDestination_cloudWatch,

    -- ** TextLogSetting
    textLogSetting_enabled,
    textLogSetting_destination,

    -- ** UtteranceAggregationDuration
    utteranceAggregationDuration_relativeAggregationDuration,

    -- ** VoiceSettings
    voiceSettings_voiceId,

    -- ** WaitAndContinueSpecification
    waitAndContinueSpecification_active,
    waitAndContinueSpecification_stillWaitingResponse,
    waitAndContinueSpecification_waitingResponse,
    waitAndContinueSpecification_continueResponse,
  )
where

import Network.AWS.LexV2Models.BuildBotLocale
import Network.AWS.LexV2Models.CreateBot
import Network.AWS.LexV2Models.CreateBotAlias
import Network.AWS.LexV2Models.CreateBotLocale
import Network.AWS.LexV2Models.CreateBotVersion
import Network.AWS.LexV2Models.CreateExport
import Network.AWS.LexV2Models.CreateIntent
import Network.AWS.LexV2Models.CreateResourcePolicy
import Network.AWS.LexV2Models.CreateResourcePolicyStatement
import Network.AWS.LexV2Models.CreateSlot
import Network.AWS.LexV2Models.CreateSlotType
import Network.AWS.LexV2Models.CreateUploadUrl
import Network.AWS.LexV2Models.DeleteBot
import Network.AWS.LexV2Models.DeleteBotAlias
import Network.AWS.LexV2Models.DeleteBotLocale
import Network.AWS.LexV2Models.DeleteBotVersion
import Network.AWS.LexV2Models.DeleteExport
import Network.AWS.LexV2Models.DeleteImport
import Network.AWS.LexV2Models.DeleteIntent
import Network.AWS.LexV2Models.DeleteResourcePolicy
import Network.AWS.LexV2Models.DeleteResourcePolicyStatement
import Network.AWS.LexV2Models.DeleteSlot
import Network.AWS.LexV2Models.DeleteSlotType
import Network.AWS.LexV2Models.DeleteUtterances
import Network.AWS.LexV2Models.DescribeBot
import Network.AWS.LexV2Models.DescribeBotAlias
import Network.AWS.LexV2Models.DescribeBotLocale
import Network.AWS.LexV2Models.DescribeBotVersion
import Network.AWS.LexV2Models.DescribeExport
import Network.AWS.LexV2Models.DescribeImport
import Network.AWS.LexV2Models.DescribeIntent
import Network.AWS.LexV2Models.DescribeResourcePolicy
import Network.AWS.LexV2Models.DescribeSlot
import Network.AWS.LexV2Models.DescribeSlotType
import Network.AWS.LexV2Models.ListAggregatedUtterances
import Network.AWS.LexV2Models.ListBotAliases
import Network.AWS.LexV2Models.ListBotLocales
import Network.AWS.LexV2Models.ListBotVersions
import Network.AWS.LexV2Models.ListBots
import Network.AWS.LexV2Models.ListBuiltInIntents
import Network.AWS.LexV2Models.ListBuiltInSlotTypes
import Network.AWS.LexV2Models.ListExports
import Network.AWS.LexV2Models.ListImports
import Network.AWS.LexV2Models.ListIntents
import Network.AWS.LexV2Models.ListSlotTypes
import Network.AWS.LexV2Models.ListSlots
import Network.AWS.LexV2Models.ListTagsForResource
import Network.AWS.LexV2Models.StartImport
import Network.AWS.LexV2Models.TagResource
import Network.AWS.LexV2Models.Types.AggregatedUtterancesFilter
import Network.AWS.LexV2Models.Types.AggregatedUtterancesSortBy
import Network.AWS.LexV2Models.Types.AggregatedUtterancesSummary
import Network.AWS.LexV2Models.Types.AudioLogDestination
import Network.AWS.LexV2Models.Types.AudioLogSetting
import Network.AWS.LexV2Models.Types.BotAliasHistoryEvent
import Network.AWS.LexV2Models.Types.BotAliasLocaleSettings
import Network.AWS.LexV2Models.Types.BotAliasSummary
import Network.AWS.LexV2Models.Types.BotExportSpecification
import Network.AWS.LexV2Models.Types.BotFilter
import Network.AWS.LexV2Models.Types.BotImportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleExportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleFilter
import Network.AWS.LexV2Models.Types.BotLocaleHistoryEvent
import Network.AWS.LexV2Models.Types.BotLocaleImportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleSortBy
import Network.AWS.LexV2Models.Types.BotLocaleSummary
import Network.AWS.LexV2Models.Types.BotSortBy
import Network.AWS.LexV2Models.Types.BotSummary
import Network.AWS.LexV2Models.Types.BotVersionLocaleDetails
import Network.AWS.LexV2Models.Types.BotVersionSortBy
import Network.AWS.LexV2Models.Types.BotVersionSummary
import Network.AWS.LexV2Models.Types.BuiltInIntentSortBy
import Network.AWS.LexV2Models.Types.BuiltInIntentSummary
import Network.AWS.LexV2Models.Types.BuiltInSlotTypeSortBy
import Network.AWS.LexV2Models.Types.BuiltInSlotTypeSummary
import Network.AWS.LexV2Models.Types.Button
import Network.AWS.LexV2Models.Types.CloudWatchLogGroupLogDestination
import Network.AWS.LexV2Models.Types.CodeHookSpecification
import Network.AWS.LexV2Models.Types.ConversationLogSettings
import Network.AWS.LexV2Models.Types.CustomPayload
import Network.AWS.LexV2Models.Types.DataPrivacy
import Network.AWS.LexV2Models.Types.DialogCodeHookSettings
import Network.AWS.LexV2Models.Types.ExportFilter
import Network.AWS.LexV2Models.Types.ExportResourceSpecification
import Network.AWS.LexV2Models.Types.ExportSortBy
import Network.AWS.LexV2Models.Types.ExportSummary
import Network.AWS.LexV2Models.Types.FulfillmentCodeHookSettings
import Network.AWS.LexV2Models.Types.FulfillmentStartResponseSpecification
import Network.AWS.LexV2Models.Types.FulfillmentUpdateResponseSpecification
import Network.AWS.LexV2Models.Types.FulfillmentUpdatesSpecification
import Network.AWS.LexV2Models.Types.ImageResponseCard
import Network.AWS.LexV2Models.Types.ImportFilter
import Network.AWS.LexV2Models.Types.ImportResourceSpecification
import Network.AWS.LexV2Models.Types.ImportSortBy
import Network.AWS.LexV2Models.Types.ImportSummary
import Network.AWS.LexV2Models.Types.InputContext
import Network.AWS.LexV2Models.Types.IntentClosingSetting
import Network.AWS.LexV2Models.Types.IntentConfirmationSetting
import Network.AWS.LexV2Models.Types.IntentFilter
import Network.AWS.LexV2Models.Types.IntentSortBy
import Network.AWS.LexV2Models.Types.IntentSummary
import Network.AWS.LexV2Models.Types.KendraConfiguration
import Network.AWS.LexV2Models.Types.LambdaCodeHook
import Network.AWS.LexV2Models.Types.Message
import Network.AWS.LexV2Models.Types.MessageGroup
import Network.AWS.LexV2Models.Types.MultipleValuesSetting
import Network.AWS.LexV2Models.Types.ObfuscationSetting
import Network.AWS.LexV2Models.Types.OutputContext
import Network.AWS.LexV2Models.Types.PlainTextMessage
import Network.AWS.LexV2Models.Types.PostFulfillmentStatusSpecification
import Network.AWS.LexV2Models.Types.Principal
import Network.AWS.LexV2Models.Types.PromptSpecification
import Network.AWS.LexV2Models.Types.RelativeAggregationDuration
import Network.AWS.LexV2Models.Types.ResponseSpecification
import Network.AWS.LexV2Models.Types.S3BucketLogDestination
import Network.AWS.LexV2Models.Types.SSMLMessage
import Network.AWS.LexV2Models.Types.SampleUtterance
import Network.AWS.LexV2Models.Types.SampleValue
import Network.AWS.LexV2Models.Types.SentimentAnalysisSettings
import Network.AWS.LexV2Models.Types.SlotDefaultValue
import Network.AWS.LexV2Models.Types.SlotDefaultValueSpecification
import Network.AWS.LexV2Models.Types.SlotFilter
import Network.AWS.LexV2Models.Types.SlotPriority
import Network.AWS.LexV2Models.Types.SlotSortBy
import Network.AWS.LexV2Models.Types.SlotSummary
import Network.AWS.LexV2Models.Types.SlotTypeFilter
import Network.AWS.LexV2Models.Types.SlotTypeSortBy
import Network.AWS.LexV2Models.Types.SlotTypeSummary
import Network.AWS.LexV2Models.Types.SlotTypeValue
import Network.AWS.LexV2Models.Types.SlotValueElicitationSetting
import Network.AWS.LexV2Models.Types.SlotValueRegexFilter
import Network.AWS.LexV2Models.Types.SlotValueSelectionSetting
import Network.AWS.LexV2Models.Types.StillWaitingResponseSpecification
import Network.AWS.LexV2Models.Types.TextLogDestination
import Network.AWS.LexV2Models.Types.TextLogSetting
import Network.AWS.LexV2Models.Types.UtteranceAggregationDuration
import Network.AWS.LexV2Models.Types.VoiceSettings
import Network.AWS.LexV2Models.Types.WaitAndContinueSpecification
import Network.AWS.LexV2Models.UntagResource
import Network.AWS.LexV2Models.UpdateBot
import Network.AWS.LexV2Models.UpdateBotAlias
import Network.AWS.LexV2Models.UpdateBotLocale
import Network.AWS.LexV2Models.UpdateExport
import Network.AWS.LexV2Models.UpdateIntent
import Network.AWS.LexV2Models.UpdateResourcePolicy
import Network.AWS.LexV2Models.UpdateSlot
import Network.AWS.LexV2Models.UpdateSlotType
