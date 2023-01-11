{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Lens
  ( -- * Operations

    -- ** BatchCreateCustomVocabularyItem
    batchCreateCustomVocabularyItem_botId,
    batchCreateCustomVocabularyItem_botVersion,
    batchCreateCustomVocabularyItem_localeId,
    batchCreateCustomVocabularyItem_customVocabularyItemList,
    batchCreateCustomVocabularyItemResponse_botId,
    batchCreateCustomVocabularyItemResponse_botVersion,
    batchCreateCustomVocabularyItemResponse_errors,
    batchCreateCustomVocabularyItemResponse_localeId,
    batchCreateCustomVocabularyItemResponse_resources,
    batchCreateCustomVocabularyItemResponse_httpStatus,

    -- ** BatchDeleteCustomVocabularyItem
    batchDeleteCustomVocabularyItem_botId,
    batchDeleteCustomVocabularyItem_botVersion,
    batchDeleteCustomVocabularyItem_localeId,
    batchDeleteCustomVocabularyItem_customVocabularyItemList,
    batchDeleteCustomVocabularyItemResponse_botId,
    batchDeleteCustomVocabularyItemResponse_botVersion,
    batchDeleteCustomVocabularyItemResponse_errors,
    batchDeleteCustomVocabularyItemResponse_localeId,
    batchDeleteCustomVocabularyItemResponse_resources,
    batchDeleteCustomVocabularyItemResponse_httpStatus,

    -- ** BatchUpdateCustomVocabularyItem
    batchUpdateCustomVocabularyItem_botId,
    batchUpdateCustomVocabularyItem_botVersion,
    batchUpdateCustomVocabularyItem_localeId,
    batchUpdateCustomVocabularyItem_customVocabularyItemList,
    batchUpdateCustomVocabularyItemResponse_botId,
    batchUpdateCustomVocabularyItemResponse_botVersion,
    batchUpdateCustomVocabularyItemResponse_errors,
    batchUpdateCustomVocabularyItemResponse_localeId,
    batchUpdateCustomVocabularyItemResponse_resources,
    batchUpdateCustomVocabularyItemResponse_httpStatus,

    -- ** BuildBotLocale
    buildBotLocale_botId,
    buildBotLocale_botVersion,
    buildBotLocale_localeId,
    buildBotLocaleResponse_botId,
    buildBotLocaleResponse_botLocaleStatus,
    buildBotLocaleResponse_botVersion,
    buildBotLocaleResponse_lastBuildSubmittedDateTime,
    buildBotLocaleResponse_localeId,
    buildBotLocaleResponse_httpStatus,

    -- ** CreateBot
    createBot_botTags,
    createBot_description,
    createBot_testBotAliasTags,
    createBot_botName,
    createBot_roleArn,
    createBot_dataPrivacy,
    createBot_idleSessionTTLInSeconds,
    createBotResponse_botId,
    createBotResponse_botName,
    createBotResponse_botStatus,
    createBotResponse_botTags,
    createBotResponse_creationDateTime,
    createBotResponse_dataPrivacy,
    createBotResponse_description,
    createBotResponse_idleSessionTTLInSeconds,
    createBotResponse_roleArn,
    createBotResponse_testBotAliasTags,
    createBotResponse_httpStatus,

    -- ** CreateBotAlias
    createBotAlias_botAliasLocaleSettings,
    createBotAlias_botVersion,
    createBotAlias_conversationLogSettings,
    createBotAlias_description,
    createBotAlias_sentimentAnalysisSettings,
    createBotAlias_tags,
    createBotAlias_botAliasName,
    createBotAlias_botId,
    createBotAliasResponse_botAliasId,
    createBotAliasResponse_botAliasLocaleSettings,
    createBotAliasResponse_botAliasName,
    createBotAliasResponse_botAliasStatus,
    createBotAliasResponse_botId,
    createBotAliasResponse_botVersion,
    createBotAliasResponse_conversationLogSettings,
    createBotAliasResponse_creationDateTime,
    createBotAliasResponse_description,
    createBotAliasResponse_sentimentAnalysisSettings,
    createBotAliasResponse_tags,
    createBotAliasResponse_httpStatus,

    -- ** CreateBotLocale
    createBotLocale_description,
    createBotLocale_voiceSettings,
    createBotLocale_botId,
    createBotLocale_botVersion,
    createBotLocale_localeId,
    createBotLocale_nluIntentConfidenceThreshold,
    createBotLocaleResponse_botId,
    createBotLocaleResponse_botLocaleStatus,
    createBotLocaleResponse_botVersion,
    createBotLocaleResponse_creationDateTime,
    createBotLocaleResponse_description,
    createBotLocaleResponse_localeId,
    createBotLocaleResponse_localeName,
    createBotLocaleResponse_nluIntentConfidenceThreshold,
    createBotLocaleResponse_voiceSettings,
    createBotLocaleResponse_httpStatus,

    -- ** CreateBotVersion
    createBotVersion_description,
    createBotVersion_botId,
    createBotVersion_botVersionLocaleSpecification,
    createBotVersionResponse_botId,
    createBotVersionResponse_botStatus,
    createBotVersionResponse_botVersion,
    createBotVersionResponse_botVersionLocaleSpecification,
    createBotVersionResponse_creationDateTime,
    createBotVersionResponse_description,
    createBotVersionResponse_httpStatus,

    -- ** CreateExport
    createExport_filePassword,
    createExport_resourceSpecification,
    createExport_fileFormat,
    createExportResponse_creationDateTime,
    createExportResponse_exportId,
    createExportResponse_exportStatus,
    createExportResponse_fileFormat,
    createExportResponse_resourceSpecification,
    createExportResponse_httpStatus,

    -- ** CreateIntent
    createIntent_description,
    createIntent_dialogCodeHook,
    createIntent_fulfillmentCodeHook,
    createIntent_initialResponseSetting,
    createIntent_inputContexts,
    createIntent_intentClosingSetting,
    createIntent_intentConfirmationSetting,
    createIntent_kendraConfiguration,
    createIntent_outputContexts,
    createIntent_parentIntentSignature,
    createIntent_sampleUtterances,
    createIntent_intentName,
    createIntent_botId,
    createIntent_botVersion,
    createIntent_localeId,
    createIntentResponse_botId,
    createIntentResponse_botVersion,
    createIntentResponse_creationDateTime,
    createIntentResponse_description,
    createIntentResponse_dialogCodeHook,
    createIntentResponse_fulfillmentCodeHook,
    createIntentResponse_initialResponseSetting,
    createIntentResponse_inputContexts,
    createIntentResponse_intentClosingSetting,
    createIntentResponse_intentConfirmationSetting,
    createIntentResponse_intentId,
    createIntentResponse_intentName,
    createIntentResponse_kendraConfiguration,
    createIntentResponse_localeId,
    createIntentResponse_outputContexts,
    createIntentResponse_parentIntentSignature,
    createIntentResponse_sampleUtterances,
    createIntentResponse_httpStatus,

    -- ** CreateResourcePolicy
    createResourcePolicy_resourceArn,
    createResourcePolicy_policy,
    createResourcePolicyResponse_resourceArn,
    createResourcePolicyResponse_revisionId,
    createResourcePolicyResponse_httpStatus,

    -- ** CreateResourcePolicyStatement
    createResourcePolicyStatement_condition,
    createResourcePolicyStatement_expectedRevisionId,
    createResourcePolicyStatement_resourceArn,
    createResourcePolicyStatement_statementId,
    createResourcePolicyStatement_effect,
    createResourcePolicyStatement_principal,
    createResourcePolicyStatement_action,
    createResourcePolicyStatementResponse_resourceArn,
    createResourcePolicyStatementResponse_revisionId,
    createResourcePolicyStatementResponse_httpStatus,

    -- ** CreateSlot
    createSlot_description,
    createSlot_multipleValuesSetting,
    createSlot_obfuscationSetting,
    createSlot_slotTypeId,
    createSlot_subSlotSetting,
    createSlot_slotName,
    createSlot_valueElicitationSetting,
    createSlot_botId,
    createSlot_botVersion,
    createSlot_localeId,
    createSlot_intentId,
    createSlotResponse_botId,
    createSlotResponse_botVersion,
    createSlotResponse_creationDateTime,
    createSlotResponse_description,
    createSlotResponse_intentId,
    createSlotResponse_localeId,
    createSlotResponse_multipleValuesSetting,
    createSlotResponse_obfuscationSetting,
    createSlotResponse_slotId,
    createSlotResponse_slotName,
    createSlotResponse_slotTypeId,
    createSlotResponse_subSlotSetting,
    createSlotResponse_valueElicitationSetting,
    createSlotResponse_httpStatus,

    -- ** CreateSlotType
    createSlotType_compositeSlotTypeSetting,
    createSlotType_description,
    createSlotType_externalSourceSetting,
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeValues,
    createSlotType_valueSelectionSetting,
    createSlotType_slotTypeName,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,
    createSlotTypeResponse_botId,
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_compositeSlotTypeSetting,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_description,
    createSlotTypeResponse_externalSourceSetting,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_httpStatus,

    -- ** CreateUploadUrl
    createUploadUrlResponse_importId,
    createUploadUrlResponse_uploadUrl,
    createUploadUrlResponse_httpStatus,

    -- ** DeleteBot
    deleteBot_skipResourceInUseCheck,
    deleteBot_botId,
    deleteBotResponse_botId,
    deleteBotResponse_botStatus,
    deleteBotResponse_httpStatus,

    -- ** DeleteBotAlias
    deleteBotAlias_skipResourceInUseCheck,
    deleteBotAlias_botAliasId,
    deleteBotAlias_botId,
    deleteBotAliasResponse_botAliasId,
    deleteBotAliasResponse_botAliasStatus,
    deleteBotAliasResponse_botId,
    deleteBotAliasResponse_httpStatus,

    -- ** DeleteBotLocale
    deleteBotLocale_botId,
    deleteBotLocale_botVersion,
    deleteBotLocale_localeId,
    deleteBotLocaleResponse_botId,
    deleteBotLocaleResponse_botLocaleStatus,
    deleteBotLocaleResponse_botVersion,
    deleteBotLocaleResponse_localeId,
    deleteBotLocaleResponse_httpStatus,

    -- ** DeleteBotVersion
    deleteBotVersion_skipResourceInUseCheck,
    deleteBotVersion_botId,
    deleteBotVersion_botVersion,
    deleteBotVersionResponse_botId,
    deleteBotVersionResponse_botStatus,
    deleteBotVersionResponse_botVersion,
    deleteBotVersionResponse_httpStatus,

    -- ** DeleteCustomVocabulary
    deleteCustomVocabulary_botId,
    deleteCustomVocabulary_botVersion,
    deleteCustomVocabulary_localeId,
    deleteCustomVocabularyResponse_botId,
    deleteCustomVocabularyResponse_botVersion,
    deleteCustomVocabularyResponse_customVocabularyStatus,
    deleteCustomVocabularyResponse_localeId,
    deleteCustomVocabularyResponse_httpStatus,

    -- ** DeleteExport
    deleteExport_exportId,
    deleteExportResponse_exportId,
    deleteExportResponse_exportStatus,
    deleteExportResponse_httpStatus,

    -- ** DeleteImport
    deleteImport_importId,
    deleteImportResponse_importId,
    deleteImportResponse_importStatus,
    deleteImportResponse_httpStatus,

    -- ** DeleteIntent
    deleteIntent_intentId,
    deleteIntent_botId,
    deleteIntent_botVersion,
    deleteIntent_localeId,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_expectedRevisionId,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_resourceArn,
    deleteResourcePolicyResponse_revisionId,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicyStatement
    deleteResourcePolicyStatement_expectedRevisionId,
    deleteResourcePolicyStatement_resourceArn,
    deleteResourcePolicyStatement_statementId,
    deleteResourcePolicyStatementResponse_resourceArn,
    deleteResourcePolicyStatementResponse_revisionId,
    deleteResourcePolicyStatementResponse_httpStatus,

    -- ** DeleteSlot
    deleteSlot_slotId,
    deleteSlot_botId,
    deleteSlot_botVersion,
    deleteSlot_localeId,
    deleteSlot_intentId,

    -- ** DeleteSlotType
    deleteSlotType_skipResourceInUseCheck,
    deleteSlotType_slotTypeId,
    deleteSlotType_botId,
    deleteSlotType_botVersion,
    deleteSlotType_localeId,

    -- ** DeleteUtterances
    deleteUtterances_localeId,
    deleteUtterances_sessionId,
    deleteUtterances_botId,
    deleteUtterancesResponse_httpStatus,

    -- ** DescribeBot
    describeBot_botId,
    describeBotResponse_botId,
    describeBotResponse_botName,
    describeBotResponse_botStatus,
    describeBotResponse_creationDateTime,
    describeBotResponse_dataPrivacy,
    describeBotResponse_description,
    describeBotResponse_idleSessionTTLInSeconds,
    describeBotResponse_lastUpdatedDateTime,
    describeBotResponse_roleArn,
    describeBotResponse_httpStatus,

    -- ** DescribeBotAlias
    describeBotAlias_botAliasId,
    describeBotAlias_botId,
    describeBotAliasResponse_botAliasHistoryEvents,
    describeBotAliasResponse_botAliasId,
    describeBotAliasResponse_botAliasLocaleSettings,
    describeBotAliasResponse_botAliasName,
    describeBotAliasResponse_botAliasStatus,
    describeBotAliasResponse_botId,
    describeBotAliasResponse_botVersion,
    describeBotAliasResponse_conversationLogSettings,
    describeBotAliasResponse_creationDateTime,
    describeBotAliasResponse_description,
    describeBotAliasResponse_lastUpdatedDateTime,
    describeBotAliasResponse_sentimentAnalysisSettings,
    describeBotAliasResponse_httpStatus,

    -- ** DescribeBotLocale
    describeBotLocale_botId,
    describeBotLocale_botVersion,
    describeBotLocale_localeId,
    describeBotLocaleResponse_botId,
    describeBotLocaleResponse_botLocaleHistoryEvents,
    describeBotLocaleResponse_botLocaleStatus,
    describeBotLocaleResponse_botVersion,
    describeBotLocaleResponse_creationDateTime,
    describeBotLocaleResponse_description,
    describeBotLocaleResponse_failureReasons,
    describeBotLocaleResponse_intentsCount,
    describeBotLocaleResponse_lastBuildSubmittedDateTime,
    describeBotLocaleResponse_lastUpdatedDateTime,
    describeBotLocaleResponse_localeId,
    describeBotLocaleResponse_localeName,
    describeBotLocaleResponse_nluIntentConfidenceThreshold,
    describeBotLocaleResponse_recommendedActions,
    describeBotLocaleResponse_slotTypesCount,
    describeBotLocaleResponse_voiceSettings,
    describeBotLocaleResponse_httpStatus,

    -- ** DescribeBotRecommendation
    describeBotRecommendation_botId,
    describeBotRecommendation_botVersion,
    describeBotRecommendation_localeId,
    describeBotRecommendation_botRecommendationId,
    describeBotRecommendationResponse_botId,
    describeBotRecommendationResponse_botRecommendationId,
    describeBotRecommendationResponse_botRecommendationResults,
    describeBotRecommendationResponse_botRecommendationStatus,
    describeBotRecommendationResponse_botVersion,
    describeBotRecommendationResponse_creationDateTime,
    describeBotRecommendationResponse_encryptionSetting,
    describeBotRecommendationResponse_failureReasons,
    describeBotRecommendationResponse_lastUpdatedDateTime,
    describeBotRecommendationResponse_localeId,
    describeBotRecommendationResponse_transcriptSourceSetting,
    describeBotRecommendationResponse_httpStatus,

    -- ** DescribeBotVersion
    describeBotVersion_botId,
    describeBotVersion_botVersion,
    describeBotVersionResponse_botId,
    describeBotVersionResponse_botName,
    describeBotVersionResponse_botStatus,
    describeBotVersionResponse_botVersion,
    describeBotVersionResponse_creationDateTime,
    describeBotVersionResponse_dataPrivacy,
    describeBotVersionResponse_description,
    describeBotVersionResponse_failureReasons,
    describeBotVersionResponse_idleSessionTTLInSeconds,
    describeBotVersionResponse_roleArn,
    describeBotVersionResponse_httpStatus,

    -- ** DescribeCustomVocabularyMetadata
    describeCustomVocabularyMetadata_botId,
    describeCustomVocabularyMetadata_botVersion,
    describeCustomVocabularyMetadata_localeId,
    describeCustomVocabularyMetadataResponse_botId,
    describeCustomVocabularyMetadataResponse_botVersion,
    describeCustomVocabularyMetadataResponse_creationDateTime,
    describeCustomVocabularyMetadataResponse_customVocabularyStatus,
    describeCustomVocabularyMetadataResponse_lastUpdatedDateTime,
    describeCustomVocabularyMetadataResponse_localeId,
    describeCustomVocabularyMetadataResponse_httpStatus,

    -- ** DescribeExport
    describeExport_exportId,
    describeExportResponse_creationDateTime,
    describeExportResponse_downloadUrl,
    describeExportResponse_exportId,
    describeExportResponse_exportStatus,
    describeExportResponse_failureReasons,
    describeExportResponse_fileFormat,
    describeExportResponse_lastUpdatedDateTime,
    describeExportResponse_resourceSpecification,
    describeExportResponse_httpStatus,

    -- ** DescribeImport
    describeImport_importId,
    describeImportResponse_creationDateTime,
    describeImportResponse_failureReasons,
    describeImportResponse_importId,
    describeImportResponse_importStatus,
    describeImportResponse_importedResourceId,
    describeImportResponse_importedResourceName,
    describeImportResponse_lastUpdatedDateTime,
    describeImportResponse_mergeStrategy,
    describeImportResponse_resourceSpecification,
    describeImportResponse_httpStatus,

    -- ** DescribeIntent
    describeIntent_intentId,
    describeIntent_botId,
    describeIntent_botVersion,
    describeIntent_localeId,
    describeIntentResponse_botId,
    describeIntentResponse_botVersion,
    describeIntentResponse_creationDateTime,
    describeIntentResponse_description,
    describeIntentResponse_dialogCodeHook,
    describeIntentResponse_fulfillmentCodeHook,
    describeIntentResponse_initialResponseSetting,
    describeIntentResponse_inputContexts,
    describeIntentResponse_intentClosingSetting,
    describeIntentResponse_intentConfirmationSetting,
    describeIntentResponse_intentId,
    describeIntentResponse_intentName,
    describeIntentResponse_kendraConfiguration,
    describeIntentResponse_lastUpdatedDateTime,
    describeIntentResponse_localeId,
    describeIntentResponse_outputContexts,
    describeIntentResponse_parentIntentSignature,
    describeIntentResponse_sampleUtterances,
    describeIntentResponse_slotPriorities,
    describeIntentResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_resourceArn,
    describeResourcePolicyResponse_revisionId,
    describeResourcePolicyResponse_httpStatus,

    -- ** DescribeSlot
    describeSlot_slotId,
    describeSlot_botId,
    describeSlot_botVersion,
    describeSlot_localeId,
    describeSlot_intentId,
    describeSlotResponse_botId,
    describeSlotResponse_botVersion,
    describeSlotResponse_creationDateTime,
    describeSlotResponse_description,
    describeSlotResponse_intentId,
    describeSlotResponse_lastUpdatedDateTime,
    describeSlotResponse_localeId,
    describeSlotResponse_multipleValuesSetting,
    describeSlotResponse_obfuscationSetting,
    describeSlotResponse_slotId,
    describeSlotResponse_slotName,
    describeSlotResponse_slotTypeId,
    describeSlotResponse_subSlotSetting,
    describeSlotResponse_valueElicitationSetting,
    describeSlotResponse_httpStatus,

    -- ** DescribeSlotType
    describeSlotType_slotTypeId,
    describeSlotType_botId,
    describeSlotType_botVersion,
    describeSlotType_localeId,
    describeSlotTypeResponse_botId,
    describeSlotTypeResponse_botVersion,
    describeSlotTypeResponse_compositeSlotTypeSetting,
    describeSlotTypeResponse_creationDateTime,
    describeSlotTypeResponse_description,
    describeSlotTypeResponse_externalSourceSetting,
    describeSlotTypeResponse_lastUpdatedDateTime,
    describeSlotTypeResponse_localeId,
    describeSlotTypeResponse_parentSlotTypeSignature,
    describeSlotTypeResponse_slotTypeId,
    describeSlotTypeResponse_slotTypeName,
    describeSlotTypeResponse_slotTypeValues,
    describeSlotTypeResponse_valueSelectionSetting,
    describeSlotTypeResponse_httpStatus,

    -- ** ListAggregatedUtterances
    listAggregatedUtterances_botAliasId,
    listAggregatedUtterances_botVersion,
    listAggregatedUtterances_filters,
    listAggregatedUtterances_maxResults,
    listAggregatedUtterances_nextToken,
    listAggregatedUtterances_sortBy,
    listAggregatedUtterances_botId,
    listAggregatedUtterances_localeId,
    listAggregatedUtterances_aggregationDuration,
    listAggregatedUtterancesResponse_aggregatedUtterancesSummaries,
    listAggregatedUtterancesResponse_aggregationDuration,
    listAggregatedUtterancesResponse_aggregationLastRefreshedDateTime,
    listAggregatedUtterancesResponse_aggregationWindowEndTime,
    listAggregatedUtterancesResponse_aggregationWindowStartTime,
    listAggregatedUtterancesResponse_botAliasId,
    listAggregatedUtterancesResponse_botId,
    listAggregatedUtterancesResponse_botVersion,
    listAggregatedUtterancesResponse_localeId,
    listAggregatedUtterancesResponse_nextToken,
    listAggregatedUtterancesResponse_httpStatus,

    -- ** ListBotAliases
    listBotAliases_maxResults,
    listBotAliases_nextToken,
    listBotAliases_botId,
    listBotAliasesResponse_botAliasSummaries,
    listBotAliasesResponse_botId,
    listBotAliasesResponse_nextToken,
    listBotAliasesResponse_httpStatus,

    -- ** ListBotLocales
    listBotLocales_filters,
    listBotLocales_maxResults,
    listBotLocales_nextToken,
    listBotLocales_sortBy,
    listBotLocales_botId,
    listBotLocales_botVersion,
    listBotLocalesResponse_botId,
    listBotLocalesResponse_botLocaleSummaries,
    listBotLocalesResponse_botVersion,
    listBotLocalesResponse_nextToken,
    listBotLocalesResponse_httpStatus,

    -- ** ListBotRecommendations
    listBotRecommendations_maxResults,
    listBotRecommendations_nextToken,
    listBotRecommendations_botId,
    listBotRecommendations_botVersion,
    listBotRecommendations_localeId,
    listBotRecommendationsResponse_botId,
    listBotRecommendationsResponse_botRecommendationSummaries,
    listBotRecommendationsResponse_botVersion,
    listBotRecommendationsResponse_localeId,
    listBotRecommendationsResponse_nextToken,
    listBotRecommendationsResponse_httpStatus,

    -- ** ListBotVersions
    listBotVersions_maxResults,
    listBotVersions_nextToken,
    listBotVersions_sortBy,
    listBotVersions_botId,
    listBotVersionsResponse_botId,
    listBotVersionsResponse_botVersionSummaries,
    listBotVersionsResponse_nextToken,
    listBotVersionsResponse_httpStatus,

    -- ** ListBots
    listBots_filters,
    listBots_maxResults,
    listBots_nextToken,
    listBots_sortBy,
    listBotsResponse_botSummaries,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,

    -- ** ListBuiltInIntents
    listBuiltInIntents_maxResults,
    listBuiltInIntents_nextToken,
    listBuiltInIntents_sortBy,
    listBuiltInIntents_localeId,
    listBuiltInIntentsResponse_builtInIntentSummaries,
    listBuiltInIntentsResponse_localeId,
    listBuiltInIntentsResponse_nextToken,
    listBuiltInIntentsResponse_httpStatus,

    -- ** ListBuiltInSlotTypes
    listBuiltInSlotTypes_maxResults,
    listBuiltInSlotTypes_nextToken,
    listBuiltInSlotTypes_sortBy,
    listBuiltInSlotTypes_localeId,
    listBuiltInSlotTypesResponse_builtInSlotTypeSummaries,
    listBuiltInSlotTypesResponse_localeId,
    listBuiltInSlotTypesResponse_nextToken,
    listBuiltInSlotTypesResponse_httpStatus,

    -- ** ListCustomVocabularyItems
    listCustomVocabularyItems_maxResults,
    listCustomVocabularyItems_nextToken,
    listCustomVocabularyItems_botId,
    listCustomVocabularyItems_botVersion,
    listCustomVocabularyItems_localeId,
    listCustomVocabularyItemsResponse_botId,
    listCustomVocabularyItemsResponse_botVersion,
    listCustomVocabularyItemsResponse_customVocabularyItems,
    listCustomVocabularyItemsResponse_localeId,
    listCustomVocabularyItemsResponse_nextToken,
    listCustomVocabularyItemsResponse_httpStatus,

    -- ** ListExports
    listExports_botId,
    listExports_botVersion,
    listExports_filters,
    listExports_localeId,
    listExports_maxResults,
    listExports_nextToken,
    listExports_sortBy,
    listExportsResponse_botId,
    listExportsResponse_botVersion,
    listExportsResponse_exportSummaries,
    listExportsResponse_localeId,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** ListImports
    listImports_botId,
    listImports_botVersion,
    listImports_filters,
    listImports_localeId,
    listImports_maxResults,
    listImports_nextToken,
    listImports_sortBy,
    listImportsResponse_botId,
    listImportsResponse_botVersion,
    listImportsResponse_importSummaries,
    listImportsResponse_localeId,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListIntents
    listIntents_filters,
    listIntents_maxResults,
    listIntents_nextToken,
    listIntents_sortBy,
    listIntents_botId,
    listIntents_botVersion,
    listIntents_localeId,
    listIntentsResponse_botId,
    listIntentsResponse_botVersion,
    listIntentsResponse_intentSummaries,
    listIntentsResponse_localeId,
    listIntentsResponse_nextToken,
    listIntentsResponse_httpStatus,

    -- ** ListRecommendedIntents
    listRecommendedIntents_maxResults,
    listRecommendedIntents_nextToken,
    listRecommendedIntents_botId,
    listRecommendedIntents_botVersion,
    listRecommendedIntents_localeId,
    listRecommendedIntents_botRecommendationId,
    listRecommendedIntentsResponse_botId,
    listRecommendedIntentsResponse_botRecommendationId,
    listRecommendedIntentsResponse_botVersion,
    listRecommendedIntentsResponse_localeId,
    listRecommendedIntentsResponse_nextToken,
    listRecommendedIntentsResponse_summaryList,
    listRecommendedIntentsResponse_httpStatus,

    -- ** ListSlotTypes
    listSlotTypes_filters,
    listSlotTypes_maxResults,
    listSlotTypes_nextToken,
    listSlotTypes_sortBy,
    listSlotTypes_botId,
    listSlotTypes_botVersion,
    listSlotTypes_localeId,
    listSlotTypesResponse_botId,
    listSlotTypesResponse_botVersion,
    listSlotTypesResponse_localeId,
    listSlotTypesResponse_nextToken,
    listSlotTypesResponse_slotTypeSummaries,
    listSlotTypesResponse_httpStatus,

    -- ** ListSlots
    listSlots_filters,
    listSlots_maxResults,
    listSlots_nextToken,
    listSlots_sortBy,
    listSlots_botId,
    listSlots_botVersion,
    listSlots_localeId,
    listSlots_intentId,
    listSlotsResponse_botId,
    listSlotsResponse_botVersion,
    listSlotsResponse_intentId,
    listSlotsResponse_localeId,
    listSlotsResponse_nextToken,
    listSlotsResponse_slotSummaries,
    listSlotsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SearchAssociatedTranscripts
    searchAssociatedTranscripts_maxResults,
    searchAssociatedTranscripts_nextIndex,
    searchAssociatedTranscripts_searchOrder,
    searchAssociatedTranscripts_botId,
    searchAssociatedTranscripts_botVersion,
    searchAssociatedTranscripts_localeId,
    searchAssociatedTranscripts_botRecommendationId,
    searchAssociatedTranscripts_filters,
    searchAssociatedTranscriptsResponse_associatedTranscripts,
    searchAssociatedTranscriptsResponse_botId,
    searchAssociatedTranscriptsResponse_botRecommendationId,
    searchAssociatedTranscriptsResponse_botVersion,
    searchAssociatedTranscriptsResponse_localeId,
    searchAssociatedTranscriptsResponse_nextIndex,
    searchAssociatedTranscriptsResponse_totalResults,
    searchAssociatedTranscriptsResponse_httpStatus,

    -- ** StartBotRecommendation
    startBotRecommendation_encryptionSetting,
    startBotRecommendation_botId,
    startBotRecommendation_botVersion,
    startBotRecommendation_localeId,
    startBotRecommendation_transcriptSourceSetting,
    startBotRecommendationResponse_botId,
    startBotRecommendationResponse_botRecommendationId,
    startBotRecommendationResponse_botRecommendationStatus,
    startBotRecommendationResponse_botVersion,
    startBotRecommendationResponse_creationDateTime,
    startBotRecommendationResponse_encryptionSetting,
    startBotRecommendationResponse_localeId,
    startBotRecommendationResponse_transcriptSourceSetting,
    startBotRecommendationResponse_httpStatus,

    -- ** StartImport
    startImport_filePassword,
    startImport_importId,
    startImport_resourceSpecification,
    startImport_mergeStrategy,
    startImportResponse_creationDateTime,
    startImportResponse_importId,
    startImportResponse_importStatus,
    startImportResponse_mergeStrategy,
    startImportResponse_resourceSpecification,
    startImportResponse_httpStatus,

    -- ** StopBotRecommendation
    stopBotRecommendation_botId,
    stopBotRecommendation_botVersion,
    stopBotRecommendation_localeId,
    stopBotRecommendation_botRecommendationId,
    stopBotRecommendationResponse_botId,
    stopBotRecommendationResponse_botRecommendationId,
    stopBotRecommendationResponse_botRecommendationStatus,
    stopBotRecommendationResponse_botVersion,
    stopBotRecommendationResponse_localeId,
    stopBotRecommendationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateBot
    updateBot_description,
    updateBot_botId,
    updateBot_botName,
    updateBot_roleArn,
    updateBot_dataPrivacy,
    updateBot_idleSessionTTLInSeconds,
    updateBotResponse_botId,
    updateBotResponse_botName,
    updateBotResponse_botStatus,
    updateBotResponse_creationDateTime,
    updateBotResponse_dataPrivacy,
    updateBotResponse_description,
    updateBotResponse_idleSessionTTLInSeconds,
    updateBotResponse_lastUpdatedDateTime,
    updateBotResponse_roleArn,
    updateBotResponse_httpStatus,

    -- ** UpdateBotAlias
    updateBotAlias_botAliasLocaleSettings,
    updateBotAlias_botVersion,
    updateBotAlias_conversationLogSettings,
    updateBotAlias_description,
    updateBotAlias_sentimentAnalysisSettings,
    updateBotAlias_botAliasId,
    updateBotAlias_botAliasName,
    updateBotAlias_botId,
    updateBotAliasResponse_botAliasId,
    updateBotAliasResponse_botAliasLocaleSettings,
    updateBotAliasResponse_botAliasName,
    updateBotAliasResponse_botAliasStatus,
    updateBotAliasResponse_botId,
    updateBotAliasResponse_botVersion,
    updateBotAliasResponse_conversationLogSettings,
    updateBotAliasResponse_creationDateTime,
    updateBotAliasResponse_description,
    updateBotAliasResponse_lastUpdatedDateTime,
    updateBotAliasResponse_sentimentAnalysisSettings,
    updateBotAliasResponse_httpStatus,

    -- ** UpdateBotLocale
    updateBotLocale_description,
    updateBotLocale_voiceSettings,
    updateBotLocale_botId,
    updateBotLocale_botVersion,
    updateBotLocale_localeId,
    updateBotLocale_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_botId,
    updateBotLocaleResponse_botLocaleStatus,
    updateBotLocaleResponse_botVersion,
    updateBotLocaleResponse_creationDateTime,
    updateBotLocaleResponse_description,
    updateBotLocaleResponse_failureReasons,
    updateBotLocaleResponse_lastUpdatedDateTime,
    updateBotLocaleResponse_localeId,
    updateBotLocaleResponse_localeName,
    updateBotLocaleResponse_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_recommendedActions,
    updateBotLocaleResponse_voiceSettings,
    updateBotLocaleResponse_httpStatus,

    -- ** UpdateBotRecommendation
    updateBotRecommendation_botId,
    updateBotRecommendation_botVersion,
    updateBotRecommendation_localeId,
    updateBotRecommendation_botRecommendationId,
    updateBotRecommendation_encryptionSetting,
    updateBotRecommendationResponse_botId,
    updateBotRecommendationResponse_botRecommendationId,
    updateBotRecommendationResponse_botRecommendationStatus,
    updateBotRecommendationResponse_botVersion,
    updateBotRecommendationResponse_creationDateTime,
    updateBotRecommendationResponse_encryptionSetting,
    updateBotRecommendationResponse_lastUpdatedDateTime,
    updateBotRecommendationResponse_localeId,
    updateBotRecommendationResponse_transcriptSourceSetting,
    updateBotRecommendationResponse_httpStatus,

    -- ** UpdateExport
    updateExport_filePassword,
    updateExport_exportId,
    updateExportResponse_creationDateTime,
    updateExportResponse_exportId,
    updateExportResponse_exportStatus,
    updateExportResponse_fileFormat,
    updateExportResponse_lastUpdatedDateTime,
    updateExportResponse_resourceSpecification,
    updateExportResponse_httpStatus,

    -- ** UpdateIntent
    updateIntent_description,
    updateIntent_dialogCodeHook,
    updateIntent_fulfillmentCodeHook,
    updateIntent_initialResponseSetting,
    updateIntent_inputContexts,
    updateIntent_intentClosingSetting,
    updateIntent_intentConfirmationSetting,
    updateIntent_kendraConfiguration,
    updateIntent_outputContexts,
    updateIntent_parentIntentSignature,
    updateIntent_sampleUtterances,
    updateIntent_slotPriorities,
    updateIntent_intentId,
    updateIntent_intentName,
    updateIntent_botId,
    updateIntent_botVersion,
    updateIntent_localeId,
    updateIntentResponse_botId,
    updateIntentResponse_botVersion,
    updateIntentResponse_creationDateTime,
    updateIntentResponse_description,
    updateIntentResponse_dialogCodeHook,
    updateIntentResponse_fulfillmentCodeHook,
    updateIntentResponse_initialResponseSetting,
    updateIntentResponse_inputContexts,
    updateIntentResponse_intentClosingSetting,
    updateIntentResponse_intentConfirmationSetting,
    updateIntentResponse_intentId,
    updateIntentResponse_intentName,
    updateIntentResponse_kendraConfiguration,
    updateIntentResponse_lastUpdatedDateTime,
    updateIntentResponse_localeId,
    updateIntentResponse_outputContexts,
    updateIntentResponse_parentIntentSignature,
    updateIntentResponse_sampleUtterances,
    updateIntentResponse_slotPriorities,
    updateIntentResponse_httpStatus,

    -- ** UpdateResourcePolicy
    updateResourcePolicy_expectedRevisionId,
    updateResourcePolicy_resourceArn,
    updateResourcePolicy_policy,
    updateResourcePolicyResponse_resourceArn,
    updateResourcePolicyResponse_revisionId,
    updateResourcePolicyResponse_httpStatus,

    -- ** UpdateSlot
    updateSlot_description,
    updateSlot_multipleValuesSetting,
    updateSlot_obfuscationSetting,
    updateSlot_slotTypeId,
    updateSlot_subSlotSetting,
    updateSlot_slotId,
    updateSlot_slotName,
    updateSlot_valueElicitationSetting,
    updateSlot_botId,
    updateSlot_botVersion,
    updateSlot_localeId,
    updateSlot_intentId,
    updateSlotResponse_botId,
    updateSlotResponse_botVersion,
    updateSlotResponse_creationDateTime,
    updateSlotResponse_description,
    updateSlotResponse_intentId,
    updateSlotResponse_lastUpdatedDateTime,
    updateSlotResponse_localeId,
    updateSlotResponse_multipleValuesSetting,
    updateSlotResponse_obfuscationSetting,
    updateSlotResponse_slotId,
    updateSlotResponse_slotName,
    updateSlotResponse_slotTypeId,
    updateSlotResponse_subSlotSetting,
    updateSlotResponse_valueElicitationSetting,
    updateSlotResponse_httpStatus,

    -- ** UpdateSlotType
    updateSlotType_compositeSlotTypeSetting,
    updateSlotType_description,
    updateSlotType_externalSourceSetting,
    updateSlotType_parentSlotTypeSignature,
    updateSlotType_slotTypeValues,
    updateSlotType_valueSelectionSetting,
    updateSlotType_slotTypeId,
    updateSlotType_slotTypeName,
    updateSlotType_botId,
    updateSlotType_botVersion,
    updateSlotType_localeId,
    updateSlotTypeResponse_botId,
    updateSlotTypeResponse_botVersion,
    updateSlotTypeResponse_compositeSlotTypeSetting,
    updateSlotTypeResponse_creationDateTime,
    updateSlotTypeResponse_description,
    updateSlotTypeResponse_externalSourceSetting,
    updateSlotTypeResponse_lastUpdatedDateTime,
    updateSlotTypeResponse_localeId,
    updateSlotTypeResponse_parentSlotTypeSignature,
    updateSlotTypeResponse_slotTypeId,
    updateSlotTypeResponse_slotTypeName,
    updateSlotTypeResponse_slotTypeValues,
    updateSlotTypeResponse_valueSelectionSetting,
    updateSlotTypeResponse_httpStatus,

    -- * Types

    -- ** AdvancedRecognitionSetting
    advancedRecognitionSetting_audioRecognitionStrategy,

    -- ** AggregatedUtterancesFilter
    aggregatedUtterancesFilter_name,
    aggregatedUtterancesFilter_values,
    aggregatedUtterancesFilter_operator,

    -- ** AggregatedUtterancesSortBy
    aggregatedUtterancesSortBy_attribute,
    aggregatedUtterancesSortBy_order,

    -- ** AggregatedUtterancesSummary
    aggregatedUtterancesSummary_containsDataFromDeletedResources,
    aggregatedUtterancesSummary_hitCount,
    aggregatedUtterancesSummary_missedCount,
    aggregatedUtterancesSummary_utterance,
    aggregatedUtterancesSummary_utteranceFirstRecordedInAggregationDuration,
    aggregatedUtterancesSummary_utteranceLastRecordedInAggregationDuration,

    -- ** AllowedInputTypes
    allowedInputTypes_allowAudioInput,
    allowedInputTypes_allowDTMFInput,

    -- ** AssociatedTranscript
    associatedTranscript_transcript,

    -- ** AssociatedTranscriptFilter
    associatedTranscriptFilter_name,
    associatedTranscriptFilter_values,

    -- ** AudioAndDTMFInputSpecification
    audioAndDTMFInputSpecification_audioSpecification,
    audioAndDTMFInputSpecification_dtmfSpecification,
    audioAndDTMFInputSpecification_startTimeoutMs,

    -- ** AudioLogDestination
    audioLogDestination_s3Bucket,

    -- ** AudioLogSetting
    audioLogSetting_enabled,
    audioLogSetting_destination,

    -- ** AudioSpecification
    audioSpecification_maxLengthMs,
    audioSpecification_endTimeoutMs,

    -- ** BotAliasHistoryEvent
    botAliasHistoryEvent_botVersion,
    botAliasHistoryEvent_endDate,
    botAliasHistoryEvent_startDate,

    -- ** BotAliasLocaleSettings
    botAliasLocaleSettings_codeHookSpecification,
    botAliasLocaleSettings_enabled,

    -- ** BotAliasSummary
    botAliasSummary_botAliasId,
    botAliasSummary_botAliasName,
    botAliasSummary_botAliasStatus,
    botAliasSummary_botVersion,
    botAliasSummary_creationDateTime,
    botAliasSummary_description,
    botAliasSummary_lastUpdatedDateTime,

    -- ** BotExportSpecification
    botExportSpecification_botId,
    botExportSpecification_botVersion,

    -- ** BotFilter
    botFilter_name,
    botFilter_values,
    botFilter_operator,

    -- ** BotImportSpecification
    botImportSpecification_botTags,
    botImportSpecification_idleSessionTTLInSeconds,
    botImportSpecification_testBotAliasTags,
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
    botLocaleSummary_botLocaleStatus,
    botLocaleSummary_description,
    botLocaleSummary_lastBuildSubmittedDateTime,
    botLocaleSummary_lastUpdatedDateTime,
    botLocaleSummary_localeId,
    botLocaleSummary_localeName,

    -- ** BotRecommendationResultStatistics
    botRecommendationResultStatistics_intents,
    botRecommendationResultStatistics_slotTypes,

    -- ** BotRecommendationResults
    botRecommendationResults_associatedTranscriptsUrl,
    botRecommendationResults_botLocaleExportUrl,
    botRecommendationResults_statistics,

    -- ** BotRecommendationSummary
    botRecommendationSummary_creationDateTime,
    botRecommendationSummary_lastUpdatedDateTime,
    botRecommendationSummary_botRecommendationStatus,
    botRecommendationSummary_botRecommendationId,

    -- ** BotSortBy
    botSortBy_attribute,
    botSortBy_order,

    -- ** BotSummary
    botSummary_botId,
    botSummary_botName,
    botSummary_botStatus,
    botSummary_description,
    botSummary_lastUpdatedDateTime,
    botSummary_latestBotVersion,

    -- ** BotVersionLocaleDetails
    botVersionLocaleDetails_sourceBotVersion,

    -- ** BotVersionSortBy
    botVersionSortBy_attribute,
    botVersionSortBy_order,

    -- ** BotVersionSummary
    botVersionSummary_botName,
    botVersionSummary_botStatus,
    botVersionSummary_botVersion,
    botVersionSummary_creationDateTime,
    botVersionSummary_description,

    -- ** BuiltInIntentSortBy
    builtInIntentSortBy_attribute,
    builtInIntentSortBy_order,

    -- ** BuiltInIntentSummary
    builtInIntentSummary_description,
    builtInIntentSummary_intentSignature,

    -- ** BuiltInSlotTypeSortBy
    builtInSlotTypeSortBy_attribute,
    builtInSlotTypeSortBy_order,

    -- ** BuiltInSlotTypeSummary
    builtInSlotTypeSummary_description,
    builtInSlotTypeSummary_slotTypeSignature,

    -- ** Button
    button_text,
    button_value,

    -- ** CloudWatchLogGroupLogDestination
    cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn,
    cloudWatchLogGroupLogDestination_logPrefix,

    -- ** CodeHookSpecification
    codeHookSpecification_lambdaCodeHook,

    -- ** CompositeSlotTypeSetting
    compositeSlotTypeSetting_subSlots,

    -- ** Condition
    condition_expressionString,

    -- ** ConditionalBranch
    conditionalBranch_response,
    conditionalBranch_name,
    conditionalBranch_condition,
    conditionalBranch_nextStep,

    -- ** ConditionalSpecification
    conditionalSpecification_active,
    conditionalSpecification_conditionalBranches,
    conditionalSpecification_defaultBranch,

    -- ** ConversationLogSettings
    conversationLogSettings_audioLogSettings,
    conversationLogSettings_textLogSettings,

    -- ** CustomPayload
    customPayload_value,

    -- ** CustomVocabularyEntryId
    customVocabularyEntryId_itemId,

    -- ** CustomVocabularyExportSpecification
    customVocabularyExportSpecification_botId,
    customVocabularyExportSpecification_botVersion,
    customVocabularyExportSpecification_localeId,

    -- ** CustomVocabularyImportSpecification
    customVocabularyImportSpecification_botId,
    customVocabularyImportSpecification_botVersion,
    customVocabularyImportSpecification_localeId,

    -- ** CustomVocabularyItem
    customVocabularyItem_displayAs,
    customVocabularyItem_weight,
    customVocabularyItem_itemId,
    customVocabularyItem_phrase,

    -- ** DTMFSpecification
    dTMFSpecification_maxLength,
    dTMFSpecification_endTimeoutMs,
    dTMFSpecification_deletionCharacter,
    dTMFSpecification_endCharacter,

    -- ** DataPrivacy
    dataPrivacy_childDirected,

    -- ** DateRangeFilter
    dateRangeFilter_startDateTime,
    dateRangeFilter_endDateTime,

    -- ** DefaultConditionalBranch
    defaultConditionalBranch_nextStep,
    defaultConditionalBranch_response,

    -- ** DialogAction
    dialogAction_slotToElicit,
    dialogAction_suppressNextMessage,
    dialogAction_type,

    -- ** DialogCodeHookInvocationSetting
    dialogCodeHookInvocationSetting_invocationLabel,
    dialogCodeHookInvocationSetting_enableCodeHookInvocation,
    dialogCodeHookInvocationSetting_active,
    dialogCodeHookInvocationSetting_postCodeHookSpecification,

    -- ** DialogCodeHookSettings
    dialogCodeHookSettings_enabled,

    -- ** DialogState
    dialogState_dialogAction,
    dialogState_intent,
    dialogState_sessionAttributes,

    -- ** ElicitationCodeHookInvocationSetting
    elicitationCodeHookInvocationSetting_invocationLabel,
    elicitationCodeHookInvocationSetting_enableCodeHookInvocation,

    -- ** EncryptionSetting
    encryptionSetting_associatedTranscriptsPassword,
    encryptionSetting_botLocaleExportPassword,
    encryptionSetting_kmsKeyArn,

    -- ** ExportFilter
    exportFilter_name,
    exportFilter_values,
    exportFilter_operator,

    -- ** ExportResourceSpecification
    exportResourceSpecification_botExportSpecification,
    exportResourceSpecification_botLocaleExportSpecification,
    exportResourceSpecification_customVocabularyExportSpecification,

    -- ** ExportSortBy
    exportSortBy_attribute,
    exportSortBy_order,

    -- ** ExportSummary
    exportSummary_creationDateTime,
    exportSummary_exportId,
    exportSummary_exportStatus,
    exportSummary_fileFormat,
    exportSummary_lastUpdatedDateTime,
    exportSummary_resourceSpecification,

    -- ** ExternalSourceSetting
    externalSourceSetting_grammarSlotTypeSetting,

    -- ** FailedCustomVocabularyItem
    failedCustomVocabularyItem_errorCode,
    failedCustomVocabularyItem_errorMessage,
    failedCustomVocabularyItem_itemId,

    -- ** FulfillmentCodeHookSettings
    fulfillmentCodeHookSettings_active,
    fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification,
    fulfillmentCodeHookSettings_postFulfillmentStatusSpecification,
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

    -- ** GrammarSlotTypeSetting
    grammarSlotTypeSetting_source,

    -- ** GrammarSlotTypeSource
    grammarSlotTypeSource_kmsKeyArn,
    grammarSlotTypeSource_s3BucketName,
    grammarSlotTypeSource_s3ObjectKey,

    -- ** ImageResponseCard
    imageResponseCard_buttons,
    imageResponseCard_imageUrl,
    imageResponseCard_subtitle,
    imageResponseCard_title,

    -- ** ImportFilter
    importFilter_name,
    importFilter_values,
    importFilter_operator,

    -- ** ImportResourceSpecification
    importResourceSpecification_botImportSpecification,
    importResourceSpecification_botLocaleImportSpecification,
    importResourceSpecification_customVocabularyImportSpecification,

    -- ** ImportSortBy
    importSortBy_attribute,
    importSortBy_order,

    -- ** ImportSummary
    importSummary_creationDateTime,
    importSummary_importId,
    importSummary_importStatus,
    importSummary_importedResourceId,
    importSummary_importedResourceName,
    importSummary_importedResourceType,
    importSummary_lastUpdatedDateTime,
    importSummary_mergeStrategy,

    -- ** InitialResponseSetting
    initialResponseSetting_codeHook,
    initialResponseSetting_conditional,
    initialResponseSetting_initialResponse,
    initialResponseSetting_nextStep,

    -- ** InputContext
    inputContext_name,

    -- ** IntentClosingSetting
    intentClosingSetting_active,
    intentClosingSetting_closingResponse,
    intentClosingSetting_conditional,
    intentClosingSetting_nextStep,

    -- ** IntentConfirmationSetting
    intentConfirmationSetting_active,
    intentConfirmationSetting_codeHook,
    intentConfirmationSetting_confirmationConditional,
    intentConfirmationSetting_confirmationNextStep,
    intentConfirmationSetting_confirmationResponse,
    intentConfirmationSetting_declinationConditional,
    intentConfirmationSetting_declinationNextStep,
    intentConfirmationSetting_declinationResponse,
    intentConfirmationSetting_elicitationCodeHook,
    intentConfirmationSetting_failureConditional,
    intentConfirmationSetting_failureNextStep,
    intentConfirmationSetting_failureResponse,
    intentConfirmationSetting_promptSpecification,

    -- ** IntentFilter
    intentFilter_name,
    intentFilter_values,
    intentFilter_operator,

    -- ** IntentOverride
    intentOverride_name,
    intentOverride_slots,

    -- ** IntentSortBy
    intentSortBy_attribute,
    intentSortBy_order,

    -- ** IntentStatistics
    intentStatistics_discoveredIntentCount,

    -- ** IntentSummary
    intentSummary_description,
    intentSummary_inputContexts,
    intentSummary_intentId,
    intentSummary_intentName,
    intentSummary_lastUpdatedDateTime,
    intentSummary_outputContexts,
    intentSummary_parentIntentSignature,

    -- ** KendraConfiguration
    kendraConfiguration_queryFilterString,
    kendraConfiguration_queryFilterStringEnabled,
    kendraConfiguration_kendraIndex,

    -- ** LambdaCodeHook
    lambdaCodeHook_lambdaARN,
    lambdaCodeHook_codeHookInterfaceVersion,

    -- ** LexTranscriptFilter
    lexTranscriptFilter_dateRangeFilter,

    -- ** Message
    message_customPayload,
    message_imageResponseCard,
    message_plainTextMessage,
    message_ssmlMessage,

    -- ** MessageGroup
    messageGroup_variations,
    messageGroup_message,

    -- ** MultipleValuesSetting
    multipleValuesSetting_allowMultipleValues,

    -- ** NewCustomVocabularyItem
    newCustomVocabularyItem_displayAs,
    newCustomVocabularyItem_weight,
    newCustomVocabularyItem_phrase,

    -- ** ObfuscationSetting
    obfuscationSetting_obfuscationSettingType,

    -- ** OutputContext
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- ** PathFormat
    pathFormat_objectPrefixes,

    -- ** PlainTextMessage
    plainTextMessage_value,

    -- ** PostDialogCodeHookInvocationSpecification
    postDialogCodeHookInvocationSpecification_failureConditional,
    postDialogCodeHookInvocationSpecification_failureNextStep,
    postDialogCodeHookInvocationSpecification_failureResponse,
    postDialogCodeHookInvocationSpecification_successConditional,
    postDialogCodeHookInvocationSpecification_successNextStep,
    postDialogCodeHookInvocationSpecification_successResponse,
    postDialogCodeHookInvocationSpecification_timeoutConditional,
    postDialogCodeHookInvocationSpecification_timeoutNextStep,
    postDialogCodeHookInvocationSpecification_timeoutResponse,

    -- ** PostFulfillmentStatusSpecification
    postFulfillmentStatusSpecification_failureConditional,
    postFulfillmentStatusSpecification_failureNextStep,
    postFulfillmentStatusSpecification_failureResponse,
    postFulfillmentStatusSpecification_successConditional,
    postFulfillmentStatusSpecification_successNextStep,
    postFulfillmentStatusSpecification_successResponse,
    postFulfillmentStatusSpecification_timeoutConditional,
    postFulfillmentStatusSpecification_timeoutNextStep,
    postFulfillmentStatusSpecification_timeoutResponse,

    -- ** Principal
    principal_arn,
    principal_service,

    -- ** PromptAttemptSpecification
    promptAttemptSpecification_allowInterrupt,
    promptAttemptSpecification_audioAndDTMFInputSpecification,
    promptAttemptSpecification_textInputSpecification,
    promptAttemptSpecification_allowedInputTypes,

    -- ** PromptSpecification
    promptSpecification_allowInterrupt,
    promptSpecification_messageSelectionStrategy,
    promptSpecification_promptAttemptsSpecification,
    promptSpecification_messageGroups,
    promptSpecification_maxRetries,

    -- ** RecommendedIntentSummary
    recommendedIntentSummary_intentId,
    recommendedIntentSummary_intentName,
    recommendedIntentSummary_sampleUtterancesCount,

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

    -- ** S3BucketTranscriptSource
    s3BucketTranscriptSource_kmsKeyArn,
    s3BucketTranscriptSource_pathFormat,
    s3BucketTranscriptSource_transcriptFilter,
    s3BucketTranscriptSource_s3BucketName,
    s3BucketTranscriptSource_transcriptFormat,

    -- ** SSMLMessage
    sSMLMessage_value,

    -- ** SampleUtterance
    sampleUtterance_utterance,

    -- ** SampleValue
    sampleValue_value,

    -- ** SentimentAnalysisSettings
    sentimentAnalysisSettings_detectSentiment,

    -- ** SlotCaptureSetting
    slotCaptureSetting_captureConditional,
    slotCaptureSetting_captureNextStep,
    slotCaptureSetting_captureResponse,
    slotCaptureSetting_codeHook,
    slotCaptureSetting_elicitationCodeHook,
    slotCaptureSetting_failureConditional,
    slotCaptureSetting_failureNextStep,
    slotCaptureSetting_failureResponse,

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
    slotSummary_description,
    slotSummary_lastUpdatedDateTime,
    slotSummary_slotConstraint,
    slotSummary_slotId,
    slotSummary_slotName,
    slotSummary_slotTypeId,
    slotSummary_valueElicitationPromptSpecification,

    -- ** SlotTypeFilter
    slotTypeFilter_name,
    slotTypeFilter_values,
    slotTypeFilter_operator,

    -- ** SlotTypeSortBy
    slotTypeSortBy_attribute,
    slotTypeSortBy_order,

    -- ** SlotTypeStatistics
    slotTypeStatistics_discoveredSlotTypeCount,

    -- ** SlotTypeSummary
    slotTypeSummary_description,
    slotTypeSummary_lastUpdatedDateTime,
    slotTypeSummary_parentSlotTypeSignature,
    slotTypeSummary_slotTypeCategory,
    slotTypeSummary_slotTypeId,
    slotTypeSummary_slotTypeName,

    -- ** SlotTypeValue
    slotTypeValue_sampleValue,
    slotTypeValue_synonyms,

    -- ** SlotValue
    slotValue_interpretedValue,

    -- ** SlotValueElicitationSetting
    slotValueElicitationSetting_defaultValueSpecification,
    slotValueElicitationSetting_promptSpecification,
    slotValueElicitationSetting_sampleUtterances,
    slotValueElicitationSetting_slotCaptureSetting,
    slotValueElicitationSetting_waitAndContinueSpecification,
    slotValueElicitationSetting_slotConstraint,

    -- ** SlotValueOverride
    slotValueOverride_shape,
    slotValueOverride_value,
    slotValueOverride_values,

    -- ** SlotValueRegexFilter
    slotValueRegexFilter_pattern,

    -- ** SlotValueSelectionSetting
    slotValueSelectionSetting_advancedRecognitionSetting,
    slotValueSelectionSetting_regexFilter,
    slotValueSelectionSetting_resolutionStrategy,

    -- ** Specifications
    specifications_slotTypeId,
    specifications_valueElicitationSetting,

    -- ** StillWaitingResponseSpecification
    stillWaitingResponseSpecification_allowInterrupt,
    stillWaitingResponseSpecification_messageGroups,
    stillWaitingResponseSpecification_frequencyInSeconds,
    stillWaitingResponseSpecification_timeoutInSeconds,

    -- ** SubSlotSetting
    subSlotSetting_expression,
    subSlotSetting_slotSpecifications,

    -- ** SubSlotTypeComposition
    subSlotTypeComposition_name,
    subSlotTypeComposition_slotTypeId,

    -- ** SubSlotValueElicitationSetting
    subSlotValueElicitationSetting_defaultValueSpecification,
    subSlotValueElicitationSetting_sampleUtterances,
    subSlotValueElicitationSetting_waitAndContinueSpecification,
    subSlotValueElicitationSetting_promptSpecification,

    -- ** TextInputSpecification
    textInputSpecification_startTimeoutMs,

    -- ** TextLogDestination
    textLogDestination_cloudWatch,

    -- ** TextLogSetting
    textLogSetting_enabled,
    textLogSetting_destination,

    -- ** TranscriptFilter
    transcriptFilter_lexTranscriptFilter,

    -- ** TranscriptSourceSetting
    transcriptSourceSetting_s3BucketTranscriptSource,

    -- ** UtteranceAggregationDuration
    utteranceAggregationDuration_relativeAggregationDuration,

    -- ** VoiceSettings
    voiceSettings_engine,
    voiceSettings_voiceId,

    -- ** WaitAndContinueSpecification
    waitAndContinueSpecification_active,
    waitAndContinueSpecification_stillWaitingResponse,
    waitAndContinueSpecification_waitingResponse,
    waitAndContinueSpecification_continueResponse,
  )
where

import Amazonka.LexV2Models.BatchCreateCustomVocabularyItem
import Amazonka.LexV2Models.BatchDeleteCustomVocabularyItem
import Amazonka.LexV2Models.BatchUpdateCustomVocabularyItem
import Amazonka.LexV2Models.BuildBotLocale
import Amazonka.LexV2Models.CreateBot
import Amazonka.LexV2Models.CreateBotAlias
import Amazonka.LexV2Models.CreateBotLocale
import Amazonka.LexV2Models.CreateBotVersion
import Amazonka.LexV2Models.CreateExport
import Amazonka.LexV2Models.CreateIntent
import Amazonka.LexV2Models.CreateResourcePolicy
import Amazonka.LexV2Models.CreateResourcePolicyStatement
import Amazonka.LexV2Models.CreateSlot
import Amazonka.LexV2Models.CreateSlotType
import Amazonka.LexV2Models.CreateUploadUrl
import Amazonka.LexV2Models.DeleteBot
import Amazonka.LexV2Models.DeleteBotAlias
import Amazonka.LexV2Models.DeleteBotLocale
import Amazonka.LexV2Models.DeleteBotVersion
import Amazonka.LexV2Models.DeleteCustomVocabulary
import Amazonka.LexV2Models.DeleteExport
import Amazonka.LexV2Models.DeleteImport
import Amazonka.LexV2Models.DeleteIntent
import Amazonka.LexV2Models.DeleteResourcePolicy
import Amazonka.LexV2Models.DeleteResourcePolicyStatement
import Amazonka.LexV2Models.DeleteSlot
import Amazonka.LexV2Models.DeleteSlotType
import Amazonka.LexV2Models.DeleteUtterances
import Amazonka.LexV2Models.DescribeBot
import Amazonka.LexV2Models.DescribeBotAlias
import Amazonka.LexV2Models.DescribeBotLocale
import Amazonka.LexV2Models.DescribeBotRecommendation
import Amazonka.LexV2Models.DescribeBotVersion
import Amazonka.LexV2Models.DescribeCustomVocabularyMetadata
import Amazonka.LexV2Models.DescribeExport
import Amazonka.LexV2Models.DescribeImport
import Amazonka.LexV2Models.DescribeIntent
import Amazonka.LexV2Models.DescribeResourcePolicy
import Amazonka.LexV2Models.DescribeSlot
import Amazonka.LexV2Models.DescribeSlotType
import Amazonka.LexV2Models.ListAggregatedUtterances
import Amazonka.LexV2Models.ListBotAliases
import Amazonka.LexV2Models.ListBotLocales
import Amazonka.LexV2Models.ListBotRecommendations
import Amazonka.LexV2Models.ListBotVersions
import Amazonka.LexV2Models.ListBots
import Amazonka.LexV2Models.ListBuiltInIntents
import Amazonka.LexV2Models.ListBuiltInSlotTypes
import Amazonka.LexV2Models.ListCustomVocabularyItems
import Amazonka.LexV2Models.ListExports
import Amazonka.LexV2Models.ListImports
import Amazonka.LexV2Models.ListIntents
import Amazonka.LexV2Models.ListRecommendedIntents
import Amazonka.LexV2Models.ListSlotTypes
import Amazonka.LexV2Models.ListSlots
import Amazonka.LexV2Models.ListTagsForResource
import Amazonka.LexV2Models.SearchAssociatedTranscripts
import Amazonka.LexV2Models.StartBotRecommendation
import Amazonka.LexV2Models.StartImport
import Amazonka.LexV2Models.StopBotRecommendation
import Amazonka.LexV2Models.TagResource
import Amazonka.LexV2Models.Types.AdvancedRecognitionSetting
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilter
import Amazonka.LexV2Models.Types.AggregatedUtterancesSortBy
import Amazonka.LexV2Models.Types.AggregatedUtterancesSummary
import Amazonka.LexV2Models.Types.AllowedInputTypes
import Amazonka.LexV2Models.Types.AssociatedTranscript
import Amazonka.LexV2Models.Types.AssociatedTranscriptFilter
import Amazonka.LexV2Models.Types.AudioAndDTMFInputSpecification
import Amazonka.LexV2Models.Types.AudioLogDestination
import Amazonka.LexV2Models.Types.AudioLogSetting
import Amazonka.LexV2Models.Types.AudioSpecification
import Amazonka.LexV2Models.Types.BotAliasHistoryEvent
import Amazonka.LexV2Models.Types.BotAliasLocaleSettings
import Amazonka.LexV2Models.Types.BotAliasSummary
import Amazonka.LexV2Models.Types.BotExportSpecification
import Amazonka.LexV2Models.Types.BotFilter
import Amazonka.LexV2Models.Types.BotImportSpecification
import Amazonka.LexV2Models.Types.BotLocaleExportSpecification
import Amazonka.LexV2Models.Types.BotLocaleFilter
import Amazonka.LexV2Models.Types.BotLocaleHistoryEvent
import Amazonka.LexV2Models.Types.BotLocaleImportSpecification
import Amazonka.LexV2Models.Types.BotLocaleSortBy
import Amazonka.LexV2Models.Types.BotLocaleSummary
import Amazonka.LexV2Models.Types.BotRecommendationResultStatistics
import Amazonka.LexV2Models.Types.BotRecommendationResults
import Amazonka.LexV2Models.Types.BotRecommendationSummary
import Amazonka.LexV2Models.Types.BotSortBy
import Amazonka.LexV2Models.Types.BotSummary
import Amazonka.LexV2Models.Types.BotVersionLocaleDetails
import Amazonka.LexV2Models.Types.BotVersionSortBy
import Amazonka.LexV2Models.Types.BotVersionSummary
import Amazonka.LexV2Models.Types.BuiltInIntentSortBy
import Amazonka.LexV2Models.Types.BuiltInIntentSummary
import Amazonka.LexV2Models.Types.BuiltInSlotTypeSortBy
import Amazonka.LexV2Models.Types.BuiltInSlotTypeSummary
import Amazonka.LexV2Models.Types.Button
import Amazonka.LexV2Models.Types.CloudWatchLogGroupLogDestination
import Amazonka.LexV2Models.Types.CodeHookSpecification
import Amazonka.LexV2Models.Types.CompositeSlotTypeSetting
import Amazonka.LexV2Models.Types.Condition
import Amazonka.LexV2Models.Types.ConditionalBranch
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.ConversationLogSettings
import Amazonka.LexV2Models.Types.CustomPayload
import Amazonka.LexV2Models.Types.CustomVocabularyEntryId
import Amazonka.LexV2Models.Types.CustomVocabularyExportSpecification
import Amazonka.LexV2Models.Types.CustomVocabularyImportSpecification
import Amazonka.LexV2Models.Types.CustomVocabularyItem
import Amazonka.LexV2Models.Types.DTMFSpecification
import Amazonka.LexV2Models.Types.DataPrivacy
import Amazonka.LexV2Models.Types.DateRangeFilter
import Amazonka.LexV2Models.Types.DefaultConditionalBranch
import Amazonka.LexV2Models.Types.DialogAction
import Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.DialogCodeHookSettings
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.EncryptionSetting
import Amazonka.LexV2Models.Types.ExportFilter
import Amazonka.LexV2Models.Types.ExportResourceSpecification
import Amazonka.LexV2Models.Types.ExportSortBy
import Amazonka.LexV2Models.Types.ExportSummary
import Amazonka.LexV2Models.Types.ExternalSourceSetting
import Amazonka.LexV2Models.Types.FailedCustomVocabularyItem
import Amazonka.LexV2Models.Types.FulfillmentCodeHookSettings
import Amazonka.LexV2Models.Types.FulfillmentStartResponseSpecification
import Amazonka.LexV2Models.Types.FulfillmentUpdateResponseSpecification
import Amazonka.LexV2Models.Types.FulfillmentUpdatesSpecification
import Amazonka.LexV2Models.Types.GrammarSlotTypeSetting
import Amazonka.LexV2Models.Types.GrammarSlotTypeSource
import Amazonka.LexV2Models.Types.ImageResponseCard
import Amazonka.LexV2Models.Types.ImportFilter
import Amazonka.LexV2Models.Types.ImportResourceSpecification
import Amazonka.LexV2Models.Types.ImportSortBy
import Amazonka.LexV2Models.Types.ImportSummary
import Amazonka.LexV2Models.Types.InitialResponseSetting
import Amazonka.LexV2Models.Types.InputContext
import Amazonka.LexV2Models.Types.IntentClosingSetting
import Amazonka.LexV2Models.Types.IntentConfirmationSetting
import Amazonka.LexV2Models.Types.IntentFilter
import Amazonka.LexV2Models.Types.IntentOverride
import Amazonka.LexV2Models.Types.IntentSortBy
import Amazonka.LexV2Models.Types.IntentStatistics
import Amazonka.LexV2Models.Types.IntentSummary
import Amazonka.LexV2Models.Types.KendraConfiguration
import Amazonka.LexV2Models.Types.LambdaCodeHook
import Amazonka.LexV2Models.Types.LexTranscriptFilter
import Amazonka.LexV2Models.Types.Message
import Amazonka.LexV2Models.Types.MessageGroup
import Amazonka.LexV2Models.Types.MultipleValuesSetting
import Amazonka.LexV2Models.Types.NewCustomVocabularyItem
import Amazonka.LexV2Models.Types.ObfuscationSetting
import Amazonka.LexV2Models.Types.OutputContext
import Amazonka.LexV2Models.Types.PathFormat
import Amazonka.LexV2Models.Types.PlainTextMessage
import Amazonka.LexV2Models.Types.PostDialogCodeHookInvocationSpecification
import Amazonka.LexV2Models.Types.PostFulfillmentStatusSpecification
import Amazonka.LexV2Models.Types.Principal
import Amazonka.LexV2Models.Types.PromptAttemptSpecification
import Amazonka.LexV2Models.Types.PromptSpecification
import Amazonka.LexV2Models.Types.RecommendedIntentSummary
import Amazonka.LexV2Models.Types.RelativeAggregationDuration
import Amazonka.LexV2Models.Types.ResponseSpecification
import Amazonka.LexV2Models.Types.S3BucketLogDestination
import Amazonka.LexV2Models.Types.S3BucketTranscriptSource
import Amazonka.LexV2Models.Types.SSMLMessage
import Amazonka.LexV2Models.Types.SampleUtterance
import Amazonka.LexV2Models.Types.SampleValue
import Amazonka.LexV2Models.Types.SentimentAnalysisSettings
import Amazonka.LexV2Models.Types.SlotCaptureSetting
import Amazonka.LexV2Models.Types.SlotDefaultValue
import Amazonka.LexV2Models.Types.SlotDefaultValueSpecification
import Amazonka.LexV2Models.Types.SlotFilter
import Amazonka.LexV2Models.Types.SlotPriority
import Amazonka.LexV2Models.Types.SlotSortBy
import Amazonka.LexV2Models.Types.SlotSummary
import Amazonka.LexV2Models.Types.SlotTypeFilter
import Amazonka.LexV2Models.Types.SlotTypeSortBy
import Amazonka.LexV2Models.Types.SlotTypeStatistics
import Amazonka.LexV2Models.Types.SlotTypeSummary
import Amazonka.LexV2Models.Types.SlotTypeValue
import Amazonka.LexV2Models.Types.SlotValue
import Amazonka.LexV2Models.Types.SlotValueElicitationSetting
import Amazonka.LexV2Models.Types.SlotValueOverride
import Amazonka.LexV2Models.Types.SlotValueRegexFilter
import Amazonka.LexV2Models.Types.SlotValueSelectionSetting
import Amazonka.LexV2Models.Types.Specifications
import Amazonka.LexV2Models.Types.StillWaitingResponseSpecification
import Amazonka.LexV2Models.Types.SubSlotSetting
import Amazonka.LexV2Models.Types.SubSlotTypeComposition
import Amazonka.LexV2Models.Types.SubSlotValueElicitationSetting
import Amazonka.LexV2Models.Types.TextInputSpecification
import Amazonka.LexV2Models.Types.TextLogDestination
import Amazonka.LexV2Models.Types.TextLogSetting
import Amazonka.LexV2Models.Types.TranscriptFilter
import Amazonka.LexV2Models.Types.TranscriptSourceSetting
import Amazonka.LexV2Models.Types.UtteranceAggregationDuration
import Amazonka.LexV2Models.Types.VoiceSettings
import Amazonka.LexV2Models.Types.WaitAndContinueSpecification
import Amazonka.LexV2Models.UntagResource
import Amazonka.LexV2Models.UpdateBot
import Amazonka.LexV2Models.UpdateBotAlias
import Amazonka.LexV2Models.UpdateBotLocale
import Amazonka.LexV2Models.UpdateBotRecommendation
import Amazonka.LexV2Models.UpdateExport
import Amazonka.LexV2Models.UpdateIntent
import Amazonka.LexV2Models.UpdateResourcePolicy
import Amazonka.LexV2Models.UpdateSlot
import Amazonka.LexV2Models.UpdateSlotType
