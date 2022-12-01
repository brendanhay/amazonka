{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchCreateCustomVocabularyItemResponse_botVersion,
    batchCreateCustomVocabularyItemResponse_localeId,
    batchCreateCustomVocabularyItemResponse_botId,
    batchCreateCustomVocabularyItemResponse_errors,
    batchCreateCustomVocabularyItemResponse_resources,
    batchCreateCustomVocabularyItemResponse_httpStatus,

    -- ** BatchDeleteCustomVocabularyItem
    batchDeleteCustomVocabularyItem_botId,
    batchDeleteCustomVocabularyItem_botVersion,
    batchDeleteCustomVocabularyItem_localeId,
    batchDeleteCustomVocabularyItem_customVocabularyItemList,
    batchDeleteCustomVocabularyItemResponse_botVersion,
    batchDeleteCustomVocabularyItemResponse_localeId,
    batchDeleteCustomVocabularyItemResponse_botId,
    batchDeleteCustomVocabularyItemResponse_errors,
    batchDeleteCustomVocabularyItemResponse_resources,
    batchDeleteCustomVocabularyItemResponse_httpStatus,

    -- ** BatchUpdateCustomVocabularyItem
    batchUpdateCustomVocabularyItem_botId,
    batchUpdateCustomVocabularyItem_botVersion,
    batchUpdateCustomVocabularyItem_localeId,
    batchUpdateCustomVocabularyItem_customVocabularyItemList,
    batchUpdateCustomVocabularyItemResponse_botVersion,
    batchUpdateCustomVocabularyItemResponse_localeId,
    batchUpdateCustomVocabularyItemResponse_botId,
    batchUpdateCustomVocabularyItemResponse_errors,
    batchUpdateCustomVocabularyItemResponse_resources,
    batchUpdateCustomVocabularyItemResponse_httpStatus,

    -- ** BuildBotLocale
    buildBotLocale_botId,
    buildBotLocale_botVersion,
    buildBotLocale_localeId,
    buildBotLocaleResponse_botVersion,
    buildBotLocaleResponse_localeId,
    buildBotLocaleResponse_botId,
    buildBotLocaleResponse_lastBuildSubmittedDateTime,
    buildBotLocaleResponse_botLocaleStatus,
    buildBotLocaleResponse_httpStatus,

    -- ** CreateBot
    createBot_description,
    createBot_botTags,
    createBot_testBotAliasTags,
    createBot_botName,
    createBot_roleArn,
    createBot_dataPrivacy,
    createBot_idleSessionTTLInSeconds,
    createBotResponse_roleArn,
    createBotResponse_creationDateTime,
    createBotResponse_description,
    createBotResponse_idleSessionTTLInSeconds,
    createBotResponse_botId,
    createBotResponse_botTags,
    createBotResponse_botName,
    createBotResponse_dataPrivacy,
    createBotResponse_botStatus,
    createBotResponse_testBotAliasTags,
    createBotResponse_httpStatus,

    -- ** CreateBotAlias
    createBotAlias_tags,
    createBotAlias_botVersion,
    createBotAlias_description,
    createBotAlias_sentimentAnalysisSettings,
    createBotAlias_conversationLogSettings,
    createBotAlias_botAliasLocaleSettings,
    createBotAlias_botAliasName,
    createBotAlias_botId,
    createBotAliasResponse_tags,
    createBotAliasResponse_botAliasStatus,
    createBotAliasResponse_botVersion,
    createBotAliasResponse_creationDateTime,
    createBotAliasResponse_description,
    createBotAliasResponse_sentimentAnalysisSettings,
    createBotAliasResponse_botId,
    createBotAliasResponse_botAliasId,
    createBotAliasResponse_conversationLogSettings,
    createBotAliasResponse_botAliasLocaleSettings,
    createBotAliasResponse_botAliasName,
    createBotAliasResponse_httpStatus,

    -- ** CreateBotLocale
    createBotLocale_description,
    createBotLocale_voiceSettings,
    createBotLocale_botId,
    createBotLocale_botVersion,
    createBotLocale_localeId,
    createBotLocale_nluIntentConfidenceThreshold,
    createBotLocaleResponse_nluIntentConfidenceThreshold,
    createBotLocaleResponse_botVersion,
    createBotLocaleResponse_creationDateTime,
    createBotLocaleResponse_localeName,
    createBotLocaleResponse_localeId,
    createBotLocaleResponse_description,
    createBotLocaleResponse_botId,
    createBotLocaleResponse_botLocaleStatus,
    createBotLocaleResponse_voiceSettings,
    createBotLocaleResponse_httpStatus,

    -- ** CreateBotVersion
    createBotVersion_description,
    createBotVersion_botId,
    createBotVersion_botVersionLocaleSpecification,
    createBotVersionResponse_botVersionLocaleSpecification,
    createBotVersionResponse_botVersion,
    createBotVersionResponse_creationDateTime,
    createBotVersionResponse_description,
    createBotVersionResponse_botId,
    createBotVersionResponse_botStatus,
    createBotVersionResponse_httpStatus,

    -- ** CreateExport
    createExport_filePassword,
    createExport_resourceSpecification,
    createExport_fileFormat,
    createExportResponse_creationDateTime,
    createExportResponse_resourceSpecification,
    createExportResponse_exportStatus,
    createExportResponse_exportId,
    createExportResponse_fileFormat,
    createExportResponse_httpStatus,

    -- ** CreateIntent
    createIntent_intentClosingSetting,
    createIntent_sampleUtterances,
    createIntent_kendraConfiguration,
    createIntent_dialogCodeHook,
    createIntent_outputContexts,
    createIntent_intentConfirmationSetting,
    createIntent_parentIntentSignature,
    createIntent_description,
    createIntent_fulfillmentCodeHook,
    createIntent_initialResponseSetting,
    createIntent_inputContexts,
    createIntent_intentName,
    createIntent_botId,
    createIntent_botVersion,
    createIntent_localeId,
    createIntentResponse_intentClosingSetting,
    createIntentResponse_sampleUtterances,
    createIntentResponse_kendraConfiguration,
    createIntentResponse_botVersion,
    createIntentResponse_creationDateTime,
    createIntentResponse_dialogCodeHook,
    createIntentResponse_localeId,
    createIntentResponse_outputContexts,
    createIntentResponse_intentConfirmationSetting,
    createIntentResponse_parentIntentSignature,
    createIntentResponse_description,
    createIntentResponse_botId,
    createIntentResponse_intentId,
    createIntentResponse_intentName,
    createIntentResponse_fulfillmentCodeHook,
    createIntentResponse_initialResponseSetting,
    createIntentResponse_inputContexts,
    createIntentResponse_httpStatus,

    -- ** CreateResourcePolicy
    createResourcePolicy_resourceArn,
    createResourcePolicy_policy,
    createResourcePolicyResponse_revisionId,
    createResourcePolicyResponse_resourceArn,
    createResourcePolicyResponse_httpStatus,

    -- ** CreateResourcePolicyStatement
    createResourcePolicyStatement_expectedRevisionId,
    createResourcePolicyStatement_condition,
    createResourcePolicyStatement_resourceArn,
    createResourcePolicyStatement_statementId,
    createResourcePolicyStatement_effect,
    createResourcePolicyStatement_principal,
    createResourcePolicyStatement_action,
    createResourcePolicyStatementResponse_revisionId,
    createResourcePolicyStatementResponse_resourceArn,
    createResourcePolicyStatementResponse_httpStatus,

    -- ** CreateSlot
    createSlot_multipleValuesSetting,
    createSlot_description,
    createSlot_obfuscationSetting,
    createSlot_subSlotSetting,
    createSlot_slotTypeId,
    createSlot_slotName,
    createSlot_valueElicitationSetting,
    createSlot_botId,
    createSlot_botVersion,
    createSlot_localeId,
    createSlot_intentId,
    createSlotResponse_multipleValuesSetting,
    createSlotResponse_slotName,
    createSlotResponse_valueElicitationSetting,
    createSlotResponse_botVersion,
    createSlotResponse_creationDateTime,
    createSlotResponse_localeId,
    createSlotResponse_description,
    createSlotResponse_botId,
    createSlotResponse_intentId,
    createSlotResponse_slotId,
    createSlotResponse_obfuscationSetting,
    createSlotResponse_subSlotSetting,
    createSlotResponse_slotTypeId,
    createSlotResponse_httpStatus,

    -- ** CreateSlotType
    createSlotType_compositeSlotTypeSetting,
    createSlotType_externalSourceSetting,
    createSlotType_valueSelectionSetting,
    createSlotType_description,
    createSlotType_slotTypeValues,
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeName,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_compositeSlotTypeSetting,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_externalSourceSetting,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_description,
    createSlotTypeResponse_botId,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_httpStatus,

    -- ** CreateUploadUrl
    createUploadUrlResponse_uploadUrl,
    createUploadUrlResponse_importId,
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
    deleteBotAliasResponse_botAliasStatus,
    deleteBotAliasResponse_botId,
    deleteBotAliasResponse_botAliasId,
    deleteBotAliasResponse_httpStatus,

    -- ** DeleteBotLocale
    deleteBotLocale_botId,
    deleteBotLocale_botVersion,
    deleteBotLocale_localeId,
    deleteBotLocaleResponse_botVersion,
    deleteBotLocaleResponse_localeId,
    deleteBotLocaleResponse_botId,
    deleteBotLocaleResponse_botLocaleStatus,
    deleteBotLocaleResponse_httpStatus,

    -- ** DeleteBotVersion
    deleteBotVersion_skipResourceInUseCheck,
    deleteBotVersion_botId,
    deleteBotVersion_botVersion,
    deleteBotVersionResponse_botVersion,
    deleteBotVersionResponse_botId,
    deleteBotVersionResponse_botStatus,
    deleteBotVersionResponse_httpStatus,

    -- ** DeleteCustomVocabulary
    deleteCustomVocabulary_botId,
    deleteCustomVocabulary_botVersion,
    deleteCustomVocabulary_localeId,
    deleteCustomVocabularyResponse_customVocabularyStatus,
    deleteCustomVocabularyResponse_botVersion,
    deleteCustomVocabularyResponse_localeId,
    deleteCustomVocabularyResponse_botId,
    deleteCustomVocabularyResponse_httpStatus,

    -- ** DeleteExport
    deleteExport_exportId,
    deleteExportResponse_exportStatus,
    deleteExportResponse_exportId,
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
    deleteResourcePolicyResponse_revisionId,
    deleteResourcePolicyResponse_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicyStatement
    deleteResourcePolicyStatement_expectedRevisionId,
    deleteResourcePolicyStatement_resourceArn,
    deleteResourcePolicyStatement_statementId,
    deleteResourcePolicyStatementResponse_revisionId,
    deleteResourcePolicyStatementResponse_resourceArn,
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
    describeBotResponse_roleArn,
    describeBotResponse_creationDateTime,
    describeBotResponse_description,
    describeBotResponse_idleSessionTTLInSeconds,
    describeBotResponse_botId,
    describeBotResponse_botName,
    describeBotResponse_dataPrivacy,
    describeBotResponse_botStatus,
    describeBotResponse_lastUpdatedDateTime,
    describeBotResponse_httpStatus,

    -- ** DescribeBotAlias
    describeBotAlias_botAliasId,
    describeBotAlias_botId,
    describeBotAliasResponse_botAliasStatus,
    describeBotAliasResponse_botVersion,
    describeBotAliasResponse_creationDateTime,
    describeBotAliasResponse_description,
    describeBotAliasResponse_sentimentAnalysisSettings,
    describeBotAliasResponse_botId,
    describeBotAliasResponse_botAliasId,
    describeBotAliasResponse_conversationLogSettings,
    describeBotAliasResponse_botAliasLocaleSettings,
    describeBotAliasResponse_botAliasName,
    describeBotAliasResponse_botAliasHistoryEvents,
    describeBotAliasResponse_lastUpdatedDateTime,
    describeBotAliasResponse_httpStatus,

    -- ** DescribeBotLocale
    describeBotLocale_botId,
    describeBotLocale_botVersion,
    describeBotLocale_localeId,
    describeBotLocaleResponse_nluIntentConfidenceThreshold,
    describeBotLocaleResponse_botVersion,
    describeBotLocaleResponse_slotTypesCount,
    describeBotLocaleResponse_creationDateTime,
    describeBotLocaleResponse_localeName,
    describeBotLocaleResponse_localeId,
    describeBotLocaleResponse_botLocaleHistoryEvents,
    describeBotLocaleResponse_recommendedActions,
    describeBotLocaleResponse_description,
    describeBotLocaleResponse_botId,
    describeBotLocaleResponse_lastBuildSubmittedDateTime,
    describeBotLocaleResponse_botLocaleStatus,
    describeBotLocaleResponse_voiceSettings,
    describeBotLocaleResponse_failureReasons,
    describeBotLocaleResponse_lastUpdatedDateTime,
    describeBotLocaleResponse_intentsCount,
    describeBotLocaleResponse_httpStatus,

    -- ** DescribeBotRecommendation
    describeBotRecommendation_botId,
    describeBotRecommendation_botVersion,
    describeBotRecommendation_localeId,
    describeBotRecommendation_botRecommendationId,
    describeBotRecommendationResponse_botVersion,
    describeBotRecommendationResponse_creationDateTime,
    describeBotRecommendationResponse_localeId,
    describeBotRecommendationResponse_encryptionSetting,
    describeBotRecommendationResponse_botRecommendationId,
    describeBotRecommendationResponse_botRecommendationResults,
    describeBotRecommendationResponse_botId,
    describeBotRecommendationResponse_botRecommendationStatus,
    describeBotRecommendationResponse_transcriptSourceSetting,
    describeBotRecommendationResponse_failureReasons,
    describeBotRecommendationResponse_lastUpdatedDateTime,
    describeBotRecommendationResponse_httpStatus,

    -- ** DescribeBotVersion
    describeBotVersion_botId,
    describeBotVersion_botVersion,
    describeBotVersionResponse_roleArn,
    describeBotVersionResponse_botVersion,
    describeBotVersionResponse_creationDateTime,
    describeBotVersionResponse_description,
    describeBotVersionResponse_idleSessionTTLInSeconds,
    describeBotVersionResponse_botId,
    describeBotVersionResponse_botName,
    describeBotVersionResponse_dataPrivacy,
    describeBotVersionResponse_failureReasons,
    describeBotVersionResponse_botStatus,
    describeBotVersionResponse_httpStatus,

    -- ** DescribeCustomVocabularyMetadata
    describeCustomVocabularyMetadata_botId,
    describeCustomVocabularyMetadata_botVersion,
    describeCustomVocabularyMetadata_localeId,
    describeCustomVocabularyMetadataResponse_customVocabularyStatus,
    describeCustomVocabularyMetadataResponse_botVersion,
    describeCustomVocabularyMetadataResponse_creationDateTime,
    describeCustomVocabularyMetadataResponse_localeId,
    describeCustomVocabularyMetadataResponse_botId,
    describeCustomVocabularyMetadataResponse_lastUpdatedDateTime,
    describeCustomVocabularyMetadataResponse_httpStatus,

    -- ** DescribeExport
    describeExport_exportId,
    describeExportResponse_creationDateTime,
    describeExportResponse_downloadUrl,
    describeExportResponse_resourceSpecification,
    describeExportResponse_exportStatus,
    describeExportResponse_exportId,
    describeExportResponse_failureReasons,
    describeExportResponse_lastUpdatedDateTime,
    describeExportResponse_fileFormat,
    describeExportResponse_httpStatus,

    -- ** DescribeImport
    describeImport_importId,
    describeImportResponse_creationDateTime,
    describeImportResponse_importedResourceName,
    describeImportResponse_resourceSpecification,
    describeImportResponse_importId,
    describeImportResponse_importStatus,
    describeImportResponse_importedResourceId,
    describeImportResponse_failureReasons,
    describeImportResponse_lastUpdatedDateTime,
    describeImportResponse_mergeStrategy,
    describeImportResponse_httpStatus,

    -- ** DescribeIntent
    describeIntent_intentId,
    describeIntent_botId,
    describeIntent_botVersion,
    describeIntent_localeId,
    describeIntentResponse_intentClosingSetting,
    describeIntentResponse_sampleUtterances,
    describeIntentResponse_kendraConfiguration,
    describeIntentResponse_botVersion,
    describeIntentResponse_creationDateTime,
    describeIntentResponse_dialogCodeHook,
    describeIntentResponse_localeId,
    describeIntentResponse_outputContexts,
    describeIntentResponse_intentConfirmationSetting,
    describeIntentResponse_parentIntentSignature,
    describeIntentResponse_description,
    describeIntentResponse_botId,
    describeIntentResponse_slotPriorities,
    describeIntentResponse_intentId,
    describeIntentResponse_intentName,
    describeIntentResponse_fulfillmentCodeHook,
    describeIntentResponse_initialResponseSetting,
    describeIntentResponse_inputContexts,
    describeIntentResponse_lastUpdatedDateTime,
    describeIntentResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_revisionId,
    describeResourcePolicyResponse_resourceArn,
    describeResourcePolicyResponse_httpStatus,

    -- ** DescribeSlot
    describeSlot_slotId,
    describeSlot_botId,
    describeSlot_botVersion,
    describeSlot_localeId,
    describeSlot_intentId,
    describeSlotResponse_multipleValuesSetting,
    describeSlotResponse_slotName,
    describeSlotResponse_valueElicitationSetting,
    describeSlotResponse_botVersion,
    describeSlotResponse_creationDateTime,
    describeSlotResponse_localeId,
    describeSlotResponse_description,
    describeSlotResponse_botId,
    describeSlotResponse_intentId,
    describeSlotResponse_slotId,
    describeSlotResponse_obfuscationSetting,
    describeSlotResponse_subSlotSetting,
    describeSlotResponse_slotTypeId,
    describeSlotResponse_lastUpdatedDateTime,
    describeSlotResponse_httpStatus,

    -- ** DescribeSlotType
    describeSlotType_slotTypeId,
    describeSlotType_botId,
    describeSlotType_botVersion,
    describeSlotType_localeId,
    describeSlotTypeResponse_botVersion,
    describeSlotTypeResponse_compositeSlotTypeSetting,
    describeSlotTypeResponse_creationDateTime,
    describeSlotTypeResponse_localeId,
    describeSlotTypeResponse_externalSourceSetting,
    describeSlotTypeResponse_valueSelectionSetting,
    describeSlotTypeResponse_description,
    describeSlotTypeResponse_botId,
    describeSlotTypeResponse_slotTypeValues,
    describeSlotTypeResponse_slotTypeName,
    describeSlotTypeResponse_slotTypeId,
    describeSlotTypeResponse_parentSlotTypeSignature,
    describeSlotTypeResponse_lastUpdatedDateTime,
    describeSlotTypeResponse_httpStatus,

    -- ** ListAggregatedUtterances
    listAggregatedUtterances_nextToken,
    listAggregatedUtterances_botVersion,
    listAggregatedUtterances_filters,
    listAggregatedUtterances_sortBy,
    listAggregatedUtterances_botAliasId,
    listAggregatedUtterances_maxResults,
    listAggregatedUtterances_botId,
    listAggregatedUtterances_localeId,
    listAggregatedUtterances_aggregationDuration,
    listAggregatedUtterancesResponse_nextToken,
    listAggregatedUtterancesResponse_botVersion,
    listAggregatedUtterancesResponse_aggregationWindowStartTime,
    listAggregatedUtterancesResponse_localeId,
    listAggregatedUtterancesResponse_botId,
    listAggregatedUtterancesResponse_botAliasId,
    listAggregatedUtterancesResponse_aggregationDuration,
    listAggregatedUtterancesResponse_aggregationWindowEndTime,
    listAggregatedUtterancesResponse_aggregationLastRefreshedDateTime,
    listAggregatedUtterancesResponse_aggregatedUtterancesSummaries,
    listAggregatedUtterancesResponse_httpStatus,

    -- ** ListBotAliases
    listBotAliases_nextToken,
    listBotAliases_maxResults,
    listBotAliases_botId,
    listBotAliasesResponse_nextToken,
    listBotAliasesResponse_botId,
    listBotAliasesResponse_botAliasSummaries,
    listBotAliasesResponse_httpStatus,

    -- ** ListBotLocales
    listBotLocales_nextToken,
    listBotLocales_filters,
    listBotLocales_sortBy,
    listBotLocales_maxResults,
    listBotLocales_botId,
    listBotLocales_botVersion,
    listBotLocalesResponse_nextToken,
    listBotLocalesResponse_botVersion,
    listBotLocalesResponse_botLocaleSummaries,
    listBotLocalesResponse_botId,
    listBotLocalesResponse_httpStatus,

    -- ** ListBotRecommendations
    listBotRecommendations_nextToken,
    listBotRecommendations_maxResults,
    listBotRecommendations_botId,
    listBotRecommendations_botVersion,
    listBotRecommendations_localeId,
    listBotRecommendationsResponse_nextToken,
    listBotRecommendationsResponse_botVersion,
    listBotRecommendationsResponse_botRecommendationSummaries,
    listBotRecommendationsResponse_localeId,
    listBotRecommendationsResponse_botId,
    listBotRecommendationsResponse_httpStatus,

    -- ** ListBotVersions
    listBotVersions_nextToken,
    listBotVersions_sortBy,
    listBotVersions_maxResults,
    listBotVersions_botId,
    listBotVersionsResponse_nextToken,
    listBotVersionsResponse_botId,
    listBotVersionsResponse_botVersionSummaries,
    listBotVersionsResponse_httpStatus,

    -- ** ListBots
    listBots_nextToken,
    listBots_filters,
    listBots_sortBy,
    listBots_maxResults,
    listBotsResponse_nextToken,
    listBotsResponse_botSummaries,
    listBotsResponse_httpStatus,

    -- ** ListBuiltInIntents
    listBuiltInIntents_nextToken,
    listBuiltInIntents_sortBy,
    listBuiltInIntents_maxResults,
    listBuiltInIntents_localeId,
    listBuiltInIntentsResponse_nextToken,
    listBuiltInIntentsResponse_builtInIntentSummaries,
    listBuiltInIntentsResponse_localeId,
    listBuiltInIntentsResponse_httpStatus,

    -- ** ListBuiltInSlotTypes
    listBuiltInSlotTypes_nextToken,
    listBuiltInSlotTypes_sortBy,
    listBuiltInSlotTypes_maxResults,
    listBuiltInSlotTypes_localeId,
    listBuiltInSlotTypesResponse_nextToken,
    listBuiltInSlotTypesResponse_localeId,
    listBuiltInSlotTypesResponse_builtInSlotTypeSummaries,
    listBuiltInSlotTypesResponse_httpStatus,

    -- ** ListCustomVocabularyItems
    listCustomVocabularyItems_nextToken,
    listCustomVocabularyItems_maxResults,
    listCustomVocabularyItems_botId,
    listCustomVocabularyItems_botVersion,
    listCustomVocabularyItems_localeId,
    listCustomVocabularyItemsResponse_nextToken,
    listCustomVocabularyItemsResponse_customVocabularyItems,
    listCustomVocabularyItemsResponse_botVersion,
    listCustomVocabularyItemsResponse_localeId,
    listCustomVocabularyItemsResponse_botId,
    listCustomVocabularyItemsResponse_httpStatus,

    -- ** ListExports
    listExports_nextToken,
    listExports_botVersion,
    listExports_localeId,
    listExports_filters,
    listExports_sortBy,
    listExports_botId,
    listExports_maxResults,
    listExportsResponse_nextToken,
    listExportsResponse_botVersion,
    listExportsResponse_localeId,
    listExportsResponse_botId,
    listExportsResponse_exportSummaries,
    listExportsResponse_httpStatus,

    -- ** ListImports
    listImports_nextToken,
    listImports_botVersion,
    listImports_localeId,
    listImports_filters,
    listImports_sortBy,
    listImports_botId,
    listImports_maxResults,
    listImportsResponse_nextToken,
    listImportsResponse_botVersion,
    listImportsResponse_localeId,
    listImportsResponse_importSummaries,
    listImportsResponse_botId,
    listImportsResponse_httpStatus,

    -- ** ListIntents
    listIntents_nextToken,
    listIntents_filters,
    listIntents_sortBy,
    listIntents_maxResults,
    listIntents_botId,
    listIntents_botVersion,
    listIntents_localeId,
    listIntentsResponse_intentSummaries,
    listIntentsResponse_nextToken,
    listIntentsResponse_botVersion,
    listIntentsResponse_localeId,
    listIntentsResponse_botId,
    listIntentsResponse_httpStatus,

    -- ** ListRecommendedIntents
    listRecommendedIntents_nextToken,
    listRecommendedIntents_maxResults,
    listRecommendedIntents_botId,
    listRecommendedIntents_botVersion,
    listRecommendedIntents_localeId,
    listRecommendedIntents_botRecommendationId,
    listRecommendedIntentsResponse_nextToken,
    listRecommendedIntentsResponse_botVersion,
    listRecommendedIntentsResponse_localeId,
    listRecommendedIntentsResponse_botRecommendationId,
    listRecommendedIntentsResponse_botId,
    listRecommendedIntentsResponse_summaryList,
    listRecommendedIntentsResponse_httpStatus,

    -- ** ListSlotTypes
    listSlotTypes_nextToken,
    listSlotTypes_filters,
    listSlotTypes_sortBy,
    listSlotTypes_maxResults,
    listSlotTypes_botId,
    listSlotTypes_botVersion,
    listSlotTypes_localeId,
    listSlotTypesResponse_nextToken,
    listSlotTypesResponse_botVersion,
    listSlotTypesResponse_localeId,
    listSlotTypesResponse_botId,
    listSlotTypesResponse_slotTypeSummaries,
    listSlotTypesResponse_httpStatus,

    -- ** ListSlots
    listSlots_nextToken,
    listSlots_filters,
    listSlots_sortBy,
    listSlots_maxResults,
    listSlots_botId,
    listSlots_botVersion,
    listSlots_localeId,
    listSlots_intentId,
    listSlotsResponse_slotSummaries,
    listSlotsResponse_nextToken,
    listSlotsResponse_botVersion,
    listSlotsResponse_localeId,
    listSlotsResponse_botId,
    listSlotsResponse_intentId,
    listSlotsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SearchAssociatedTranscripts
    searchAssociatedTranscripts_nextIndex,
    searchAssociatedTranscripts_searchOrder,
    searchAssociatedTranscripts_maxResults,
    searchAssociatedTranscripts_botId,
    searchAssociatedTranscripts_botVersion,
    searchAssociatedTranscripts_localeId,
    searchAssociatedTranscripts_botRecommendationId,
    searchAssociatedTranscripts_filters,
    searchAssociatedTranscriptsResponse_nextIndex,
    searchAssociatedTranscriptsResponse_botVersion,
    searchAssociatedTranscriptsResponse_localeId,
    searchAssociatedTranscriptsResponse_associatedTranscripts,
    searchAssociatedTranscriptsResponse_botRecommendationId,
    searchAssociatedTranscriptsResponse_botId,
    searchAssociatedTranscriptsResponse_totalResults,
    searchAssociatedTranscriptsResponse_httpStatus,

    -- ** StartBotRecommendation
    startBotRecommendation_encryptionSetting,
    startBotRecommendation_botId,
    startBotRecommendation_botVersion,
    startBotRecommendation_localeId,
    startBotRecommendation_transcriptSourceSetting,
    startBotRecommendationResponse_botVersion,
    startBotRecommendationResponse_creationDateTime,
    startBotRecommendationResponse_localeId,
    startBotRecommendationResponse_encryptionSetting,
    startBotRecommendationResponse_botRecommendationId,
    startBotRecommendationResponse_botId,
    startBotRecommendationResponse_botRecommendationStatus,
    startBotRecommendationResponse_transcriptSourceSetting,
    startBotRecommendationResponse_httpStatus,

    -- ** StartImport
    startImport_filePassword,
    startImport_importId,
    startImport_resourceSpecification,
    startImport_mergeStrategy,
    startImportResponse_creationDateTime,
    startImportResponse_resourceSpecification,
    startImportResponse_importId,
    startImportResponse_importStatus,
    startImportResponse_mergeStrategy,
    startImportResponse_httpStatus,

    -- ** StopBotRecommendation
    stopBotRecommendation_botId,
    stopBotRecommendation_botVersion,
    stopBotRecommendation_localeId,
    stopBotRecommendation_botRecommendationId,
    stopBotRecommendationResponse_botVersion,
    stopBotRecommendationResponse_localeId,
    stopBotRecommendationResponse_botRecommendationId,
    stopBotRecommendationResponse_botId,
    stopBotRecommendationResponse_botRecommendationStatus,
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
    updateBotResponse_roleArn,
    updateBotResponse_creationDateTime,
    updateBotResponse_description,
    updateBotResponse_idleSessionTTLInSeconds,
    updateBotResponse_botId,
    updateBotResponse_botName,
    updateBotResponse_dataPrivacy,
    updateBotResponse_botStatus,
    updateBotResponse_lastUpdatedDateTime,
    updateBotResponse_httpStatus,

    -- ** UpdateBotAlias
    updateBotAlias_botVersion,
    updateBotAlias_description,
    updateBotAlias_sentimentAnalysisSettings,
    updateBotAlias_conversationLogSettings,
    updateBotAlias_botAliasLocaleSettings,
    updateBotAlias_botAliasId,
    updateBotAlias_botAliasName,
    updateBotAlias_botId,
    updateBotAliasResponse_botAliasStatus,
    updateBotAliasResponse_botVersion,
    updateBotAliasResponse_creationDateTime,
    updateBotAliasResponse_description,
    updateBotAliasResponse_sentimentAnalysisSettings,
    updateBotAliasResponse_botId,
    updateBotAliasResponse_botAliasId,
    updateBotAliasResponse_conversationLogSettings,
    updateBotAliasResponse_botAliasLocaleSettings,
    updateBotAliasResponse_botAliasName,
    updateBotAliasResponse_lastUpdatedDateTime,
    updateBotAliasResponse_httpStatus,

    -- ** UpdateBotLocale
    updateBotLocale_description,
    updateBotLocale_voiceSettings,
    updateBotLocale_botId,
    updateBotLocale_botVersion,
    updateBotLocale_localeId,
    updateBotLocale_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_botVersion,
    updateBotLocaleResponse_creationDateTime,
    updateBotLocaleResponse_localeName,
    updateBotLocaleResponse_localeId,
    updateBotLocaleResponse_recommendedActions,
    updateBotLocaleResponse_description,
    updateBotLocaleResponse_botId,
    updateBotLocaleResponse_botLocaleStatus,
    updateBotLocaleResponse_voiceSettings,
    updateBotLocaleResponse_failureReasons,
    updateBotLocaleResponse_lastUpdatedDateTime,
    updateBotLocaleResponse_httpStatus,

    -- ** UpdateBotRecommendation
    updateBotRecommendation_botId,
    updateBotRecommendation_botVersion,
    updateBotRecommendation_localeId,
    updateBotRecommendation_botRecommendationId,
    updateBotRecommendation_encryptionSetting,
    updateBotRecommendationResponse_botVersion,
    updateBotRecommendationResponse_creationDateTime,
    updateBotRecommendationResponse_localeId,
    updateBotRecommendationResponse_encryptionSetting,
    updateBotRecommendationResponse_botRecommendationId,
    updateBotRecommendationResponse_botId,
    updateBotRecommendationResponse_botRecommendationStatus,
    updateBotRecommendationResponse_transcriptSourceSetting,
    updateBotRecommendationResponse_lastUpdatedDateTime,
    updateBotRecommendationResponse_httpStatus,

    -- ** UpdateExport
    updateExport_filePassword,
    updateExport_exportId,
    updateExportResponse_creationDateTime,
    updateExportResponse_resourceSpecification,
    updateExportResponse_exportStatus,
    updateExportResponse_exportId,
    updateExportResponse_lastUpdatedDateTime,
    updateExportResponse_fileFormat,
    updateExportResponse_httpStatus,

    -- ** UpdateIntent
    updateIntent_intentClosingSetting,
    updateIntent_sampleUtterances,
    updateIntent_kendraConfiguration,
    updateIntent_dialogCodeHook,
    updateIntent_outputContexts,
    updateIntent_intentConfirmationSetting,
    updateIntent_parentIntentSignature,
    updateIntent_description,
    updateIntent_slotPriorities,
    updateIntent_fulfillmentCodeHook,
    updateIntent_initialResponseSetting,
    updateIntent_inputContexts,
    updateIntent_intentId,
    updateIntent_intentName,
    updateIntent_botId,
    updateIntent_botVersion,
    updateIntent_localeId,
    updateIntentResponse_intentClosingSetting,
    updateIntentResponse_sampleUtterances,
    updateIntentResponse_kendraConfiguration,
    updateIntentResponse_botVersion,
    updateIntentResponse_creationDateTime,
    updateIntentResponse_dialogCodeHook,
    updateIntentResponse_localeId,
    updateIntentResponse_outputContexts,
    updateIntentResponse_intentConfirmationSetting,
    updateIntentResponse_parentIntentSignature,
    updateIntentResponse_description,
    updateIntentResponse_botId,
    updateIntentResponse_slotPriorities,
    updateIntentResponse_intentId,
    updateIntentResponse_intentName,
    updateIntentResponse_fulfillmentCodeHook,
    updateIntentResponse_initialResponseSetting,
    updateIntentResponse_inputContexts,
    updateIntentResponse_lastUpdatedDateTime,
    updateIntentResponse_httpStatus,

    -- ** UpdateResourcePolicy
    updateResourcePolicy_expectedRevisionId,
    updateResourcePolicy_resourceArn,
    updateResourcePolicy_policy,
    updateResourcePolicyResponse_revisionId,
    updateResourcePolicyResponse_resourceArn,
    updateResourcePolicyResponse_httpStatus,

    -- ** UpdateSlot
    updateSlot_multipleValuesSetting,
    updateSlot_description,
    updateSlot_obfuscationSetting,
    updateSlot_subSlotSetting,
    updateSlot_slotTypeId,
    updateSlot_slotId,
    updateSlot_slotName,
    updateSlot_valueElicitationSetting,
    updateSlot_botId,
    updateSlot_botVersion,
    updateSlot_localeId,
    updateSlot_intentId,
    updateSlotResponse_multipleValuesSetting,
    updateSlotResponse_slotName,
    updateSlotResponse_valueElicitationSetting,
    updateSlotResponse_botVersion,
    updateSlotResponse_creationDateTime,
    updateSlotResponse_localeId,
    updateSlotResponse_description,
    updateSlotResponse_botId,
    updateSlotResponse_intentId,
    updateSlotResponse_slotId,
    updateSlotResponse_obfuscationSetting,
    updateSlotResponse_subSlotSetting,
    updateSlotResponse_slotTypeId,
    updateSlotResponse_lastUpdatedDateTime,
    updateSlotResponse_httpStatus,

    -- ** UpdateSlotType
    updateSlotType_compositeSlotTypeSetting,
    updateSlotType_externalSourceSetting,
    updateSlotType_valueSelectionSetting,
    updateSlotType_description,
    updateSlotType_slotTypeValues,
    updateSlotType_parentSlotTypeSignature,
    updateSlotType_slotTypeId,
    updateSlotType_slotTypeName,
    updateSlotType_botId,
    updateSlotType_botVersion,
    updateSlotType_localeId,
    updateSlotTypeResponse_botVersion,
    updateSlotTypeResponse_compositeSlotTypeSetting,
    updateSlotTypeResponse_creationDateTime,
    updateSlotTypeResponse_localeId,
    updateSlotTypeResponse_externalSourceSetting,
    updateSlotTypeResponse_valueSelectionSetting,
    updateSlotTypeResponse_description,
    updateSlotTypeResponse_botId,
    updateSlotTypeResponse_slotTypeValues,
    updateSlotTypeResponse_slotTypeName,
    updateSlotTypeResponse_slotTypeId,
    updateSlotTypeResponse_parentSlotTypeSignature,
    updateSlotTypeResponse_lastUpdatedDateTime,
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
    aggregatedUtterancesSummary_utteranceLastRecordedInAggregationDuration,
    aggregatedUtterancesSummary_containsDataFromDeletedResources,
    aggregatedUtterancesSummary_utterance,
    aggregatedUtterancesSummary_utteranceFirstRecordedInAggregationDuration,
    aggregatedUtterancesSummary_missedCount,
    aggregatedUtterancesSummary_hitCount,

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
    botAliasSummary_botAliasStatus,
    botAliasSummary_botVersion,
    botAliasSummary_creationDateTime,
    botAliasSummary_description,
    botAliasSummary_botAliasId,
    botAliasSummary_botAliasName,
    botAliasSummary_lastUpdatedDateTime,

    -- ** BotExportSpecification
    botExportSpecification_botId,
    botExportSpecification_botVersion,

    -- ** BotFilter
    botFilter_name,
    botFilter_values,
    botFilter_operator,

    -- ** BotImportSpecification
    botImportSpecification_idleSessionTTLInSeconds,
    botImportSpecification_botTags,
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
    botLocaleSummary_localeName,
    botLocaleSummary_localeId,
    botLocaleSummary_description,
    botLocaleSummary_lastBuildSubmittedDateTime,
    botLocaleSummary_botLocaleStatus,
    botLocaleSummary_lastUpdatedDateTime,

    -- ** BotRecommendationResultStatistics
    botRecommendationResultStatistics_slotTypes,
    botRecommendationResultStatistics_intents,

    -- ** BotRecommendationResults
    botRecommendationResults_botLocaleExportUrl,
    botRecommendationResults_statistics,
    botRecommendationResults_associatedTranscriptsUrl,

    -- ** BotRecommendationSummary
    botRecommendationSummary_creationDateTime,
    botRecommendationSummary_lastUpdatedDateTime,
    botRecommendationSummary_botRecommendationStatus,
    botRecommendationSummary_botRecommendationId,

    -- ** BotSortBy
    botSortBy_attribute,
    botSortBy_order,

    -- ** BotSummary
    botSummary_description,
    botSummary_botId,
    botSummary_botName,
    botSummary_latestBotVersion,
    botSummary_botStatus,
    botSummary_lastUpdatedDateTime,

    -- ** BotVersionLocaleDetails
    botVersionLocaleDetails_sourceBotVersion,

    -- ** BotVersionSortBy
    botVersionSortBy_attribute,
    botVersionSortBy_order,

    -- ** BotVersionSummary
    botVersionSummary_botVersion,
    botVersionSummary_creationDateTime,
    botVersionSummary_description,
    botVersionSummary_botName,
    botVersionSummary_botStatus,

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
    defaultConditionalBranch_response,
    defaultConditionalBranch_nextStep,

    -- ** DialogAction
    dialogAction_suppressNextMessage,
    dialogAction_slotToElicit,
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
    dialogState_sessionAttributes,
    dialogState_intent,

    -- ** ElicitationCodeHookInvocationSetting
    elicitationCodeHookInvocationSetting_invocationLabel,
    elicitationCodeHookInvocationSetting_enableCodeHookInvocation,

    -- ** EncryptionSetting
    encryptionSetting_kmsKeyArn,
    encryptionSetting_associatedTranscriptsPassword,
    encryptionSetting_botLocaleExportPassword,

    -- ** ExportFilter
    exportFilter_name,
    exportFilter_values,
    exportFilter_operator,

    -- ** ExportResourceSpecification
    exportResourceSpecification_botLocaleExportSpecification,
    exportResourceSpecification_botExportSpecification,
    exportResourceSpecification_customVocabularyExportSpecification,

    -- ** ExportSortBy
    exportSortBy_attribute,
    exportSortBy_order,

    -- ** ExportSummary
    exportSummary_creationDateTime,
    exportSummary_resourceSpecification,
    exportSummary_exportStatus,
    exportSummary_exportId,
    exportSummary_lastUpdatedDateTime,
    exportSummary_fileFormat,

    -- ** ExternalSourceSetting
    externalSourceSetting_grammarSlotTypeSetting,

    -- ** FailedCustomVocabularyItem
    failedCustomVocabularyItem_errorMessage,
    failedCustomVocabularyItem_errorCode,
    failedCustomVocabularyItem_itemId,

    -- ** FulfillmentCodeHookSettings
    fulfillmentCodeHookSettings_active,
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

    -- ** GrammarSlotTypeSetting
    grammarSlotTypeSetting_source,

    -- ** GrammarSlotTypeSource
    grammarSlotTypeSource_kmsKeyArn,
    grammarSlotTypeSource_s3BucketName,
    grammarSlotTypeSource_s3ObjectKey,

    -- ** ImageResponseCard
    imageResponseCard_subtitle,
    imageResponseCard_imageUrl,
    imageResponseCard_buttons,
    imageResponseCard_title,

    -- ** ImportFilter
    importFilter_name,
    importFilter_values,
    importFilter_operator,

    -- ** ImportResourceSpecification
    importResourceSpecification_botLocaleImportSpecification,
    importResourceSpecification_botImportSpecification,
    importResourceSpecification_customVocabularyImportSpecification,

    -- ** ImportSortBy
    importSortBy_attribute,
    importSortBy_order,

    -- ** ImportSummary
    importSummary_creationDateTime,
    importSummary_importedResourceName,
    importSummary_importId,
    importSummary_importStatus,
    importSummary_importedResourceId,
    importSummary_importedResourceType,
    importSummary_lastUpdatedDateTime,
    importSummary_mergeStrategy,

    -- ** InitialResponseSetting
    initialResponseSetting_initialResponse,
    initialResponseSetting_codeHook,
    initialResponseSetting_nextStep,
    initialResponseSetting_conditional,

    -- ** InputContext
    inputContext_name,

    -- ** IntentClosingSetting
    intentClosingSetting_active,
    intentClosingSetting_closingResponse,
    intentClosingSetting_nextStep,
    intentClosingSetting_conditional,

    -- ** IntentConfirmationSetting
    intentConfirmationSetting_active,
    intentConfirmationSetting_failureNextStep,
    intentConfirmationSetting_codeHook,
    intentConfirmationSetting_elicitationCodeHook,
    intentConfirmationSetting_declinationResponse,
    intentConfirmationSetting_declinationConditional,
    intentConfirmationSetting_failureConditional,
    intentConfirmationSetting_confirmationResponse,
    intentConfirmationSetting_confirmationNextStep,
    intentConfirmationSetting_failureResponse,
    intentConfirmationSetting_confirmationConditional,
    intentConfirmationSetting_declinationNextStep,
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
    intentSummary_outputContexts,
    intentSummary_parentIntentSignature,
    intentSummary_description,
    intentSummary_intentId,
    intentSummary_intentName,
    intentSummary_inputContexts,
    intentSummary_lastUpdatedDateTime,

    -- ** KendraConfiguration
    kendraConfiguration_queryFilterStringEnabled,
    kendraConfiguration_queryFilterString,
    kendraConfiguration_kendraIndex,

    -- ** LambdaCodeHook
    lambdaCodeHook_lambdaARN,
    lambdaCodeHook_codeHookInterfaceVersion,

    -- ** LexTranscriptFilter
    lexTranscriptFilter_dateRangeFilter,

    -- ** Message
    message_ssmlMessage,
    message_imageResponseCard,
    message_customPayload,
    message_plainTextMessage,

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
    postDialogCodeHookInvocationSpecification_timeoutNextStep,
    postDialogCodeHookInvocationSpecification_timeoutConditional,
    postDialogCodeHookInvocationSpecification_failureNextStep,
    postDialogCodeHookInvocationSpecification_timeoutResponse,
    postDialogCodeHookInvocationSpecification_successNextStep,
    postDialogCodeHookInvocationSpecification_successConditional,
    postDialogCodeHookInvocationSpecification_successResponse,
    postDialogCodeHookInvocationSpecification_failureConditional,
    postDialogCodeHookInvocationSpecification_failureResponse,

    -- ** PostFulfillmentStatusSpecification
    postFulfillmentStatusSpecification_timeoutNextStep,
    postFulfillmentStatusSpecification_timeoutConditional,
    postFulfillmentStatusSpecification_failureNextStep,
    postFulfillmentStatusSpecification_timeoutResponse,
    postFulfillmentStatusSpecification_successNextStep,
    postFulfillmentStatusSpecification_successConditional,
    postFulfillmentStatusSpecification_successResponse,
    postFulfillmentStatusSpecification_failureConditional,
    postFulfillmentStatusSpecification_failureResponse,

    -- ** Principal
    principal_arn,
    principal_service,

    -- ** PromptAttemptSpecification
    promptAttemptSpecification_allowInterrupt,
    promptAttemptSpecification_textInputSpecification,
    promptAttemptSpecification_audioAndDTMFInputSpecification,
    promptAttemptSpecification_allowedInputTypes,

    -- ** PromptSpecification
    promptSpecification_allowInterrupt,
    promptSpecification_promptAttemptsSpecification,
    promptSpecification_messageSelectionStrategy,
    promptSpecification_messageGroups,
    promptSpecification_maxRetries,

    -- ** RecommendedIntentSummary
    recommendedIntentSummary_sampleUtterancesCount,
    recommendedIntentSummary_intentId,
    recommendedIntentSummary_intentName,

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
    s3BucketTranscriptSource_transcriptFilter,
    s3BucketTranscriptSource_kmsKeyArn,
    s3BucketTranscriptSource_pathFormat,
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
    slotCaptureSetting_captureResponse,
    slotCaptureSetting_failureNextStep,
    slotCaptureSetting_codeHook,
    slotCaptureSetting_elicitationCodeHook,
    slotCaptureSetting_captureConditional,
    slotCaptureSetting_failureConditional,
    slotCaptureSetting_failureResponse,
    slotCaptureSetting_captureNextStep,

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
    slotSummary_description,
    slotSummary_slotConstraint,
    slotSummary_slotId,
    slotSummary_valueElicitationPromptSpecification,
    slotSummary_slotTypeId,
    slotSummary_lastUpdatedDateTime,

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
    slotTypeSummary_slotTypeCategory,
    slotTypeSummary_description,
    slotTypeSummary_slotTypeName,
    slotTypeSummary_slotTypeId,
    slotTypeSummary_parentSlotTypeSignature,
    slotTypeSummary_lastUpdatedDateTime,

    -- ** SlotTypeValue
    slotTypeValue_sampleValue,
    slotTypeValue_synonyms,

    -- ** SlotValue
    slotValue_interpretedValue,

    -- ** SlotValueElicitationSetting
    slotValueElicitationSetting_sampleUtterances,
    slotValueElicitationSetting_waitAndContinueSpecification,
    slotValueElicitationSetting_promptSpecification,
    slotValueElicitationSetting_defaultValueSpecification,
    slotValueElicitationSetting_slotCaptureSetting,
    slotValueElicitationSetting_slotConstraint,

    -- ** SlotValueOverride
    slotValueOverride_shape,
    slotValueOverride_values,
    slotValueOverride_value,

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
    subSlotValueElicitationSetting_sampleUtterances,
    subSlotValueElicitationSetting_waitAndContinueSpecification,
    subSlotValueElicitationSetting_defaultValueSpecification,
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
