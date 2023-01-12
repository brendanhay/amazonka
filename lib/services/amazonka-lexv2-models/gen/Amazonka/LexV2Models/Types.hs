{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _PreconditionFailedException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AggregatedUtterancesFilterName
    AggregatedUtterancesFilterName (..),

    -- * AggregatedUtterancesFilterOperator
    AggregatedUtterancesFilterOperator (..),

    -- * AggregatedUtterancesSortAttribute
    AggregatedUtterancesSortAttribute (..),

    -- * AssociatedTranscriptFilterName
    AssociatedTranscriptFilterName (..),

    -- * AudioRecognitionStrategy
    AudioRecognitionStrategy (..),

    -- * BotAliasStatus
    BotAliasStatus (..),

    -- * BotFilterName
    BotFilterName (..),

    -- * BotFilterOperator
    BotFilterOperator (..),

    -- * BotLocaleFilterName
    BotLocaleFilterName (..),

    -- * BotLocaleFilterOperator
    BotLocaleFilterOperator (..),

    -- * BotLocaleSortAttribute
    BotLocaleSortAttribute (..),

    -- * BotLocaleStatus
    BotLocaleStatus (..),

    -- * BotRecommendationStatus
    BotRecommendationStatus (..),

    -- * BotSortAttribute
    BotSortAttribute (..),

    -- * BotStatus
    BotStatus (..),

    -- * BotVersionSortAttribute
    BotVersionSortAttribute (..),

    -- * BuiltInIntentSortAttribute
    BuiltInIntentSortAttribute (..),

    -- * BuiltInSlotTypeSortAttribute
    BuiltInSlotTypeSortAttribute (..),

    -- * CustomVocabularyStatus
    CustomVocabularyStatus (..),

    -- * DialogActionType
    DialogActionType (..),

    -- * Effect
    Effect (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ExportFilterName
    ExportFilterName (..),

    -- * ExportFilterOperator
    ExportFilterOperator (..),

    -- * ExportSortAttribute
    ExportSortAttribute (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * ImportExportFileFormat
    ImportExportFileFormat (..),

    -- * ImportFilterName
    ImportFilterName (..),

    -- * ImportFilterOperator
    ImportFilterOperator (..),

    -- * ImportResourceType
    ImportResourceType (..),

    -- * ImportSortAttribute
    ImportSortAttribute (..),

    -- * ImportStatus
    ImportStatus (..),

    -- * IntentFilterName
    IntentFilterName (..),

    -- * IntentFilterOperator
    IntentFilterOperator (..),

    -- * IntentSortAttribute
    IntentSortAttribute (..),

    -- * MergeStrategy
    MergeStrategy (..),

    -- * MessageSelectionStrategy
    MessageSelectionStrategy (..),

    -- * ObfuscationSettingType
    ObfuscationSettingType (..),

    -- * PromptAttempt
    PromptAttempt (..),

    -- * SearchOrder
    SearchOrder (..),

    -- * SlotConstraint
    SlotConstraint (..),

    -- * SlotFilterName
    SlotFilterName (..),

    -- * SlotFilterOperator
    SlotFilterOperator (..),

    -- * SlotShape
    SlotShape (..),

    -- * SlotSortAttribute
    SlotSortAttribute (..),

    -- * SlotTypeCategory
    SlotTypeCategory (..),

    -- * SlotTypeFilterName
    SlotTypeFilterName (..),

    -- * SlotTypeFilterOperator
    SlotTypeFilterOperator (..),

    -- * SlotTypeSortAttribute
    SlotTypeSortAttribute (..),

    -- * SlotValueResolutionStrategy
    SlotValueResolutionStrategy (..),

    -- * SortOrder
    SortOrder (..),

    -- * TimeDimension
    TimeDimension (..),

    -- * TranscriptFormat
    TranscriptFormat (..),

    -- * VoiceEngine
    VoiceEngine (..),

    -- * AdvancedRecognitionSetting
    AdvancedRecognitionSetting (..),
    newAdvancedRecognitionSetting,
    advancedRecognitionSetting_audioRecognitionStrategy,

    -- * AggregatedUtterancesFilter
    AggregatedUtterancesFilter (..),
    newAggregatedUtterancesFilter,
    aggregatedUtterancesFilter_name,
    aggregatedUtterancesFilter_values,
    aggregatedUtterancesFilter_operator,

    -- * AggregatedUtterancesSortBy
    AggregatedUtterancesSortBy (..),
    newAggregatedUtterancesSortBy,
    aggregatedUtterancesSortBy_attribute,
    aggregatedUtterancesSortBy_order,

    -- * AggregatedUtterancesSummary
    AggregatedUtterancesSummary (..),
    newAggregatedUtterancesSummary,
    aggregatedUtterancesSummary_containsDataFromDeletedResources,
    aggregatedUtterancesSummary_hitCount,
    aggregatedUtterancesSummary_missedCount,
    aggregatedUtterancesSummary_utterance,
    aggregatedUtterancesSummary_utteranceFirstRecordedInAggregationDuration,
    aggregatedUtterancesSummary_utteranceLastRecordedInAggregationDuration,

    -- * AllowedInputTypes
    AllowedInputTypes (..),
    newAllowedInputTypes,
    allowedInputTypes_allowAudioInput,
    allowedInputTypes_allowDTMFInput,

    -- * AssociatedTranscript
    AssociatedTranscript (..),
    newAssociatedTranscript,
    associatedTranscript_transcript,

    -- * AssociatedTranscriptFilter
    AssociatedTranscriptFilter (..),
    newAssociatedTranscriptFilter,
    associatedTranscriptFilter_name,
    associatedTranscriptFilter_values,

    -- * AudioAndDTMFInputSpecification
    AudioAndDTMFInputSpecification (..),
    newAudioAndDTMFInputSpecification,
    audioAndDTMFInputSpecification_audioSpecification,
    audioAndDTMFInputSpecification_dtmfSpecification,
    audioAndDTMFInputSpecification_startTimeoutMs,

    -- * AudioLogDestination
    AudioLogDestination (..),
    newAudioLogDestination,
    audioLogDestination_s3Bucket,

    -- * AudioLogSetting
    AudioLogSetting (..),
    newAudioLogSetting,
    audioLogSetting_enabled,
    audioLogSetting_destination,

    -- * AudioSpecification
    AudioSpecification (..),
    newAudioSpecification,
    audioSpecification_maxLengthMs,
    audioSpecification_endTimeoutMs,

    -- * BotAliasHistoryEvent
    BotAliasHistoryEvent (..),
    newBotAliasHistoryEvent,
    botAliasHistoryEvent_botVersion,
    botAliasHistoryEvent_endDate,
    botAliasHistoryEvent_startDate,

    -- * BotAliasLocaleSettings
    BotAliasLocaleSettings (..),
    newBotAliasLocaleSettings,
    botAliasLocaleSettings_codeHookSpecification,
    botAliasLocaleSettings_enabled,

    -- * BotAliasSummary
    BotAliasSummary (..),
    newBotAliasSummary,
    botAliasSummary_botAliasId,
    botAliasSummary_botAliasName,
    botAliasSummary_botAliasStatus,
    botAliasSummary_botVersion,
    botAliasSummary_creationDateTime,
    botAliasSummary_description,
    botAliasSummary_lastUpdatedDateTime,

    -- * BotExportSpecification
    BotExportSpecification (..),
    newBotExportSpecification,
    botExportSpecification_botId,
    botExportSpecification_botVersion,

    -- * BotFilter
    BotFilter (..),
    newBotFilter,
    botFilter_name,
    botFilter_values,
    botFilter_operator,

    -- * BotImportSpecification
    BotImportSpecification (..),
    newBotImportSpecification,
    botImportSpecification_botTags,
    botImportSpecification_idleSessionTTLInSeconds,
    botImportSpecification_testBotAliasTags,
    botImportSpecification_botName,
    botImportSpecification_roleArn,
    botImportSpecification_dataPrivacy,

    -- * BotLocaleExportSpecification
    BotLocaleExportSpecification (..),
    newBotLocaleExportSpecification,
    botLocaleExportSpecification_botId,
    botLocaleExportSpecification_botVersion,
    botLocaleExportSpecification_localeId,

    -- * BotLocaleFilter
    BotLocaleFilter (..),
    newBotLocaleFilter,
    botLocaleFilter_name,
    botLocaleFilter_values,
    botLocaleFilter_operator,

    -- * BotLocaleHistoryEvent
    BotLocaleHistoryEvent (..),
    newBotLocaleHistoryEvent,
    botLocaleHistoryEvent_event,
    botLocaleHistoryEvent_eventDate,

    -- * BotLocaleImportSpecification
    BotLocaleImportSpecification (..),
    newBotLocaleImportSpecification,
    botLocaleImportSpecification_nluIntentConfidenceThreshold,
    botLocaleImportSpecification_voiceSettings,
    botLocaleImportSpecification_botId,
    botLocaleImportSpecification_botVersion,
    botLocaleImportSpecification_localeId,

    -- * BotLocaleSortBy
    BotLocaleSortBy (..),
    newBotLocaleSortBy,
    botLocaleSortBy_attribute,
    botLocaleSortBy_order,

    -- * BotLocaleSummary
    BotLocaleSummary (..),
    newBotLocaleSummary,
    botLocaleSummary_botLocaleStatus,
    botLocaleSummary_description,
    botLocaleSummary_lastBuildSubmittedDateTime,
    botLocaleSummary_lastUpdatedDateTime,
    botLocaleSummary_localeId,
    botLocaleSummary_localeName,

    -- * BotRecommendationResultStatistics
    BotRecommendationResultStatistics (..),
    newBotRecommendationResultStatistics,
    botRecommendationResultStatistics_intents,
    botRecommendationResultStatistics_slotTypes,

    -- * BotRecommendationResults
    BotRecommendationResults (..),
    newBotRecommendationResults,
    botRecommendationResults_associatedTranscriptsUrl,
    botRecommendationResults_botLocaleExportUrl,
    botRecommendationResults_statistics,

    -- * BotRecommendationSummary
    BotRecommendationSummary (..),
    newBotRecommendationSummary,
    botRecommendationSummary_creationDateTime,
    botRecommendationSummary_lastUpdatedDateTime,
    botRecommendationSummary_botRecommendationStatus,
    botRecommendationSummary_botRecommendationId,

    -- * BotSortBy
    BotSortBy (..),
    newBotSortBy,
    botSortBy_attribute,
    botSortBy_order,

    -- * BotSummary
    BotSummary (..),
    newBotSummary,
    botSummary_botId,
    botSummary_botName,
    botSummary_botStatus,
    botSummary_description,
    botSummary_lastUpdatedDateTime,
    botSummary_latestBotVersion,

    -- * BotVersionLocaleDetails
    BotVersionLocaleDetails (..),
    newBotVersionLocaleDetails,
    botVersionLocaleDetails_sourceBotVersion,

    -- * BotVersionSortBy
    BotVersionSortBy (..),
    newBotVersionSortBy,
    botVersionSortBy_attribute,
    botVersionSortBy_order,

    -- * BotVersionSummary
    BotVersionSummary (..),
    newBotVersionSummary,
    botVersionSummary_botName,
    botVersionSummary_botStatus,
    botVersionSummary_botVersion,
    botVersionSummary_creationDateTime,
    botVersionSummary_description,

    -- * BuiltInIntentSortBy
    BuiltInIntentSortBy (..),
    newBuiltInIntentSortBy,
    builtInIntentSortBy_attribute,
    builtInIntentSortBy_order,

    -- * BuiltInIntentSummary
    BuiltInIntentSummary (..),
    newBuiltInIntentSummary,
    builtInIntentSummary_description,
    builtInIntentSummary_intentSignature,

    -- * BuiltInSlotTypeSortBy
    BuiltInSlotTypeSortBy (..),
    newBuiltInSlotTypeSortBy,
    builtInSlotTypeSortBy_attribute,
    builtInSlotTypeSortBy_order,

    -- * BuiltInSlotTypeSummary
    BuiltInSlotTypeSummary (..),
    newBuiltInSlotTypeSummary,
    builtInSlotTypeSummary_description,
    builtInSlotTypeSummary_slotTypeSignature,

    -- * Button
    Button (..),
    newButton,
    button_text,
    button_value,

    -- * CloudWatchLogGroupLogDestination
    CloudWatchLogGroupLogDestination (..),
    newCloudWatchLogGroupLogDestination,
    cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn,
    cloudWatchLogGroupLogDestination_logPrefix,

    -- * CodeHookSpecification
    CodeHookSpecification (..),
    newCodeHookSpecification,
    codeHookSpecification_lambdaCodeHook,

    -- * CompositeSlotTypeSetting
    CompositeSlotTypeSetting (..),
    newCompositeSlotTypeSetting,
    compositeSlotTypeSetting_subSlots,

    -- * Condition
    Condition (..),
    newCondition,
    condition_expressionString,

    -- * ConditionalBranch
    ConditionalBranch (..),
    newConditionalBranch,
    conditionalBranch_response,
    conditionalBranch_name,
    conditionalBranch_condition,
    conditionalBranch_nextStep,

    -- * ConditionalSpecification
    ConditionalSpecification (..),
    newConditionalSpecification,
    conditionalSpecification_active,
    conditionalSpecification_conditionalBranches,
    conditionalSpecification_defaultBranch,

    -- * ConversationLogSettings
    ConversationLogSettings (..),
    newConversationLogSettings,
    conversationLogSettings_audioLogSettings,
    conversationLogSettings_textLogSettings,

    -- * CustomPayload
    CustomPayload (..),
    newCustomPayload,
    customPayload_value,

    -- * CustomVocabularyEntryId
    CustomVocabularyEntryId (..),
    newCustomVocabularyEntryId,
    customVocabularyEntryId_itemId,

    -- * CustomVocabularyExportSpecification
    CustomVocabularyExportSpecification (..),
    newCustomVocabularyExportSpecification,
    customVocabularyExportSpecification_botId,
    customVocabularyExportSpecification_botVersion,
    customVocabularyExportSpecification_localeId,

    -- * CustomVocabularyImportSpecification
    CustomVocabularyImportSpecification (..),
    newCustomVocabularyImportSpecification,
    customVocabularyImportSpecification_botId,
    customVocabularyImportSpecification_botVersion,
    customVocabularyImportSpecification_localeId,

    -- * CustomVocabularyItem
    CustomVocabularyItem (..),
    newCustomVocabularyItem,
    customVocabularyItem_displayAs,
    customVocabularyItem_weight,
    customVocabularyItem_itemId,
    customVocabularyItem_phrase,

    -- * DTMFSpecification
    DTMFSpecification (..),
    newDTMFSpecification,
    dTMFSpecification_maxLength,
    dTMFSpecification_endTimeoutMs,
    dTMFSpecification_deletionCharacter,
    dTMFSpecification_endCharacter,

    -- * DataPrivacy
    DataPrivacy (..),
    newDataPrivacy,
    dataPrivacy_childDirected,

    -- * DateRangeFilter
    DateRangeFilter (..),
    newDateRangeFilter,
    dateRangeFilter_startDateTime,
    dateRangeFilter_endDateTime,

    -- * DefaultConditionalBranch
    DefaultConditionalBranch (..),
    newDefaultConditionalBranch,
    defaultConditionalBranch_nextStep,
    defaultConditionalBranch_response,

    -- * DialogAction
    DialogAction (..),
    newDialogAction,
    dialogAction_slotToElicit,
    dialogAction_suppressNextMessage,
    dialogAction_type,

    -- * DialogCodeHookInvocationSetting
    DialogCodeHookInvocationSetting (..),
    newDialogCodeHookInvocationSetting,
    dialogCodeHookInvocationSetting_invocationLabel,
    dialogCodeHookInvocationSetting_enableCodeHookInvocation,
    dialogCodeHookInvocationSetting_active,
    dialogCodeHookInvocationSetting_postCodeHookSpecification,

    -- * DialogCodeHookSettings
    DialogCodeHookSettings (..),
    newDialogCodeHookSettings,
    dialogCodeHookSettings_enabled,

    -- * DialogState
    DialogState (..),
    newDialogState,
    dialogState_dialogAction,
    dialogState_intent,
    dialogState_sessionAttributes,

    -- * ElicitationCodeHookInvocationSetting
    ElicitationCodeHookInvocationSetting (..),
    newElicitationCodeHookInvocationSetting,
    elicitationCodeHookInvocationSetting_invocationLabel,
    elicitationCodeHookInvocationSetting_enableCodeHookInvocation,

    -- * EncryptionSetting
    EncryptionSetting (..),
    newEncryptionSetting,
    encryptionSetting_associatedTranscriptsPassword,
    encryptionSetting_botLocaleExportPassword,
    encryptionSetting_kmsKeyArn,

    -- * ExportFilter
    ExportFilter (..),
    newExportFilter,
    exportFilter_name,
    exportFilter_values,
    exportFilter_operator,

    -- * ExportResourceSpecification
    ExportResourceSpecification (..),
    newExportResourceSpecification,
    exportResourceSpecification_botExportSpecification,
    exportResourceSpecification_botLocaleExportSpecification,
    exportResourceSpecification_customVocabularyExportSpecification,

    -- * ExportSortBy
    ExportSortBy (..),
    newExportSortBy,
    exportSortBy_attribute,
    exportSortBy_order,

    -- * ExportSummary
    ExportSummary (..),
    newExportSummary,
    exportSummary_creationDateTime,
    exportSummary_exportId,
    exportSummary_exportStatus,
    exportSummary_fileFormat,
    exportSummary_lastUpdatedDateTime,
    exportSummary_resourceSpecification,

    -- * ExternalSourceSetting
    ExternalSourceSetting (..),
    newExternalSourceSetting,
    externalSourceSetting_grammarSlotTypeSetting,

    -- * FailedCustomVocabularyItem
    FailedCustomVocabularyItem (..),
    newFailedCustomVocabularyItem,
    failedCustomVocabularyItem_errorCode,
    failedCustomVocabularyItem_errorMessage,
    failedCustomVocabularyItem_itemId,

    -- * FulfillmentCodeHookSettings
    FulfillmentCodeHookSettings (..),
    newFulfillmentCodeHookSettings,
    fulfillmentCodeHookSettings_active,
    fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification,
    fulfillmentCodeHookSettings_postFulfillmentStatusSpecification,
    fulfillmentCodeHookSettings_enabled,

    -- * FulfillmentStartResponseSpecification
    FulfillmentStartResponseSpecification (..),
    newFulfillmentStartResponseSpecification,
    fulfillmentStartResponseSpecification_allowInterrupt,
    fulfillmentStartResponseSpecification_delayInSeconds,
    fulfillmentStartResponseSpecification_messageGroups,

    -- * FulfillmentUpdateResponseSpecification
    FulfillmentUpdateResponseSpecification (..),
    newFulfillmentUpdateResponseSpecification,
    fulfillmentUpdateResponseSpecification_allowInterrupt,
    fulfillmentUpdateResponseSpecification_frequencyInSeconds,
    fulfillmentUpdateResponseSpecification_messageGroups,

    -- * FulfillmentUpdatesSpecification
    FulfillmentUpdatesSpecification (..),
    newFulfillmentUpdatesSpecification,
    fulfillmentUpdatesSpecification_startResponse,
    fulfillmentUpdatesSpecification_timeoutInSeconds,
    fulfillmentUpdatesSpecification_updateResponse,
    fulfillmentUpdatesSpecification_active,

    -- * GrammarSlotTypeSetting
    GrammarSlotTypeSetting (..),
    newGrammarSlotTypeSetting,
    grammarSlotTypeSetting_source,

    -- * GrammarSlotTypeSource
    GrammarSlotTypeSource (..),
    newGrammarSlotTypeSource,
    grammarSlotTypeSource_kmsKeyArn,
    grammarSlotTypeSource_s3BucketName,
    grammarSlotTypeSource_s3ObjectKey,

    -- * ImageResponseCard
    ImageResponseCard (..),
    newImageResponseCard,
    imageResponseCard_buttons,
    imageResponseCard_imageUrl,
    imageResponseCard_subtitle,
    imageResponseCard_title,

    -- * ImportFilter
    ImportFilter (..),
    newImportFilter,
    importFilter_name,
    importFilter_values,
    importFilter_operator,

    -- * ImportResourceSpecification
    ImportResourceSpecification (..),
    newImportResourceSpecification,
    importResourceSpecification_botImportSpecification,
    importResourceSpecification_botLocaleImportSpecification,
    importResourceSpecification_customVocabularyImportSpecification,

    -- * ImportSortBy
    ImportSortBy (..),
    newImportSortBy,
    importSortBy_attribute,
    importSortBy_order,

    -- * ImportSummary
    ImportSummary (..),
    newImportSummary,
    importSummary_creationDateTime,
    importSummary_importId,
    importSummary_importStatus,
    importSummary_importedResourceId,
    importSummary_importedResourceName,
    importSummary_importedResourceType,
    importSummary_lastUpdatedDateTime,
    importSummary_mergeStrategy,

    -- * InitialResponseSetting
    InitialResponseSetting (..),
    newInitialResponseSetting,
    initialResponseSetting_codeHook,
    initialResponseSetting_conditional,
    initialResponseSetting_initialResponse,
    initialResponseSetting_nextStep,

    -- * InputContext
    InputContext (..),
    newInputContext,
    inputContext_name,

    -- * IntentClosingSetting
    IntentClosingSetting (..),
    newIntentClosingSetting,
    intentClosingSetting_active,
    intentClosingSetting_closingResponse,
    intentClosingSetting_conditional,
    intentClosingSetting_nextStep,

    -- * IntentConfirmationSetting
    IntentConfirmationSetting (..),
    newIntentConfirmationSetting,
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

    -- * IntentFilter
    IntentFilter (..),
    newIntentFilter,
    intentFilter_name,
    intentFilter_values,
    intentFilter_operator,

    -- * IntentOverride
    IntentOverride (..),
    newIntentOverride,
    intentOverride_name,
    intentOverride_slots,

    -- * IntentSortBy
    IntentSortBy (..),
    newIntentSortBy,
    intentSortBy_attribute,
    intentSortBy_order,

    -- * IntentStatistics
    IntentStatistics (..),
    newIntentStatistics,
    intentStatistics_discoveredIntentCount,

    -- * IntentSummary
    IntentSummary (..),
    newIntentSummary,
    intentSummary_description,
    intentSummary_inputContexts,
    intentSummary_intentId,
    intentSummary_intentName,
    intentSummary_lastUpdatedDateTime,
    intentSummary_outputContexts,
    intentSummary_parentIntentSignature,

    -- * KendraConfiguration
    KendraConfiguration (..),
    newKendraConfiguration,
    kendraConfiguration_queryFilterString,
    kendraConfiguration_queryFilterStringEnabled,
    kendraConfiguration_kendraIndex,

    -- * LambdaCodeHook
    LambdaCodeHook (..),
    newLambdaCodeHook,
    lambdaCodeHook_lambdaARN,
    lambdaCodeHook_codeHookInterfaceVersion,

    -- * LexTranscriptFilter
    LexTranscriptFilter (..),
    newLexTranscriptFilter,
    lexTranscriptFilter_dateRangeFilter,

    -- * Message
    Message (..),
    newMessage,
    message_customPayload,
    message_imageResponseCard,
    message_plainTextMessage,
    message_ssmlMessage,

    -- * MessageGroup
    MessageGroup (..),
    newMessageGroup,
    messageGroup_variations,
    messageGroup_message,

    -- * MultipleValuesSetting
    MultipleValuesSetting (..),
    newMultipleValuesSetting,
    multipleValuesSetting_allowMultipleValues,

    -- * NewCustomVocabularyItem
    NewCustomVocabularyItem (..),
    newNewCustomVocabularyItem,
    newCustomVocabularyItem_displayAs,
    newCustomVocabularyItem_weight,
    newCustomVocabularyItem_phrase,

    -- * ObfuscationSetting
    ObfuscationSetting (..),
    newObfuscationSetting,
    obfuscationSetting_obfuscationSettingType,

    -- * OutputContext
    OutputContext (..),
    newOutputContext,
    outputContext_name,
    outputContext_timeToLiveInSeconds,
    outputContext_turnsToLive,

    -- * PathFormat
    PathFormat (..),
    newPathFormat,
    pathFormat_objectPrefixes,

    -- * PlainTextMessage
    PlainTextMessage (..),
    newPlainTextMessage,
    plainTextMessage_value,

    -- * PostDialogCodeHookInvocationSpecification
    PostDialogCodeHookInvocationSpecification (..),
    newPostDialogCodeHookInvocationSpecification,
    postDialogCodeHookInvocationSpecification_failureConditional,
    postDialogCodeHookInvocationSpecification_failureNextStep,
    postDialogCodeHookInvocationSpecification_failureResponse,
    postDialogCodeHookInvocationSpecification_successConditional,
    postDialogCodeHookInvocationSpecification_successNextStep,
    postDialogCodeHookInvocationSpecification_successResponse,
    postDialogCodeHookInvocationSpecification_timeoutConditional,
    postDialogCodeHookInvocationSpecification_timeoutNextStep,
    postDialogCodeHookInvocationSpecification_timeoutResponse,

    -- * PostFulfillmentStatusSpecification
    PostFulfillmentStatusSpecification (..),
    newPostFulfillmentStatusSpecification,
    postFulfillmentStatusSpecification_failureConditional,
    postFulfillmentStatusSpecification_failureNextStep,
    postFulfillmentStatusSpecification_failureResponse,
    postFulfillmentStatusSpecification_successConditional,
    postFulfillmentStatusSpecification_successNextStep,
    postFulfillmentStatusSpecification_successResponse,
    postFulfillmentStatusSpecification_timeoutConditional,
    postFulfillmentStatusSpecification_timeoutNextStep,
    postFulfillmentStatusSpecification_timeoutResponse,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_arn,
    principal_service,

    -- * PromptAttemptSpecification
    PromptAttemptSpecification (..),
    newPromptAttemptSpecification,
    promptAttemptSpecification_allowInterrupt,
    promptAttemptSpecification_audioAndDTMFInputSpecification,
    promptAttemptSpecification_textInputSpecification,
    promptAttemptSpecification_allowedInputTypes,

    -- * PromptSpecification
    PromptSpecification (..),
    newPromptSpecification,
    promptSpecification_allowInterrupt,
    promptSpecification_messageSelectionStrategy,
    promptSpecification_promptAttemptsSpecification,
    promptSpecification_messageGroups,
    promptSpecification_maxRetries,

    -- * RecommendedIntentSummary
    RecommendedIntentSummary (..),
    newRecommendedIntentSummary,
    recommendedIntentSummary_intentId,
    recommendedIntentSummary_intentName,
    recommendedIntentSummary_sampleUtterancesCount,

    -- * RelativeAggregationDuration
    RelativeAggregationDuration (..),
    newRelativeAggregationDuration,
    relativeAggregationDuration_timeDimension,
    relativeAggregationDuration_timeValue,

    -- * ResponseSpecification
    ResponseSpecification (..),
    newResponseSpecification,
    responseSpecification_allowInterrupt,
    responseSpecification_messageGroups,

    -- * S3BucketLogDestination
    S3BucketLogDestination (..),
    newS3BucketLogDestination,
    s3BucketLogDestination_kmsKeyArn,
    s3BucketLogDestination_s3BucketArn,
    s3BucketLogDestination_logPrefix,

    -- * S3BucketTranscriptSource
    S3BucketTranscriptSource (..),
    newS3BucketTranscriptSource,
    s3BucketTranscriptSource_kmsKeyArn,
    s3BucketTranscriptSource_pathFormat,
    s3BucketTranscriptSource_transcriptFilter,
    s3BucketTranscriptSource_s3BucketName,
    s3BucketTranscriptSource_transcriptFormat,

    -- * SSMLMessage
    SSMLMessage (..),
    newSSMLMessage,
    sSMLMessage_value,

    -- * SampleUtterance
    SampleUtterance (..),
    newSampleUtterance,
    sampleUtterance_utterance,

    -- * SampleValue
    SampleValue (..),
    newSampleValue,
    sampleValue_value,

    -- * SentimentAnalysisSettings
    SentimentAnalysisSettings (..),
    newSentimentAnalysisSettings,
    sentimentAnalysisSettings_detectSentiment,

    -- * SlotCaptureSetting
    SlotCaptureSetting (..),
    newSlotCaptureSetting,
    slotCaptureSetting_captureConditional,
    slotCaptureSetting_captureNextStep,
    slotCaptureSetting_captureResponse,
    slotCaptureSetting_codeHook,
    slotCaptureSetting_elicitationCodeHook,
    slotCaptureSetting_failureConditional,
    slotCaptureSetting_failureNextStep,
    slotCaptureSetting_failureResponse,

    -- * SlotDefaultValue
    SlotDefaultValue (..),
    newSlotDefaultValue,
    slotDefaultValue_defaultValue,

    -- * SlotDefaultValueSpecification
    SlotDefaultValueSpecification (..),
    newSlotDefaultValueSpecification,
    slotDefaultValueSpecification_defaultValueList,

    -- * SlotFilter
    SlotFilter (..),
    newSlotFilter,
    slotFilter_name,
    slotFilter_values,
    slotFilter_operator,

    -- * SlotPriority
    SlotPriority (..),
    newSlotPriority,
    slotPriority_priority,
    slotPriority_slotId,

    -- * SlotSortBy
    SlotSortBy (..),
    newSlotSortBy,
    slotSortBy_attribute,
    slotSortBy_order,

    -- * SlotSummary
    SlotSummary (..),
    newSlotSummary,
    slotSummary_description,
    slotSummary_lastUpdatedDateTime,
    slotSummary_slotConstraint,
    slotSummary_slotId,
    slotSummary_slotName,
    slotSummary_slotTypeId,
    slotSummary_valueElicitationPromptSpecification,

    -- * SlotTypeFilter
    SlotTypeFilter (..),
    newSlotTypeFilter,
    slotTypeFilter_name,
    slotTypeFilter_values,
    slotTypeFilter_operator,

    -- * SlotTypeSortBy
    SlotTypeSortBy (..),
    newSlotTypeSortBy,
    slotTypeSortBy_attribute,
    slotTypeSortBy_order,

    -- * SlotTypeStatistics
    SlotTypeStatistics (..),
    newSlotTypeStatistics,
    slotTypeStatistics_discoveredSlotTypeCount,

    -- * SlotTypeSummary
    SlotTypeSummary (..),
    newSlotTypeSummary,
    slotTypeSummary_description,
    slotTypeSummary_lastUpdatedDateTime,
    slotTypeSummary_parentSlotTypeSignature,
    slotTypeSummary_slotTypeCategory,
    slotTypeSummary_slotTypeId,
    slotTypeSummary_slotTypeName,

    -- * SlotTypeValue
    SlotTypeValue (..),
    newSlotTypeValue,
    slotTypeValue_sampleValue,
    slotTypeValue_synonyms,

    -- * SlotValue
    SlotValue (..),
    newSlotValue,
    slotValue_interpretedValue,

    -- * SlotValueElicitationSetting
    SlotValueElicitationSetting (..),
    newSlotValueElicitationSetting,
    slotValueElicitationSetting_defaultValueSpecification,
    slotValueElicitationSetting_promptSpecification,
    slotValueElicitationSetting_sampleUtterances,
    slotValueElicitationSetting_slotCaptureSetting,
    slotValueElicitationSetting_waitAndContinueSpecification,
    slotValueElicitationSetting_slotConstraint,

    -- * SlotValueOverride
    SlotValueOverride (..),
    newSlotValueOverride,
    slotValueOverride_shape,
    slotValueOverride_value,
    slotValueOverride_values,

    -- * SlotValueRegexFilter
    SlotValueRegexFilter (..),
    newSlotValueRegexFilter,
    slotValueRegexFilter_pattern,

    -- * SlotValueSelectionSetting
    SlotValueSelectionSetting (..),
    newSlotValueSelectionSetting,
    slotValueSelectionSetting_advancedRecognitionSetting,
    slotValueSelectionSetting_regexFilter,
    slotValueSelectionSetting_resolutionStrategy,

    -- * Specifications
    Specifications (..),
    newSpecifications,
    specifications_slotTypeId,
    specifications_valueElicitationSetting,

    -- * StillWaitingResponseSpecification
    StillWaitingResponseSpecification (..),
    newStillWaitingResponseSpecification,
    stillWaitingResponseSpecification_allowInterrupt,
    stillWaitingResponseSpecification_messageGroups,
    stillWaitingResponseSpecification_frequencyInSeconds,
    stillWaitingResponseSpecification_timeoutInSeconds,

    -- * SubSlotSetting
    SubSlotSetting (..),
    newSubSlotSetting,
    subSlotSetting_expression,
    subSlotSetting_slotSpecifications,

    -- * SubSlotTypeComposition
    SubSlotTypeComposition (..),
    newSubSlotTypeComposition,
    subSlotTypeComposition_name,
    subSlotTypeComposition_slotTypeId,

    -- * SubSlotValueElicitationSetting
    SubSlotValueElicitationSetting (..),
    newSubSlotValueElicitationSetting,
    subSlotValueElicitationSetting_defaultValueSpecification,
    subSlotValueElicitationSetting_sampleUtterances,
    subSlotValueElicitationSetting_waitAndContinueSpecification,
    subSlotValueElicitationSetting_promptSpecification,

    -- * TextInputSpecification
    TextInputSpecification (..),
    newTextInputSpecification,
    textInputSpecification_startTimeoutMs,

    -- * TextLogDestination
    TextLogDestination (..),
    newTextLogDestination,
    textLogDestination_cloudWatch,

    -- * TextLogSetting
    TextLogSetting (..),
    newTextLogSetting,
    textLogSetting_enabled,
    textLogSetting_destination,

    -- * TranscriptFilter
    TranscriptFilter (..),
    newTranscriptFilter,
    transcriptFilter_lexTranscriptFilter,

    -- * TranscriptSourceSetting
    TranscriptSourceSetting (..),
    newTranscriptSourceSetting,
    transcriptSourceSetting_s3BucketTranscriptSource,

    -- * UtteranceAggregationDuration
    UtteranceAggregationDuration (..),
    newUtteranceAggregationDuration,
    utteranceAggregationDuration_relativeAggregationDuration,

    -- * VoiceSettings
    VoiceSettings (..),
    newVoiceSettings,
    voiceSettings_engine,
    voiceSettings_voiceId,

    -- * WaitAndContinueSpecification
    WaitAndContinueSpecification (..),
    newWaitAndContinueSpecification,
    waitAndContinueSpecification_active,
    waitAndContinueSpecification_stillWaitingResponse,
    waitAndContinueSpecification_waitingResponse,
    waitAndContinueSpecification_continueResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.AdvancedRecognitionSetting
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilter
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilterName
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilterOperator
import Amazonka.LexV2Models.Types.AggregatedUtterancesSortAttribute
import Amazonka.LexV2Models.Types.AggregatedUtterancesSortBy
import Amazonka.LexV2Models.Types.AggregatedUtterancesSummary
import Amazonka.LexV2Models.Types.AllowedInputTypes
import Amazonka.LexV2Models.Types.AssociatedTranscript
import Amazonka.LexV2Models.Types.AssociatedTranscriptFilter
import Amazonka.LexV2Models.Types.AssociatedTranscriptFilterName
import Amazonka.LexV2Models.Types.AudioAndDTMFInputSpecification
import Amazonka.LexV2Models.Types.AudioLogDestination
import Amazonka.LexV2Models.Types.AudioLogSetting
import Amazonka.LexV2Models.Types.AudioRecognitionStrategy
import Amazonka.LexV2Models.Types.AudioSpecification
import Amazonka.LexV2Models.Types.BotAliasHistoryEvent
import Amazonka.LexV2Models.Types.BotAliasLocaleSettings
import Amazonka.LexV2Models.Types.BotAliasStatus
import Amazonka.LexV2Models.Types.BotAliasSummary
import Amazonka.LexV2Models.Types.BotExportSpecification
import Amazonka.LexV2Models.Types.BotFilter
import Amazonka.LexV2Models.Types.BotFilterName
import Amazonka.LexV2Models.Types.BotFilterOperator
import Amazonka.LexV2Models.Types.BotImportSpecification
import Amazonka.LexV2Models.Types.BotLocaleExportSpecification
import Amazonka.LexV2Models.Types.BotLocaleFilter
import Amazonka.LexV2Models.Types.BotLocaleFilterName
import Amazonka.LexV2Models.Types.BotLocaleFilterOperator
import Amazonka.LexV2Models.Types.BotLocaleHistoryEvent
import Amazonka.LexV2Models.Types.BotLocaleImportSpecification
import Amazonka.LexV2Models.Types.BotLocaleSortAttribute
import Amazonka.LexV2Models.Types.BotLocaleSortBy
import Amazonka.LexV2Models.Types.BotLocaleStatus
import Amazonka.LexV2Models.Types.BotLocaleSummary
import Amazonka.LexV2Models.Types.BotRecommendationResultStatistics
import Amazonka.LexV2Models.Types.BotRecommendationResults
import Amazonka.LexV2Models.Types.BotRecommendationStatus
import Amazonka.LexV2Models.Types.BotRecommendationSummary
import Amazonka.LexV2Models.Types.BotSortAttribute
import Amazonka.LexV2Models.Types.BotSortBy
import Amazonka.LexV2Models.Types.BotStatus
import Amazonka.LexV2Models.Types.BotSummary
import Amazonka.LexV2Models.Types.BotVersionLocaleDetails
import Amazonka.LexV2Models.Types.BotVersionSortAttribute
import Amazonka.LexV2Models.Types.BotVersionSortBy
import Amazonka.LexV2Models.Types.BotVersionSummary
import Amazonka.LexV2Models.Types.BuiltInIntentSortAttribute
import Amazonka.LexV2Models.Types.BuiltInIntentSortBy
import Amazonka.LexV2Models.Types.BuiltInIntentSummary
import Amazonka.LexV2Models.Types.BuiltInSlotTypeSortAttribute
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
import Amazonka.LexV2Models.Types.CustomVocabularyStatus
import Amazonka.LexV2Models.Types.DTMFSpecification
import Amazonka.LexV2Models.Types.DataPrivacy
import Amazonka.LexV2Models.Types.DateRangeFilter
import Amazonka.LexV2Models.Types.DefaultConditionalBranch
import Amazonka.LexV2Models.Types.DialogAction
import Amazonka.LexV2Models.Types.DialogActionType
import Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.DialogCodeHookSettings
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.Effect
import Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.EncryptionSetting
import Amazonka.LexV2Models.Types.ErrorCode
import Amazonka.LexV2Models.Types.ExportFilter
import Amazonka.LexV2Models.Types.ExportFilterName
import Amazonka.LexV2Models.Types.ExportFilterOperator
import Amazonka.LexV2Models.Types.ExportResourceSpecification
import Amazonka.LexV2Models.Types.ExportSortAttribute
import Amazonka.LexV2Models.Types.ExportSortBy
import Amazonka.LexV2Models.Types.ExportStatus
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
import Amazonka.LexV2Models.Types.ImportExportFileFormat
import Amazonka.LexV2Models.Types.ImportFilter
import Amazonka.LexV2Models.Types.ImportFilterName
import Amazonka.LexV2Models.Types.ImportFilterOperator
import Amazonka.LexV2Models.Types.ImportResourceSpecification
import Amazonka.LexV2Models.Types.ImportResourceType
import Amazonka.LexV2Models.Types.ImportSortAttribute
import Amazonka.LexV2Models.Types.ImportSortBy
import Amazonka.LexV2Models.Types.ImportStatus
import Amazonka.LexV2Models.Types.ImportSummary
import Amazonka.LexV2Models.Types.InitialResponseSetting
import Amazonka.LexV2Models.Types.InputContext
import Amazonka.LexV2Models.Types.IntentClosingSetting
import Amazonka.LexV2Models.Types.IntentConfirmationSetting
import Amazonka.LexV2Models.Types.IntentFilter
import Amazonka.LexV2Models.Types.IntentFilterName
import Amazonka.LexV2Models.Types.IntentFilterOperator
import Amazonka.LexV2Models.Types.IntentOverride
import Amazonka.LexV2Models.Types.IntentSortAttribute
import Amazonka.LexV2Models.Types.IntentSortBy
import Amazonka.LexV2Models.Types.IntentStatistics
import Amazonka.LexV2Models.Types.IntentSummary
import Amazonka.LexV2Models.Types.KendraConfiguration
import Amazonka.LexV2Models.Types.LambdaCodeHook
import Amazonka.LexV2Models.Types.LexTranscriptFilter
import Amazonka.LexV2Models.Types.MergeStrategy
import Amazonka.LexV2Models.Types.Message
import Amazonka.LexV2Models.Types.MessageGroup
import Amazonka.LexV2Models.Types.MessageSelectionStrategy
import Amazonka.LexV2Models.Types.MultipleValuesSetting
import Amazonka.LexV2Models.Types.NewCustomVocabularyItem
import Amazonka.LexV2Models.Types.ObfuscationSetting
import Amazonka.LexV2Models.Types.ObfuscationSettingType
import Amazonka.LexV2Models.Types.OutputContext
import Amazonka.LexV2Models.Types.PathFormat
import Amazonka.LexV2Models.Types.PlainTextMessage
import Amazonka.LexV2Models.Types.PostDialogCodeHookInvocationSpecification
import Amazonka.LexV2Models.Types.PostFulfillmentStatusSpecification
import Amazonka.LexV2Models.Types.Principal
import Amazonka.LexV2Models.Types.PromptAttempt
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
import Amazonka.LexV2Models.Types.SearchOrder
import Amazonka.LexV2Models.Types.SentimentAnalysisSettings
import Amazonka.LexV2Models.Types.SlotCaptureSetting
import Amazonka.LexV2Models.Types.SlotConstraint
import Amazonka.LexV2Models.Types.SlotDefaultValue
import Amazonka.LexV2Models.Types.SlotDefaultValueSpecification
import Amazonka.LexV2Models.Types.SlotFilter
import Amazonka.LexV2Models.Types.SlotFilterName
import Amazonka.LexV2Models.Types.SlotFilterOperator
import Amazonka.LexV2Models.Types.SlotPriority
import Amazonka.LexV2Models.Types.SlotShape
import Amazonka.LexV2Models.Types.SlotSortAttribute
import Amazonka.LexV2Models.Types.SlotSortBy
import Amazonka.LexV2Models.Types.SlotSummary
import Amazonka.LexV2Models.Types.SlotTypeCategory
import Amazonka.LexV2Models.Types.SlotTypeFilter
import Amazonka.LexV2Models.Types.SlotTypeFilterName
import Amazonka.LexV2Models.Types.SlotTypeFilterOperator
import Amazonka.LexV2Models.Types.SlotTypeSortAttribute
import Amazonka.LexV2Models.Types.SlotTypeSortBy
import Amazonka.LexV2Models.Types.SlotTypeStatistics
import Amazonka.LexV2Models.Types.SlotTypeSummary
import Amazonka.LexV2Models.Types.SlotTypeValue
import Amazonka.LexV2Models.Types.SlotValue
import Amazonka.LexV2Models.Types.SlotValueElicitationSetting
import Amazonka.LexV2Models.Types.SlotValueOverride
import Amazonka.LexV2Models.Types.SlotValueRegexFilter
import Amazonka.LexV2Models.Types.SlotValueResolutionStrategy
import Amazonka.LexV2Models.Types.SlotValueSelectionSetting
import Amazonka.LexV2Models.Types.SortOrder
import Amazonka.LexV2Models.Types.Specifications
import Amazonka.LexV2Models.Types.StillWaitingResponseSpecification
import Amazonka.LexV2Models.Types.SubSlotSetting
import Amazonka.LexV2Models.Types.SubSlotTypeComposition
import Amazonka.LexV2Models.Types.SubSlotValueElicitationSetting
import Amazonka.LexV2Models.Types.TextInputSpecification
import Amazonka.LexV2Models.Types.TextLogDestination
import Amazonka.LexV2Models.Types.TextLogSetting
import Amazonka.LexV2Models.Types.TimeDimension
import Amazonka.LexV2Models.Types.TranscriptFilter
import Amazonka.LexV2Models.Types.TranscriptFormat
import Amazonka.LexV2Models.Types.TranscriptSourceSetting
import Amazonka.LexV2Models.Types.UtteranceAggregationDuration
import Amazonka.LexV2Models.Types.VoiceEngine
import Amazonka.LexV2Models.Types.VoiceSettings
import Amazonka.LexV2Models.Types.WaitAndContinueSpecification
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-07@ of the Amazon Lex Model Building V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LexV2Models",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "models-v2-lex",
      Core.signingName = "lex",
      Core.version = "2020-08-07",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LexV2Models",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The action that you tried to perform couldn\'t be completed because the
-- resource is in a conflicting state. For example, deleting a bot that is
-- in the CREATING state. Try your request again.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The service encountered an unexpected condition. Try your request again.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Your request couldn\'t be completed because one or more request fields
-- aren\'t valid. Check the fields in your request and try again.
_PreconditionFailedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | You asked to describe a resource that doesn\'t exist. Check the resource
-- that you are requesting and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have reached a quota for your bot.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Your request rate is too high. Reduce the frequency of requests.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One of the input parameters in your request isn\'t valid. Check the
-- parameters and try your request again.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
