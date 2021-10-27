{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Models.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _PreconditionFailedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AggregatedUtterancesFilterName
    AggregatedUtterancesFilterName (..),

    -- * AggregatedUtterancesFilterOperator
    AggregatedUtterancesFilterOperator (..),

    -- * AggregatedUtterancesSortAttribute
    AggregatedUtterancesSortAttribute (..),

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

    -- * Effect
    Effect (..),

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

    -- * ObfuscationSettingType
    ObfuscationSettingType (..),

    -- * SlotConstraint
    SlotConstraint (..),

    -- * SlotFilterName
    SlotFilterName (..),

    -- * SlotFilterOperator
    SlotFilterOperator (..),

    -- * SlotSortAttribute
    SlotSortAttribute (..),

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
    aggregatedUtterancesSummary_utteranceFirstRecordedInAggregationDuration,
    aggregatedUtterancesSummary_utteranceLastRecordedInAggregationDuration,
    aggregatedUtterancesSummary_hitCount,
    aggregatedUtterancesSummary_missedCount,
    aggregatedUtterancesSummary_containsDataFromDeletedResources,
    aggregatedUtterancesSummary_utterance,

    -- * AudioLogDestination
    AudioLogDestination (..),
    newAudioLogDestination,
    audioLogDestination_s3Bucket,

    -- * AudioLogSetting
    AudioLogSetting (..),
    newAudioLogSetting,
    audioLogSetting_enabled,
    audioLogSetting_destination,

    -- * BotAliasHistoryEvent
    BotAliasHistoryEvent (..),
    newBotAliasHistoryEvent,
    botAliasHistoryEvent_endDate,
    botAliasHistoryEvent_botVersion,
    botAliasHistoryEvent_startDate,

    -- * BotAliasLocaleSettings
    BotAliasLocaleSettings (..),
    newBotAliasLocaleSettings,
    botAliasLocaleSettings_codeHookSpecification,
    botAliasLocaleSettings_enabled,

    -- * BotAliasSummary
    BotAliasSummary (..),
    newBotAliasSummary,
    botAliasSummary_botAliasStatus,
    botAliasSummary_botVersion,
    botAliasSummary_lastUpdatedDateTime,
    botAliasSummary_botAliasId,
    botAliasSummary_creationDateTime,
    botAliasSummary_botAliasName,
    botAliasSummary_description,

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
    botImportSpecification_testBotAliasTags,
    botImportSpecification_idleSessionTTLInSeconds,
    botImportSpecification_botTags,
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
    botLocaleSummary_lastBuildSubmittedDateTime,
    botLocaleSummary_botLocaleStatus,
    botLocaleSummary_lastUpdatedDateTime,
    botLocaleSummary_localeName,
    botLocaleSummary_localeId,
    botLocaleSummary_description,

    -- * BotSortBy
    BotSortBy (..),
    newBotSortBy,
    botSortBy_attribute,
    botSortBy_order,

    -- * BotSummary
    BotSummary (..),
    newBotSummary,
    botSummary_botStatus,
    botSummary_botName,
    botSummary_lastUpdatedDateTime,
    botSummary_botId,
    botSummary_latestBotVersion,
    botSummary_description,

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
    botVersionSummary_botStatus,
    botVersionSummary_botVersion,
    botVersionSummary_botName,
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
    builtInIntentSummary_intentSignature,
    builtInIntentSummary_description,

    -- * BuiltInSlotTypeSortBy
    BuiltInSlotTypeSortBy (..),
    newBuiltInSlotTypeSortBy,
    builtInSlotTypeSortBy_attribute,
    builtInSlotTypeSortBy_order,

    -- * BuiltInSlotTypeSummary
    BuiltInSlotTypeSummary (..),
    newBuiltInSlotTypeSummary,
    builtInSlotTypeSummary_slotTypeSignature,
    builtInSlotTypeSummary_description,

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

    -- * ConversationLogSettings
    ConversationLogSettings (..),
    newConversationLogSettings,
    conversationLogSettings_audioLogSettings,
    conversationLogSettings_textLogSettings,

    -- * CustomPayload
    CustomPayload (..),
    newCustomPayload,
    customPayload_value,

    -- * DataPrivacy
    DataPrivacy (..),
    newDataPrivacy,
    dataPrivacy_childDirected,

    -- * DialogCodeHookSettings
    DialogCodeHookSettings (..),
    newDialogCodeHookSettings,
    dialogCodeHookSettings_enabled,

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

    -- * ExportSortBy
    ExportSortBy (..),
    newExportSortBy,
    exportSortBy_attribute,
    exportSortBy_order,

    -- * ExportSummary
    ExportSummary (..),
    newExportSummary,
    exportSummary_resourceSpecification,
    exportSummary_fileFormat,
    exportSummary_exportStatus,
    exportSummary_lastUpdatedDateTime,
    exportSummary_creationDateTime,
    exportSummary_exportId,

    -- * FulfillmentCodeHookSettings
    FulfillmentCodeHookSettings (..),
    newFulfillmentCodeHookSettings,
    fulfillmentCodeHookSettings_postFulfillmentStatusSpecification,
    fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification,
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

    -- * ImageResponseCard
    ImageResponseCard (..),
    newImageResponseCard,
    imageResponseCard_buttons,
    imageResponseCard_subtitle,
    imageResponseCard_imageUrl,
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

    -- * ImportSortBy
    ImportSortBy (..),
    newImportSortBy,
    importSortBy_attribute,
    importSortBy_order,

    -- * ImportSummary
    ImportSummary (..),
    newImportSummary,
    importSummary_importId,
    importSummary_importedResourceId,
    importSummary_lastUpdatedDateTime,
    importSummary_importedResourceName,
    importSummary_creationDateTime,
    importSummary_mergeStrategy,
    importSummary_importStatus,

    -- * InputContext
    InputContext (..),
    newInputContext,
    inputContext_name,

    -- * IntentClosingSetting
    IntentClosingSetting (..),
    newIntentClosingSetting,
    intentClosingSetting_active,
    intentClosingSetting_closingResponse,

    -- * IntentConfirmationSetting
    IntentConfirmationSetting (..),
    newIntentConfirmationSetting,
    intentConfirmationSetting_active,
    intentConfirmationSetting_promptSpecification,
    intentConfirmationSetting_declinationResponse,

    -- * IntentFilter
    IntentFilter (..),
    newIntentFilter,
    intentFilter_name,
    intentFilter_values,
    intentFilter_operator,

    -- * IntentSortBy
    IntentSortBy (..),
    newIntentSortBy,
    intentSortBy_attribute,
    intentSortBy_order,

    -- * IntentSummary
    IntentSummary (..),
    newIntentSummary,
    intentSummary_intentName,
    intentSummary_lastUpdatedDateTime,
    intentSummary_intentId,
    intentSummary_parentIntentSignature,
    intentSummary_inputContexts,
    intentSummary_outputContexts,
    intentSummary_description,

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

    -- * Message
    Message (..),
    newMessage,
    message_ssmlMessage,
    message_customPayload,
    message_imageResponseCard,
    message_plainTextMessage,

    -- * MessageGroup
    MessageGroup (..),
    newMessageGroup,
    messageGroup_variations,
    messageGroup_message,

    -- * MultipleValuesSetting
    MultipleValuesSetting (..),
    newMultipleValuesSetting,
    multipleValuesSetting_allowMultipleValues,

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

    -- * PlainTextMessage
    PlainTextMessage (..),
    newPlainTextMessage,
    plainTextMessage_value,

    -- * PostFulfillmentStatusSpecification
    PostFulfillmentStatusSpecification (..),
    newPostFulfillmentStatusSpecification,
    postFulfillmentStatusSpecification_successResponse,
    postFulfillmentStatusSpecification_timeoutResponse,
    postFulfillmentStatusSpecification_failureResponse,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_arn,
    principal_service,

    -- * PromptSpecification
    PromptSpecification (..),
    newPromptSpecification,
    promptSpecification_allowInterrupt,
    promptSpecification_messageGroups,
    promptSpecification_maxRetries,

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
    slotSummary_slotName,
    slotSummary_lastUpdatedDateTime,
    slotSummary_slotConstraint,
    slotSummary_slotId,
    slotSummary_description,
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

    -- * SlotTypeSummary
    SlotTypeSummary (..),
    newSlotTypeSummary,
    slotTypeSummary_parentSlotTypeSignature,
    slotTypeSummary_lastUpdatedDateTime,
    slotTypeSummary_slotTypeName,
    slotTypeSummary_description,
    slotTypeSummary_slotTypeId,

    -- * SlotTypeValue
    SlotTypeValue (..),
    newSlotTypeValue,
    slotTypeValue_sampleValue,
    slotTypeValue_synonyms,

    -- * SlotValueElicitationSetting
    SlotValueElicitationSetting (..),
    newSlotValueElicitationSetting,
    slotValueElicitationSetting_waitAndContinueSpecification,
    slotValueElicitationSetting_defaultValueSpecification,
    slotValueElicitationSetting_sampleUtterances,
    slotValueElicitationSetting_promptSpecification,
    slotValueElicitationSetting_slotConstraint,

    -- * SlotValueRegexFilter
    SlotValueRegexFilter (..),
    newSlotValueRegexFilter,
    slotValueRegexFilter_pattern,

    -- * SlotValueSelectionSetting
    SlotValueSelectionSetting (..),
    newSlotValueSelectionSetting,
    slotValueSelectionSetting_regexFilter,
    slotValueSelectionSetting_resolutionStrategy,

    -- * StillWaitingResponseSpecification
    StillWaitingResponseSpecification (..),
    newStillWaitingResponseSpecification,
    stillWaitingResponseSpecification_allowInterrupt,
    stillWaitingResponseSpecification_messageGroups,
    stillWaitingResponseSpecification_frequencyInSeconds,
    stillWaitingResponseSpecification_timeoutInSeconds,

    -- * TextLogDestination
    TextLogDestination (..),
    newTextLogDestination,
    textLogDestination_cloudWatch,

    -- * TextLogSetting
    TextLogSetting (..),
    newTextLogSetting,
    textLogSetting_enabled,
    textLogSetting_destination,

    -- * UtteranceAggregationDuration
    UtteranceAggregationDuration (..),
    newUtteranceAggregationDuration,
    utteranceAggregationDuration_relativeAggregationDuration,

    -- * VoiceSettings
    VoiceSettings (..),
    newVoiceSettings,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.AggregatedUtterancesFilter
import Network.AWS.LexV2Models.Types.AggregatedUtterancesFilterName
import Network.AWS.LexV2Models.Types.AggregatedUtterancesFilterOperator
import Network.AWS.LexV2Models.Types.AggregatedUtterancesSortAttribute
import Network.AWS.LexV2Models.Types.AggregatedUtterancesSortBy
import Network.AWS.LexV2Models.Types.AggregatedUtterancesSummary
import Network.AWS.LexV2Models.Types.AudioLogDestination
import Network.AWS.LexV2Models.Types.AudioLogSetting
import Network.AWS.LexV2Models.Types.BotAliasHistoryEvent
import Network.AWS.LexV2Models.Types.BotAliasLocaleSettings
import Network.AWS.LexV2Models.Types.BotAliasStatus
import Network.AWS.LexV2Models.Types.BotAliasSummary
import Network.AWS.LexV2Models.Types.BotExportSpecification
import Network.AWS.LexV2Models.Types.BotFilter
import Network.AWS.LexV2Models.Types.BotFilterName
import Network.AWS.LexV2Models.Types.BotFilterOperator
import Network.AWS.LexV2Models.Types.BotImportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleExportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleFilter
import Network.AWS.LexV2Models.Types.BotLocaleFilterName
import Network.AWS.LexV2Models.Types.BotLocaleFilterOperator
import Network.AWS.LexV2Models.Types.BotLocaleHistoryEvent
import Network.AWS.LexV2Models.Types.BotLocaleImportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleSortAttribute
import Network.AWS.LexV2Models.Types.BotLocaleSortBy
import Network.AWS.LexV2Models.Types.BotLocaleStatus
import Network.AWS.LexV2Models.Types.BotLocaleSummary
import Network.AWS.LexV2Models.Types.BotSortAttribute
import Network.AWS.LexV2Models.Types.BotSortBy
import Network.AWS.LexV2Models.Types.BotStatus
import Network.AWS.LexV2Models.Types.BotSummary
import Network.AWS.LexV2Models.Types.BotVersionLocaleDetails
import Network.AWS.LexV2Models.Types.BotVersionSortAttribute
import Network.AWS.LexV2Models.Types.BotVersionSortBy
import Network.AWS.LexV2Models.Types.BotVersionSummary
import Network.AWS.LexV2Models.Types.BuiltInIntentSortAttribute
import Network.AWS.LexV2Models.Types.BuiltInIntentSortBy
import Network.AWS.LexV2Models.Types.BuiltInIntentSummary
import Network.AWS.LexV2Models.Types.BuiltInSlotTypeSortAttribute
import Network.AWS.LexV2Models.Types.BuiltInSlotTypeSortBy
import Network.AWS.LexV2Models.Types.BuiltInSlotTypeSummary
import Network.AWS.LexV2Models.Types.Button
import Network.AWS.LexV2Models.Types.CloudWatchLogGroupLogDestination
import Network.AWS.LexV2Models.Types.CodeHookSpecification
import Network.AWS.LexV2Models.Types.ConversationLogSettings
import Network.AWS.LexV2Models.Types.CustomPayload
import Network.AWS.LexV2Models.Types.DataPrivacy
import Network.AWS.LexV2Models.Types.DialogCodeHookSettings
import Network.AWS.LexV2Models.Types.Effect
import Network.AWS.LexV2Models.Types.ExportFilter
import Network.AWS.LexV2Models.Types.ExportFilterName
import Network.AWS.LexV2Models.Types.ExportFilterOperator
import Network.AWS.LexV2Models.Types.ExportResourceSpecification
import Network.AWS.LexV2Models.Types.ExportSortAttribute
import Network.AWS.LexV2Models.Types.ExportSortBy
import Network.AWS.LexV2Models.Types.ExportStatus
import Network.AWS.LexV2Models.Types.ExportSummary
import Network.AWS.LexV2Models.Types.FulfillmentCodeHookSettings
import Network.AWS.LexV2Models.Types.FulfillmentStartResponseSpecification
import Network.AWS.LexV2Models.Types.FulfillmentUpdateResponseSpecification
import Network.AWS.LexV2Models.Types.FulfillmentUpdatesSpecification
import Network.AWS.LexV2Models.Types.ImageResponseCard
import Network.AWS.LexV2Models.Types.ImportExportFileFormat
import Network.AWS.LexV2Models.Types.ImportFilter
import Network.AWS.LexV2Models.Types.ImportFilterName
import Network.AWS.LexV2Models.Types.ImportFilterOperator
import Network.AWS.LexV2Models.Types.ImportResourceSpecification
import Network.AWS.LexV2Models.Types.ImportSortAttribute
import Network.AWS.LexV2Models.Types.ImportSortBy
import Network.AWS.LexV2Models.Types.ImportStatus
import Network.AWS.LexV2Models.Types.ImportSummary
import Network.AWS.LexV2Models.Types.InputContext
import Network.AWS.LexV2Models.Types.IntentClosingSetting
import Network.AWS.LexV2Models.Types.IntentConfirmationSetting
import Network.AWS.LexV2Models.Types.IntentFilter
import Network.AWS.LexV2Models.Types.IntentFilterName
import Network.AWS.LexV2Models.Types.IntentFilterOperator
import Network.AWS.LexV2Models.Types.IntentSortAttribute
import Network.AWS.LexV2Models.Types.IntentSortBy
import Network.AWS.LexV2Models.Types.IntentSummary
import Network.AWS.LexV2Models.Types.KendraConfiguration
import Network.AWS.LexV2Models.Types.LambdaCodeHook
import Network.AWS.LexV2Models.Types.MergeStrategy
import Network.AWS.LexV2Models.Types.Message
import Network.AWS.LexV2Models.Types.MessageGroup
import Network.AWS.LexV2Models.Types.MultipleValuesSetting
import Network.AWS.LexV2Models.Types.ObfuscationSetting
import Network.AWS.LexV2Models.Types.ObfuscationSettingType
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
import Network.AWS.LexV2Models.Types.SlotConstraint
import Network.AWS.LexV2Models.Types.SlotDefaultValue
import Network.AWS.LexV2Models.Types.SlotDefaultValueSpecification
import Network.AWS.LexV2Models.Types.SlotFilter
import Network.AWS.LexV2Models.Types.SlotFilterName
import Network.AWS.LexV2Models.Types.SlotFilterOperator
import Network.AWS.LexV2Models.Types.SlotPriority
import Network.AWS.LexV2Models.Types.SlotSortAttribute
import Network.AWS.LexV2Models.Types.SlotSortBy
import Network.AWS.LexV2Models.Types.SlotSummary
import Network.AWS.LexV2Models.Types.SlotTypeFilter
import Network.AWS.LexV2Models.Types.SlotTypeFilterName
import Network.AWS.LexV2Models.Types.SlotTypeFilterOperator
import Network.AWS.LexV2Models.Types.SlotTypeSortAttribute
import Network.AWS.LexV2Models.Types.SlotTypeSortBy
import Network.AWS.LexV2Models.Types.SlotTypeSummary
import Network.AWS.LexV2Models.Types.SlotTypeValue
import Network.AWS.LexV2Models.Types.SlotValueElicitationSetting
import Network.AWS.LexV2Models.Types.SlotValueRegexFilter
import Network.AWS.LexV2Models.Types.SlotValueResolutionStrategy
import Network.AWS.LexV2Models.Types.SlotValueSelectionSetting
import Network.AWS.LexV2Models.Types.SortOrder
import Network.AWS.LexV2Models.Types.StillWaitingResponseSpecification
import Network.AWS.LexV2Models.Types.TextLogDestination
import Network.AWS.LexV2Models.Types.TextLogSetting
import Network.AWS.LexV2Models.Types.TimeDimension
import Network.AWS.LexV2Models.Types.UtteranceAggregationDuration
import Network.AWS.LexV2Models.Types.VoiceSettings
import Network.AWS.LexV2Models.Types.WaitAndContinueSpecification
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-07@ of the Amazon Lex Model Building V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LexV2Models",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "models-v2-lex",
      Core._serviceSigningName = "lex",
      Core._serviceVersion = "2020-08-07",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LexV2Models",
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

-- | One of the input parameters in your request isn\'t valid. Check the
-- parameters and try your request again.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Your request couldn\'t be completed because one or more request fields
-- aren\'t valid. Check the fields in your request and try again.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | The action that you tried to perform couldn\'t be completed because the
-- resource is in a conflicting state. For example, deleting a bot that is
-- in the CREATING state. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | You have reached a quota for your bot.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Your request rate is too high. Reduce the frequency of requests.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The service encountered an unexpected condition. Try your request again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | You asked to describe a resource that doesn\'t exist. Check the resource
-- that you are requesting and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
