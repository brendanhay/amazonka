{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.LexV2Models
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-07@ of the AWS service descriptions, licensed under Apache 2.0.
module Network.AWS.LexV2Models
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** BotLocaleExpressTestingAvailable
    newBotLocaleExpressTestingAvailable,

    -- ** BotLocaleBuilt
    newBotLocaleBuilt,

    -- ** BotVersionAvailable
    newBotVersionAvailable,

    -- ** BotAvailable
    newBotAvailable,

    -- ** BotExportCompleted
    newBotExportCompleted,

    -- ** BotImportCompleted
    newBotImportCompleted,

    -- ** BotAliasAvailable
    newBotAliasAvailable,

    -- ** BotLocaleCreated
    newBotLocaleCreated,

    -- * Operations
    -- $operations

    -- ** DeleteImport
    DeleteImport (DeleteImport'),
    newDeleteImport,
    DeleteImportResponse (DeleteImportResponse'),
    newDeleteImportResponse,

    -- ** DescribeBot
    DescribeBot (DescribeBot'),
    newDescribeBot,
    DescribeBotResponse (DescribeBotResponse'),
    newDescribeBotResponse,

    -- ** DescribeBotLocale
    DescribeBotLocale (DescribeBotLocale'),
    newDescribeBotLocale,
    DescribeBotLocaleResponse (DescribeBotLocaleResponse'),
    newDescribeBotLocaleResponse,

    -- ** ListBuiltInSlotTypes
    ListBuiltInSlotTypes (ListBuiltInSlotTypes'),
    newListBuiltInSlotTypes,
    ListBuiltInSlotTypesResponse (ListBuiltInSlotTypesResponse'),
    newListBuiltInSlotTypesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListBotVersions
    ListBotVersions (ListBotVersions'),
    newListBotVersions,
    ListBotVersionsResponse (ListBotVersionsResponse'),
    newListBotVersionsResponse,

    -- ** ListIntents
    ListIntents (ListIntents'),
    newListIntents,
    ListIntentsResponse (ListIntentsResponse'),
    newListIntentsResponse,

    -- ** ListSlots
    ListSlots (ListSlots'),
    newListSlots,
    ListSlotsResponse (ListSlotsResponse'),
    newListSlotsResponse,

    -- ** DeleteIntent
    DeleteIntent (DeleteIntent'),
    newDeleteIntent,
    DeleteIntentResponse (DeleteIntentResponse'),
    newDeleteIntentResponse,

    -- ** DeleteSlot
    DeleteSlot (DeleteSlot'),
    newDeleteSlot,
    DeleteSlotResponse (DeleteSlotResponse'),
    newDeleteSlotResponse,

    -- ** UpdateIntent
    UpdateIntent (UpdateIntent'),
    newUpdateIntent,
    UpdateIntentResponse (UpdateIntentResponse'),
    newUpdateIntentResponse,

    -- ** UpdateSlot
    UpdateSlot (UpdateSlot'),
    newUpdateSlot,
    UpdateSlotResponse (UpdateSlotResponse'),
    newUpdateSlotResponse,

    -- ** CreateSlot
    CreateSlot (CreateSlot'),
    newCreateSlot,
    CreateSlotResponse (CreateSlotResponse'),
    newCreateSlotResponse,

    -- ** ListBots
    ListBots (ListBots'),
    newListBots,
    ListBotsResponse (ListBotsResponse'),
    newListBotsResponse,

    -- ** DeleteBotLocale
    DeleteBotLocale (DeleteBotLocale'),
    newDeleteBotLocale,
    DeleteBotLocaleResponse (DeleteBotLocaleResponse'),
    newDeleteBotLocaleResponse,

    -- ** UpdateBotLocale
    UpdateBotLocale (UpdateBotLocale'),
    newUpdateBotLocale,
    UpdateBotLocaleResponse (UpdateBotLocaleResponse'),
    newUpdateBotLocaleResponse,

    -- ** CreateIntent
    CreateIntent (CreateIntent'),
    newCreateIntent,
    CreateIntentResponse (CreateIntentResponse'),
    newCreateIntentResponse,

    -- ** DescribeImport
    DescribeImport (DescribeImport'),
    newDescribeImport,
    DescribeImportResponse (DescribeImportResponse'),
    newDescribeImportResponse,

    -- ** DeleteBot
    DeleteBot (DeleteBot'),
    newDeleteBot,
    DeleteBotResponse (DeleteBotResponse'),
    newDeleteBotResponse,

    -- ** UpdateBot
    UpdateBot (UpdateBot'),
    newUpdateBot,
    UpdateBotResponse (UpdateBotResponse'),
    newUpdateBotResponse,

    -- ** ListBotLocales
    ListBotLocales (ListBotLocales'),
    newListBotLocales,
    ListBotLocalesResponse (ListBotLocalesResponse'),
    newListBotLocalesResponse,

    -- ** CreateResourcePolicy
    CreateResourcePolicy (CreateResourcePolicy'),
    newCreateResourcePolicy,
    CreateResourcePolicyResponse (CreateResourcePolicyResponse'),
    newCreateResourcePolicyResponse,

    -- ** DeleteBotAlias
    DeleteBotAlias (DeleteBotAlias'),
    newDeleteBotAlias,
    DeleteBotAliasResponse (DeleteBotAliasResponse'),
    newDeleteBotAliasResponse,

    -- ** UpdateBotAlias
    UpdateBotAlias (UpdateBotAlias'),
    newUpdateBotAlias,
    UpdateBotAliasResponse (UpdateBotAliasResponse'),
    newUpdateBotAliasResponse,

    -- ** DescribeBotVersion
    DescribeBotVersion (DescribeBotVersion'),
    newDescribeBotVersion,
    DescribeBotVersionResponse (DescribeBotVersionResponse'),
    newDescribeBotVersionResponse,

    -- ** DescribeSlot
    DescribeSlot (DescribeSlot'),
    newDescribeSlot,
    DescribeSlotResponse (DescribeSlotResponse'),
    newDescribeSlotResponse,

    -- ** DescribeIntent
    DescribeIntent (DescribeIntent'),
    newDescribeIntent,
    DescribeIntentResponse (DescribeIntentResponse'),
    newDescribeIntentResponse,

    -- ** DeleteUtterances
    DeleteUtterances (DeleteUtterances'),
    newDeleteUtterances,
    DeleteUtterancesResponse (DeleteUtterancesResponse'),
    newDeleteUtterancesResponse,

    -- ** CreateUploadUrl
    CreateUploadUrl (CreateUploadUrl'),
    newCreateUploadUrl,
    CreateUploadUrlResponse (CreateUploadUrlResponse'),
    newCreateUploadUrlResponse,

    -- ** ListBuiltInIntents
    ListBuiltInIntents (ListBuiltInIntents'),
    newListBuiltInIntents,
    ListBuiltInIntentsResponse (ListBuiltInIntentsResponse'),
    newListBuiltInIntentsResponse,

    -- ** ListImports
    ListImports (ListImports'),
    newListImports,
    ListImportsResponse (ListImportsResponse'),
    newListImportsResponse,

    -- ** ListAggregatedUtterances
    ListAggregatedUtterances (ListAggregatedUtterances'),
    newListAggregatedUtterances,
    ListAggregatedUtterancesResponse (ListAggregatedUtterancesResponse'),
    newListAggregatedUtterancesResponse,

    -- ** CreateBotVersion
    CreateBotVersion (CreateBotVersion'),
    newCreateBotVersion,
    CreateBotVersionResponse (CreateBotVersionResponse'),
    newCreateBotVersionResponse,

    -- ** BuildBotLocale
    BuildBotLocale (BuildBotLocale'),
    newBuildBotLocale,
    BuildBotLocaleResponse (BuildBotLocaleResponse'),
    newBuildBotLocaleResponse,

    -- ** DescribeResourcePolicy
    DescribeResourcePolicy (DescribeResourcePolicy'),
    newDescribeResourcePolicy,
    DescribeResourcePolicyResponse (DescribeResourcePolicyResponse'),
    newDescribeResourcePolicyResponse,

    -- ** DeleteBotVersion
    DeleteBotVersion (DeleteBotVersion'),
    newDeleteBotVersion,
    DeleteBotVersionResponse (DeleteBotVersionResponse'),
    newDeleteBotVersionResponse,

    -- ** DescribeBotAlias
    DescribeBotAlias (DescribeBotAlias'),
    newDescribeBotAlias,
    DescribeBotAliasResponse (DescribeBotAliasResponse'),
    newDescribeBotAliasResponse,

    -- ** UpdateSlotType
    UpdateSlotType (UpdateSlotType'),
    newUpdateSlotType,
    UpdateSlotTypeResponse (UpdateSlotTypeResponse'),
    newUpdateSlotTypeResponse,

    -- ** DeleteSlotType
    DeleteSlotType (DeleteSlotType'),
    newDeleteSlotType,
    DeleteSlotTypeResponse (DeleteSlotTypeResponse'),
    newDeleteSlotTypeResponse,

    -- ** CreateBotLocale
    CreateBotLocale (CreateBotLocale'),
    newCreateBotLocale,
    CreateBotLocaleResponse (CreateBotLocaleResponse'),
    newCreateBotLocaleResponse,

    -- ** ListSlotTypes
    ListSlotTypes (ListSlotTypes'),
    newListSlotTypes,
    ListSlotTypesResponse (ListSlotTypesResponse'),
    newListSlotTypesResponse,

    -- ** DeleteExport
    DeleteExport (DeleteExport'),
    newDeleteExport,
    DeleteExportResponse (DeleteExportResponse'),
    newDeleteExportResponse,

    -- ** StartImport
    StartImport (StartImport'),
    newStartImport,
    StartImportResponse (StartImportResponse'),
    newStartImportResponse,

    -- ** UpdateExport
    UpdateExport (UpdateExport'),
    newUpdateExport,
    UpdateExportResponse (UpdateExportResponse'),
    newUpdateExportResponse,

    -- ** CreateBot
    CreateBot (CreateBot'),
    newCreateBot,
    CreateBotResponse (CreateBotResponse'),
    newCreateBotResponse,

    -- ** ListExports
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** CreateSlotType
    CreateSlotType (CreateSlotType'),
    newCreateSlotType,
    CreateSlotTypeResponse (CreateSlotTypeResponse'),
    newCreateSlotTypeResponse,

    -- ** CreateExport
    CreateExport (CreateExport'),
    newCreateExport,
    CreateExportResponse (CreateExportResponse'),
    newCreateExportResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListBotAliases
    ListBotAliases (ListBotAliases'),
    newListBotAliases,
    ListBotAliasesResponse (ListBotAliasesResponse'),
    newListBotAliasesResponse,

    -- ** CreateBotAlias
    CreateBotAlias (CreateBotAlias'),
    newCreateBotAlias,
    CreateBotAliasResponse (CreateBotAliasResponse'),
    newCreateBotAliasResponse,

    -- ** CreateResourcePolicyStatement
    CreateResourcePolicyStatement (CreateResourcePolicyStatement'),
    newCreateResourcePolicyStatement,
    CreateResourcePolicyStatementResponse (CreateResourcePolicyStatementResponse'),
    newCreateResourcePolicyStatementResponse,

    -- ** UpdateResourcePolicy
    UpdateResourcePolicy (UpdateResourcePolicy'),
    newUpdateResourcePolicy,
    UpdateResourcePolicyResponse (UpdateResourcePolicyResponse'),
    newUpdateResourcePolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteResourcePolicyStatement
    DeleteResourcePolicyStatement (DeleteResourcePolicyStatement'),
    newDeleteResourcePolicyStatement,
    DeleteResourcePolicyStatementResponse (DeleteResourcePolicyStatementResponse'),
    newDeleteResourcePolicyStatementResponse,

    -- ** DescribeSlotType
    DescribeSlotType (DescribeSlotType'),
    newDescribeSlotType,
    DescribeSlotTypeResponse (DescribeSlotTypeResponse'),
    newDescribeSlotTypeResponse,

    -- ** DescribeExport
    DescribeExport (DescribeExport'),
    newDescribeExport,
    DescribeExportResponse (DescribeExportResponse'),
    newDescribeExportResponse,

    -- * Types

    -- ** AggregatedUtterancesFilterName
    AggregatedUtterancesFilterName (..),

    -- ** AggregatedUtterancesFilterOperator
    AggregatedUtterancesFilterOperator (..),

    -- ** AggregatedUtterancesSortAttribute
    AggregatedUtterancesSortAttribute (..),

    -- ** BotAliasStatus
    BotAliasStatus (..),

    -- ** BotFilterName
    BotFilterName (..),

    -- ** BotFilterOperator
    BotFilterOperator (..),

    -- ** BotLocaleFilterName
    BotLocaleFilterName (..),

    -- ** BotLocaleFilterOperator
    BotLocaleFilterOperator (..),

    -- ** BotLocaleSortAttribute
    BotLocaleSortAttribute (..),

    -- ** BotLocaleStatus
    BotLocaleStatus (..),

    -- ** BotSortAttribute
    BotSortAttribute (..),

    -- ** BotStatus
    BotStatus (..),

    -- ** BotVersionSortAttribute
    BotVersionSortAttribute (..),

    -- ** BuiltInIntentSortAttribute
    BuiltInIntentSortAttribute (..),

    -- ** BuiltInSlotTypeSortAttribute
    BuiltInSlotTypeSortAttribute (..),

    -- ** Effect
    Effect (..),

    -- ** ExportFilterName
    ExportFilterName (..),

    -- ** ExportFilterOperator
    ExportFilterOperator (..),

    -- ** ExportSortAttribute
    ExportSortAttribute (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** ImportExportFileFormat
    ImportExportFileFormat (..),

    -- ** ImportFilterName
    ImportFilterName (..),

    -- ** ImportFilterOperator
    ImportFilterOperator (..),

    -- ** ImportSortAttribute
    ImportSortAttribute (..),

    -- ** ImportStatus
    ImportStatus (..),

    -- ** IntentFilterName
    IntentFilterName (..),

    -- ** IntentFilterOperator
    IntentFilterOperator (..),

    -- ** IntentSortAttribute
    IntentSortAttribute (..),

    -- ** MergeStrategy
    MergeStrategy (..),

    -- ** ObfuscationSettingType
    ObfuscationSettingType (..),

    -- ** SlotConstraint
    SlotConstraint (..),

    -- ** SlotFilterName
    SlotFilterName (..),

    -- ** SlotFilterOperator
    SlotFilterOperator (..),

    -- ** SlotSortAttribute
    SlotSortAttribute (..),

    -- ** SlotTypeFilterName
    SlotTypeFilterName (..),

    -- ** SlotTypeFilterOperator
    SlotTypeFilterOperator (..),

    -- ** SlotTypeSortAttribute
    SlotTypeSortAttribute (..),

    -- ** SlotValueResolutionStrategy
    SlotValueResolutionStrategy (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** TimeDimension
    TimeDimension (..),

    -- ** AggregatedUtterancesFilter
    AggregatedUtterancesFilter (AggregatedUtterancesFilter'),
    newAggregatedUtterancesFilter,

    -- ** AggregatedUtterancesSortBy
    AggregatedUtterancesSortBy (AggregatedUtterancesSortBy'),
    newAggregatedUtterancesSortBy,

    -- ** AggregatedUtterancesSummary
    AggregatedUtterancesSummary (AggregatedUtterancesSummary'),
    newAggregatedUtterancesSummary,

    -- ** AudioLogDestination
    AudioLogDestination (AudioLogDestination'),
    newAudioLogDestination,

    -- ** AudioLogSetting
    AudioLogSetting (AudioLogSetting'),
    newAudioLogSetting,

    -- ** BotAliasHistoryEvent
    BotAliasHistoryEvent (BotAliasHistoryEvent'),
    newBotAliasHistoryEvent,

    -- ** BotAliasLocaleSettings
    BotAliasLocaleSettings (BotAliasLocaleSettings'),
    newBotAliasLocaleSettings,

    -- ** BotAliasSummary
    BotAliasSummary (BotAliasSummary'),
    newBotAliasSummary,

    -- ** BotExportSpecification
    BotExportSpecification (BotExportSpecification'),
    newBotExportSpecification,

    -- ** BotFilter
    BotFilter (BotFilter'),
    newBotFilter,

    -- ** BotImportSpecification
    BotImportSpecification (BotImportSpecification'),
    newBotImportSpecification,

    -- ** BotLocaleExportSpecification
    BotLocaleExportSpecification (BotLocaleExportSpecification'),
    newBotLocaleExportSpecification,

    -- ** BotLocaleFilter
    BotLocaleFilter (BotLocaleFilter'),
    newBotLocaleFilter,

    -- ** BotLocaleHistoryEvent
    BotLocaleHistoryEvent (BotLocaleHistoryEvent'),
    newBotLocaleHistoryEvent,

    -- ** BotLocaleImportSpecification
    BotLocaleImportSpecification (BotLocaleImportSpecification'),
    newBotLocaleImportSpecification,

    -- ** BotLocaleSortBy
    BotLocaleSortBy (BotLocaleSortBy'),
    newBotLocaleSortBy,

    -- ** BotLocaleSummary
    BotLocaleSummary (BotLocaleSummary'),
    newBotLocaleSummary,

    -- ** BotSortBy
    BotSortBy (BotSortBy'),
    newBotSortBy,

    -- ** BotSummary
    BotSummary (BotSummary'),
    newBotSummary,

    -- ** BotVersionLocaleDetails
    BotVersionLocaleDetails (BotVersionLocaleDetails'),
    newBotVersionLocaleDetails,

    -- ** BotVersionSortBy
    BotVersionSortBy (BotVersionSortBy'),
    newBotVersionSortBy,

    -- ** BotVersionSummary
    BotVersionSummary (BotVersionSummary'),
    newBotVersionSummary,

    -- ** BuiltInIntentSortBy
    BuiltInIntentSortBy (BuiltInIntentSortBy'),
    newBuiltInIntentSortBy,

    -- ** BuiltInIntentSummary
    BuiltInIntentSummary (BuiltInIntentSummary'),
    newBuiltInIntentSummary,

    -- ** BuiltInSlotTypeSortBy
    BuiltInSlotTypeSortBy (BuiltInSlotTypeSortBy'),
    newBuiltInSlotTypeSortBy,

    -- ** BuiltInSlotTypeSummary
    BuiltInSlotTypeSummary (BuiltInSlotTypeSummary'),
    newBuiltInSlotTypeSummary,

    -- ** Button
    Button (Button'),
    newButton,

    -- ** CloudWatchLogGroupLogDestination
    CloudWatchLogGroupLogDestination (CloudWatchLogGroupLogDestination'),
    newCloudWatchLogGroupLogDestination,

    -- ** CodeHookSpecification
    CodeHookSpecification (CodeHookSpecification'),
    newCodeHookSpecification,

    -- ** ConversationLogSettings
    ConversationLogSettings (ConversationLogSettings'),
    newConversationLogSettings,

    -- ** CustomPayload
    CustomPayload (CustomPayload'),
    newCustomPayload,

    -- ** DataPrivacy
    DataPrivacy (DataPrivacy'),
    newDataPrivacy,

    -- ** DialogCodeHookSettings
    DialogCodeHookSettings (DialogCodeHookSettings'),
    newDialogCodeHookSettings,

    -- ** ExportFilter
    ExportFilter (ExportFilter'),
    newExportFilter,

    -- ** ExportResourceSpecification
    ExportResourceSpecification (ExportResourceSpecification'),
    newExportResourceSpecification,

    -- ** ExportSortBy
    ExportSortBy (ExportSortBy'),
    newExportSortBy,

    -- ** ExportSummary
    ExportSummary (ExportSummary'),
    newExportSummary,

    -- ** FulfillmentCodeHookSettings
    FulfillmentCodeHookSettings (FulfillmentCodeHookSettings'),
    newFulfillmentCodeHookSettings,

    -- ** FulfillmentStartResponseSpecification
    FulfillmentStartResponseSpecification (FulfillmentStartResponseSpecification'),
    newFulfillmentStartResponseSpecification,

    -- ** FulfillmentUpdateResponseSpecification
    FulfillmentUpdateResponseSpecification (FulfillmentUpdateResponseSpecification'),
    newFulfillmentUpdateResponseSpecification,

    -- ** FulfillmentUpdatesSpecification
    FulfillmentUpdatesSpecification (FulfillmentUpdatesSpecification'),
    newFulfillmentUpdatesSpecification,

    -- ** ImageResponseCard
    ImageResponseCard (ImageResponseCard'),
    newImageResponseCard,

    -- ** ImportFilter
    ImportFilter (ImportFilter'),
    newImportFilter,

    -- ** ImportResourceSpecification
    ImportResourceSpecification (ImportResourceSpecification'),
    newImportResourceSpecification,

    -- ** ImportSortBy
    ImportSortBy (ImportSortBy'),
    newImportSortBy,

    -- ** ImportSummary
    ImportSummary (ImportSummary'),
    newImportSummary,

    -- ** InputContext
    InputContext (InputContext'),
    newInputContext,

    -- ** IntentClosingSetting
    IntentClosingSetting (IntentClosingSetting'),
    newIntentClosingSetting,

    -- ** IntentConfirmationSetting
    IntentConfirmationSetting (IntentConfirmationSetting'),
    newIntentConfirmationSetting,

    -- ** IntentFilter
    IntentFilter (IntentFilter'),
    newIntentFilter,

    -- ** IntentSortBy
    IntentSortBy (IntentSortBy'),
    newIntentSortBy,

    -- ** IntentSummary
    IntentSummary (IntentSummary'),
    newIntentSummary,

    -- ** KendraConfiguration
    KendraConfiguration (KendraConfiguration'),
    newKendraConfiguration,

    -- ** LambdaCodeHook
    LambdaCodeHook (LambdaCodeHook'),
    newLambdaCodeHook,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** MessageGroup
    MessageGroup (MessageGroup'),
    newMessageGroup,

    -- ** MultipleValuesSetting
    MultipleValuesSetting (MultipleValuesSetting'),
    newMultipleValuesSetting,

    -- ** ObfuscationSetting
    ObfuscationSetting (ObfuscationSetting'),
    newObfuscationSetting,

    -- ** OutputContext
    OutputContext (OutputContext'),
    newOutputContext,

    -- ** PlainTextMessage
    PlainTextMessage (PlainTextMessage'),
    newPlainTextMessage,

    -- ** PostFulfillmentStatusSpecification
    PostFulfillmentStatusSpecification (PostFulfillmentStatusSpecification'),
    newPostFulfillmentStatusSpecification,

    -- ** Principal
    Principal (Principal'),
    newPrincipal,

    -- ** PromptSpecification
    PromptSpecification (PromptSpecification'),
    newPromptSpecification,

    -- ** RelativeAggregationDuration
    RelativeAggregationDuration (RelativeAggregationDuration'),
    newRelativeAggregationDuration,

    -- ** ResponseSpecification
    ResponseSpecification (ResponseSpecification'),
    newResponseSpecification,

    -- ** S3BucketLogDestination
    S3BucketLogDestination (S3BucketLogDestination'),
    newS3BucketLogDestination,

    -- ** SSMLMessage
    SSMLMessage (SSMLMessage'),
    newSSMLMessage,

    -- ** SampleUtterance
    SampleUtterance (SampleUtterance'),
    newSampleUtterance,

    -- ** SampleValue
    SampleValue (SampleValue'),
    newSampleValue,

    -- ** SentimentAnalysisSettings
    SentimentAnalysisSettings (SentimentAnalysisSettings'),
    newSentimentAnalysisSettings,

    -- ** SlotDefaultValue
    SlotDefaultValue (SlotDefaultValue'),
    newSlotDefaultValue,

    -- ** SlotDefaultValueSpecification
    SlotDefaultValueSpecification (SlotDefaultValueSpecification'),
    newSlotDefaultValueSpecification,

    -- ** SlotFilter
    SlotFilter (SlotFilter'),
    newSlotFilter,

    -- ** SlotPriority
    SlotPriority (SlotPriority'),
    newSlotPriority,

    -- ** SlotSortBy
    SlotSortBy (SlotSortBy'),
    newSlotSortBy,

    -- ** SlotSummary
    SlotSummary (SlotSummary'),
    newSlotSummary,

    -- ** SlotTypeFilter
    SlotTypeFilter (SlotTypeFilter'),
    newSlotTypeFilter,

    -- ** SlotTypeSortBy
    SlotTypeSortBy (SlotTypeSortBy'),
    newSlotTypeSortBy,

    -- ** SlotTypeSummary
    SlotTypeSummary (SlotTypeSummary'),
    newSlotTypeSummary,

    -- ** SlotTypeValue
    SlotTypeValue (SlotTypeValue'),
    newSlotTypeValue,

    -- ** SlotValueElicitationSetting
    SlotValueElicitationSetting (SlotValueElicitationSetting'),
    newSlotValueElicitationSetting,

    -- ** SlotValueRegexFilter
    SlotValueRegexFilter (SlotValueRegexFilter'),
    newSlotValueRegexFilter,

    -- ** SlotValueSelectionSetting
    SlotValueSelectionSetting (SlotValueSelectionSetting'),
    newSlotValueSelectionSetting,

    -- ** StillWaitingResponseSpecification
    StillWaitingResponseSpecification (StillWaitingResponseSpecification'),
    newStillWaitingResponseSpecification,

    -- ** TextLogDestination
    TextLogDestination (TextLogDestination'),
    newTextLogDestination,

    -- ** TextLogSetting
    TextLogSetting (TextLogSetting'),
    newTextLogSetting,

    -- ** UtteranceAggregationDuration
    UtteranceAggregationDuration (UtteranceAggregationDuration'),
    newUtteranceAggregationDuration,

    -- ** VoiceSettings
    VoiceSettings (VoiceSettings'),
    newVoiceSettings,

    -- ** WaitAndContinueSpecification
    WaitAndContinueSpecification (WaitAndContinueSpecification'),
    newWaitAndContinueSpecification,
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
import Network.AWS.LexV2Models.Lens
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
import Network.AWS.LexV2Models.Types
import Network.AWS.LexV2Models.UntagResource
import Network.AWS.LexV2Models.UpdateBot
import Network.AWS.LexV2Models.UpdateBotAlias
import Network.AWS.LexV2Models.UpdateBotLocale
import Network.AWS.LexV2Models.UpdateExport
import Network.AWS.LexV2Models.UpdateIntent
import Network.AWS.LexV2Models.UpdateResourcePolicy
import Network.AWS.LexV2Models.UpdateSlot
import Network.AWS.LexV2Models.UpdateSlotType
import Network.AWS.LexV2Models.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LexV2Models'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
