{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-07-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Events monitors your equipment or device fleets for failures or
-- changes in operation, and triggers actions when such events occur. You
-- can use AWS IoT Events API operations to create, read, update, and
-- delete inputs and detector models, and to list their versions.
module Amazonka.IoTEvents
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAlarmModel
    CreateAlarmModel (CreateAlarmModel'),
    newCreateAlarmModel,
    CreateAlarmModelResponse (CreateAlarmModelResponse'),
    newCreateAlarmModelResponse,

    -- ** CreateDetectorModel
    CreateDetectorModel (CreateDetectorModel'),
    newCreateDetectorModel,
    CreateDetectorModelResponse (CreateDetectorModelResponse'),
    newCreateDetectorModelResponse,

    -- ** CreateInput
    CreateInput (CreateInput'),
    newCreateInput,
    CreateInputResponse (CreateInputResponse'),
    newCreateInputResponse,

    -- ** DeleteAlarmModel
    DeleteAlarmModel (DeleteAlarmModel'),
    newDeleteAlarmModel,
    DeleteAlarmModelResponse (DeleteAlarmModelResponse'),
    newDeleteAlarmModelResponse,

    -- ** DeleteDetectorModel
    DeleteDetectorModel (DeleteDetectorModel'),
    newDeleteDetectorModel,
    DeleteDetectorModelResponse (DeleteDetectorModelResponse'),
    newDeleteDetectorModelResponse,

    -- ** DeleteInput
    DeleteInput (DeleteInput'),
    newDeleteInput,
    DeleteInputResponse (DeleteInputResponse'),
    newDeleteInputResponse,

    -- ** DescribeAlarmModel
    DescribeAlarmModel (DescribeAlarmModel'),
    newDescribeAlarmModel,
    DescribeAlarmModelResponse (DescribeAlarmModelResponse'),
    newDescribeAlarmModelResponse,

    -- ** DescribeDetectorModel
    DescribeDetectorModel (DescribeDetectorModel'),
    newDescribeDetectorModel,
    DescribeDetectorModelResponse (DescribeDetectorModelResponse'),
    newDescribeDetectorModelResponse,

    -- ** DescribeDetectorModelAnalysis
    DescribeDetectorModelAnalysis (DescribeDetectorModelAnalysis'),
    newDescribeDetectorModelAnalysis,
    DescribeDetectorModelAnalysisResponse (DescribeDetectorModelAnalysisResponse'),
    newDescribeDetectorModelAnalysisResponse,

    -- ** DescribeInput
    DescribeInput (DescribeInput'),
    newDescribeInput,
    DescribeInputResponse (DescribeInputResponse'),
    newDescribeInputResponse,

    -- ** DescribeLoggingOptions
    DescribeLoggingOptions (DescribeLoggingOptions'),
    newDescribeLoggingOptions,
    DescribeLoggingOptionsResponse (DescribeLoggingOptionsResponse'),
    newDescribeLoggingOptionsResponse,

    -- ** GetDetectorModelAnalysisResults
    GetDetectorModelAnalysisResults (GetDetectorModelAnalysisResults'),
    newGetDetectorModelAnalysisResults,
    GetDetectorModelAnalysisResultsResponse (GetDetectorModelAnalysisResultsResponse'),
    newGetDetectorModelAnalysisResultsResponse,

    -- ** ListAlarmModelVersions
    ListAlarmModelVersions (ListAlarmModelVersions'),
    newListAlarmModelVersions,
    ListAlarmModelVersionsResponse (ListAlarmModelVersionsResponse'),
    newListAlarmModelVersionsResponse,

    -- ** ListAlarmModels
    ListAlarmModels (ListAlarmModels'),
    newListAlarmModels,
    ListAlarmModelsResponse (ListAlarmModelsResponse'),
    newListAlarmModelsResponse,

    -- ** ListDetectorModelVersions
    ListDetectorModelVersions (ListDetectorModelVersions'),
    newListDetectorModelVersions,
    ListDetectorModelVersionsResponse (ListDetectorModelVersionsResponse'),
    newListDetectorModelVersionsResponse,

    -- ** ListDetectorModels
    ListDetectorModels (ListDetectorModels'),
    newListDetectorModels,
    ListDetectorModelsResponse (ListDetectorModelsResponse'),
    newListDetectorModelsResponse,

    -- ** ListInputRoutings
    ListInputRoutings (ListInputRoutings'),
    newListInputRoutings,
    ListInputRoutingsResponse (ListInputRoutingsResponse'),
    newListInputRoutingsResponse,

    -- ** ListInputs
    ListInputs (ListInputs'),
    newListInputs,
    ListInputsResponse (ListInputsResponse'),
    newListInputsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** StartDetectorModelAnalysis
    StartDetectorModelAnalysis (StartDetectorModelAnalysis'),
    newStartDetectorModelAnalysis,
    StartDetectorModelAnalysisResponse (StartDetectorModelAnalysisResponse'),
    newStartDetectorModelAnalysisResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAlarmModel
    UpdateAlarmModel (UpdateAlarmModel'),
    newUpdateAlarmModel,
    UpdateAlarmModelResponse (UpdateAlarmModelResponse'),
    newUpdateAlarmModelResponse,

    -- ** UpdateDetectorModel
    UpdateDetectorModel (UpdateDetectorModel'),
    newUpdateDetectorModel,
    UpdateDetectorModelResponse (UpdateDetectorModelResponse'),
    newUpdateDetectorModelResponse,

    -- ** UpdateInput
    UpdateInput (UpdateInput'),
    newUpdateInput,
    UpdateInputResponse (UpdateInputResponse'),
    newUpdateInputResponse,

    -- * Types

    -- ** AlarmModelVersionStatus
    AlarmModelVersionStatus (..),

    -- ** AnalysisResultLevel
    AnalysisResultLevel (..),

    -- ** AnalysisStatus
    AnalysisStatus (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** DetectorModelVersionStatus
    DetectorModelVersionStatus (..),

    -- ** EvaluationMethod
    EvaluationMethod (..),

    -- ** InputStatus
    InputStatus (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** PayloadType
    PayloadType (..),

    -- ** AcknowledgeFlow
    AcknowledgeFlow (AcknowledgeFlow'),
    newAcknowledgeFlow,

    -- ** Action
    Action (Action'),
    newAction,

    -- ** AlarmAction
    AlarmAction (AlarmAction'),
    newAlarmAction,

    -- ** AlarmCapabilities
    AlarmCapabilities (AlarmCapabilities'),
    newAlarmCapabilities,

    -- ** AlarmEventActions
    AlarmEventActions (AlarmEventActions'),
    newAlarmEventActions,

    -- ** AlarmModelSummary
    AlarmModelSummary (AlarmModelSummary'),
    newAlarmModelSummary,

    -- ** AlarmModelVersionSummary
    AlarmModelVersionSummary (AlarmModelVersionSummary'),
    newAlarmModelVersionSummary,

    -- ** AlarmNotification
    AlarmNotification (AlarmNotification'),
    newAlarmNotification,

    -- ** AlarmRule
    AlarmRule (AlarmRule'),
    newAlarmRule,

    -- ** AnalysisResult
    AnalysisResult (AnalysisResult'),
    newAnalysisResult,

    -- ** AnalysisResultLocation
    AnalysisResultLocation (AnalysisResultLocation'),
    newAnalysisResultLocation,

    -- ** AssetPropertyTimestamp
    AssetPropertyTimestamp (AssetPropertyTimestamp'),
    newAssetPropertyTimestamp,

    -- ** AssetPropertyValue
    AssetPropertyValue (AssetPropertyValue'),
    newAssetPropertyValue,

    -- ** AssetPropertyVariant
    AssetPropertyVariant (AssetPropertyVariant'),
    newAssetPropertyVariant,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** ClearTimerAction
    ClearTimerAction (ClearTimerAction'),
    newClearTimerAction,

    -- ** DetectorDebugOption
    DetectorDebugOption (DetectorDebugOption'),
    newDetectorDebugOption,

    -- ** DetectorModel
    DetectorModel (DetectorModel'),
    newDetectorModel,

    -- ** DetectorModelConfiguration
    DetectorModelConfiguration (DetectorModelConfiguration'),
    newDetectorModelConfiguration,

    -- ** DetectorModelDefinition
    DetectorModelDefinition (DetectorModelDefinition'),
    newDetectorModelDefinition,

    -- ** DetectorModelSummary
    DetectorModelSummary (DetectorModelSummary'),
    newDetectorModelSummary,

    -- ** DetectorModelVersionSummary
    DetectorModelVersionSummary (DetectorModelVersionSummary'),
    newDetectorModelVersionSummary,

    -- ** DynamoDBAction
    DynamoDBAction (DynamoDBAction'),
    newDynamoDBAction,

    -- ** DynamoDBv2Action
    DynamoDBv2Action (DynamoDBv2Action'),
    newDynamoDBv2Action,

    -- ** EmailConfiguration
    EmailConfiguration (EmailConfiguration'),
    newEmailConfiguration,

    -- ** EmailContent
    EmailContent (EmailContent'),
    newEmailContent,

    -- ** EmailRecipients
    EmailRecipients (EmailRecipients'),
    newEmailRecipients,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** FirehoseAction
    FirehoseAction (FirehoseAction'),
    newFirehoseAction,

    -- ** InitializationConfiguration
    InitializationConfiguration (InitializationConfiguration'),
    newInitializationConfiguration,

    -- ** Input
    Input (Input'),
    newInput,

    -- ** InputConfiguration
    InputConfiguration (InputConfiguration'),
    newInputConfiguration,

    -- ** InputDefinition
    InputDefinition (InputDefinition'),
    newInputDefinition,

    -- ** InputIdentifier
    InputIdentifier (InputIdentifier'),
    newInputIdentifier,

    -- ** InputSummary
    InputSummary (InputSummary'),
    newInputSummary,

    -- ** IotEventsAction
    IotEventsAction (IotEventsAction'),
    newIotEventsAction,

    -- ** IotEventsInputIdentifier
    IotEventsInputIdentifier (IotEventsInputIdentifier'),
    newIotEventsInputIdentifier,

    -- ** IotSiteWiseAction
    IotSiteWiseAction (IotSiteWiseAction'),
    newIotSiteWiseAction,

    -- ** IotSiteWiseAssetModelPropertyIdentifier
    IotSiteWiseAssetModelPropertyIdentifier (IotSiteWiseAssetModelPropertyIdentifier'),
    newIotSiteWiseAssetModelPropertyIdentifier,

    -- ** IotSiteWiseInputIdentifier
    IotSiteWiseInputIdentifier (IotSiteWiseInputIdentifier'),
    newIotSiteWiseInputIdentifier,

    -- ** IotTopicPublishAction
    IotTopicPublishAction (IotTopicPublishAction'),
    newIotTopicPublishAction,

    -- ** LambdaAction
    LambdaAction (LambdaAction'),
    newLambdaAction,

    -- ** LoggingOptions
    LoggingOptions (LoggingOptions'),
    newLoggingOptions,

    -- ** NotificationAction
    NotificationAction (NotificationAction'),
    newNotificationAction,

    -- ** NotificationTargetActions
    NotificationTargetActions (NotificationTargetActions'),
    newNotificationTargetActions,

    -- ** OnEnterLifecycle
    OnEnterLifecycle (OnEnterLifecycle'),
    newOnEnterLifecycle,

    -- ** OnExitLifecycle
    OnExitLifecycle (OnExitLifecycle'),
    newOnExitLifecycle,

    -- ** OnInputLifecycle
    OnInputLifecycle (OnInputLifecycle'),
    newOnInputLifecycle,

    -- ** Payload
    Payload (Payload'),
    newPayload,

    -- ** RecipientDetail
    RecipientDetail (RecipientDetail'),
    newRecipientDetail,

    -- ** ResetTimerAction
    ResetTimerAction (ResetTimerAction'),
    newResetTimerAction,

    -- ** RoutedResource
    RoutedResource (RoutedResource'),
    newRoutedResource,

    -- ** SMSConfiguration
    SMSConfiguration (SMSConfiguration'),
    newSMSConfiguration,

    -- ** SNSTopicPublishAction
    SNSTopicPublishAction (SNSTopicPublishAction'),
    newSNSTopicPublishAction,

    -- ** SSOIdentity
    SSOIdentity (SSOIdentity'),
    newSSOIdentity,

    -- ** SetTimerAction
    SetTimerAction (SetTimerAction'),
    newSetTimerAction,

    -- ** SetVariableAction
    SetVariableAction (SetVariableAction'),
    newSetVariableAction,

    -- ** SimpleRule
    SimpleRule (SimpleRule'),
    newSimpleRule,

    -- ** SqsAction
    SqsAction (SqsAction'),
    newSqsAction,

    -- ** State
    State (State'),
    newState,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TransitionEvent
    TransitionEvent (TransitionEvent'),
    newTransitionEvent,
  )
where

import Amazonka.IoTEvents.CreateAlarmModel
import Amazonka.IoTEvents.CreateDetectorModel
import Amazonka.IoTEvents.CreateInput
import Amazonka.IoTEvents.DeleteAlarmModel
import Amazonka.IoTEvents.DeleteDetectorModel
import Amazonka.IoTEvents.DeleteInput
import Amazonka.IoTEvents.DescribeAlarmModel
import Amazonka.IoTEvents.DescribeDetectorModel
import Amazonka.IoTEvents.DescribeDetectorModelAnalysis
import Amazonka.IoTEvents.DescribeInput
import Amazonka.IoTEvents.DescribeLoggingOptions
import Amazonka.IoTEvents.GetDetectorModelAnalysisResults
import Amazonka.IoTEvents.Lens
import Amazonka.IoTEvents.ListAlarmModelVersions
import Amazonka.IoTEvents.ListAlarmModels
import Amazonka.IoTEvents.ListDetectorModelVersions
import Amazonka.IoTEvents.ListDetectorModels
import Amazonka.IoTEvents.ListInputRoutings
import Amazonka.IoTEvents.ListInputs
import Amazonka.IoTEvents.ListTagsForResource
import Amazonka.IoTEvents.PutLoggingOptions
import Amazonka.IoTEvents.StartDetectorModelAnalysis
import Amazonka.IoTEvents.TagResource
import Amazonka.IoTEvents.Types
import Amazonka.IoTEvents.UntagResource
import Amazonka.IoTEvents.UpdateAlarmModel
import Amazonka.IoTEvents.UpdateDetectorModel
import Amazonka.IoTEvents.UpdateInput
import Amazonka.IoTEvents.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTEvents'.

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
