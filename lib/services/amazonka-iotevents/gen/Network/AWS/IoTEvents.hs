{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoTEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-07-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Events monitors your equipment or device fleets for failures or
-- changes in operation, and triggers actions when such events occur. You
-- can use AWS IoT Events API operations to create, read, update, and
-- delete inputs and detector models, and to list their versions.
module Network.AWS.IoTEvents
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListInputs
    ListInputs (ListInputs'),
    newListInputs,
    ListInputsResponse (ListInputsResponse'),
    newListInputsResponse,

    -- ** StartDetectorModelAnalysis
    StartDetectorModelAnalysis (StartDetectorModelAnalysis'),
    newStartDetectorModelAnalysis,
    StartDetectorModelAnalysisResponse (StartDetectorModelAnalysisResponse'),
    newStartDetectorModelAnalysisResponse,

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** DescribeDetectorModelAnalysis
    DescribeDetectorModelAnalysis (DescribeDetectorModelAnalysis'),
    newDescribeDetectorModelAnalysis,
    DescribeDetectorModelAnalysisResponse (DescribeDetectorModelAnalysisResponse'),
    newDescribeDetectorModelAnalysisResponse,

    -- ** CreateInput
    CreateInput (CreateInput'),
    newCreateInput,
    CreateInputResponse (CreateInputResponse'),
    newCreateInputResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListAlarmModels
    ListAlarmModels (ListAlarmModels'),
    newListAlarmModels,
    ListAlarmModelsResponse (ListAlarmModelsResponse'),
    newListAlarmModelsResponse,

    -- ** DeleteAlarmModel
    DeleteAlarmModel (DeleteAlarmModel'),
    newDeleteAlarmModel,
    DeleteAlarmModelResponse (DeleteAlarmModelResponse'),
    newDeleteAlarmModelResponse,

    -- ** UpdateAlarmModel
    UpdateAlarmModel (UpdateAlarmModel'),
    newUpdateAlarmModel,
    UpdateAlarmModelResponse (UpdateAlarmModelResponse'),
    newUpdateAlarmModelResponse,

    -- ** CreateAlarmModel
    CreateAlarmModel (CreateAlarmModel'),
    newCreateAlarmModel,
    CreateAlarmModelResponse (CreateAlarmModelResponse'),
    newCreateAlarmModelResponse,

    -- ** GetDetectorModelAnalysisResults
    GetDetectorModelAnalysisResults (GetDetectorModelAnalysisResults'),
    newGetDetectorModelAnalysisResults,
    GetDetectorModelAnalysisResultsResponse (GetDetectorModelAnalysisResultsResponse'),
    newGetDetectorModelAnalysisResultsResponse,

    -- ** ListDetectorModelVersions
    ListDetectorModelVersions (ListDetectorModelVersions'),
    newListDetectorModelVersions,
    ListDetectorModelVersionsResponse (ListDetectorModelVersionsResponse'),
    newListDetectorModelVersionsResponse,

    -- ** DescribeAlarmModel
    DescribeAlarmModel (DescribeAlarmModel'),
    newDescribeAlarmModel,
    DescribeAlarmModelResponse (DescribeAlarmModelResponse'),
    newDescribeAlarmModelResponse,

    -- ** CreateDetectorModel
    CreateDetectorModel (CreateDetectorModel'),
    newCreateDetectorModel,
    CreateDetectorModelResponse (CreateDetectorModelResponse'),
    newCreateDetectorModelResponse,

    -- ** ListDetectorModels
    ListDetectorModels (ListDetectorModels'),
    newListDetectorModels,
    ListDetectorModelsResponse (ListDetectorModelsResponse'),
    newListDetectorModelsResponse,

    -- ** UpdateDetectorModel
    UpdateDetectorModel (UpdateDetectorModel'),
    newUpdateDetectorModel,
    UpdateDetectorModelResponse (UpdateDetectorModelResponse'),
    newUpdateDetectorModelResponse,

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

    -- ** UpdateInput
    UpdateInput (UpdateInput'),
    newUpdateInput,
    UpdateInputResponse (UpdateInputResponse'),
    newUpdateInputResponse,

    -- ** ListAlarmModelVersions
    ListAlarmModelVersions (ListAlarmModelVersions'),
    newListAlarmModelVersions,
    ListAlarmModelVersionsResponse (ListAlarmModelVersionsResponse'),
    newListAlarmModelVersionsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeDetectorModel
    DescribeDetectorModel (DescribeDetectorModel'),
    newDescribeDetectorModel,
    DescribeDetectorModelResponse (DescribeDetectorModelResponse'),
    newDescribeDetectorModelResponse,

    -- ** DescribeInput
    DescribeInput (DescribeInput'),
    newDescribeInput,
    DescribeInputResponse (DescribeInputResponse'),
    newDescribeInputResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListInputRoutings
    ListInputRoutings (ListInputRoutings'),
    newListInputRoutings,
    ListInputRoutingsResponse (ListInputRoutingsResponse'),
    newListInputRoutingsResponse,

    -- ** DescribeLoggingOptions
    DescribeLoggingOptions (DescribeLoggingOptions'),
    newDescribeLoggingOptions,
    DescribeLoggingOptionsResponse (DescribeLoggingOptionsResponse'),
    newDescribeLoggingOptionsResponse,

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

import Network.AWS.IoTEvents.CreateAlarmModel
import Network.AWS.IoTEvents.CreateDetectorModel
import Network.AWS.IoTEvents.CreateInput
import Network.AWS.IoTEvents.DeleteAlarmModel
import Network.AWS.IoTEvents.DeleteDetectorModel
import Network.AWS.IoTEvents.DeleteInput
import Network.AWS.IoTEvents.DescribeAlarmModel
import Network.AWS.IoTEvents.DescribeDetectorModel
import Network.AWS.IoTEvents.DescribeDetectorModelAnalysis
import Network.AWS.IoTEvents.DescribeInput
import Network.AWS.IoTEvents.DescribeLoggingOptions
import Network.AWS.IoTEvents.GetDetectorModelAnalysisResults
import Network.AWS.IoTEvents.Lens
import Network.AWS.IoTEvents.ListAlarmModelVersions
import Network.AWS.IoTEvents.ListAlarmModels
import Network.AWS.IoTEvents.ListDetectorModelVersions
import Network.AWS.IoTEvents.ListDetectorModels
import Network.AWS.IoTEvents.ListInputRoutings
import Network.AWS.IoTEvents.ListInputs
import Network.AWS.IoTEvents.ListTagsForResource
import Network.AWS.IoTEvents.PutLoggingOptions
import Network.AWS.IoTEvents.StartDetectorModelAnalysis
import Network.AWS.IoTEvents.TagResource
import Network.AWS.IoTEvents.Types
import Network.AWS.IoTEvents.UntagResource
import Network.AWS.IoTEvents.UpdateAlarmModel
import Network.AWS.IoTEvents.UpdateDetectorModel
import Network.AWS.IoTEvents.UpdateInput
import Network.AWS.IoTEvents.Waiters

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
