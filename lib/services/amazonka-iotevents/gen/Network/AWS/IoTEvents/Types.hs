{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTEvents.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _UnsupportedOperationException,
    _ResourceAlreadyExistsException,
    _ThrottlingException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * AlarmModelVersionStatus
    AlarmModelVersionStatus (..),

    -- * AnalysisResultLevel
    AnalysisResultLevel (..),

    -- * AnalysisStatus
    AnalysisStatus (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * DetectorModelVersionStatus
    DetectorModelVersionStatus (..),

    -- * EvaluationMethod
    EvaluationMethod (..),

    -- * InputStatus
    InputStatus (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * PayloadType
    PayloadType (..),

    -- * AcknowledgeFlow
    AcknowledgeFlow (..),
    newAcknowledgeFlow,
    acknowledgeFlow_enabled,

    -- * Action
    Action (..),
    newAction,
    action_iotTopicPublish,
    action_dynamoDBv2,
    action_resetTimer,
    action_setTimer,
    action_sns,
    action_clearTimer,
    action_dynamoDB,
    action_firehose,
    action_iotSiteWise,
    action_setVariable,
    action_lambda,
    action_iotEvents,
    action_sqs,

    -- * AlarmAction
    AlarmAction (..),
    newAlarmAction,
    alarmAction_iotTopicPublish,
    alarmAction_dynamoDBv2,
    alarmAction_sns,
    alarmAction_dynamoDB,
    alarmAction_firehose,
    alarmAction_iotSiteWise,
    alarmAction_lambda,
    alarmAction_iotEvents,
    alarmAction_sqs,

    -- * AlarmCapabilities
    AlarmCapabilities (..),
    newAlarmCapabilities,
    alarmCapabilities_acknowledgeFlow,
    alarmCapabilities_initializationConfiguration,

    -- * AlarmEventActions
    AlarmEventActions (..),
    newAlarmEventActions,
    alarmEventActions_alarmActions,

    -- * AlarmModelSummary
    AlarmModelSummary (..),
    newAlarmModelSummary,
    alarmModelSummary_creationTime,
    alarmModelSummary_alarmModelName,
    alarmModelSummary_alarmModelDescription,

    -- * AlarmModelVersionSummary
    AlarmModelVersionSummary (..),
    newAlarmModelVersionSummary,
    alarmModelVersionSummary_creationTime,
    alarmModelVersionSummary_status,
    alarmModelVersionSummary_alarmModelName,
    alarmModelVersionSummary_statusMessage,
    alarmModelVersionSummary_lastUpdateTime,
    alarmModelVersionSummary_alarmModelVersion,
    alarmModelVersionSummary_alarmModelArn,
    alarmModelVersionSummary_roleArn,

    -- * AlarmNotification
    AlarmNotification (..),
    newAlarmNotification,
    alarmNotification_notificationActions,

    -- * AlarmRule
    AlarmRule (..),
    newAlarmRule,
    alarmRule_simpleRule,

    -- * AnalysisResult
    AnalysisResult (..),
    newAnalysisResult,
    analysisResult_type,
    analysisResult_message,
    analysisResult_level,
    analysisResult_locations,

    -- * AnalysisResultLocation
    AnalysisResultLocation (..),
    newAnalysisResultLocation,
    analysisResultLocation_path,

    -- * AssetPropertyTimestamp
    AssetPropertyTimestamp (..),
    newAssetPropertyTimestamp,
    assetPropertyTimestamp_offsetInNanos,
    assetPropertyTimestamp_timeInSeconds,

    -- * AssetPropertyValue
    AssetPropertyValue (..),
    newAssetPropertyValue,
    assetPropertyValue_value,
    assetPropertyValue_quality,
    assetPropertyValue_timestamp,

    -- * AssetPropertyVariant
    AssetPropertyVariant (..),
    newAssetPropertyVariant,
    assetPropertyVariant_integerValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_stringValue,
    assetPropertyVariant_booleanValue,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_jsonPath,

    -- * ClearTimerAction
    ClearTimerAction (..),
    newClearTimerAction,
    clearTimerAction_timerName,

    -- * DetectorDebugOption
    DetectorDebugOption (..),
    newDetectorDebugOption,
    detectorDebugOption_keyValue,
    detectorDebugOption_detectorModelName,

    -- * DetectorModel
    DetectorModel (..),
    newDetectorModel,
    detectorModel_detectorModelConfiguration,
    detectorModel_detectorModelDefinition,

    -- * DetectorModelConfiguration
    DetectorModelConfiguration (..),
    newDetectorModelConfiguration,
    detectorModelConfiguration_creationTime,
    detectorModelConfiguration_status,
    detectorModelConfiguration_detectorModelName,
    detectorModelConfiguration_detectorModelArn,
    detectorModelConfiguration_detectorModelDescription,
    detectorModelConfiguration_key,
    detectorModelConfiguration_detectorModelVersion,
    detectorModelConfiguration_lastUpdateTime,
    detectorModelConfiguration_evaluationMethod,
    detectorModelConfiguration_roleArn,

    -- * DetectorModelDefinition
    DetectorModelDefinition (..),
    newDetectorModelDefinition,
    detectorModelDefinition_states,
    detectorModelDefinition_initialStateName,

    -- * DetectorModelSummary
    DetectorModelSummary (..),
    newDetectorModelSummary,
    detectorModelSummary_creationTime,
    detectorModelSummary_detectorModelName,
    detectorModelSummary_detectorModelDescription,

    -- * DetectorModelVersionSummary
    DetectorModelVersionSummary (..),
    newDetectorModelVersionSummary,
    detectorModelVersionSummary_creationTime,
    detectorModelVersionSummary_status,
    detectorModelVersionSummary_detectorModelName,
    detectorModelVersionSummary_detectorModelArn,
    detectorModelVersionSummary_detectorModelVersion,
    detectorModelVersionSummary_lastUpdateTime,
    detectorModelVersionSummary_evaluationMethod,
    detectorModelVersionSummary_roleArn,

    -- * DynamoDBAction
    DynamoDBAction (..),
    newDynamoDBAction,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_payload,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyValue,
    dynamoDBAction_hashKeyField,
    dynamoDBAction_hashKeyValue,
    dynamoDBAction_tableName,

    -- * DynamoDBv2Action
    DynamoDBv2Action (..),
    newDynamoDBv2Action,
    dynamoDBv2Action_payload,
    dynamoDBv2Action_tableName,

    -- * EmailConfiguration
    EmailConfiguration (..),
    newEmailConfiguration,
    emailConfiguration_content,
    emailConfiguration_from,
    emailConfiguration_recipients,

    -- * EmailContent
    EmailContent (..),
    newEmailContent,
    emailContent_subject,
    emailContent_additionalMessage,

    -- * EmailRecipients
    EmailRecipients (..),
    newEmailRecipients,
    emailRecipients_to,

    -- * Event
    Event (..),
    newEvent,
    event_actions,
    event_condition,
    event_eventName,

    -- * FirehoseAction
    FirehoseAction (..),
    newFirehoseAction,
    firehoseAction_separator,
    firehoseAction_payload,
    firehoseAction_deliveryStreamName,

    -- * InitializationConfiguration
    InitializationConfiguration (..),
    newInitializationConfiguration,
    initializationConfiguration_disabledOnInitialization,

    -- * Input
    Input (..),
    newInput,
    input_inputDefinition,
    input_inputConfiguration,

    -- * InputConfiguration
    InputConfiguration (..),
    newInputConfiguration,
    inputConfiguration_inputDescription,
    inputConfiguration_inputName,
    inputConfiguration_inputArn,
    inputConfiguration_creationTime,
    inputConfiguration_lastUpdateTime,
    inputConfiguration_status,

    -- * InputDefinition
    InputDefinition (..),
    newInputDefinition,
    inputDefinition_attributes,

    -- * InputIdentifier
    InputIdentifier (..),
    newInputIdentifier,
    inputIdentifier_iotSiteWiseInputIdentifier,
    inputIdentifier_iotEventsInputIdentifier,

    -- * InputSummary
    InputSummary (..),
    newInputSummary,
    inputSummary_creationTime,
    inputSummary_status,
    inputSummary_inputName,
    inputSummary_inputDescription,
    inputSummary_inputArn,
    inputSummary_lastUpdateTime,

    -- * IotEventsAction
    IotEventsAction (..),
    newIotEventsAction,
    iotEventsAction_payload,
    iotEventsAction_inputName,

    -- * IotEventsInputIdentifier
    IotEventsInputIdentifier (..),
    newIotEventsInputIdentifier,
    iotEventsInputIdentifier_inputName,

    -- * IotSiteWiseAction
    IotSiteWiseAction (..),
    newIotSiteWiseAction,
    iotSiteWiseAction_entryId,
    iotSiteWiseAction_propertyAlias,
    iotSiteWiseAction_propertyValue,
    iotSiteWiseAction_propertyId,
    iotSiteWiseAction_assetId,

    -- * IotSiteWiseAssetModelPropertyIdentifier
    IotSiteWiseAssetModelPropertyIdentifier (..),
    newIotSiteWiseAssetModelPropertyIdentifier,
    iotSiteWiseAssetModelPropertyIdentifier_assetModelId,
    iotSiteWiseAssetModelPropertyIdentifier_propertyId,

    -- * IotSiteWiseInputIdentifier
    IotSiteWiseInputIdentifier (..),
    newIotSiteWiseInputIdentifier,
    iotSiteWiseInputIdentifier_iotSiteWiseAssetModelPropertyIdentifier,

    -- * IotTopicPublishAction
    IotTopicPublishAction (..),
    newIotTopicPublishAction,
    iotTopicPublishAction_payload,
    iotTopicPublishAction_mqttTopic,

    -- * LambdaAction
    LambdaAction (..),
    newLambdaAction,
    lambdaAction_payload,
    lambdaAction_functionArn,

    -- * LoggingOptions
    LoggingOptions (..),
    newLoggingOptions,
    loggingOptions_detectorDebugOptions,
    loggingOptions_roleArn,
    loggingOptions_level,
    loggingOptions_enabled,

    -- * NotificationAction
    NotificationAction (..),
    newNotificationAction,
    notificationAction_emailConfigurations,
    notificationAction_smsConfigurations,
    notificationAction_action,

    -- * NotificationTargetActions
    NotificationTargetActions (..),
    newNotificationTargetActions,
    notificationTargetActions_lambdaAction,

    -- * OnEnterLifecycle
    OnEnterLifecycle (..),
    newOnEnterLifecycle,
    onEnterLifecycle_events,

    -- * OnExitLifecycle
    OnExitLifecycle (..),
    newOnExitLifecycle,
    onExitLifecycle_events,

    -- * OnInputLifecycle
    OnInputLifecycle (..),
    newOnInputLifecycle,
    onInputLifecycle_events,
    onInputLifecycle_transitionEvents,

    -- * Payload
    Payload (..),
    newPayload,
    payload_contentExpression,
    payload_type,

    -- * RecipientDetail
    RecipientDetail (..),
    newRecipientDetail,
    recipientDetail_ssoIdentity,

    -- * ResetTimerAction
    ResetTimerAction (..),
    newResetTimerAction,
    resetTimerAction_timerName,

    -- * RoutedResource
    RoutedResource (..),
    newRoutedResource,
    routedResource_arn,
    routedResource_name,

    -- * SMSConfiguration
    SMSConfiguration (..),
    newSMSConfiguration,
    sMSConfiguration_additionalMessage,
    sMSConfiguration_senderId,
    sMSConfiguration_recipients,

    -- * SNSTopicPublishAction
    SNSTopicPublishAction (..),
    newSNSTopicPublishAction,
    sNSTopicPublishAction_payload,
    sNSTopicPublishAction_targetArn,

    -- * SSOIdentity
    SSOIdentity (..),
    newSSOIdentity,
    sSOIdentity_userId,
    sSOIdentity_identityStoreId,

    -- * SetTimerAction
    SetTimerAction (..),
    newSetTimerAction,
    setTimerAction_durationExpression,
    setTimerAction_seconds,
    setTimerAction_timerName,

    -- * SetVariableAction
    SetVariableAction (..),
    newSetVariableAction,
    setVariableAction_variableName,
    setVariableAction_value,

    -- * SimpleRule
    SimpleRule (..),
    newSimpleRule,
    simpleRule_inputProperty,
    simpleRule_comparisonOperator,
    simpleRule_threshold,

    -- * SqsAction
    SqsAction (..),
    newSqsAction,
    sqsAction_payload,
    sqsAction_useBase64,
    sqsAction_queueUrl,

    -- * State
    State (..),
    newState,
    state_onEnter,
    state_onInput,
    state_onExit,
    state_stateName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TransitionEvent
    TransitionEvent (..),
    newTransitionEvent,
    transitionEvent_actions,
    transitionEvent_eventName,
    transitionEvent_condition,
    transitionEvent_nextState,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.AcknowledgeFlow
import Network.AWS.IoTEvents.Types.Action
import Network.AWS.IoTEvents.Types.AlarmAction
import Network.AWS.IoTEvents.Types.AlarmCapabilities
import Network.AWS.IoTEvents.Types.AlarmEventActions
import Network.AWS.IoTEvents.Types.AlarmModelSummary
import Network.AWS.IoTEvents.Types.AlarmModelVersionStatus
import Network.AWS.IoTEvents.Types.AlarmModelVersionSummary
import Network.AWS.IoTEvents.Types.AlarmNotification
import Network.AWS.IoTEvents.Types.AlarmRule
import Network.AWS.IoTEvents.Types.AnalysisResult
import Network.AWS.IoTEvents.Types.AnalysisResultLevel
import Network.AWS.IoTEvents.Types.AnalysisResultLocation
import Network.AWS.IoTEvents.Types.AnalysisStatus
import Network.AWS.IoTEvents.Types.AssetPropertyTimestamp
import Network.AWS.IoTEvents.Types.AssetPropertyValue
import Network.AWS.IoTEvents.Types.AssetPropertyVariant
import Network.AWS.IoTEvents.Types.Attribute
import Network.AWS.IoTEvents.Types.ClearTimerAction
import Network.AWS.IoTEvents.Types.ComparisonOperator
import Network.AWS.IoTEvents.Types.DetectorDebugOption
import Network.AWS.IoTEvents.Types.DetectorModel
import Network.AWS.IoTEvents.Types.DetectorModelConfiguration
import Network.AWS.IoTEvents.Types.DetectorModelDefinition
import Network.AWS.IoTEvents.Types.DetectorModelSummary
import Network.AWS.IoTEvents.Types.DetectorModelVersionStatus
import Network.AWS.IoTEvents.Types.DetectorModelVersionSummary
import Network.AWS.IoTEvents.Types.DynamoDBAction
import Network.AWS.IoTEvents.Types.DynamoDBv2Action
import Network.AWS.IoTEvents.Types.EmailConfiguration
import Network.AWS.IoTEvents.Types.EmailContent
import Network.AWS.IoTEvents.Types.EmailRecipients
import Network.AWS.IoTEvents.Types.EvaluationMethod
import Network.AWS.IoTEvents.Types.Event
import Network.AWS.IoTEvents.Types.FirehoseAction
import Network.AWS.IoTEvents.Types.InitializationConfiguration
import Network.AWS.IoTEvents.Types.Input
import Network.AWS.IoTEvents.Types.InputConfiguration
import Network.AWS.IoTEvents.Types.InputDefinition
import Network.AWS.IoTEvents.Types.InputIdentifier
import Network.AWS.IoTEvents.Types.InputStatus
import Network.AWS.IoTEvents.Types.InputSummary
import Network.AWS.IoTEvents.Types.IotEventsAction
import Network.AWS.IoTEvents.Types.IotEventsInputIdentifier
import Network.AWS.IoTEvents.Types.IotSiteWiseAction
import Network.AWS.IoTEvents.Types.IotSiteWiseAssetModelPropertyIdentifier
import Network.AWS.IoTEvents.Types.IotSiteWiseInputIdentifier
import Network.AWS.IoTEvents.Types.IotTopicPublishAction
import Network.AWS.IoTEvents.Types.LambdaAction
import Network.AWS.IoTEvents.Types.LoggingLevel
import Network.AWS.IoTEvents.Types.LoggingOptions
import Network.AWS.IoTEvents.Types.NotificationAction
import Network.AWS.IoTEvents.Types.NotificationTargetActions
import Network.AWS.IoTEvents.Types.OnEnterLifecycle
import Network.AWS.IoTEvents.Types.OnExitLifecycle
import Network.AWS.IoTEvents.Types.OnInputLifecycle
import Network.AWS.IoTEvents.Types.Payload
import Network.AWS.IoTEvents.Types.PayloadType
import Network.AWS.IoTEvents.Types.RecipientDetail
import Network.AWS.IoTEvents.Types.ResetTimerAction
import Network.AWS.IoTEvents.Types.RoutedResource
import Network.AWS.IoTEvents.Types.SMSConfiguration
import Network.AWS.IoTEvents.Types.SNSTopicPublishAction
import Network.AWS.IoTEvents.Types.SSOIdentity
import Network.AWS.IoTEvents.Types.SetTimerAction
import Network.AWS.IoTEvents.Types.SetVariableAction
import Network.AWS.IoTEvents.Types.SimpleRule
import Network.AWS.IoTEvents.Types.SqsAction
import Network.AWS.IoTEvents.Types.State
import Network.AWS.IoTEvents.Types.Tag
import Network.AWS.IoTEvents.Types.TransitionEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-07-27@ of the Amazon IoT Events SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoTEvents",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "iotevents",
      Core._serviceSigningName = "iotevents",
      Core._serviceVersion = "2018-07-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "IoTEvents",
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

-- | The request was invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The requested operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
    Prelude.. Core.hasStatus 501

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The request could not be completed due to throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410

-- | The resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409
