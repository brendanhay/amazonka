{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTEvents.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalFailureException,
    _InvalidRequestException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _UnsupportedOperationException,

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
    action_clearTimer,
    action_dynamoDB,
    action_dynamoDBv2,
    action_firehose,
    action_iotEvents,
    action_iotSiteWise,
    action_iotTopicPublish,
    action_lambda,
    action_resetTimer,
    action_setTimer,
    action_setVariable,
    action_sns,
    action_sqs,

    -- * AlarmAction
    AlarmAction (..),
    newAlarmAction,
    alarmAction_dynamoDB,
    alarmAction_dynamoDBv2,
    alarmAction_firehose,
    alarmAction_iotEvents,
    alarmAction_iotSiteWise,
    alarmAction_iotTopicPublish,
    alarmAction_lambda,
    alarmAction_sns,
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
    alarmModelSummary_alarmModelDescription,
    alarmModelSummary_alarmModelName,
    alarmModelSummary_creationTime,

    -- * AlarmModelVersionSummary
    AlarmModelVersionSummary (..),
    newAlarmModelVersionSummary,
    alarmModelVersionSummary_alarmModelArn,
    alarmModelVersionSummary_alarmModelName,
    alarmModelVersionSummary_alarmModelVersion,
    alarmModelVersionSummary_creationTime,
    alarmModelVersionSummary_lastUpdateTime,
    alarmModelVersionSummary_roleArn,
    alarmModelVersionSummary_status,
    alarmModelVersionSummary_statusMessage,

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
    analysisResult_level,
    analysisResult_locations,
    analysisResult_message,
    analysisResult_type,

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
    assetPropertyValue_quality,
    assetPropertyValue_timestamp,
    assetPropertyValue_value,

    -- * AssetPropertyVariant
    AssetPropertyVariant (..),
    newAssetPropertyVariant,
    assetPropertyVariant_booleanValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_integerValue,
    assetPropertyVariant_stringValue,

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
    detectorModelConfiguration_detectorModelArn,
    detectorModelConfiguration_detectorModelDescription,
    detectorModelConfiguration_detectorModelName,
    detectorModelConfiguration_detectorModelVersion,
    detectorModelConfiguration_evaluationMethod,
    detectorModelConfiguration_key,
    detectorModelConfiguration_lastUpdateTime,
    detectorModelConfiguration_roleArn,
    detectorModelConfiguration_status,

    -- * DetectorModelDefinition
    DetectorModelDefinition (..),
    newDetectorModelDefinition,
    detectorModelDefinition_states,
    detectorModelDefinition_initialStateName,

    -- * DetectorModelSummary
    DetectorModelSummary (..),
    newDetectorModelSummary,
    detectorModelSummary_creationTime,
    detectorModelSummary_detectorModelDescription,
    detectorModelSummary_detectorModelName,

    -- * DetectorModelVersionSummary
    DetectorModelVersionSummary (..),
    newDetectorModelVersionSummary,
    detectorModelVersionSummary_creationTime,
    detectorModelVersionSummary_detectorModelArn,
    detectorModelVersionSummary_detectorModelName,
    detectorModelVersionSummary_detectorModelVersion,
    detectorModelVersionSummary_evaluationMethod,
    detectorModelVersionSummary_lastUpdateTime,
    detectorModelVersionSummary_roleArn,
    detectorModelVersionSummary_status,

    -- * DynamoDBAction
    DynamoDBAction (..),
    newDynamoDBAction,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_payload,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyType,
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
    emailContent_additionalMessage,
    emailContent_subject,

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
    firehoseAction_payload,
    firehoseAction_separator,
    firehoseAction_deliveryStreamName,

    -- * InitializationConfiguration
    InitializationConfiguration (..),
    newInitializationConfiguration,
    initializationConfiguration_disabledOnInitialization,

    -- * Input
    Input (..),
    newInput,
    input_inputConfiguration,
    input_inputDefinition,

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
    inputIdentifier_iotEventsInputIdentifier,
    inputIdentifier_iotSiteWiseInputIdentifier,

    -- * InputSummary
    InputSummary (..),
    newInputSummary,
    inputSummary_creationTime,
    inputSummary_inputArn,
    inputSummary_inputDescription,
    inputSummary_inputName,
    inputSummary_lastUpdateTime,
    inputSummary_status,

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
    iotSiteWiseAction_assetId,
    iotSiteWiseAction_entryId,
    iotSiteWiseAction_propertyAlias,
    iotSiteWiseAction_propertyId,
    iotSiteWiseAction_propertyValue,

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
    state_onExit,
    state_onInput,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.AcknowledgeFlow
import Amazonka.IoTEvents.Types.Action
import Amazonka.IoTEvents.Types.AlarmAction
import Amazonka.IoTEvents.Types.AlarmCapabilities
import Amazonka.IoTEvents.Types.AlarmEventActions
import Amazonka.IoTEvents.Types.AlarmModelSummary
import Amazonka.IoTEvents.Types.AlarmModelVersionStatus
import Amazonka.IoTEvents.Types.AlarmModelVersionSummary
import Amazonka.IoTEvents.Types.AlarmNotification
import Amazonka.IoTEvents.Types.AlarmRule
import Amazonka.IoTEvents.Types.AnalysisResult
import Amazonka.IoTEvents.Types.AnalysisResultLevel
import Amazonka.IoTEvents.Types.AnalysisResultLocation
import Amazonka.IoTEvents.Types.AnalysisStatus
import Amazonka.IoTEvents.Types.AssetPropertyTimestamp
import Amazonka.IoTEvents.Types.AssetPropertyValue
import Amazonka.IoTEvents.Types.AssetPropertyVariant
import Amazonka.IoTEvents.Types.Attribute
import Amazonka.IoTEvents.Types.ClearTimerAction
import Amazonka.IoTEvents.Types.ComparisonOperator
import Amazonka.IoTEvents.Types.DetectorDebugOption
import Amazonka.IoTEvents.Types.DetectorModel
import Amazonka.IoTEvents.Types.DetectorModelConfiguration
import Amazonka.IoTEvents.Types.DetectorModelDefinition
import Amazonka.IoTEvents.Types.DetectorModelSummary
import Amazonka.IoTEvents.Types.DetectorModelVersionStatus
import Amazonka.IoTEvents.Types.DetectorModelVersionSummary
import Amazonka.IoTEvents.Types.DynamoDBAction
import Amazonka.IoTEvents.Types.DynamoDBv2Action
import Amazonka.IoTEvents.Types.EmailConfiguration
import Amazonka.IoTEvents.Types.EmailContent
import Amazonka.IoTEvents.Types.EmailRecipients
import Amazonka.IoTEvents.Types.EvaluationMethod
import Amazonka.IoTEvents.Types.Event
import Amazonka.IoTEvents.Types.FirehoseAction
import Amazonka.IoTEvents.Types.InitializationConfiguration
import Amazonka.IoTEvents.Types.Input
import Amazonka.IoTEvents.Types.InputConfiguration
import Amazonka.IoTEvents.Types.InputDefinition
import Amazonka.IoTEvents.Types.InputIdentifier
import Amazonka.IoTEvents.Types.InputStatus
import Amazonka.IoTEvents.Types.InputSummary
import Amazonka.IoTEvents.Types.IotEventsAction
import Amazonka.IoTEvents.Types.IotEventsInputIdentifier
import Amazonka.IoTEvents.Types.IotSiteWiseAction
import Amazonka.IoTEvents.Types.IotSiteWiseAssetModelPropertyIdentifier
import Amazonka.IoTEvents.Types.IotSiteWiseInputIdentifier
import Amazonka.IoTEvents.Types.IotTopicPublishAction
import Amazonka.IoTEvents.Types.LambdaAction
import Amazonka.IoTEvents.Types.LoggingLevel
import Amazonka.IoTEvents.Types.LoggingOptions
import Amazonka.IoTEvents.Types.NotificationAction
import Amazonka.IoTEvents.Types.NotificationTargetActions
import Amazonka.IoTEvents.Types.OnEnterLifecycle
import Amazonka.IoTEvents.Types.OnExitLifecycle
import Amazonka.IoTEvents.Types.OnInputLifecycle
import Amazonka.IoTEvents.Types.Payload
import Amazonka.IoTEvents.Types.PayloadType
import Amazonka.IoTEvents.Types.RecipientDetail
import Amazonka.IoTEvents.Types.ResetTimerAction
import Amazonka.IoTEvents.Types.RoutedResource
import Amazonka.IoTEvents.Types.SMSConfiguration
import Amazonka.IoTEvents.Types.SNSTopicPublishAction
import Amazonka.IoTEvents.Types.SSOIdentity
import Amazonka.IoTEvents.Types.SetTimerAction
import Amazonka.IoTEvents.Types.SetVariableAction
import Amazonka.IoTEvents.Types.SimpleRule
import Amazonka.IoTEvents.Types.SqsAction
import Amazonka.IoTEvents.Types.State
import Amazonka.IoTEvents.Types.Tag
import Amazonka.IoTEvents.Types.TransitionEvent
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-07-27@ of the Amazon IoT Events SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTEvents",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "iotevents",
      Core.signingName = "iotevents",
      Core.version = "2018-07-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTEvents",
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

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The request was invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | The resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request could not be completed due to throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The requested operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
    Prelude.. Core.hasStatus 501
