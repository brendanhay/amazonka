{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationInsights.Types.Observation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.Observation where

import Amazonka.ApplicationInsights.Types.CloudWatchEventSource
import Amazonka.ApplicationInsights.Types.LogFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an anomaly or error with the application.
--
-- /See:/ 'newObservation' smart constructor.
data Observation = Observation'
  { -- | The detail type of the CloudWatch Event-based observation, for example,
    -- @EC2 Instance State-change Notification@.
    cloudWatchEventDetailType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CloudWatch Event-based observation related to the detected
    -- problem.
    cloudWatchEventId :: Prelude.Maybe Prelude.Text,
    -- | The source of the CloudWatch Event.
    cloudWatchEventSource :: Prelude.Maybe CloudWatchEventSource,
    -- | The CodeDeploy application to which the deployment belongs.
    codeDeployApplication :: Prelude.Maybe Prelude.Text,
    -- | The deployment group to which the CodeDeploy deployment belongs.
    codeDeployDeploymentGroup :: Prelude.Maybe Prelude.Text,
    -- | The deployment ID of the CodeDeploy-based observation related to the
    -- detected problem.
    codeDeployDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The instance group to which the CodeDeploy instance belongs.
    codeDeployInstanceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the CodeDeploy deployment, for example @SUCCESS@ or
    -- @ FAILURE@.
    codeDeployState :: Prelude.Maybe Prelude.Text,
    -- | The cause of an EBS CloudWatch event.
    ebsCause :: Prelude.Maybe Prelude.Text,
    -- | The type of EBS CloudWatch event, such as @createVolume@, @deleteVolume@
    -- or @attachVolume@.
    ebsEvent :: Prelude.Maybe Prelude.Text,
    -- | The request ID of an EBS CloudWatch event.
    ebsRequestId :: Prelude.Maybe Prelude.Text,
    -- | The result of an EBS CloudWatch event, such as @failed@ or @succeeded@.
    ebsResult :: Prelude.Maybe Prelude.Text,
    -- | The state of the instance, such as @STOPPING@ or @TERMINATING@.
    ec2State :: Prelude.Maybe Prelude.Text,
    -- | The time when the observation ended, in epoch seconds.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the AWS Health Event-based
    -- observation.
    healthEventArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the AWS Health event provided by the service, such as
    -- Amazon EC2.
    healthEventDescription :: Prelude.Maybe Prelude.Text,
    -- | The category of the AWS Health event, such as @issue@.
    healthEventTypeCategory :: Prelude.Maybe Prelude.Text,
    -- | The type of the AWS Health event, for example,
    -- @AWS_EC2_POWER_CONNECTIVITY_ISSUE@.
    healthEventTypeCode :: Prelude.Maybe Prelude.Text,
    -- | The service to which the AWS Health Event belongs, such as EC2.
    healthService :: Prelude.Maybe Prelude.Text,
    -- | The ID of the observation type.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp in the CloudWatch Logs that specifies when the matched
    -- line occurred.
    lineTime :: Prelude.Maybe Data.POSIX,
    -- | The log filter of the observation.
    logFilter :: Prelude.Maybe LogFilter,
    -- | The log group name.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The log text of the observation.
    logText :: Prelude.Maybe Prelude.Text,
    -- | The name of the observation metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the observation metric.
    metricNamespace :: Prelude.Maybe Prelude.Text,
    -- | The category of an RDS event.
    rdsEventCategories :: Prelude.Maybe Prelude.Text,
    -- | The message of an RDS event.
    rdsEventMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 CloudWatch Event-based observation.
    s3EventName :: Prelude.Maybe Prelude.Text,
    -- | The source resource ARN of the observation.
    sourceARN :: Prelude.Maybe Prelude.Text,
    -- | The source type of the observation.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The time when the observation was first detected, in epoch seconds.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the step function-based observation.
    statesArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the step function execution-based
    -- observation.
    statesExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The input to the step function-based observation.
    statesInput :: Prelude.Maybe Prelude.Text,
    -- | The status of the step function-related observation.
    statesStatus :: Prelude.Maybe Prelude.Text,
    -- | The unit of the source observation metric.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The value of the source observation metric.
    value :: Prelude.Maybe Prelude.Double,
    -- | The X-Ray request error percentage for this node.
    xRayErrorPercent :: Prelude.Maybe Prelude.Int,
    -- | The X-Ray request fault percentage for this node.
    xRayFaultPercent :: Prelude.Maybe Prelude.Int,
    -- | The name of the X-Ray node.
    xRayNodeName :: Prelude.Maybe Prelude.Text,
    -- | The type of the X-Ray node.
    xRayNodeType :: Prelude.Maybe Prelude.Text,
    -- | The X-Ray node request average latency for this node.
    xRayRequestAverageLatency :: Prelude.Maybe Prelude.Integer,
    -- | The X-Ray request count for this node.
    xRayRequestCount :: Prelude.Maybe Prelude.Int,
    -- | The X-Ray request throttle percentage for this node.
    xRayThrottlePercent :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Observation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchEventDetailType', 'observation_cloudWatchEventDetailType' - The detail type of the CloudWatch Event-based observation, for example,
-- @EC2 Instance State-change Notification@.
--
-- 'cloudWatchEventId', 'observation_cloudWatchEventId' - The ID of the CloudWatch Event-based observation related to the detected
-- problem.
--
-- 'cloudWatchEventSource', 'observation_cloudWatchEventSource' - The source of the CloudWatch Event.
--
-- 'codeDeployApplication', 'observation_codeDeployApplication' - The CodeDeploy application to which the deployment belongs.
--
-- 'codeDeployDeploymentGroup', 'observation_codeDeployDeploymentGroup' - The deployment group to which the CodeDeploy deployment belongs.
--
-- 'codeDeployDeploymentId', 'observation_codeDeployDeploymentId' - The deployment ID of the CodeDeploy-based observation related to the
-- detected problem.
--
-- 'codeDeployInstanceGroupId', 'observation_codeDeployInstanceGroupId' - The instance group to which the CodeDeploy instance belongs.
--
-- 'codeDeployState', 'observation_codeDeployState' - The status of the CodeDeploy deployment, for example @SUCCESS@ or
-- @ FAILURE@.
--
-- 'ebsCause', 'observation_ebsCause' - The cause of an EBS CloudWatch event.
--
-- 'ebsEvent', 'observation_ebsEvent' - The type of EBS CloudWatch event, such as @createVolume@, @deleteVolume@
-- or @attachVolume@.
--
-- 'ebsRequestId', 'observation_ebsRequestId' - The request ID of an EBS CloudWatch event.
--
-- 'ebsResult', 'observation_ebsResult' - The result of an EBS CloudWatch event, such as @failed@ or @succeeded@.
--
-- 'ec2State', 'observation_ec2State' - The state of the instance, such as @STOPPING@ or @TERMINATING@.
--
-- 'endTime', 'observation_endTime' - The time when the observation ended, in epoch seconds.
--
-- 'healthEventArn', 'observation_healthEventArn' - The Amazon Resource Name (ARN) of the AWS Health Event-based
-- observation.
--
-- 'healthEventDescription', 'observation_healthEventDescription' - The description of the AWS Health event provided by the service, such as
-- Amazon EC2.
--
-- 'healthEventTypeCategory', 'observation_healthEventTypeCategory' - The category of the AWS Health event, such as @issue@.
--
-- 'healthEventTypeCode', 'observation_healthEventTypeCode' - The type of the AWS Health event, for example,
-- @AWS_EC2_POWER_CONNECTIVITY_ISSUE@.
--
-- 'healthService', 'observation_healthService' - The service to which the AWS Health Event belongs, such as EC2.
--
-- 'id', 'observation_id' - The ID of the observation type.
--
-- 'lineTime', 'observation_lineTime' - The timestamp in the CloudWatch Logs that specifies when the matched
-- line occurred.
--
-- 'logFilter', 'observation_logFilter' - The log filter of the observation.
--
-- 'logGroup', 'observation_logGroup' - The log group name.
--
-- 'logText', 'observation_logText' - The log text of the observation.
--
-- 'metricName', 'observation_metricName' - The name of the observation metric.
--
-- 'metricNamespace', 'observation_metricNamespace' - The namespace of the observation metric.
--
-- 'rdsEventCategories', 'observation_rdsEventCategories' - The category of an RDS event.
--
-- 'rdsEventMessage', 'observation_rdsEventMessage' - The message of an RDS event.
--
-- 's3EventName', 'observation_s3EventName' - The name of the S3 CloudWatch Event-based observation.
--
-- 'sourceARN', 'observation_sourceARN' - The source resource ARN of the observation.
--
-- 'sourceType', 'observation_sourceType' - The source type of the observation.
--
-- 'startTime', 'observation_startTime' - The time when the observation was first detected, in epoch seconds.
--
-- 'statesArn', 'observation_statesArn' - The Amazon Resource Name (ARN) of the step function-based observation.
--
-- 'statesExecutionArn', 'observation_statesExecutionArn' - The Amazon Resource Name (ARN) of the step function execution-based
-- observation.
--
-- 'statesInput', 'observation_statesInput' - The input to the step function-based observation.
--
-- 'statesStatus', 'observation_statesStatus' - The status of the step function-related observation.
--
-- 'unit', 'observation_unit' - The unit of the source observation metric.
--
-- 'value', 'observation_value' - The value of the source observation metric.
--
-- 'xRayErrorPercent', 'observation_xRayErrorPercent' - The X-Ray request error percentage for this node.
--
-- 'xRayFaultPercent', 'observation_xRayFaultPercent' - The X-Ray request fault percentage for this node.
--
-- 'xRayNodeName', 'observation_xRayNodeName' - The name of the X-Ray node.
--
-- 'xRayNodeType', 'observation_xRayNodeType' - The type of the X-Ray node.
--
-- 'xRayRequestAverageLatency', 'observation_xRayRequestAverageLatency' - The X-Ray node request average latency for this node.
--
-- 'xRayRequestCount', 'observation_xRayRequestCount' - The X-Ray request count for this node.
--
-- 'xRayThrottlePercent', 'observation_xRayThrottlePercent' - The X-Ray request throttle percentage for this node.
newObservation ::
  Observation
newObservation =
  Observation'
    { cloudWatchEventDetailType =
        Prelude.Nothing,
      cloudWatchEventId = Prelude.Nothing,
      cloudWatchEventSource = Prelude.Nothing,
      codeDeployApplication = Prelude.Nothing,
      codeDeployDeploymentGroup = Prelude.Nothing,
      codeDeployDeploymentId = Prelude.Nothing,
      codeDeployInstanceGroupId = Prelude.Nothing,
      codeDeployState = Prelude.Nothing,
      ebsCause = Prelude.Nothing,
      ebsEvent = Prelude.Nothing,
      ebsRequestId = Prelude.Nothing,
      ebsResult = Prelude.Nothing,
      ec2State = Prelude.Nothing,
      endTime = Prelude.Nothing,
      healthEventArn = Prelude.Nothing,
      healthEventDescription = Prelude.Nothing,
      healthEventTypeCategory = Prelude.Nothing,
      healthEventTypeCode = Prelude.Nothing,
      healthService = Prelude.Nothing,
      id = Prelude.Nothing,
      lineTime = Prelude.Nothing,
      logFilter = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      logText = Prelude.Nothing,
      metricName = Prelude.Nothing,
      metricNamespace = Prelude.Nothing,
      rdsEventCategories = Prelude.Nothing,
      rdsEventMessage = Prelude.Nothing,
      s3EventName = Prelude.Nothing,
      sourceARN = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      startTime = Prelude.Nothing,
      statesArn = Prelude.Nothing,
      statesExecutionArn = Prelude.Nothing,
      statesInput = Prelude.Nothing,
      statesStatus = Prelude.Nothing,
      unit = Prelude.Nothing,
      value = Prelude.Nothing,
      xRayErrorPercent = Prelude.Nothing,
      xRayFaultPercent = Prelude.Nothing,
      xRayNodeName = Prelude.Nothing,
      xRayNodeType = Prelude.Nothing,
      xRayRequestAverageLatency = Prelude.Nothing,
      xRayRequestCount = Prelude.Nothing,
      xRayThrottlePercent = Prelude.Nothing
    }

-- | The detail type of the CloudWatch Event-based observation, for example,
-- @EC2 Instance State-change Notification@.
observation_cloudWatchEventDetailType :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_cloudWatchEventDetailType = Lens.lens (\Observation' {cloudWatchEventDetailType} -> cloudWatchEventDetailType) (\s@Observation' {} a -> s {cloudWatchEventDetailType = a} :: Observation)

-- | The ID of the CloudWatch Event-based observation related to the detected
-- problem.
observation_cloudWatchEventId :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_cloudWatchEventId = Lens.lens (\Observation' {cloudWatchEventId} -> cloudWatchEventId) (\s@Observation' {} a -> s {cloudWatchEventId = a} :: Observation)

-- | The source of the CloudWatch Event.
observation_cloudWatchEventSource :: Lens.Lens' Observation (Prelude.Maybe CloudWatchEventSource)
observation_cloudWatchEventSource = Lens.lens (\Observation' {cloudWatchEventSource} -> cloudWatchEventSource) (\s@Observation' {} a -> s {cloudWatchEventSource = a} :: Observation)

-- | The CodeDeploy application to which the deployment belongs.
observation_codeDeployApplication :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_codeDeployApplication = Lens.lens (\Observation' {codeDeployApplication} -> codeDeployApplication) (\s@Observation' {} a -> s {codeDeployApplication = a} :: Observation)

-- | The deployment group to which the CodeDeploy deployment belongs.
observation_codeDeployDeploymentGroup :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_codeDeployDeploymentGroup = Lens.lens (\Observation' {codeDeployDeploymentGroup} -> codeDeployDeploymentGroup) (\s@Observation' {} a -> s {codeDeployDeploymentGroup = a} :: Observation)

-- | The deployment ID of the CodeDeploy-based observation related to the
-- detected problem.
observation_codeDeployDeploymentId :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_codeDeployDeploymentId = Lens.lens (\Observation' {codeDeployDeploymentId} -> codeDeployDeploymentId) (\s@Observation' {} a -> s {codeDeployDeploymentId = a} :: Observation)

-- | The instance group to which the CodeDeploy instance belongs.
observation_codeDeployInstanceGroupId :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_codeDeployInstanceGroupId = Lens.lens (\Observation' {codeDeployInstanceGroupId} -> codeDeployInstanceGroupId) (\s@Observation' {} a -> s {codeDeployInstanceGroupId = a} :: Observation)

-- | The status of the CodeDeploy deployment, for example @SUCCESS@ or
-- @ FAILURE@.
observation_codeDeployState :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_codeDeployState = Lens.lens (\Observation' {codeDeployState} -> codeDeployState) (\s@Observation' {} a -> s {codeDeployState = a} :: Observation)

-- | The cause of an EBS CloudWatch event.
observation_ebsCause :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_ebsCause = Lens.lens (\Observation' {ebsCause} -> ebsCause) (\s@Observation' {} a -> s {ebsCause = a} :: Observation)

-- | The type of EBS CloudWatch event, such as @createVolume@, @deleteVolume@
-- or @attachVolume@.
observation_ebsEvent :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_ebsEvent = Lens.lens (\Observation' {ebsEvent} -> ebsEvent) (\s@Observation' {} a -> s {ebsEvent = a} :: Observation)

-- | The request ID of an EBS CloudWatch event.
observation_ebsRequestId :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_ebsRequestId = Lens.lens (\Observation' {ebsRequestId} -> ebsRequestId) (\s@Observation' {} a -> s {ebsRequestId = a} :: Observation)

-- | The result of an EBS CloudWatch event, such as @failed@ or @succeeded@.
observation_ebsResult :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_ebsResult = Lens.lens (\Observation' {ebsResult} -> ebsResult) (\s@Observation' {} a -> s {ebsResult = a} :: Observation)

-- | The state of the instance, such as @STOPPING@ or @TERMINATING@.
observation_ec2State :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_ec2State = Lens.lens (\Observation' {ec2State} -> ec2State) (\s@Observation' {} a -> s {ec2State = a} :: Observation)

-- | The time when the observation ended, in epoch seconds.
observation_endTime :: Lens.Lens' Observation (Prelude.Maybe Prelude.UTCTime)
observation_endTime = Lens.lens (\Observation' {endTime} -> endTime) (\s@Observation' {} a -> s {endTime = a} :: Observation) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the AWS Health Event-based
-- observation.
observation_healthEventArn :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_healthEventArn = Lens.lens (\Observation' {healthEventArn} -> healthEventArn) (\s@Observation' {} a -> s {healthEventArn = a} :: Observation)

-- | The description of the AWS Health event provided by the service, such as
-- Amazon EC2.
observation_healthEventDescription :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_healthEventDescription = Lens.lens (\Observation' {healthEventDescription} -> healthEventDescription) (\s@Observation' {} a -> s {healthEventDescription = a} :: Observation)

-- | The category of the AWS Health event, such as @issue@.
observation_healthEventTypeCategory :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_healthEventTypeCategory = Lens.lens (\Observation' {healthEventTypeCategory} -> healthEventTypeCategory) (\s@Observation' {} a -> s {healthEventTypeCategory = a} :: Observation)

-- | The type of the AWS Health event, for example,
-- @AWS_EC2_POWER_CONNECTIVITY_ISSUE@.
observation_healthEventTypeCode :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_healthEventTypeCode = Lens.lens (\Observation' {healthEventTypeCode} -> healthEventTypeCode) (\s@Observation' {} a -> s {healthEventTypeCode = a} :: Observation)

-- | The service to which the AWS Health Event belongs, such as EC2.
observation_healthService :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_healthService = Lens.lens (\Observation' {healthService} -> healthService) (\s@Observation' {} a -> s {healthService = a} :: Observation)

-- | The ID of the observation type.
observation_id :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_id = Lens.lens (\Observation' {id} -> id) (\s@Observation' {} a -> s {id = a} :: Observation)

-- | The timestamp in the CloudWatch Logs that specifies when the matched
-- line occurred.
observation_lineTime :: Lens.Lens' Observation (Prelude.Maybe Prelude.UTCTime)
observation_lineTime = Lens.lens (\Observation' {lineTime} -> lineTime) (\s@Observation' {} a -> s {lineTime = a} :: Observation) Prelude.. Lens.mapping Data._Time

-- | The log filter of the observation.
observation_logFilter :: Lens.Lens' Observation (Prelude.Maybe LogFilter)
observation_logFilter = Lens.lens (\Observation' {logFilter} -> logFilter) (\s@Observation' {} a -> s {logFilter = a} :: Observation)

-- | The log group name.
observation_logGroup :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_logGroup = Lens.lens (\Observation' {logGroup} -> logGroup) (\s@Observation' {} a -> s {logGroup = a} :: Observation)

-- | The log text of the observation.
observation_logText :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_logText = Lens.lens (\Observation' {logText} -> logText) (\s@Observation' {} a -> s {logText = a} :: Observation)

-- | The name of the observation metric.
observation_metricName :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_metricName = Lens.lens (\Observation' {metricName} -> metricName) (\s@Observation' {} a -> s {metricName = a} :: Observation)

-- | The namespace of the observation metric.
observation_metricNamespace :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_metricNamespace = Lens.lens (\Observation' {metricNamespace} -> metricNamespace) (\s@Observation' {} a -> s {metricNamespace = a} :: Observation)

-- | The category of an RDS event.
observation_rdsEventCategories :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_rdsEventCategories = Lens.lens (\Observation' {rdsEventCategories} -> rdsEventCategories) (\s@Observation' {} a -> s {rdsEventCategories = a} :: Observation)

-- | The message of an RDS event.
observation_rdsEventMessage :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_rdsEventMessage = Lens.lens (\Observation' {rdsEventMessage} -> rdsEventMessage) (\s@Observation' {} a -> s {rdsEventMessage = a} :: Observation)

-- | The name of the S3 CloudWatch Event-based observation.
observation_s3EventName :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_s3EventName = Lens.lens (\Observation' {s3EventName} -> s3EventName) (\s@Observation' {} a -> s {s3EventName = a} :: Observation)

-- | The source resource ARN of the observation.
observation_sourceARN :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_sourceARN = Lens.lens (\Observation' {sourceARN} -> sourceARN) (\s@Observation' {} a -> s {sourceARN = a} :: Observation)

-- | The source type of the observation.
observation_sourceType :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_sourceType = Lens.lens (\Observation' {sourceType} -> sourceType) (\s@Observation' {} a -> s {sourceType = a} :: Observation)

-- | The time when the observation was first detected, in epoch seconds.
observation_startTime :: Lens.Lens' Observation (Prelude.Maybe Prelude.UTCTime)
observation_startTime = Lens.lens (\Observation' {startTime} -> startTime) (\s@Observation' {} a -> s {startTime = a} :: Observation) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the step function-based observation.
observation_statesArn :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_statesArn = Lens.lens (\Observation' {statesArn} -> statesArn) (\s@Observation' {} a -> s {statesArn = a} :: Observation)

-- | The Amazon Resource Name (ARN) of the step function execution-based
-- observation.
observation_statesExecutionArn :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_statesExecutionArn = Lens.lens (\Observation' {statesExecutionArn} -> statesExecutionArn) (\s@Observation' {} a -> s {statesExecutionArn = a} :: Observation)

-- | The input to the step function-based observation.
observation_statesInput :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_statesInput = Lens.lens (\Observation' {statesInput} -> statesInput) (\s@Observation' {} a -> s {statesInput = a} :: Observation)

-- | The status of the step function-related observation.
observation_statesStatus :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_statesStatus = Lens.lens (\Observation' {statesStatus} -> statesStatus) (\s@Observation' {} a -> s {statesStatus = a} :: Observation)

-- | The unit of the source observation metric.
observation_unit :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_unit = Lens.lens (\Observation' {unit} -> unit) (\s@Observation' {} a -> s {unit = a} :: Observation)

-- | The value of the source observation metric.
observation_value :: Lens.Lens' Observation (Prelude.Maybe Prelude.Double)
observation_value = Lens.lens (\Observation' {value} -> value) (\s@Observation' {} a -> s {value = a} :: Observation)

-- | The X-Ray request error percentage for this node.
observation_xRayErrorPercent :: Lens.Lens' Observation (Prelude.Maybe Prelude.Int)
observation_xRayErrorPercent = Lens.lens (\Observation' {xRayErrorPercent} -> xRayErrorPercent) (\s@Observation' {} a -> s {xRayErrorPercent = a} :: Observation)

-- | The X-Ray request fault percentage for this node.
observation_xRayFaultPercent :: Lens.Lens' Observation (Prelude.Maybe Prelude.Int)
observation_xRayFaultPercent = Lens.lens (\Observation' {xRayFaultPercent} -> xRayFaultPercent) (\s@Observation' {} a -> s {xRayFaultPercent = a} :: Observation)

-- | The name of the X-Ray node.
observation_xRayNodeName :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_xRayNodeName = Lens.lens (\Observation' {xRayNodeName} -> xRayNodeName) (\s@Observation' {} a -> s {xRayNodeName = a} :: Observation)

-- | The type of the X-Ray node.
observation_xRayNodeType :: Lens.Lens' Observation (Prelude.Maybe Prelude.Text)
observation_xRayNodeType = Lens.lens (\Observation' {xRayNodeType} -> xRayNodeType) (\s@Observation' {} a -> s {xRayNodeType = a} :: Observation)

-- | The X-Ray node request average latency for this node.
observation_xRayRequestAverageLatency :: Lens.Lens' Observation (Prelude.Maybe Prelude.Integer)
observation_xRayRequestAverageLatency = Lens.lens (\Observation' {xRayRequestAverageLatency} -> xRayRequestAverageLatency) (\s@Observation' {} a -> s {xRayRequestAverageLatency = a} :: Observation)

-- | The X-Ray request count for this node.
observation_xRayRequestCount :: Lens.Lens' Observation (Prelude.Maybe Prelude.Int)
observation_xRayRequestCount = Lens.lens (\Observation' {xRayRequestCount} -> xRayRequestCount) (\s@Observation' {} a -> s {xRayRequestCount = a} :: Observation)

-- | The X-Ray request throttle percentage for this node.
observation_xRayThrottlePercent :: Lens.Lens' Observation (Prelude.Maybe Prelude.Int)
observation_xRayThrottlePercent = Lens.lens (\Observation' {xRayThrottlePercent} -> xRayThrottlePercent) (\s@Observation' {} a -> s {xRayThrottlePercent = a} :: Observation)

instance Data.FromJSON Observation where
  parseJSON =
    Data.withObject
      "Observation"
      ( \x ->
          Observation'
            Prelude.<$> (x Data..:? "CloudWatchEventDetailType")
            Prelude.<*> (x Data..:? "CloudWatchEventId")
            Prelude.<*> (x Data..:? "CloudWatchEventSource")
            Prelude.<*> (x Data..:? "CodeDeployApplication")
            Prelude.<*> (x Data..:? "CodeDeployDeploymentGroup")
            Prelude.<*> (x Data..:? "CodeDeployDeploymentId")
            Prelude.<*> (x Data..:? "CodeDeployInstanceGroupId")
            Prelude.<*> (x Data..:? "CodeDeployState")
            Prelude.<*> (x Data..:? "EbsCause")
            Prelude.<*> (x Data..:? "EbsEvent")
            Prelude.<*> (x Data..:? "EbsRequestId")
            Prelude.<*> (x Data..:? "EbsResult")
            Prelude.<*> (x Data..:? "Ec2State")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "HealthEventArn")
            Prelude.<*> (x Data..:? "HealthEventDescription")
            Prelude.<*> (x Data..:? "HealthEventTypeCategory")
            Prelude.<*> (x Data..:? "HealthEventTypeCode")
            Prelude.<*> (x Data..:? "HealthService")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LineTime")
            Prelude.<*> (x Data..:? "LogFilter")
            Prelude.<*> (x Data..:? "LogGroup")
            Prelude.<*> (x Data..:? "LogText")
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "MetricNamespace")
            Prelude.<*> (x Data..:? "RdsEventCategories")
            Prelude.<*> (x Data..:? "RdsEventMessage")
            Prelude.<*> (x Data..:? "S3EventName")
            Prelude.<*> (x Data..:? "SourceARN")
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "StatesArn")
            Prelude.<*> (x Data..:? "StatesExecutionArn")
            Prelude.<*> (x Data..:? "StatesInput")
            Prelude.<*> (x Data..:? "StatesStatus")
            Prelude.<*> (x Data..:? "Unit")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..:? "XRayErrorPercent")
            Prelude.<*> (x Data..:? "XRayFaultPercent")
            Prelude.<*> (x Data..:? "XRayNodeName")
            Prelude.<*> (x Data..:? "XRayNodeType")
            Prelude.<*> (x Data..:? "XRayRequestAverageLatency")
            Prelude.<*> (x Data..:? "XRayRequestCount")
            Prelude.<*> (x Data..:? "XRayThrottlePercent")
      )

instance Prelude.Hashable Observation where
  hashWithSalt _salt Observation' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchEventDetailType
      `Prelude.hashWithSalt` cloudWatchEventId
      `Prelude.hashWithSalt` cloudWatchEventSource
      `Prelude.hashWithSalt` codeDeployApplication
      `Prelude.hashWithSalt` codeDeployDeploymentGroup
      `Prelude.hashWithSalt` codeDeployDeploymentId
      `Prelude.hashWithSalt` codeDeployInstanceGroupId
      `Prelude.hashWithSalt` codeDeployState
      `Prelude.hashWithSalt` ebsCause
      `Prelude.hashWithSalt` ebsEvent
      `Prelude.hashWithSalt` ebsRequestId
      `Prelude.hashWithSalt` ebsResult
      `Prelude.hashWithSalt` ec2State
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` healthEventArn
      `Prelude.hashWithSalt` healthEventDescription
      `Prelude.hashWithSalt` healthEventTypeCategory
      `Prelude.hashWithSalt` healthEventTypeCode
      `Prelude.hashWithSalt` healthService
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lineTime
      `Prelude.hashWithSalt` logFilter
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` logText
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metricNamespace
      `Prelude.hashWithSalt` rdsEventCategories
      `Prelude.hashWithSalt` rdsEventMessage
      `Prelude.hashWithSalt` s3EventName
      `Prelude.hashWithSalt` sourceARN
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statesArn
      `Prelude.hashWithSalt` statesExecutionArn
      `Prelude.hashWithSalt` statesInput
      `Prelude.hashWithSalt` statesStatus
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` xRayErrorPercent
      `Prelude.hashWithSalt` xRayFaultPercent
      `Prelude.hashWithSalt` xRayNodeName
      `Prelude.hashWithSalt` xRayNodeType
      `Prelude.hashWithSalt` xRayRequestAverageLatency
      `Prelude.hashWithSalt` xRayRequestCount
      `Prelude.hashWithSalt` xRayThrottlePercent

instance Prelude.NFData Observation where
  rnf Observation' {..} =
    Prelude.rnf cloudWatchEventDetailType
      `Prelude.seq` Prelude.rnf cloudWatchEventId
      `Prelude.seq` Prelude.rnf cloudWatchEventSource
      `Prelude.seq` Prelude.rnf codeDeployApplication
      `Prelude.seq` Prelude.rnf codeDeployDeploymentGroup
      `Prelude.seq` Prelude.rnf codeDeployDeploymentId
      `Prelude.seq` Prelude.rnf codeDeployInstanceGroupId
      `Prelude.seq` Prelude.rnf codeDeployState
      `Prelude.seq` Prelude.rnf ebsCause
      `Prelude.seq` Prelude.rnf ebsEvent
      `Prelude.seq` Prelude.rnf ebsRequestId
      `Prelude.seq` Prelude.rnf ebsResult
      `Prelude.seq` Prelude.rnf ec2State
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf healthEventArn
      `Prelude.seq` Prelude.rnf healthEventDescription
      `Prelude.seq` Prelude.rnf healthEventTypeCategory
      `Prelude.seq` Prelude.rnf healthEventTypeCode
      `Prelude.seq` Prelude.rnf healthService
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lineTime
      `Prelude.seq` Prelude.rnf logFilter
      `Prelude.seq` Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf logText
      `Prelude.seq` Prelude.rnf
        metricName
      `Prelude.seq` Prelude.rnf
        metricNamespace
      `Prelude.seq` Prelude.rnf
        rdsEventCategories
      `Prelude.seq` Prelude.rnf
        rdsEventMessage
      `Prelude.seq` Prelude.rnf
        s3EventName
      `Prelude.seq` Prelude.rnf
        sourceARN
      `Prelude.seq` Prelude.rnf
        sourceType
      `Prelude.seq` Prelude.rnf
        startTime
      `Prelude.seq` Prelude.rnf
        statesArn
      `Prelude.seq` Prelude.rnf
        statesExecutionArn
      `Prelude.seq` Prelude.rnf
        statesInput
      `Prelude.seq` Prelude.rnf
        statesStatus
      `Prelude.seq` Prelude.rnf
        unit
      `Prelude.seq` Prelude.rnf
        value
      `Prelude.seq` Prelude.rnf
        xRayErrorPercent
      `Prelude.seq` Prelude.rnf
        xRayFaultPercent
      `Prelude.seq` Prelude.rnf
        xRayNodeName
      `Prelude.seq` Prelude.rnf
        xRayNodeType
      `Prelude.seq` Prelude.rnf
        xRayRequestAverageLatency
      `Prelude.seq` Prelude.rnf
        xRayRequestCount
      `Prelude.seq` Prelude.rnf
        xRayThrottlePercent
