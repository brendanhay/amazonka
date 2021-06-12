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
-- Module      : Network.AWS.IoT.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Action where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.CloudwatchAlarmAction
import Network.AWS.IoT.Types.CloudwatchLogsAction
import Network.AWS.IoT.Types.CloudwatchMetricAction
import Network.AWS.IoT.Types.DynamoDBAction
import Network.AWS.IoT.Types.DynamoDBv2Action
import Network.AWS.IoT.Types.ElasticsearchAction
import Network.AWS.IoT.Types.FirehoseAction
import Network.AWS.IoT.Types.HttpAction
import Network.AWS.IoT.Types.IotAnalyticsAction
import Network.AWS.IoT.Types.IotEventsAction
import Network.AWS.IoT.Types.IotSiteWiseAction
import Network.AWS.IoT.Types.KafkaAction
import Network.AWS.IoT.Types.KinesisAction
import Network.AWS.IoT.Types.LambdaAction
import Network.AWS.IoT.Types.RepublishAction
import Network.AWS.IoT.Types.S3Action
import Network.AWS.IoT.Types.SalesforceAction
import Network.AWS.IoT.Types.SnsAction
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.TimestreamAction
import qualified Network.AWS.Lens as Lens

-- | Describes the actions associated with a rule.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Send data to CloudWatch Logs.
    cloudwatchLogs :: Core.Maybe CloudwatchLogsAction,
    -- | Capture a CloudWatch metric.
    cloudwatchMetric :: Core.Maybe CloudwatchMetricAction,
    -- | Publish to an Amazon SQS queue.
    sqs :: Core.Maybe SqsAction,
    -- | The Timestream rule action writes attributes (measures) from an MQTT
    -- message into an Amazon Timestream table. For more information, see the
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
    -- topic rule action documentation.
    timestream :: Core.Maybe TimestreamAction,
    -- | Write to an Amazon Kinesis Firehose stream.
    firehose :: Core.Maybe FirehoseAction,
    -- | Publish to an Amazon SNS topic.
    sns :: Core.Maybe SnsAction,
    -- | Write data to an Amazon Elasticsearch Service domain.
    elasticsearch :: Core.Maybe ElasticsearchAction,
    -- | Write data to an Amazon Kinesis stream.
    kinesis :: Core.Maybe KinesisAction,
    -- | Send a message to a Salesforce IoT Cloud Input Stream.
    salesforce :: Core.Maybe SalesforceAction,
    -- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
    -- It allows you to write each attribute in an MQTT message payload into a
    -- separate DynamoDB column.
    dynamoDBv2 :: Core.Maybe DynamoDBv2Action,
    -- | Invoke a Lambda function.
    lambda :: Core.Maybe LambdaAction,
    -- | Sends message data to an AWS IoT Analytics channel.
    iotAnalytics :: Core.Maybe IotAnalyticsAction,
    -- | Sends data from the MQTT message that triggered the rule to AWS IoT
    -- SiteWise asset properties.
    iotSiteWise :: Core.Maybe IotSiteWiseAction,
    -- | Publish to another MQTT topic.
    republish :: Core.Maybe RepublishAction,
    -- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
    -- MSK) or self-managed Apache Kafka cluster.
    kafka :: Core.Maybe KafkaAction,
    -- | Write to a DynamoDB table.
    dynamoDB :: Core.Maybe DynamoDBAction,
    -- | Starts execution of a Step Functions state machine.
    stepFunctions :: Core.Maybe StepFunctionsAction,
    -- | Change the state of a CloudWatch alarm.
    cloudwatchAlarm :: Core.Maybe CloudwatchAlarmAction,
    -- | Send data to an HTTPS endpoint.
    http :: Core.Maybe HttpAction,
    -- | Write to an Amazon S3 bucket.
    s3 :: Core.Maybe S3Action,
    -- | Sends an input to an AWS IoT Events detector.
    iotEvents :: Core.Maybe IotEventsAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudwatchLogs', 'action_cloudwatchLogs' - Send data to CloudWatch Logs.
--
-- 'cloudwatchMetric', 'action_cloudwatchMetric' - Capture a CloudWatch metric.
--
-- 'sqs', 'action_sqs' - Publish to an Amazon SQS queue.
--
-- 'timestream', 'action_timestream' - The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
--
-- 'firehose', 'action_firehose' - Write to an Amazon Kinesis Firehose stream.
--
-- 'sns', 'action_sns' - Publish to an Amazon SNS topic.
--
-- 'elasticsearch', 'action_elasticsearch' - Write data to an Amazon Elasticsearch Service domain.
--
-- 'kinesis', 'action_kinesis' - Write data to an Amazon Kinesis stream.
--
-- 'salesforce', 'action_salesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
--
-- 'dynamoDBv2', 'action_dynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
--
-- 'lambda', 'action_lambda' - Invoke a Lambda function.
--
-- 'iotAnalytics', 'action_iotAnalytics' - Sends message data to an AWS IoT Analytics channel.
--
-- 'iotSiteWise', 'action_iotSiteWise' - Sends data from the MQTT message that triggered the rule to AWS IoT
-- SiteWise asset properties.
--
-- 'republish', 'action_republish' - Publish to another MQTT topic.
--
-- 'kafka', 'action_kafka' - Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
--
-- 'dynamoDB', 'action_dynamoDB' - Write to a DynamoDB table.
--
-- 'stepFunctions', 'action_stepFunctions' - Starts execution of a Step Functions state machine.
--
-- 'cloudwatchAlarm', 'action_cloudwatchAlarm' - Change the state of a CloudWatch alarm.
--
-- 'http', 'action_http' - Send data to an HTTPS endpoint.
--
-- 's3', 'action_s3' - Write to an Amazon S3 bucket.
--
-- 'iotEvents', 'action_iotEvents' - Sends an input to an AWS IoT Events detector.
newAction ::
  Action
newAction =
  Action'
    { cloudwatchLogs = Core.Nothing,
      cloudwatchMetric = Core.Nothing,
      sqs = Core.Nothing,
      timestream = Core.Nothing,
      firehose = Core.Nothing,
      sns = Core.Nothing,
      elasticsearch = Core.Nothing,
      kinesis = Core.Nothing,
      salesforce = Core.Nothing,
      dynamoDBv2 = Core.Nothing,
      lambda = Core.Nothing,
      iotAnalytics = Core.Nothing,
      iotSiteWise = Core.Nothing,
      republish = Core.Nothing,
      kafka = Core.Nothing,
      dynamoDB = Core.Nothing,
      stepFunctions = Core.Nothing,
      cloudwatchAlarm = Core.Nothing,
      http = Core.Nothing,
      s3 = Core.Nothing,
      iotEvents = Core.Nothing
    }

-- | Send data to CloudWatch Logs.
action_cloudwatchLogs :: Lens.Lens' Action (Core.Maybe CloudwatchLogsAction)
action_cloudwatchLogs = Lens.lens (\Action' {cloudwatchLogs} -> cloudwatchLogs) (\s@Action' {} a -> s {cloudwatchLogs = a} :: Action)

-- | Capture a CloudWatch metric.
action_cloudwatchMetric :: Lens.Lens' Action (Core.Maybe CloudwatchMetricAction)
action_cloudwatchMetric = Lens.lens (\Action' {cloudwatchMetric} -> cloudwatchMetric) (\s@Action' {} a -> s {cloudwatchMetric = a} :: Action)

-- | Publish to an Amazon SQS queue.
action_sqs :: Lens.Lens' Action (Core.Maybe SqsAction)
action_sqs = Lens.lens (\Action' {sqs} -> sqs) (\s@Action' {} a -> s {sqs = a} :: Action)

-- | The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
action_timestream :: Lens.Lens' Action (Core.Maybe TimestreamAction)
action_timestream = Lens.lens (\Action' {timestream} -> timestream) (\s@Action' {} a -> s {timestream = a} :: Action)

-- | Write to an Amazon Kinesis Firehose stream.
action_firehose :: Lens.Lens' Action (Core.Maybe FirehoseAction)
action_firehose = Lens.lens (\Action' {firehose} -> firehose) (\s@Action' {} a -> s {firehose = a} :: Action)

-- | Publish to an Amazon SNS topic.
action_sns :: Lens.Lens' Action (Core.Maybe SnsAction)
action_sns = Lens.lens (\Action' {sns} -> sns) (\s@Action' {} a -> s {sns = a} :: Action)

-- | Write data to an Amazon Elasticsearch Service domain.
action_elasticsearch :: Lens.Lens' Action (Core.Maybe ElasticsearchAction)
action_elasticsearch = Lens.lens (\Action' {elasticsearch} -> elasticsearch) (\s@Action' {} a -> s {elasticsearch = a} :: Action)

-- | Write data to an Amazon Kinesis stream.
action_kinesis :: Lens.Lens' Action (Core.Maybe KinesisAction)
action_kinesis = Lens.lens (\Action' {kinesis} -> kinesis) (\s@Action' {} a -> s {kinesis = a} :: Action)

-- | Send a message to a Salesforce IoT Cloud Input Stream.
action_salesforce :: Lens.Lens' Action (Core.Maybe SalesforceAction)
action_salesforce = Lens.lens (\Action' {salesforce} -> salesforce) (\s@Action' {} a -> s {salesforce = a} :: Action)

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
action_dynamoDBv2 :: Lens.Lens' Action (Core.Maybe DynamoDBv2Action)
action_dynamoDBv2 = Lens.lens (\Action' {dynamoDBv2} -> dynamoDBv2) (\s@Action' {} a -> s {dynamoDBv2 = a} :: Action)

-- | Invoke a Lambda function.
action_lambda :: Lens.Lens' Action (Core.Maybe LambdaAction)
action_lambda = Lens.lens (\Action' {lambda} -> lambda) (\s@Action' {} a -> s {lambda = a} :: Action)

-- | Sends message data to an AWS IoT Analytics channel.
action_iotAnalytics :: Lens.Lens' Action (Core.Maybe IotAnalyticsAction)
action_iotAnalytics = Lens.lens (\Action' {iotAnalytics} -> iotAnalytics) (\s@Action' {} a -> s {iotAnalytics = a} :: Action)

-- | Sends data from the MQTT message that triggered the rule to AWS IoT
-- SiteWise asset properties.
action_iotSiteWise :: Lens.Lens' Action (Core.Maybe IotSiteWiseAction)
action_iotSiteWise = Lens.lens (\Action' {iotSiteWise} -> iotSiteWise) (\s@Action' {} a -> s {iotSiteWise = a} :: Action)

-- | Publish to another MQTT topic.
action_republish :: Lens.Lens' Action (Core.Maybe RepublishAction)
action_republish = Lens.lens (\Action' {republish} -> republish) (\s@Action' {} a -> s {republish = a} :: Action)

-- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
action_kafka :: Lens.Lens' Action (Core.Maybe KafkaAction)
action_kafka = Lens.lens (\Action' {kafka} -> kafka) (\s@Action' {} a -> s {kafka = a} :: Action)

-- | Write to a DynamoDB table.
action_dynamoDB :: Lens.Lens' Action (Core.Maybe DynamoDBAction)
action_dynamoDB = Lens.lens (\Action' {dynamoDB} -> dynamoDB) (\s@Action' {} a -> s {dynamoDB = a} :: Action)

-- | Starts execution of a Step Functions state machine.
action_stepFunctions :: Lens.Lens' Action (Core.Maybe StepFunctionsAction)
action_stepFunctions = Lens.lens (\Action' {stepFunctions} -> stepFunctions) (\s@Action' {} a -> s {stepFunctions = a} :: Action)

-- | Change the state of a CloudWatch alarm.
action_cloudwatchAlarm :: Lens.Lens' Action (Core.Maybe CloudwatchAlarmAction)
action_cloudwatchAlarm = Lens.lens (\Action' {cloudwatchAlarm} -> cloudwatchAlarm) (\s@Action' {} a -> s {cloudwatchAlarm = a} :: Action)

-- | Send data to an HTTPS endpoint.
action_http :: Lens.Lens' Action (Core.Maybe HttpAction)
action_http = Lens.lens (\Action' {http} -> http) (\s@Action' {} a -> s {http = a} :: Action)

-- | Write to an Amazon S3 bucket.
action_s3 :: Lens.Lens' Action (Core.Maybe S3Action)
action_s3 = Lens.lens (\Action' {s3} -> s3) (\s@Action' {} a -> s {s3 = a} :: Action)

-- | Sends an input to an AWS IoT Events detector.
action_iotEvents :: Lens.Lens' Action (Core.Maybe IotEventsAction)
action_iotEvents = Lens.lens (\Action' {iotEvents} -> iotEvents) (\s@Action' {} a -> s {iotEvents = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Core.<$> (x Core..:? "cloudwatchLogs")
            Core.<*> (x Core..:? "cloudwatchMetric")
            Core.<*> (x Core..:? "sqs")
            Core.<*> (x Core..:? "timestream")
            Core.<*> (x Core..:? "firehose")
            Core.<*> (x Core..:? "sns")
            Core.<*> (x Core..:? "elasticsearch")
            Core.<*> (x Core..:? "kinesis")
            Core.<*> (x Core..:? "salesforce")
            Core.<*> (x Core..:? "dynamoDBv2")
            Core.<*> (x Core..:? "lambda")
            Core.<*> (x Core..:? "iotAnalytics")
            Core.<*> (x Core..:? "iotSiteWise")
            Core.<*> (x Core..:? "republish")
            Core.<*> (x Core..:? "kafka")
            Core.<*> (x Core..:? "dynamoDB")
            Core.<*> (x Core..:? "stepFunctions")
            Core.<*> (x Core..:? "cloudwatchAlarm")
            Core.<*> (x Core..:? "http")
            Core.<*> (x Core..:? "s3")
            Core.<*> (x Core..:? "iotEvents")
      )

instance Core.Hashable Action

instance Core.NFData Action

instance Core.ToJSON Action where
  toJSON Action' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cloudwatchLogs" Core..=) Core.<$> cloudwatchLogs,
            ("cloudwatchMetric" Core..=)
              Core.<$> cloudwatchMetric,
            ("sqs" Core..=) Core.<$> sqs,
            ("timestream" Core..=) Core.<$> timestream,
            ("firehose" Core..=) Core.<$> firehose,
            ("sns" Core..=) Core.<$> sns,
            ("elasticsearch" Core..=) Core.<$> elasticsearch,
            ("kinesis" Core..=) Core.<$> kinesis,
            ("salesforce" Core..=) Core.<$> salesforce,
            ("dynamoDBv2" Core..=) Core.<$> dynamoDBv2,
            ("lambda" Core..=) Core.<$> lambda,
            ("iotAnalytics" Core..=) Core.<$> iotAnalytics,
            ("iotSiteWise" Core..=) Core.<$> iotSiteWise,
            ("republish" Core..=) Core.<$> republish,
            ("kafka" Core..=) Core.<$> kafka,
            ("dynamoDB" Core..=) Core.<$> dynamoDB,
            ("stepFunctions" Core..=) Core.<$> stepFunctions,
            ("cloudwatchAlarm" Core..=) Core.<$> cloudwatchAlarm,
            ("http" Core..=) Core.<$> http,
            ("s3" Core..=) Core.<$> s3,
            ("iotEvents" Core..=) Core.<$> iotEvents
          ]
      )
