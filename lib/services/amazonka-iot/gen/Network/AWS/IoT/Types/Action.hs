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
import Network.AWS.IoT.Types.OpenSearchAction
import Network.AWS.IoT.Types.RepublishAction
import Network.AWS.IoT.Types.S3Action
import Network.AWS.IoT.Types.SalesforceAction
import Network.AWS.IoT.Types.SnsAction
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.TimestreamAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the actions associated with a rule.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Capture a CloudWatch metric.
    cloudwatchMetric :: Prelude.Maybe CloudwatchMetricAction,
    -- | Send data to CloudWatch Logs.
    cloudwatchLogs :: Prelude.Maybe CloudwatchLogsAction,
    -- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
    -- It allows you to write each attribute in an MQTT message payload into a
    -- separate DynamoDB column.
    dynamoDBv2 :: Prelude.Maybe DynamoDBv2Action,
    -- | Starts execution of a Step Functions state machine.
    stepFunctions :: Prelude.Maybe StepFunctionsAction,
    -- | Change the state of a CloudWatch alarm.
    cloudwatchAlarm :: Prelude.Maybe CloudwatchAlarmAction,
    -- | Publish to an Amazon SNS topic.
    sns :: Prelude.Maybe SnsAction,
    -- | Write to a DynamoDB table.
    dynamoDB :: Prelude.Maybe DynamoDBAction,
    -- | Write to an Amazon Kinesis Firehose stream.
    firehose :: Prelude.Maybe FirehoseAction,
    -- | The Timestream rule action writes attributes (measures) from an MQTT
    -- message into an Amazon Timestream table. For more information, see the
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
    -- topic rule action documentation.
    timestream :: Prelude.Maybe TimestreamAction,
    -- | Sends data from the MQTT message that triggered the rule to IoT SiteWise
    -- asset properties.
    iotSiteWise :: Prelude.Maybe IotSiteWiseAction,
    -- | Sends message data to an IoT Analytics channel.
    iotAnalytics :: Prelude.Maybe IotAnalyticsAction,
    -- | Invoke a Lambda function.
    lambda :: Prelude.Maybe LambdaAction,
    -- | Write data to an Amazon OpenSearch Service domain.
    openSearch :: Prelude.Maybe OpenSearchAction,
    -- | Sends an input to an IoT Events detector.
    iotEvents :: Prelude.Maybe IotEventsAction,
    -- | Send a message to a Salesforce IoT Cloud Input Stream.
    salesforce :: Prelude.Maybe SalesforceAction,
    -- | Write data to an Amazon Kinesis stream.
    kinesis :: Prelude.Maybe KinesisAction,
    -- | Write to an Amazon S3 bucket.
    s3 :: Prelude.Maybe S3Action,
    -- | Send data to an HTTPS endpoint.
    http :: Prelude.Maybe HttpAction,
    -- | Write data to an Amazon OpenSearch Service domain.
    --
    -- The @Elasticsearch@ action can only be used by existing rule actions. To
    -- create a new rule action or to update an existing rule action, use the
    -- @OpenSearch@ rule action instead. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
    elasticsearch :: Prelude.Maybe ElasticsearchAction,
    -- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
    -- MSK) or self-managed Apache Kafka cluster.
    kafka :: Prelude.Maybe KafkaAction,
    -- | Publish to another MQTT topic.
    republish :: Prelude.Maybe RepublishAction,
    -- | Publish to an Amazon SQS queue.
    sqs :: Prelude.Maybe SqsAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudwatchMetric', 'action_cloudwatchMetric' - Capture a CloudWatch metric.
--
-- 'cloudwatchLogs', 'action_cloudwatchLogs' - Send data to CloudWatch Logs.
--
-- 'dynamoDBv2', 'action_dynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
--
-- 'stepFunctions', 'action_stepFunctions' - Starts execution of a Step Functions state machine.
--
-- 'cloudwatchAlarm', 'action_cloudwatchAlarm' - Change the state of a CloudWatch alarm.
--
-- 'sns', 'action_sns' - Publish to an Amazon SNS topic.
--
-- 'dynamoDB', 'action_dynamoDB' - Write to a DynamoDB table.
--
-- 'firehose', 'action_firehose' - Write to an Amazon Kinesis Firehose stream.
--
-- 'timestream', 'action_timestream' - The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
--
-- 'iotSiteWise', 'action_iotSiteWise' - Sends data from the MQTT message that triggered the rule to IoT SiteWise
-- asset properties.
--
-- 'iotAnalytics', 'action_iotAnalytics' - Sends message data to an IoT Analytics channel.
--
-- 'lambda', 'action_lambda' - Invoke a Lambda function.
--
-- 'openSearch', 'action_openSearch' - Write data to an Amazon OpenSearch Service domain.
--
-- 'iotEvents', 'action_iotEvents' - Sends an input to an IoT Events detector.
--
-- 'salesforce', 'action_salesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
--
-- 'kinesis', 'action_kinesis' - Write data to an Amazon Kinesis stream.
--
-- 's3', 'action_s3' - Write to an Amazon S3 bucket.
--
-- 'http', 'action_http' - Send data to an HTTPS endpoint.
--
-- 'elasticsearch', 'action_elasticsearch' - Write data to an Amazon OpenSearch Service domain.
--
-- The @Elasticsearch@ action can only be used by existing rule actions. To
-- create a new rule action or to update an existing rule action, use the
-- @OpenSearch@ rule action instead. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
--
-- 'kafka', 'action_kafka' - Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
--
-- 'republish', 'action_republish' - Publish to another MQTT topic.
--
-- 'sqs', 'action_sqs' - Publish to an Amazon SQS queue.
newAction ::
  Action
newAction =
  Action'
    { cloudwatchMetric = Prelude.Nothing,
      cloudwatchLogs = Prelude.Nothing,
      dynamoDBv2 = Prelude.Nothing,
      stepFunctions = Prelude.Nothing,
      cloudwatchAlarm = Prelude.Nothing,
      sns = Prelude.Nothing,
      dynamoDB = Prelude.Nothing,
      firehose = Prelude.Nothing,
      timestream = Prelude.Nothing,
      iotSiteWise = Prelude.Nothing,
      iotAnalytics = Prelude.Nothing,
      lambda = Prelude.Nothing,
      openSearch = Prelude.Nothing,
      iotEvents = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      kinesis = Prelude.Nothing,
      s3 = Prelude.Nothing,
      http = Prelude.Nothing,
      elasticsearch = Prelude.Nothing,
      kafka = Prelude.Nothing,
      republish = Prelude.Nothing,
      sqs = Prelude.Nothing
    }

-- | Capture a CloudWatch metric.
action_cloudwatchMetric :: Lens.Lens' Action (Prelude.Maybe CloudwatchMetricAction)
action_cloudwatchMetric = Lens.lens (\Action' {cloudwatchMetric} -> cloudwatchMetric) (\s@Action' {} a -> s {cloudwatchMetric = a} :: Action)

-- | Send data to CloudWatch Logs.
action_cloudwatchLogs :: Lens.Lens' Action (Prelude.Maybe CloudwatchLogsAction)
action_cloudwatchLogs = Lens.lens (\Action' {cloudwatchLogs} -> cloudwatchLogs) (\s@Action' {} a -> s {cloudwatchLogs = a} :: Action)

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
action_dynamoDBv2 :: Lens.Lens' Action (Prelude.Maybe DynamoDBv2Action)
action_dynamoDBv2 = Lens.lens (\Action' {dynamoDBv2} -> dynamoDBv2) (\s@Action' {} a -> s {dynamoDBv2 = a} :: Action)

-- | Starts execution of a Step Functions state machine.
action_stepFunctions :: Lens.Lens' Action (Prelude.Maybe StepFunctionsAction)
action_stepFunctions = Lens.lens (\Action' {stepFunctions} -> stepFunctions) (\s@Action' {} a -> s {stepFunctions = a} :: Action)

-- | Change the state of a CloudWatch alarm.
action_cloudwatchAlarm :: Lens.Lens' Action (Prelude.Maybe CloudwatchAlarmAction)
action_cloudwatchAlarm = Lens.lens (\Action' {cloudwatchAlarm} -> cloudwatchAlarm) (\s@Action' {} a -> s {cloudwatchAlarm = a} :: Action)

-- | Publish to an Amazon SNS topic.
action_sns :: Lens.Lens' Action (Prelude.Maybe SnsAction)
action_sns = Lens.lens (\Action' {sns} -> sns) (\s@Action' {} a -> s {sns = a} :: Action)

-- | Write to a DynamoDB table.
action_dynamoDB :: Lens.Lens' Action (Prelude.Maybe DynamoDBAction)
action_dynamoDB = Lens.lens (\Action' {dynamoDB} -> dynamoDB) (\s@Action' {} a -> s {dynamoDB = a} :: Action)

-- | Write to an Amazon Kinesis Firehose stream.
action_firehose :: Lens.Lens' Action (Prelude.Maybe FirehoseAction)
action_firehose = Lens.lens (\Action' {firehose} -> firehose) (\s@Action' {} a -> s {firehose = a} :: Action)

-- | The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
action_timestream :: Lens.Lens' Action (Prelude.Maybe TimestreamAction)
action_timestream = Lens.lens (\Action' {timestream} -> timestream) (\s@Action' {} a -> s {timestream = a} :: Action)

-- | Sends data from the MQTT message that triggered the rule to IoT SiteWise
-- asset properties.
action_iotSiteWise :: Lens.Lens' Action (Prelude.Maybe IotSiteWiseAction)
action_iotSiteWise = Lens.lens (\Action' {iotSiteWise} -> iotSiteWise) (\s@Action' {} a -> s {iotSiteWise = a} :: Action)

-- | Sends message data to an IoT Analytics channel.
action_iotAnalytics :: Lens.Lens' Action (Prelude.Maybe IotAnalyticsAction)
action_iotAnalytics = Lens.lens (\Action' {iotAnalytics} -> iotAnalytics) (\s@Action' {} a -> s {iotAnalytics = a} :: Action)

-- | Invoke a Lambda function.
action_lambda :: Lens.Lens' Action (Prelude.Maybe LambdaAction)
action_lambda = Lens.lens (\Action' {lambda} -> lambda) (\s@Action' {} a -> s {lambda = a} :: Action)

-- | Write data to an Amazon OpenSearch Service domain.
action_openSearch :: Lens.Lens' Action (Prelude.Maybe OpenSearchAction)
action_openSearch = Lens.lens (\Action' {openSearch} -> openSearch) (\s@Action' {} a -> s {openSearch = a} :: Action)

-- | Sends an input to an IoT Events detector.
action_iotEvents :: Lens.Lens' Action (Prelude.Maybe IotEventsAction)
action_iotEvents = Lens.lens (\Action' {iotEvents} -> iotEvents) (\s@Action' {} a -> s {iotEvents = a} :: Action)

-- | Send a message to a Salesforce IoT Cloud Input Stream.
action_salesforce :: Lens.Lens' Action (Prelude.Maybe SalesforceAction)
action_salesforce = Lens.lens (\Action' {salesforce} -> salesforce) (\s@Action' {} a -> s {salesforce = a} :: Action)

-- | Write data to an Amazon Kinesis stream.
action_kinesis :: Lens.Lens' Action (Prelude.Maybe KinesisAction)
action_kinesis = Lens.lens (\Action' {kinesis} -> kinesis) (\s@Action' {} a -> s {kinesis = a} :: Action)

-- | Write to an Amazon S3 bucket.
action_s3 :: Lens.Lens' Action (Prelude.Maybe S3Action)
action_s3 = Lens.lens (\Action' {s3} -> s3) (\s@Action' {} a -> s {s3 = a} :: Action)

-- | Send data to an HTTPS endpoint.
action_http :: Lens.Lens' Action (Prelude.Maybe HttpAction)
action_http = Lens.lens (\Action' {http} -> http) (\s@Action' {} a -> s {http = a} :: Action)

-- | Write data to an Amazon OpenSearch Service domain.
--
-- The @Elasticsearch@ action can only be used by existing rule actions. To
-- create a new rule action or to update an existing rule action, use the
-- @OpenSearch@ rule action instead. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
action_elasticsearch :: Lens.Lens' Action (Prelude.Maybe ElasticsearchAction)
action_elasticsearch = Lens.lens (\Action' {elasticsearch} -> elasticsearch) (\s@Action' {} a -> s {elasticsearch = a} :: Action)

-- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
action_kafka :: Lens.Lens' Action (Prelude.Maybe KafkaAction)
action_kafka = Lens.lens (\Action' {kafka} -> kafka) (\s@Action' {} a -> s {kafka = a} :: Action)

-- | Publish to another MQTT topic.
action_republish :: Lens.Lens' Action (Prelude.Maybe RepublishAction)
action_republish = Lens.lens (\Action' {republish} -> republish) (\s@Action' {} a -> s {republish = a} :: Action)

-- | Publish to an Amazon SQS queue.
action_sqs :: Lens.Lens' Action (Prelude.Maybe SqsAction)
action_sqs = Lens.lens (\Action' {sqs} -> sqs) (\s@Action' {} a -> s {sqs = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Core..:? "cloudwatchMetric")
            Prelude.<*> (x Core..:? "cloudwatchLogs")
            Prelude.<*> (x Core..:? "dynamoDBv2")
            Prelude.<*> (x Core..:? "stepFunctions")
            Prelude.<*> (x Core..:? "cloudwatchAlarm")
            Prelude.<*> (x Core..:? "sns")
            Prelude.<*> (x Core..:? "dynamoDB")
            Prelude.<*> (x Core..:? "firehose")
            Prelude.<*> (x Core..:? "timestream")
            Prelude.<*> (x Core..:? "iotSiteWise")
            Prelude.<*> (x Core..:? "iotAnalytics")
            Prelude.<*> (x Core..:? "lambda")
            Prelude.<*> (x Core..:? "openSearch")
            Prelude.<*> (x Core..:? "iotEvents")
            Prelude.<*> (x Core..:? "salesforce")
            Prelude.<*> (x Core..:? "kinesis")
            Prelude.<*> (x Core..:? "s3")
            Prelude.<*> (x Core..:? "http")
            Prelude.<*> (x Core..:? "elasticsearch")
            Prelude.<*> (x Core..:? "kafka")
            Prelude.<*> (x Core..:? "republish")
            Prelude.<*> (x Core..:? "sqs")
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action

instance Core.ToJSON Action where
  toJSON Action' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cloudwatchMetric" Core..=)
              Prelude.<$> cloudwatchMetric,
            ("cloudwatchLogs" Core..=)
              Prelude.<$> cloudwatchLogs,
            ("dynamoDBv2" Core..=) Prelude.<$> dynamoDBv2,
            ("stepFunctions" Core..=) Prelude.<$> stepFunctions,
            ("cloudwatchAlarm" Core..=)
              Prelude.<$> cloudwatchAlarm,
            ("sns" Core..=) Prelude.<$> sns,
            ("dynamoDB" Core..=) Prelude.<$> dynamoDB,
            ("firehose" Core..=) Prelude.<$> firehose,
            ("timestream" Core..=) Prelude.<$> timestream,
            ("iotSiteWise" Core..=) Prelude.<$> iotSiteWise,
            ("iotAnalytics" Core..=) Prelude.<$> iotAnalytics,
            ("lambda" Core..=) Prelude.<$> lambda,
            ("openSearch" Core..=) Prelude.<$> openSearch,
            ("iotEvents" Core..=) Prelude.<$> iotEvents,
            ("salesforce" Core..=) Prelude.<$> salesforce,
            ("kinesis" Core..=) Prelude.<$> kinesis,
            ("s3" Core..=) Prelude.<$> s3,
            ("http" Core..=) Prelude.<$> http,
            ("elasticsearch" Core..=) Prelude.<$> elasticsearch,
            ("kafka" Core..=) Prelude.<$> kafka,
            ("republish" Core..=) Prelude.<$> republish,
            ("sqs" Core..=) Prelude.<$> sqs
          ]
      )
