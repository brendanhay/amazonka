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
-- Module      : Amazonka.IoT.Types.Action
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CloudwatchAlarmAction
import Amazonka.IoT.Types.CloudwatchLogsAction
import Amazonka.IoT.Types.CloudwatchMetricAction
import Amazonka.IoT.Types.DynamoDBAction
import Amazonka.IoT.Types.DynamoDBv2Action
import Amazonka.IoT.Types.ElasticsearchAction
import Amazonka.IoT.Types.FirehoseAction
import Amazonka.IoT.Types.HttpAction
import Amazonka.IoT.Types.IotAnalyticsAction
import Amazonka.IoT.Types.IotEventsAction
import Amazonka.IoT.Types.IotSiteWiseAction
import Amazonka.IoT.Types.KafkaAction
import Amazonka.IoT.Types.KinesisAction
import Amazonka.IoT.Types.LambdaAction
import Amazonka.IoT.Types.LocationAction
import Amazonka.IoT.Types.OpenSearchAction
import Amazonka.IoT.Types.RepublishAction
import Amazonka.IoT.Types.S3Action
import Amazonka.IoT.Types.SalesforceAction
import Amazonka.IoT.Types.SnsAction
import Amazonka.IoT.Types.SqsAction
import Amazonka.IoT.Types.StepFunctionsAction
import Amazonka.IoT.Types.TimestreamAction
import qualified Amazonka.Prelude as Prelude

-- | Describes the actions associated with a rule.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Write data to an Amazon Kinesis stream.
    kinesis :: Prelude.Maybe KinesisAction,
    -- | Change the state of a CloudWatch alarm.
    cloudwatchAlarm :: Prelude.Maybe CloudwatchAlarmAction,
    -- | Send data to an HTTPS endpoint.
    http :: Prelude.Maybe HttpAction,
    -- | Write to an Amazon Kinesis Firehose stream.
    firehose :: Prelude.Maybe FirehoseAction,
    -- | Send data to CloudWatch Logs.
    cloudwatchLogs :: Prelude.Maybe CloudwatchLogsAction,
    -- | Write to an Amazon S3 bucket.
    s3 :: Prelude.Maybe S3Action,
    -- | Send a message to a Salesforce IoT Cloud Input Stream.
    salesforce :: Prelude.Maybe SalesforceAction,
    -- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
    -- It allows you to write each attribute in an MQTT message payload into a
    -- separate DynamoDB column.
    dynamoDBv2 :: Prelude.Maybe DynamoDBv2Action,
    -- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
    -- MSK) or self-managed Apache Kafka cluster.
    kafka :: Prelude.Maybe KafkaAction,
    -- | Write to a DynamoDB table.
    dynamoDB :: Prelude.Maybe DynamoDBAction,
    -- | Capture a CloudWatch metric.
    cloudwatchMetric :: Prelude.Maybe CloudwatchMetricAction,
    -- | Publish to another MQTT topic.
    republish :: Prelude.Maybe RepublishAction,
    -- | The Amazon Location Service rule action sends device location updates
    -- from an MQTT message to an Amazon Location tracker resource.
    location :: Prelude.Maybe LocationAction,
    -- | The Timestream rule action writes attributes (measures) from an MQTT
    -- message into an Amazon Timestream table. For more information, see the
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
    -- topic rule action documentation.
    timestream :: Prelude.Maybe TimestreamAction,
    -- | Invoke a Lambda function.
    lambda :: Prelude.Maybe LambdaAction,
    -- | Sends an input to an IoT Events detector.
    iotEvents :: Prelude.Maybe IotEventsAction,
    -- | Publish to an Amazon SQS queue.
    sqs :: Prelude.Maybe SqsAction,
    -- | Sends data from the MQTT message that triggered the rule to IoT SiteWise
    -- asset properties.
    iotSiteWise :: Prelude.Maybe IotSiteWiseAction,
    -- | Starts execution of a Step Functions state machine.
    stepFunctions :: Prelude.Maybe StepFunctionsAction,
    -- | Publish to an Amazon SNS topic.
    sns :: Prelude.Maybe SnsAction,
    -- | Write data to an Amazon OpenSearch Service domain.
    openSearch :: Prelude.Maybe OpenSearchAction,
    -- | Write data to an Amazon OpenSearch Service domain.
    --
    -- The @Elasticsearch@ action can only be used by existing rule actions. To
    -- create a new rule action or to update an existing rule action, use the
    -- @OpenSearch@ rule action instead. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
    elasticsearch :: Prelude.Maybe ElasticsearchAction,
    -- | Sends message data to an IoT Analytics channel.
    iotAnalytics :: Prelude.Maybe IotAnalyticsAction
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
-- 'kinesis', 'action_kinesis' - Write data to an Amazon Kinesis stream.
--
-- 'cloudwatchAlarm', 'action_cloudwatchAlarm' - Change the state of a CloudWatch alarm.
--
-- 'http', 'action_http' - Send data to an HTTPS endpoint.
--
-- 'firehose', 'action_firehose' - Write to an Amazon Kinesis Firehose stream.
--
-- 'cloudwatchLogs', 'action_cloudwatchLogs' - Send data to CloudWatch Logs.
--
-- 's3', 'action_s3' - Write to an Amazon S3 bucket.
--
-- 'salesforce', 'action_salesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
--
-- 'dynamoDBv2', 'action_dynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
--
-- 'kafka', 'action_kafka' - Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
--
-- 'dynamoDB', 'action_dynamoDB' - Write to a DynamoDB table.
--
-- 'cloudwatchMetric', 'action_cloudwatchMetric' - Capture a CloudWatch metric.
--
-- 'republish', 'action_republish' - Publish to another MQTT topic.
--
-- 'location', 'action_location' - The Amazon Location Service rule action sends device location updates
-- from an MQTT message to an Amazon Location tracker resource.
--
-- 'timestream', 'action_timestream' - The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
--
-- 'lambda', 'action_lambda' - Invoke a Lambda function.
--
-- 'iotEvents', 'action_iotEvents' - Sends an input to an IoT Events detector.
--
-- 'sqs', 'action_sqs' - Publish to an Amazon SQS queue.
--
-- 'iotSiteWise', 'action_iotSiteWise' - Sends data from the MQTT message that triggered the rule to IoT SiteWise
-- asset properties.
--
-- 'stepFunctions', 'action_stepFunctions' - Starts execution of a Step Functions state machine.
--
-- 'sns', 'action_sns' - Publish to an Amazon SNS topic.
--
-- 'openSearch', 'action_openSearch' - Write data to an Amazon OpenSearch Service domain.
--
-- 'elasticsearch', 'action_elasticsearch' - Write data to an Amazon OpenSearch Service domain.
--
-- The @Elasticsearch@ action can only be used by existing rule actions. To
-- create a new rule action or to update an existing rule action, use the
-- @OpenSearch@ rule action instead. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
--
-- 'iotAnalytics', 'action_iotAnalytics' - Sends message data to an IoT Analytics channel.
newAction ::
  Action
newAction =
  Action'
    { kinesis = Prelude.Nothing,
      cloudwatchAlarm = Prelude.Nothing,
      http = Prelude.Nothing,
      firehose = Prelude.Nothing,
      cloudwatchLogs = Prelude.Nothing,
      s3 = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      dynamoDBv2 = Prelude.Nothing,
      kafka = Prelude.Nothing,
      dynamoDB = Prelude.Nothing,
      cloudwatchMetric = Prelude.Nothing,
      republish = Prelude.Nothing,
      location = Prelude.Nothing,
      timestream = Prelude.Nothing,
      lambda = Prelude.Nothing,
      iotEvents = Prelude.Nothing,
      sqs = Prelude.Nothing,
      iotSiteWise = Prelude.Nothing,
      stepFunctions = Prelude.Nothing,
      sns = Prelude.Nothing,
      openSearch = Prelude.Nothing,
      elasticsearch = Prelude.Nothing,
      iotAnalytics = Prelude.Nothing
    }

-- | Write data to an Amazon Kinesis stream.
action_kinesis :: Lens.Lens' Action (Prelude.Maybe KinesisAction)
action_kinesis = Lens.lens (\Action' {kinesis} -> kinesis) (\s@Action' {} a -> s {kinesis = a} :: Action)

-- | Change the state of a CloudWatch alarm.
action_cloudwatchAlarm :: Lens.Lens' Action (Prelude.Maybe CloudwatchAlarmAction)
action_cloudwatchAlarm = Lens.lens (\Action' {cloudwatchAlarm} -> cloudwatchAlarm) (\s@Action' {} a -> s {cloudwatchAlarm = a} :: Action)

-- | Send data to an HTTPS endpoint.
action_http :: Lens.Lens' Action (Prelude.Maybe HttpAction)
action_http = Lens.lens (\Action' {http} -> http) (\s@Action' {} a -> s {http = a} :: Action)

-- | Write to an Amazon Kinesis Firehose stream.
action_firehose :: Lens.Lens' Action (Prelude.Maybe FirehoseAction)
action_firehose = Lens.lens (\Action' {firehose} -> firehose) (\s@Action' {} a -> s {firehose = a} :: Action)

-- | Send data to CloudWatch Logs.
action_cloudwatchLogs :: Lens.Lens' Action (Prelude.Maybe CloudwatchLogsAction)
action_cloudwatchLogs = Lens.lens (\Action' {cloudwatchLogs} -> cloudwatchLogs) (\s@Action' {} a -> s {cloudwatchLogs = a} :: Action)

-- | Write to an Amazon S3 bucket.
action_s3 :: Lens.Lens' Action (Prelude.Maybe S3Action)
action_s3 = Lens.lens (\Action' {s3} -> s3) (\s@Action' {} a -> s {s3 = a} :: Action)

-- | Send a message to a Salesforce IoT Cloud Input Stream.
action_salesforce :: Lens.Lens' Action (Prelude.Maybe SalesforceAction)
action_salesforce = Lens.lens (\Action' {salesforce} -> salesforce) (\s@Action' {} a -> s {salesforce = a} :: Action)

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action.
-- It allows you to write each attribute in an MQTT message payload into a
-- separate DynamoDB column.
action_dynamoDBv2 :: Lens.Lens' Action (Prelude.Maybe DynamoDBv2Action)
action_dynamoDBv2 = Lens.lens (\Action' {dynamoDBv2} -> dynamoDBv2) (\s@Action' {} a -> s {dynamoDBv2 = a} :: Action)

-- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
action_kafka :: Lens.Lens' Action (Prelude.Maybe KafkaAction)
action_kafka = Lens.lens (\Action' {kafka} -> kafka) (\s@Action' {} a -> s {kafka = a} :: Action)

-- | Write to a DynamoDB table.
action_dynamoDB :: Lens.Lens' Action (Prelude.Maybe DynamoDBAction)
action_dynamoDB = Lens.lens (\Action' {dynamoDB} -> dynamoDB) (\s@Action' {} a -> s {dynamoDB = a} :: Action)

-- | Capture a CloudWatch metric.
action_cloudwatchMetric :: Lens.Lens' Action (Prelude.Maybe CloudwatchMetricAction)
action_cloudwatchMetric = Lens.lens (\Action' {cloudwatchMetric} -> cloudwatchMetric) (\s@Action' {} a -> s {cloudwatchMetric = a} :: Action)

-- | Publish to another MQTT topic.
action_republish :: Lens.Lens' Action (Prelude.Maybe RepublishAction)
action_republish = Lens.lens (\Action' {republish} -> republish) (\s@Action' {} a -> s {republish = a} :: Action)

-- | The Amazon Location Service rule action sends device location updates
-- from an MQTT message to an Amazon Location tracker resource.
action_location :: Lens.Lens' Action (Prelude.Maybe LocationAction)
action_location = Lens.lens (\Action' {location} -> location) (\s@Action' {} a -> s {location = a} :: Action)

-- | The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
action_timestream :: Lens.Lens' Action (Prelude.Maybe TimestreamAction)
action_timestream = Lens.lens (\Action' {timestream} -> timestream) (\s@Action' {} a -> s {timestream = a} :: Action)

-- | Invoke a Lambda function.
action_lambda :: Lens.Lens' Action (Prelude.Maybe LambdaAction)
action_lambda = Lens.lens (\Action' {lambda} -> lambda) (\s@Action' {} a -> s {lambda = a} :: Action)

-- | Sends an input to an IoT Events detector.
action_iotEvents :: Lens.Lens' Action (Prelude.Maybe IotEventsAction)
action_iotEvents = Lens.lens (\Action' {iotEvents} -> iotEvents) (\s@Action' {} a -> s {iotEvents = a} :: Action)

-- | Publish to an Amazon SQS queue.
action_sqs :: Lens.Lens' Action (Prelude.Maybe SqsAction)
action_sqs = Lens.lens (\Action' {sqs} -> sqs) (\s@Action' {} a -> s {sqs = a} :: Action)

-- | Sends data from the MQTT message that triggered the rule to IoT SiteWise
-- asset properties.
action_iotSiteWise :: Lens.Lens' Action (Prelude.Maybe IotSiteWiseAction)
action_iotSiteWise = Lens.lens (\Action' {iotSiteWise} -> iotSiteWise) (\s@Action' {} a -> s {iotSiteWise = a} :: Action)

-- | Starts execution of a Step Functions state machine.
action_stepFunctions :: Lens.Lens' Action (Prelude.Maybe StepFunctionsAction)
action_stepFunctions = Lens.lens (\Action' {stepFunctions} -> stepFunctions) (\s@Action' {} a -> s {stepFunctions = a} :: Action)

-- | Publish to an Amazon SNS topic.
action_sns :: Lens.Lens' Action (Prelude.Maybe SnsAction)
action_sns = Lens.lens (\Action' {sns} -> sns) (\s@Action' {} a -> s {sns = a} :: Action)

-- | Write data to an Amazon OpenSearch Service domain.
action_openSearch :: Lens.Lens' Action (Prelude.Maybe OpenSearchAction)
action_openSearch = Lens.lens (\Action' {openSearch} -> openSearch) (\s@Action' {} a -> s {openSearch = a} :: Action)

-- | Write data to an Amazon OpenSearch Service domain.
--
-- The @Elasticsearch@ action can only be used by existing rule actions. To
-- create a new rule action or to update an existing rule action, use the
-- @OpenSearch@ rule action instead. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
action_elasticsearch :: Lens.Lens' Action (Prelude.Maybe ElasticsearchAction)
action_elasticsearch = Lens.lens (\Action' {elasticsearch} -> elasticsearch) (\s@Action' {} a -> s {elasticsearch = a} :: Action)

-- | Sends message data to an IoT Analytics channel.
action_iotAnalytics :: Lens.Lens' Action (Prelude.Maybe IotAnalyticsAction)
action_iotAnalytics = Lens.lens (\Action' {iotAnalytics} -> iotAnalytics) (\s@Action' {} a -> s {iotAnalytics = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "kinesis")
            Prelude.<*> (x Data..:? "cloudwatchAlarm")
            Prelude.<*> (x Data..:? "http")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "cloudwatchLogs")
            Prelude.<*> (x Data..:? "s3")
            Prelude.<*> (x Data..:? "salesforce")
            Prelude.<*> (x Data..:? "dynamoDBv2")
            Prelude.<*> (x Data..:? "kafka")
            Prelude.<*> (x Data..:? "dynamoDB")
            Prelude.<*> (x Data..:? "cloudwatchMetric")
            Prelude.<*> (x Data..:? "republish")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "timestream")
            Prelude.<*> (x Data..:? "lambda")
            Prelude.<*> (x Data..:? "iotEvents")
            Prelude.<*> (x Data..:? "sqs")
            Prelude.<*> (x Data..:? "iotSiteWise")
            Prelude.<*> (x Data..:? "stepFunctions")
            Prelude.<*> (x Data..:? "sns")
            Prelude.<*> (x Data..:? "openSearch")
            Prelude.<*> (x Data..:? "elasticsearch")
            Prelude.<*> (x Data..:? "iotAnalytics")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` kinesis
      `Prelude.hashWithSalt` cloudwatchAlarm
      `Prelude.hashWithSalt` http
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` cloudwatchLogs
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` dynamoDBv2
      `Prelude.hashWithSalt` kafka
      `Prelude.hashWithSalt` dynamoDB
      `Prelude.hashWithSalt` cloudwatchMetric
      `Prelude.hashWithSalt` republish
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` timestream
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` iotEvents
      `Prelude.hashWithSalt` sqs
      `Prelude.hashWithSalt` iotSiteWise
      `Prelude.hashWithSalt` stepFunctions
      `Prelude.hashWithSalt` sns
      `Prelude.hashWithSalt` openSearch
      `Prelude.hashWithSalt` elasticsearch
      `Prelude.hashWithSalt` iotAnalytics

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf kinesis
      `Prelude.seq` Prelude.rnf cloudwatchAlarm
      `Prelude.seq` Prelude.rnf http
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf cloudwatchLogs
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf dynamoDBv2
      `Prelude.seq` Prelude.rnf kafka
      `Prelude.seq` Prelude.rnf dynamoDB
      `Prelude.seq` Prelude.rnf cloudwatchMetric
      `Prelude.seq` Prelude.rnf republish
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf timestream
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf iotEvents
      `Prelude.seq` Prelude.rnf sqs
      `Prelude.seq` Prelude.rnf iotSiteWise
      `Prelude.seq` Prelude.rnf stepFunctions
      `Prelude.seq` Prelude.rnf sns
      `Prelude.seq` Prelude.rnf openSearch
      `Prelude.seq` Prelude.rnf elasticsearch
      `Prelude.seq` Prelude.rnf iotAnalytics

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kinesis" Data..=) Prelude.<$> kinesis,
            ("cloudwatchAlarm" Data..=)
              Prelude.<$> cloudwatchAlarm,
            ("http" Data..=) Prelude.<$> http,
            ("firehose" Data..=) Prelude.<$> firehose,
            ("cloudwatchLogs" Data..=)
              Prelude.<$> cloudwatchLogs,
            ("s3" Data..=) Prelude.<$> s3,
            ("salesforce" Data..=) Prelude.<$> salesforce,
            ("dynamoDBv2" Data..=) Prelude.<$> dynamoDBv2,
            ("kafka" Data..=) Prelude.<$> kafka,
            ("dynamoDB" Data..=) Prelude.<$> dynamoDB,
            ("cloudwatchMetric" Data..=)
              Prelude.<$> cloudwatchMetric,
            ("republish" Data..=) Prelude.<$> republish,
            ("location" Data..=) Prelude.<$> location,
            ("timestream" Data..=) Prelude.<$> timestream,
            ("lambda" Data..=) Prelude.<$> lambda,
            ("iotEvents" Data..=) Prelude.<$> iotEvents,
            ("sqs" Data..=) Prelude.<$> sqs,
            ("iotSiteWise" Data..=) Prelude.<$> iotSiteWise,
            ("stepFunctions" Data..=) Prelude.<$> stepFunctions,
            ("sns" Data..=) Prelude.<$> sns,
            ("openSearch" Data..=) Prelude.<$> openSearch,
            ("elasticsearch" Data..=) Prelude.<$> elasticsearch,
            ("iotAnalytics" Data..=) Prelude.<$> iotAnalytics
          ]
      )
