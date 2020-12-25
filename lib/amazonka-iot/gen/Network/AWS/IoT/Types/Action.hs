{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aCloudwatchAlarm,
    aCloudwatchLogs,
    aCloudwatchMetric,
    aDynamoDB,
    aDynamoDBv2,
    aElasticsearch,
    aFirehose,
    aHttp,
    aIotAnalytics,
    aIotEvents,
    aIotSiteWise,
    aKinesis,
    aLambda,
    aRepublish,
    aS3,
    aSalesforce,
    aSns,
    aSqs,
    aStepFunctions,
    aTimestream,
  )
where

import qualified Network.AWS.IoT.Types.CloudwatchAlarmAction as Types
import qualified Network.AWS.IoT.Types.CloudwatchLogsAction as Types
import qualified Network.AWS.IoT.Types.CloudwatchMetricAction as Types
import qualified Network.AWS.IoT.Types.DynamoDBAction as Types
import qualified Network.AWS.IoT.Types.DynamoDBv2Action as Types
import qualified Network.AWS.IoT.Types.ElasticsearchAction as Types
import qualified Network.AWS.IoT.Types.FirehoseAction as Types
import qualified Network.AWS.IoT.Types.HttpAction as Types
import qualified Network.AWS.IoT.Types.IotAnalyticsAction as Types
import qualified Network.AWS.IoT.Types.IotEventsAction as Types
import qualified Network.AWS.IoT.Types.IotSiteWiseAction as Types
import qualified Network.AWS.IoT.Types.KinesisAction as Types
import qualified Network.AWS.IoT.Types.LambdaAction as Types
import qualified Network.AWS.IoT.Types.RepublishAction as Types
import qualified Network.AWS.IoT.Types.S3Action as Types
import qualified Network.AWS.IoT.Types.SalesforceAction as Types
import qualified Network.AWS.IoT.Types.SnsAction as Types
import qualified Network.AWS.IoT.Types.SqsAction as Types
import qualified Network.AWS.IoT.Types.StepFunctionsAction as Types
import qualified Network.AWS.IoT.Types.TimestreamAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the actions associated with a rule.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | Change the state of a CloudWatch alarm.
    cloudwatchAlarm :: Core.Maybe Types.CloudwatchAlarmAction,
    -- | Send data to CloudWatch Logs.
    cloudwatchLogs :: Core.Maybe Types.CloudwatchLogsAction,
    -- | Capture a CloudWatch metric.
    cloudwatchMetric :: Core.Maybe Types.CloudwatchMetricAction,
    -- | Write to a DynamoDB table.
    dynamoDB :: Core.Maybe Types.DynamoDBAction,
    -- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
    dynamoDBv2 :: Core.Maybe Types.DynamoDBv2Action,
    -- | Write data to an Amazon Elasticsearch Service domain.
    elasticsearch :: Core.Maybe Types.ElasticsearchAction,
    -- | Write to an Amazon Kinesis Firehose stream.
    firehose :: Core.Maybe Types.FirehoseAction,
    -- | Send data to an HTTPS endpoint.
    http :: Core.Maybe Types.HttpAction,
    -- | Sends message data to an AWS IoT Analytics channel.
    iotAnalytics :: Core.Maybe Types.IotAnalyticsAction,
    -- | Sends an input to an AWS IoT Events detector.
    iotEvents :: Core.Maybe Types.IotEventsAction,
    -- | Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
    iotSiteWise :: Core.Maybe Types.IotSiteWiseAction,
    -- | Write data to an Amazon Kinesis stream.
    kinesis :: Core.Maybe Types.KinesisAction,
    -- | Invoke a Lambda function.
    lambda :: Core.Maybe Types.LambdaAction,
    -- | Publish to another MQTT topic.
    republish :: Core.Maybe Types.RepublishAction,
    -- | Write to an Amazon S3 bucket.
    s3 :: Core.Maybe Types.S3Action,
    -- | Send a message to a Salesforce IoT Cloud Input Stream.
    salesforce :: Core.Maybe Types.SalesforceAction,
    -- | Publish to an Amazon SNS topic.
    sns :: Core.Maybe Types.SnsAction,
    -- | Publish to an Amazon SQS queue.
    sqs :: Core.Maybe Types.SqsAction,
    -- | Starts execution of a Step Functions state machine.
    stepFunctions :: Core.Maybe Types.StepFunctionsAction,
    -- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
    timestream :: Core.Maybe Types.TimestreamAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Action' value with any optional fields omitted.
mkAction ::
  Action
mkAction =
  Action'
    { cloudwatchAlarm = Core.Nothing,
      cloudwatchLogs = Core.Nothing,
      cloudwatchMetric = Core.Nothing,
      dynamoDB = Core.Nothing,
      dynamoDBv2 = Core.Nothing,
      elasticsearch = Core.Nothing,
      firehose = Core.Nothing,
      http = Core.Nothing,
      iotAnalytics = Core.Nothing,
      iotEvents = Core.Nothing,
      iotSiteWise = Core.Nothing,
      kinesis = Core.Nothing,
      lambda = Core.Nothing,
      republish = Core.Nothing,
      s3 = Core.Nothing,
      salesforce = Core.Nothing,
      sns = Core.Nothing,
      sqs = Core.Nothing,
      stepFunctions = Core.Nothing,
      timestream = Core.Nothing
    }

-- | Change the state of a CloudWatch alarm.
--
-- /Note:/ Consider using 'cloudwatchAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchAlarm :: Lens.Lens' Action (Core.Maybe Types.CloudwatchAlarmAction)
aCloudwatchAlarm = Lens.field @"cloudwatchAlarm"
{-# DEPRECATED aCloudwatchAlarm "Use generic-lens or generic-optics with 'cloudwatchAlarm' instead." #-}

-- | Send data to CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudwatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchLogs :: Lens.Lens' Action (Core.Maybe Types.CloudwatchLogsAction)
aCloudwatchLogs = Lens.field @"cloudwatchLogs"
{-# DEPRECATED aCloudwatchLogs "Use generic-lens or generic-optics with 'cloudwatchLogs' instead." #-}

-- | Capture a CloudWatch metric.
--
-- /Note:/ Consider using 'cloudwatchMetric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchMetric :: Lens.Lens' Action (Core.Maybe Types.CloudwatchMetricAction)
aCloudwatchMetric = Lens.field @"cloudwatchMetric"
{-# DEPRECATED aCloudwatchMetric "Use generic-lens or generic-optics with 'cloudwatchMetric' instead." #-}

-- | Write to a DynamoDB table.
--
-- /Note:/ Consider using 'dynamoDB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamoDB :: Lens.Lens' Action (Core.Maybe Types.DynamoDBAction)
aDynamoDB = Lens.field @"dynamoDB"
{-# DEPRECATED aDynamoDB "Use generic-lens or generic-optics with 'dynamoDB' instead." #-}

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
--
-- /Note:/ Consider using 'dynamoDBv2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamoDBv2 :: Lens.Lens' Action (Core.Maybe Types.DynamoDBv2Action)
aDynamoDBv2 = Lens.field @"dynamoDBv2"
{-# DEPRECATED aDynamoDBv2 "Use generic-lens or generic-optics with 'dynamoDBv2' instead." #-}

-- | Write data to an Amazon Elasticsearch Service domain.
--
-- /Note:/ Consider using 'elasticsearch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aElasticsearch :: Lens.Lens' Action (Core.Maybe Types.ElasticsearchAction)
aElasticsearch = Lens.field @"elasticsearch"
{-# DEPRECATED aElasticsearch "Use generic-lens or generic-optics with 'elasticsearch' instead." #-}

-- | Write to an Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'firehose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFirehose :: Lens.Lens' Action (Core.Maybe Types.FirehoseAction)
aFirehose = Lens.field @"firehose"
{-# DEPRECATED aFirehose "Use generic-lens or generic-optics with 'firehose' instead." #-}

-- | Send data to an HTTPS endpoint.
--
-- /Note:/ Consider using 'http' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHttp :: Lens.Lens' Action (Core.Maybe Types.HttpAction)
aHttp = Lens.field @"http"
{-# DEPRECATED aHttp "Use generic-lens or generic-optics with 'http' instead." #-}

-- | Sends message data to an AWS IoT Analytics channel.
--
-- /Note:/ Consider using 'iotAnalytics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotAnalytics :: Lens.Lens' Action (Core.Maybe Types.IotAnalyticsAction)
aIotAnalytics = Lens.field @"iotAnalytics"
{-# DEPRECATED aIotAnalytics "Use generic-lens or generic-optics with 'iotAnalytics' instead." #-}

-- | Sends an input to an AWS IoT Events detector.
--
-- /Note:/ Consider using 'iotEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotEvents :: Lens.Lens' Action (Core.Maybe Types.IotEventsAction)
aIotEvents = Lens.field @"iotEvents"
{-# DEPRECATED aIotEvents "Use generic-lens or generic-optics with 'iotEvents' instead." #-}

-- | Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
-- /Note:/ Consider using 'iotSiteWise' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotSiteWise :: Lens.Lens' Action (Core.Maybe Types.IotSiteWiseAction)
aIotSiteWise = Lens.field @"iotSiteWise"
{-# DEPRECATED aIotSiteWise "Use generic-lens or generic-optics with 'iotSiteWise' instead." #-}

-- | Write data to an Amazon Kinesis stream.
--
-- /Note:/ Consider using 'kinesis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKinesis :: Lens.Lens' Action (Core.Maybe Types.KinesisAction)
aKinesis = Lens.field @"kinesis"
{-# DEPRECATED aKinesis "Use generic-lens or generic-optics with 'kinesis' instead." #-}

-- | Invoke a Lambda function.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLambda :: Lens.Lens' Action (Core.Maybe Types.LambdaAction)
aLambda = Lens.field @"lambda"
{-# DEPRECATED aLambda "Use generic-lens or generic-optics with 'lambda' instead." #-}

-- | Publish to another MQTT topic.
--
-- /Note:/ Consider using 'republish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRepublish :: Lens.Lens' Action (Core.Maybe Types.RepublishAction)
aRepublish = Lens.field @"republish"
{-# DEPRECATED aRepublish "Use generic-lens or generic-optics with 'republish' instead." #-}

-- | Write to an Amazon S3 bucket.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aS3 :: Lens.Lens' Action (Core.Maybe Types.S3Action)
aS3 = Lens.field @"s3"
{-# DEPRECATED aS3 "Use generic-lens or generic-optics with 's3' instead." #-}

-- | Send a message to a Salesforce IoT Cloud Input Stream.
--
-- /Note:/ Consider using 'salesforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSalesforce :: Lens.Lens' Action (Core.Maybe Types.SalesforceAction)
aSalesforce = Lens.field @"salesforce"
{-# DEPRECATED aSalesforce "Use generic-lens or generic-optics with 'salesforce' instead." #-}

-- | Publish to an Amazon SNS topic.
--
-- /Note:/ Consider using 'sns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSns :: Lens.Lens' Action (Core.Maybe Types.SnsAction)
aSns = Lens.field @"sns"
{-# DEPRECATED aSns "Use generic-lens or generic-optics with 'sns' instead." #-}

-- | Publish to an Amazon SQS queue.
--
-- /Note:/ Consider using 'sqs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSqs :: Lens.Lens' Action (Core.Maybe Types.SqsAction)
aSqs = Lens.field @"sqs"
{-# DEPRECATED aSqs "Use generic-lens or generic-optics with 'sqs' instead." #-}

-- | Starts execution of a Step Functions state machine.
--
-- /Note:/ Consider using 'stepFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStepFunctions :: Lens.Lens' Action (Core.Maybe Types.StepFunctionsAction)
aStepFunctions = Lens.field @"stepFunctions"
{-# DEPRECATED aStepFunctions "Use generic-lens or generic-optics with 'stepFunctions' instead." #-}

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
-- /Note:/ Consider using 'timestream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimestream :: Lens.Lens' Action (Core.Maybe Types.TimestreamAction)
aTimestream = Lens.field @"timestream"
{-# DEPRECATED aTimestream "Use generic-lens or generic-optics with 'timestream' instead." #-}

instance Core.FromJSON Action where
  toJSON Action {..} =
    Core.object
      ( Core.catMaybes
          [ ("cloudwatchAlarm" Core..=) Core.<$> cloudwatchAlarm,
            ("cloudwatchLogs" Core..=) Core.<$> cloudwatchLogs,
            ("cloudwatchMetric" Core..=) Core.<$> cloudwatchMetric,
            ("dynamoDB" Core..=) Core.<$> dynamoDB,
            ("dynamoDBv2" Core..=) Core.<$> dynamoDBv2,
            ("elasticsearch" Core..=) Core.<$> elasticsearch,
            ("firehose" Core..=) Core.<$> firehose,
            ("http" Core..=) Core.<$> http,
            ("iotAnalytics" Core..=) Core.<$> iotAnalytics,
            ("iotEvents" Core..=) Core.<$> iotEvents,
            ("iotSiteWise" Core..=) Core.<$> iotSiteWise,
            ("kinesis" Core..=) Core.<$> kinesis,
            ("lambda" Core..=) Core.<$> lambda,
            ("republish" Core..=) Core.<$> republish,
            ("s3" Core..=) Core.<$> s3,
            ("salesforce" Core..=) Core.<$> salesforce,
            ("sns" Core..=) Core.<$> sns,
            ("sqs" Core..=) Core.<$> sqs,
            ("stepFunctions" Core..=) Core.<$> stepFunctions,
            ("timestream" Core..=) Core.<$> timestream
          ]
      )

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject "Action" Core.$
      \x ->
        Action'
          Core.<$> (x Core..:? "cloudwatchAlarm")
          Core.<*> (x Core..:? "cloudwatchLogs")
          Core.<*> (x Core..:? "cloudwatchMetric")
          Core.<*> (x Core..:? "dynamoDB")
          Core.<*> (x Core..:? "dynamoDBv2")
          Core.<*> (x Core..:? "elasticsearch")
          Core.<*> (x Core..:? "firehose")
          Core.<*> (x Core..:? "http")
          Core.<*> (x Core..:? "iotAnalytics")
          Core.<*> (x Core..:? "iotEvents")
          Core.<*> (x Core..:? "iotSiteWise")
          Core.<*> (x Core..:? "kinesis")
          Core.<*> (x Core..:? "lambda")
          Core.<*> (x Core..:? "republish")
          Core.<*> (x Core..:? "s3")
          Core.<*> (x Core..:? "salesforce")
          Core.<*> (x Core..:? "sns")
          Core.<*> (x Core..:? "sqs")
          Core.<*> (x Core..:? "stepFunctions")
          Core.<*> (x Core..:? "timestream")
