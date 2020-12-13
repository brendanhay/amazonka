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
    aCloudwatchMetric,
    aCloudwatchLogs,
    aDynamoDBv2,
    aStepFunctions,
    aCloudwatchAlarm,
    aSns,
    aDynamoDB,
    aFirehose,
    aTimestream,
    aIotSiteWise,
    aIotAnalytics,
    aLambda,
    aIotEvents,
    aSalesforce,
    aKinesis,
    aS3,
    aHttp,
    aElasticsearch,
    aRepublish,
    aSqs,
  )
where

import Network.AWS.IoT.Types.CloudwatchAlarmAction
import Network.AWS.IoT.Types.CloudwatchLogsAction
import Network.AWS.IoT.Types.CloudwatchMetricAction
import Network.AWS.IoT.Types.DynamoDBAction
import Network.AWS.IoT.Types.DynamoDBv2Action
import Network.AWS.IoT.Types.ElasticsearchAction
import Network.AWS.IoT.Types.FirehoseAction
import Network.AWS.IoT.Types.HTTPAction
import Network.AWS.IoT.Types.IotAnalyticsAction
import Network.AWS.IoT.Types.IotEventsAction
import Network.AWS.IoT.Types.IotSiteWiseAction
import Network.AWS.IoT.Types.KinesisAction
import Network.AWS.IoT.Types.LambdaAction
import Network.AWS.IoT.Types.RepublishAction
import Network.AWS.IoT.Types.S3Action
import Network.AWS.IoT.Types.SNSAction
import Network.AWS.IoT.Types.SalesforceAction
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.TimestreamAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the actions associated with a rule.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | Capture a CloudWatch metric.
    cloudwatchMetric :: Lude.Maybe CloudwatchMetricAction,
    -- | Send data to CloudWatch Logs.
    cloudwatchLogs :: Lude.Maybe CloudwatchLogsAction,
    -- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
    dynamoDBv2 :: Lude.Maybe DynamoDBv2Action,
    -- | Starts execution of a Step Functions state machine.
    stepFunctions :: Lude.Maybe StepFunctionsAction,
    -- | Change the state of a CloudWatch alarm.
    cloudwatchAlarm :: Lude.Maybe CloudwatchAlarmAction,
    -- | Publish to an Amazon SNS topic.
    sns :: Lude.Maybe SNSAction,
    -- | Write to a DynamoDB table.
    dynamoDB :: Lude.Maybe DynamoDBAction,
    -- | Write to an Amazon Kinesis Firehose stream.
    firehose :: Lude.Maybe FirehoseAction,
    -- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
    timestream :: Lude.Maybe TimestreamAction,
    -- | Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
    iotSiteWise :: Lude.Maybe IotSiteWiseAction,
    -- | Sends message data to an AWS IoT Analytics channel.
    iotAnalytics :: Lude.Maybe IotAnalyticsAction,
    -- | Invoke a Lambda function.
    lambda :: Lude.Maybe LambdaAction,
    -- | Sends an input to an AWS IoT Events detector.
    iotEvents :: Lude.Maybe IotEventsAction,
    -- | Send a message to a Salesforce IoT Cloud Input Stream.
    salesforce :: Lude.Maybe SalesforceAction,
    -- | Write data to an Amazon Kinesis stream.
    kinesis :: Lude.Maybe KinesisAction,
    -- | Write to an Amazon S3 bucket.
    s3 :: Lude.Maybe S3Action,
    -- | Send data to an HTTPS endpoint.
    http :: Lude.Maybe HTTPAction,
    -- | Write data to an Amazon Elasticsearch Service domain.
    elasticsearch :: Lude.Maybe ElasticsearchAction,
    -- | Publish to another MQTT topic.
    republish :: Lude.Maybe RepublishAction,
    -- | Publish to an Amazon SQS queue.
    sqs :: Lude.Maybe SqsAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- * 'cloudwatchMetric' - Capture a CloudWatch metric.
-- * 'cloudwatchLogs' - Send data to CloudWatch Logs.
-- * 'dynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
-- * 'stepFunctions' - Starts execution of a Step Functions state machine.
-- * 'cloudwatchAlarm' - Change the state of a CloudWatch alarm.
-- * 'sns' - Publish to an Amazon SNS topic.
-- * 'dynamoDB' - Write to a DynamoDB table.
-- * 'firehose' - Write to an Amazon Kinesis Firehose stream.
-- * 'timestream' - The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
-- * 'iotSiteWise' - Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
-- * 'iotAnalytics' - Sends message data to an AWS IoT Analytics channel.
-- * 'lambda' - Invoke a Lambda function.
-- * 'iotEvents' - Sends an input to an AWS IoT Events detector.
-- * 'salesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
-- * 'kinesis' - Write data to an Amazon Kinesis stream.
-- * 's3' - Write to an Amazon S3 bucket.
-- * 'http' - Send data to an HTTPS endpoint.
-- * 'elasticsearch' - Write data to an Amazon Elasticsearch Service domain.
-- * 'republish' - Publish to another MQTT topic.
-- * 'sqs' - Publish to an Amazon SQS queue.
mkAction ::
  Action
mkAction =
  Action'
    { cloudwatchMetric = Lude.Nothing,
      cloudwatchLogs = Lude.Nothing,
      dynamoDBv2 = Lude.Nothing,
      stepFunctions = Lude.Nothing,
      cloudwatchAlarm = Lude.Nothing,
      sns = Lude.Nothing,
      dynamoDB = Lude.Nothing,
      firehose = Lude.Nothing,
      timestream = Lude.Nothing,
      iotSiteWise = Lude.Nothing,
      iotAnalytics = Lude.Nothing,
      lambda = Lude.Nothing,
      iotEvents = Lude.Nothing,
      salesforce = Lude.Nothing,
      kinesis = Lude.Nothing,
      s3 = Lude.Nothing,
      http = Lude.Nothing,
      elasticsearch = Lude.Nothing,
      republish = Lude.Nothing,
      sqs = Lude.Nothing
    }

-- | Capture a CloudWatch metric.
--
-- /Note:/ Consider using 'cloudwatchMetric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchMetric :: Lens.Lens' Action (Lude.Maybe CloudwatchMetricAction)
aCloudwatchMetric = Lens.lens (cloudwatchMetric :: Action -> Lude.Maybe CloudwatchMetricAction) (\s a -> s {cloudwatchMetric = a} :: Action)
{-# DEPRECATED aCloudwatchMetric "Use generic-lens or generic-optics with 'cloudwatchMetric' instead." #-}

-- | Send data to CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudwatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchLogs :: Lens.Lens' Action (Lude.Maybe CloudwatchLogsAction)
aCloudwatchLogs = Lens.lens (cloudwatchLogs :: Action -> Lude.Maybe CloudwatchLogsAction) (\s a -> s {cloudwatchLogs = a} :: Action)
{-# DEPRECATED aCloudwatchLogs "Use generic-lens or generic-optics with 'cloudwatchLogs' instead." #-}

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
--
-- /Note:/ Consider using 'dynamoDBv2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamoDBv2 :: Lens.Lens' Action (Lude.Maybe DynamoDBv2Action)
aDynamoDBv2 = Lens.lens (dynamoDBv2 :: Action -> Lude.Maybe DynamoDBv2Action) (\s a -> s {dynamoDBv2 = a} :: Action)
{-# DEPRECATED aDynamoDBv2 "Use generic-lens or generic-optics with 'dynamoDBv2' instead." #-}

-- | Starts execution of a Step Functions state machine.
--
-- /Note:/ Consider using 'stepFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStepFunctions :: Lens.Lens' Action (Lude.Maybe StepFunctionsAction)
aStepFunctions = Lens.lens (stepFunctions :: Action -> Lude.Maybe StepFunctionsAction) (\s a -> s {stepFunctions = a} :: Action)
{-# DEPRECATED aStepFunctions "Use generic-lens or generic-optics with 'stepFunctions' instead." #-}

-- | Change the state of a CloudWatch alarm.
--
-- /Note:/ Consider using 'cloudwatchAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCloudwatchAlarm :: Lens.Lens' Action (Lude.Maybe CloudwatchAlarmAction)
aCloudwatchAlarm = Lens.lens (cloudwatchAlarm :: Action -> Lude.Maybe CloudwatchAlarmAction) (\s a -> s {cloudwatchAlarm = a} :: Action)
{-# DEPRECATED aCloudwatchAlarm "Use generic-lens or generic-optics with 'cloudwatchAlarm' instead." #-}

-- | Publish to an Amazon SNS topic.
--
-- /Note:/ Consider using 'sns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSns :: Lens.Lens' Action (Lude.Maybe SNSAction)
aSns = Lens.lens (sns :: Action -> Lude.Maybe SNSAction) (\s a -> s {sns = a} :: Action)
{-# DEPRECATED aSns "Use generic-lens or generic-optics with 'sns' instead." #-}

-- | Write to a DynamoDB table.
--
-- /Note:/ Consider using 'dynamoDB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamoDB :: Lens.Lens' Action (Lude.Maybe DynamoDBAction)
aDynamoDB = Lens.lens (dynamoDB :: Action -> Lude.Maybe DynamoDBAction) (\s a -> s {dynamoDB = a} :: Action)
{-# DEPRECATED aDynamoDB "Use generic-lens or generic-optics with 'dynamoDB' instead." #-}

-- | Write to an Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'firehose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFirehose :: Lens.Lens' Action (Lude.Maybe FirehoseAction)
aFirehose = Lens.lens (firehose :: Action -> Lude.Maybe FirehoseAction) (\s a -> s {firehose = a} :: Action)
{-# DEPRECATED aFirehose "Use generic-lens or generic-optics with 'firehose' instead." #-}

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
-- /Note:/ Consider using 'timestream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimestream :: Lens.Lens' Action (Lude.Maybe TimestreamAction)
aTimestream = Lens.lens (timestream :: Action -> Lude.Maybe TimestreamAction) (\s a -> s {timestream = a} :: Action)
{-# DEPRECATED aTimestream "Use generic-lens or generic-optics with 'timestream' instead." #-}

-- | Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
-- /Note:/ Consider using 'iotSiteWise' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotSiteWise :: Lens.Lens' Action (Lude.Maybe IotSiteWiseAction)
aIotSiteWise = Lens.lens (iotSiteWise :: Action -> Lude.Maybe IotSiteWiseAction) (\s a -> s {iotSiteWise = a} :: Action)
{-# DEPRECATED aIotSiteWise "Use generic-lens or generic-optics with 'iotSiteWise' instead." #-}

-- | Sends message data to an AWS IoT Analytics channel.
--
-- /Note:/ Consider using 'iotAnalytics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotAnalytics :: Lens.Lens' Action (Lude.Maybe IotAnalyticsAction)
aIotAnalytics = Lens.lens (iotAnalytics :: Action -> Lude.Maybe IotAnalyticsAction) (\s a -> s {iotAnalytics = a} :: Action)
{-# DEPRECATED aIotAnalytics "Use generic-lens or generic-optics with 'iotAnalytics' instead." #-}

-- | Invoke a Lambda function.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLambda :: Lens.Lens' Action (Lude.Maybe LambdaAction)
aLambda = Lens.lens (lambda :: Action -> Lude.Maybe LambdaAction) (\s a -> s {lambda = a} :: Action)
{-# DEPRECATED aLambda "Use generic-lens or generic-optics with 'lambda' instead." #-}

-- | Sends an input to an AWS IoT Events detector.
--
-- /Note:/ Consider using 'iotEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIotEvents :: Lens.Lens' Action (Lude.Maybe IotEventsAction)
aIotEvents = Lens.lens (iotEvents :: Action -> Lude.Maybe IotEventsAction) (\s a -> s {iotEvents = a} :: Action)
{-# DEPRECATED aIotEvents "Use generic-lens or generic-optics with 'iotEvents' instead." #-}

-- | Send a message to a Salesforce IoT Cloud Input Stream.
--
-- /Note:/ Consider using 'salesforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSalesforce :: Lens.Lens' Action (Lude.Maybe SalesforceAction)
aSalesforce = Lens.lens (salesforce :: Action -> Lude.Maybe SalesforceAction) (\s a -> s {salesforce = a} :: Action)
{-# DEPRECATED aSalesforce "Use generic-lens or generic-optics with 'salesforce' instead." #-}

-- | Write data to an Amazon Kinesis stream.
--
-- /Note:/ Consider using 'kinesis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKinesis :: Lens.Lens' Action (Lude.Maybe KinesisAction)
aKinesis = Lens.lens (kinesis :: Action -> Lude.Maybe KinesisAction) (\s a -> s {kinesis = a} :: Action)
{-# DEPRECATED aKinesis "Use generic-lens or generic-optics with 'kinesis' instead." #-}

-- | Write to an Amazon S3 bucket.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aS3 :: Lens.Lens' Action (Lude.Maybe S3Action)
aS3 = Lens.lens (s3 :: Action -> Lude.Maybe S3Action) (\s a -> s {s3 = a} :: Action)
{-# DEPRECATED aS3 "Use generic-lens or generic-optics with 's3' instead." #-}

-- | Send data to an HTTPS endpoint.
--
-- /Note:/ Consider using 'http' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHttp :: Lens.Lens' Action (Lude.Maybe HTTPAction)
aHttp = Lens.lens (http :: Action -> Lude.Maybe HTTPAction) (\s a -> s {http = a} :: Action)
{-# DEPRECATED aHttp "Use generic-lens or generic-optics with 'http' instead." #-}

-- | Write data to an Amazon Elasticsearch Service domain.
--
-- /Note:/ Consider using 'elasticsearch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aElasticsearch :: Lens.Lens' Action (Lude.Maybe ElasticsearchAction)
aElasticsearch = Lens.lens (elasticsearch :: Action -> Lude.Maybe ElasticsearchAction) (\s a -> s {elasticsearch = a} :: Action)
{-# DEPRECATED aElasticsearch "Use generic-lens or generic-optics with 'elasticsearch' instead." #-}

-- | Publish to another MQTT topic.
--
-- /Note:/ Consider using 'republish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRepublish :: Lens.Lens' Action (Lude.Maybe RepublishAction)
aRepublish = Lens.lens (republish :: Action -> Lude.Maybe RepublishAction) (\s a -> s {republish = a} :: Action)
{-# DEPRECATED aRepublish "Use generic-lens or generic-optics with 'republish' instead." #-}

-- | Publish to an Amazon SQS queue.
--
-- /Note:/ Consider using 'sqs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSqs :: Lens.Lens' Action (Lude.Maybe SqsAction)
aSqs = Lens.lens (sqs :: Action -> Lude.Maybe SqsAction) (\s a -> s {sqs = a} :: Action)
{-# DEPRECATED aSqs "Use generic-lens or generic-optics with 'sqs' instead." #-}

instance Lude.FromJSON Action where
  parseJSON =
    Lude.withObject
      "Action"
      ( \x ->
          Action'
            Lude.<$> (x Lude..:? "cloudwatchMetric")
            Lude.<*> (x Lude..:? "cloudwatchLogs")
            Lude.<*> (x Lude..:? "dynamoDBv2")
            Lude.<*> (x Lude..:? "stepFunctions")
            Lude.<*> (x Lude..:? "cloudwatchAlarm")
            Lude.<*> (x Lude..:? "sns")
            Lude.<*> (x Lude..:? "dynamoDB")
            Lude.<*> (x Lude..:? "firehose")
            Lude.<*> (x Lude..:? "timestream")
            Lude.<*> (x Lude..:? "iotSiteWise")
            Lude.<*> (x Lude..:? "iotAnalytics")
            Lude.<*> (x Lude..:? "lambda")
            Lude.<*> (x Lude..:? "iotEvents")
            Lude.<*> (x Lude..:? "salesforce")
            Lude.<*> (x Lude..:? "kinesis")
            Lude.<*> (x Lude..:? "s3")
            Lude.<*> (x Lude..:? "http")
            Lude.<*> (x Lude..:? "elasticsearch")
            Lude.<*> (x Lude..:? "republish")
            Lude.<*> (x Lude..:? "sqs")
      )

instance Lude.ToJSON Action where
  toJSON Action' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cloudwatchMetric" Lude..=) Lude.<$> cloudwatchMetric,
            ("cloudwatchLogs" Lude..=) Lude.<$> cloudwatchLogs,
            ("dynamoDBv2" Lude..=) Lude.<$> dynamoDBv2,
            ("stepFunctions" Lude..=) Lude.<$> stepFunctions,
            ("cloudwatchAlarm" Lude..=) Lude.<$> cloudwatchAlarm,
            ("sns" Lude..=) Lude.<$> sns,
            ("dynamoDB" Lude..=) Lude.<$> dynamoDB,
            ("firehose" Lude..=) Lude.<$> firehose,
            ("timestream" Lude..=) Lude.<$> timestream,
            ("iotSiteWise" Lude..=) Lude.<$> iotSiteWise,
            ("iotAnalytics" Lude..=) Lude.<$> iotAnalytics,
            ("lambda" Lude..=) Lude.<$> lambda,
            ("iotEvents" Lude..=) Lude.<$> iotEvents,
            ("salesforce" Lude..=) Lude.<$> salesforce,
            ("kinesis" Lude..=) Lude.<$> kinesis,
            ("s3" Lude..=) Lude.<$> s3,
            ("http" Lude..=) Lude.<$> http,
            ("elasticsearch" Lude..=) Lude.<$> elasticsearch,
            ("republish" Lude..=) Lude.<$> republish,
            ("sqs" Lude..=) Lude.<$> sqs
          ]
      )
