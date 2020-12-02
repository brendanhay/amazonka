{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Action where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the actions associated with a rule.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aCloudwatchMetric ::
      !(Maybe CloudwatchMetricAction),
    _aCloudwatchLogs :: !(Maybe CloudwatchLogsAction),
    _aDynamoDBv2 :: !(Maybe DynamoDBv2Action),
    _aStepFunctions :: !(Maybe StepFunctionsAction),
    _aCloudwatchAlarm :: !(Maybe CloudwatchAlarmAction),
    _aSns :: !(Maybe SNSAction),
    _aDynamoDB :: !(Maybe DynamoDBAction),
    _aFirehose :: !(Maybe FirehoseAction),
    _aTimestream :: !(Maybe TimestreamAction),
    _aIotSiteWise :: !(Maybe IotSiteWiseAction),
    _aIotAnalytics :: !(Maybe IotAnalyticsAction),
    _aLambda :: !(Maybe LambdaAction),
    _aIotEvents :: !(Maybe IotEventsAction),
    _aSalesforce :: !(Maybe SalesforceAction),
    _aKinesis :: !(Maybe KinesisAction),
    _aS3 :: !(Maybe S3Action),
    _aHttp :: !(Maybe HTTPAction),
    _aElasticsearch :: !(Maybe ElasticsearchAction),
    _aRepublish :: !(Maybe RepublishAction),
    _aSqs :: !(Maybe SqsAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCloudwatchMetric' - Capture a CloudWatch metric.
--
-- * 'aCloudwatchLogs' - Send data to CloudWatch Logs.
--
-- * 'aDynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
--
-- * 'aStepFunctions' - Starts execution of a Step Functions state machine.
--
-- * 'aCloudwatchAlarm' - Change the state of a CloudWatch alarm.
--
-- * 'aSns' - Publish to an Amazon SNS topic.
--
-- * 'aDynamoDB' - Write to a DynamoDB table.
--
-- * 'aFirehose' - Write to an Amazon Kinesis Firehose stream.
--
-- * 'aTimestream' - The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
-- * 'aIotSiteWise' - Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
-- * 'aIotAnalytics' - Sends message data to an AWS IoT Analytics channel.
--
-- * 'aLambda' - Invoke a Lambda function.
--
-- * 'aIotEvents' - Sends an input to an AWS IoT Events detector.
--
-- * 'aSalesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
--
-- * 'aKinesis' - Write data to an Amazon Kinesis stream.
--
-- * 'aS3' - Write to an Amazon S3 bucket.
--
-- * 'aHttp' - Send data to an HTTPS endpoint.
--
-- * 'aElasticsearch' - Write data to an Amazon Elasticsearch Service domain.
--
-- * 'aRepublish' - Publish to another MQTT topic.
--
-- * 'aSqs' - Publish to an Amazon SQS queue.
action ::
  Action
action =
  Action'
    { _aCloudwatchMetric = Nothing,
      _aCloudwatchLogs = Nothing,
      _aDynamoDBv2 = Nothing,
      _aStepFunctions = Nothing,
      _aCloudwatchAlarm = Nothing,
      _aSns = Nothing,
      _aDynamoDB = Nothing,
      _aFirehose = Nothing,
      _aTimestream = Nothing,
      _aIotSiteWise = Nothing,
      _aIotAnalytics = Nothing,
      _aLambda = Nothing,
      _aIotEvents = Nothing,
      _aSalesforce = Nothing,
      _aKinesis = Nothing,
      _aS3 = Nothing,
      _aHttp = Nothing,
      _aElasticsearch = Nothing,
      _aRepublish = Nothing,
      _aSqs = Nothing
    }

-- | Capture a CloudWatch metric.
aCloudwatchMetric :: Lens' Action (Maybe CloudwatchMetricAction)
aCloudwatchMetric = lens _aCloudwatchMetric (\s a -> s {_aCloudwatchMetric = a})

-- | Send data to CloudWatch Logs.
aCloudwatchLogs :: Lens' Action (Maybe CloudwatchLogsAction)
aCloudwatchLogs = lens _aCloudwatchLogs (\s a -> s {_aCloudwatchLogs = a})

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
aDynamoDBv2 :: Lens' Action (Maybe DynamoDBv2Action)
aDynamoDBv2 = lens _aDynamoDBv2 (\s a -> s {_aDynamoDBv2 = a})

-- | Starts execution of a Step Functions state machine.
aStepFunctions :: Lens' Action (Maybe StepFunctionsAction)
aStepFunctions = lens _aStepFunctions (\s a -> s {_aStepFunctions = a})

-- | Change the state of a CloudWatch alarm.
aCloudwatchAlarm :: Lens' Action (Maybe CloudwatchAlarmAction)
aCloudwatchAlarm = lens _aCloudwatchAlarm (\s a -> s {_aCloudwatchAlarm = a})

-- | Publish to an Amazon SNS topic.
aSns :: Lens' Action (Maybe SNSAction)
aSns = lens _aSns (\s a -> s {_aSns = a})

-- | Write to a DynamoDB table.
aDynamoDB :: Lens' Action (Maybe DynamoDBAction)
aDynamoDB = lens _aDynamoDB (\s a -> s {_aDynamoDB = a})

-- | Write to an Amazon Kinesis Firehose stream.
aFirehose :: Lens' Action (Maybe FirehoseAction)
aFirehose = lens _aFirehose (\s a -> s {_aFirehose = a})

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
aTimestream :: Lens' Action (Maybe TimestreamAction)
aTimestream = lens _aTimestream (\s a -> s {_aTimestream = a})

-- | Sends data from the MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
aIotSiteWise :: Lens' Action (Maybe IotSiteWiseAction)
aIotSiteWise = lens _aIotSiteWise (\s a -> s {_aIotSiteWise = a})

-- | Sends message data to an AWS IoT Analytics channel.
aIotAnalytics :: Lens' Action (Maybe IotAnalyticsAction)
aIotAnalytics = lens _aIotAnalytics (\s a -> s {_aIotAnalytics = a})

-- | Invoke a Lambda function.
aLambda :: Lens' Action (Maybe LambdaAction)
aLambda = lens _aLambda (\s a -> s {_aLambda = a})

-- | Sends an input to an AWS IoT Events detector.
aIotEvents :: Lens' Action (Maybe IotEventsAction)
aIotEvents = lens _aIotEvents (\s a -> s {_aIotEvents = a})

-- | Send a message to a Salesforce IoT Cloud Input Stream.
aSalesforce :: Lens' Action (Maybe SalesforceAction)
aSalesforce = lens _aSalesforce (\s a -> s {_aSalesforce = a})

-- | Write data to an Amazon Kinesis stream.
aKinesis :: Lens' Action (Maybe KinesisAction)
aKinesis = lens _aKinesis (\s a -> s {_aKinesis = a})

-- | Write to an Amazon S3 bucket.
aS3 :: Lens' Action (Maybe S3Action)
aS3 = lens _aS3 (\s a -> s {_aS3 = a})

-- | Send data to an HTTPS endpoint.
aHttp :: Lens' Action (Maybe HTTPAction)
aHttp = lens _aHttp (\s a -> s {_aHttp = a})

-- | Write data to an Amazon Elasticsearch Service domain.
aElasticsearch :: Lens' Action (Maybe ElasticsearchAction)
aElasticsearch = lens _aElasticsearch (\s a -> s {_aElasticsearch = a})

-- | Publish to another MQTT topic.
aRepublish :: Lens' Action (Maybe RepublishAction)
aRepublish = lens _aRepublish (\s a -> s {_aRepublish = a})

-- | Publish to an Amazon SQS queue.
aSqs :: Lens' Action (Maybe SqsAction)
aSqs = lens _aSqs (\s a -> s {_aSqs = a})

instance FromJSON Action where
  parseJSON =
    withObject
      "Action"
      ( \x ->
          Action'
            <$> (x .:? "cloudwatchMetric")
            <*> (x .:? "cloudwatchLogs")
            <*> (x .:? "dynamoDBv2")
            <*> (x .:? "stepFunctions")
            <*> (x .:? "cloudwatchAlarm")
            <*> (x .:? "sns")
            <*> (x .:? "dynamoDB")
            <*> (x .:? "firehose")
            <*> (x .:? "timestream")
            <*> (x .:? "iotSiteWise")
            <*> (x .:? "iotAnalytics")
            <*> (x .:? "lambda")
            <*> (x .:? "iotEvents")
            <*> (x .:? "salesforce")
            <*> (x .:? "kinesis")
            <*> (x .:? "s3")
            <*> (x .:? "http")
            <*> (x .:? "elasticsearch")
            <*> (x .:? "republish")
            <*> (x .:? "sqs")
      )

instance Hashable Action

instance NFData Action

instance ToJSON Action where
  toJSON Action' {..} =
    object
      ( catMaybes
          [ ("cloudwatchMetric" .=) <$> _aCloudwatchMetric,
            ("cloudwatchLogs" .=) <$> _aCloudwatchLogs,
            ("dynamoDBv2" .=) <$> _aDynamoDBv2,
            ("stepFunctions" .=) <$> _aStepFunctions,
            ("cloudwatchAlarm" .=) <$> _aCloudwatchAlarm,
            ("sns" .=) <$> _aSns,
            ("dynamoDB" .=) <$> _aDynamoDB,
            ("firehose" .=) <$> _aFirehose,
            ("timestream" .=) <$> _aTimestream,
            ("iotSiteWise" .=) <$> _aIotSiteWise,
            ("iotAnalytics" .=) <$> _aIotAnalytics,
            ("lambda" .=) <$> _aLambda,
            ("iotEvents" .=) <$> _aIotEvents,
            ("salesforce" .=) <$> _aSalesforce,
            ("kinesis" .=) <$> _aKinesis,
            ("s3" .=) <$> _aS3,
            ("http" .=) <$> _aHttp,
            ("elasticsearch" .=) <$> _aElasticsearch,
            ("republish" .=) <$> _aRepublish,
            ("sqs" .=) <$> _aSqs
          ]
      )
