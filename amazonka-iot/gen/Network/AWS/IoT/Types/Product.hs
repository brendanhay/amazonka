{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Product where

import           Network.AWS.IoT.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Describes the actions associated with a rule.
--
-- /See:/ 'action' smart constructor.
data Action = Action'
    { _aCloudwatchMetric :: !(Maybe CloudwatchMetricAction)
    , _aCloudwatchAlarm  :: !(Maybe CloudwatchAlarmAction)
    , _aSns              :: !(Maybe SNSAction)
    , _aDynamoDB         :: !(Maybe DynamoDBAction)
    , _aFirehose         :: !(Maybe FirehoseAction)
    , _aLambda           :: !(Maybe LambdaAction)
    , _aKinesis          :: !(Maybe KinesisAction)
    , _aS3               :: !(Maybe S3Action)
    , _aElasticsearch    :: !(Maybe ElasticsearchAction)
    , _aRepublish        :: !(Maybe RepublishAction)
    , _aSqs              :: !(Maybe SqsAction)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCloudwatchMetric'
--
-- * 'aCloudwatchAlarm'
--
-- * 'aSns'
--
-- * 'aDynamoDB'
--
-- * 'aFirehose'
--
-- * 'aLambda'
--
-- * 'aKinesis'
--
-- * 'aS3'
--
-- * 'aElasticsearch'
--
-- * 'aRepublish'
--
-- * 'aSqs'
action
    :: Action
action =
    Action'
    { _aCloudwatchMetric = Nothing
    , _aCloudwatchAlarm = Nothing
    , _aSns = Nothing
    , _aDynamoDB = Nothing
    , _aFirehose = Nothing
    , _aLambda = Nothing
    , _aKinesis = Nothing
    , _aS3 = Nothing
    , _aElasticsearch = Nothing
    , _aRepublish = Nothing
    , _aSqs = Nothing
    }

-- | Capture a CloudWatch metric.
aCloudwatchMetric :: Lens' Action (Maybe CloudwatchMetricAction)
aCloudwatchMetric = lens _aCloudwatchMetric (\ s a -> s{_aCloudwatchMetric = a});

-- | Change the state of a CloudWatch alarm.
aCloudwatchAlarm :: Lens' Action (Maybe CloudwatchAlarmAction)
aCloudwatchAlarm = lens _aCloudwatchAlarm (\ s a -> s{_aCloudwatchAlarm = a});

-- | Publish to an Amazon SNS topic.
aSns :: Lens' Action (Maybe SNSAction)
aSns = lens _aSns (\ s a -> s{_aSns = a});

-- | Write to a DynamoDB table.
aDynamoDB :: Lens' Action (Maybe DynamoDBAction)
aDynamoDB = lens _aDynamoDB (\ s a -> s{_aDynamoDB = a});

-- | Write to an Amazon Kinesis Firehose stream.
aFirehose :: Lens' Action (Maybe FirehoseAction)
aFirehose = lens _aFirehose (\ s a -> s{_aFirehose = a});

-- | Invoke a Lambda function.
aLambda :: Lens' Action (Maybe LambdaAction)
aLambda = lens _aLambda (\ s a -> s{_aLambda = a});

-- | Write data to an Amazon Kinesis stream.
aKinesis :: Lens' Action (Maybe KinesisAction)
aKinesis = lens _aKinesis (\ s a -> s{_aKinesis = a});

-- | Write to an Amazon S3 bucket.
aS3 :: Lens' Action (Maybe S3Action)
aS3 = lens _aS3 (\ s a -> s{_aS3 = a});

-- | Write data to an Amazon Elasticsearch Service; domain.
aElasticsearch :: Lens' Action (Maybe ElasticsearchAction)
aElasticsearch = lens _aElasticsearch (\ s a -> s{_aElasticsearch = a});

-- | Publish to another MQTT topic.
aRepublish :: Lens' Action (Maybe RepublishAction)
aRepublish = lens _aRepublish (\ s a -> s{_aRepublish = a});

-- | Publish to an Amazon SQS queue.
aSqs :: Lens' Action (Maybe SqsAction)
aSqs = lens _aSqs (\ s a -> s{_aSqs = a});

instance FromJSON Action where
        parseJSON
          = withObject "Action"
              (\ x ->
                 Action' <$>
                   (x .:? "cloudwatchMetric") <*>
                     (x .:? "cloudwatchAlarm")
                     <*> (x .:? "sns")
                     <*> (x .:? "dynamoDB")
                     <*> (x .:? "firehose")
                     <*> (x .:? "lambda")
                     <*> (x .:? "kinesis")
                     <*> (x .:? "s3")
                     <*> (x .:? "elasticsearch")
                     <*> (x .:? "republish")
                     <*> (x .:? "sqs"))

instance Hashable Action

instance NFData Action

instance ToJSON Action where
        toJSON Action'{..}
          = object
              (catMaybes
                 [("cloudwatchMetric" .=) <$> _aCloudwatchMetric,
                  ("cloudwatchAlarm" .=) <$> _aCloudwatchAlarm,
                  ("sns" .=) <$> _aSns, ("dynamoDB" .=) <$> _aDynamoDB,
                  ("firehose" .=) <$> _aFirehose,
                  ("lambda" .=) <$> _aLambda,
                  ("kinesis" .=) <$> _aKinesis, ("s3" .=) <$> _aS3,
                  ("elasticsearch" .=) <$> _aElasticsearch,
                  ("republish" .=) <$> _aRepublish,
                  ("sqs" .=) <$> _aSqs])

-- | The attribute payload, a JSON string containing up to three key-value
-- pairs (for example,
-- {\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}).
--
-- /See:/ 'attributePayload' smart constructor.
newtype AttributePayload = AttributePayload'
    { _apAttributes :: Maybe (Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttributePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAttributes'
attributePayload
    :: AttributePayload
attributePayload =
    AttributePayload'
    { _apAttributes = Nothing
    }

-- | A JSON string containing up to three key-value pair in JSON format (for
-- example, {\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}).
apAttributes :: Lens' AttributePayload (HashMap Text Text)
apAttributes = lens _apAttributes (\ s a -> s{_apAttributes = a}) . _Default . _Map;

instance Hashable AttributePayload

instance NFData AttributePayload

instance ToJSON AttributePayload where
        toJSON AttributePayload'{..}
          = object
              (catMaybes [("attributes" .=) <$> _apAttributes])

-- | A CA certificate.
--
-- /See:/ 'cACertificate' smart constructor.
data CACertificate = CACertificate'
    { _cacStatus         :: !(Maybe CACertificateStatus)
    , _cacCertificateARN :: !(Maybe Text)
    , _cacCertificateId  :: !(Maybe Text)
    , _cacCreationDate   :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacStatus'
--
-- * 'cacCertificateARN'
--
-- * 'cacCertificateId'
--
-- * 'cacCreationDate'
cACertificate
    :: CACertificate
cACertificate =
    CACertificate'
    { _cacStatus = Nothing
    , _cacCertificateARN = Nothing
    , _cacCertificateId = Nothing
    , _cacCreationDate = Nothing
    }

-- | The status of the CA certificate.
cacStatus :: Lens' CACertificate (Maybe CACertificateStatus)
cacStatus = lens _cacStatus (\ s a -> s{_cacStatus = a});

-- | The ARN of the CA certificate.
cacCertificateARN :: Lens' CACertificate (Maybe Text)
cacCertificateARN = lens _cacCertificateARN (\ s a -> s{_cacCertificateARN = a});

-- | The ID of the CA certificate.
cacCertificateId :: Lens' CACertificate (Maybe Text)
cacCertificateId = lens _cacCertificateId (\ s a -> s{_cacCertificateId = a});

-- | The date the CA certificate was created.
cacCreationDate :: Lens' CACertificate (Maybe UTCTime)
cacCreationDate = lens _cacCreationDate (\ s a -> s{_cacCreationDate = a}) . mapping _Time;

instance FromJSON CACertificate where
        parseJSON
          = withObject "CACertificate"
              (\ x ->
                 CACertificate' <$>
                   (x .:? "status") <*> (x .:? "certificateArn") <*>
                     (x .:? "certificateId")
                     <*> (x .:? "creationDate"))

instance Hashable CACertificate

instance NFData CACertificate

-- | Describes a CA certificate.
--
-- /See:/ 'cACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
    { _cacdStatus         :: !(Maybe CACertificateStatus)
    , _cacdOwnedBy        :: !(Maybe Text)
    , _cacdCertificatePem :: !(Maybe Text)
    , _cacdCertificateARN :: !(Maybe Text)
    , _cacdCertificateId  :: !(Maybe Text)
    , _cacdCreationDate   :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CACertificateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacdStatus'
--
-- * 'cacdOwnedBy'
--
-- * 'cacdCertificatePem'
--
-- * 'cacdCertificateARN'
--
-- * 'cacdCertificateId'
--
-- * 'cacdCreationDate'
cACertificateDescription
    :: CACertificateDescription
cACertificateDescription =
    CACertificateDescription'
    { _cacdStatus = Nothing
    , _cacdOwnedBy = Nothing
    , _cacdCertificatePem = Nothing
    , _cacdCertificateARN = Nothing
    , _cacdCertificateId = Nothing
    , _cacdCreationDate = Nothing
    }

-- | The status of a CA certificate.
cacdStatus :: Lens' CACertificateDescription (Maybe CACertificateStatus)
cacdStatus = lens _cacdStatus (\ s a -> s{_cacdStatus = a});

-- | The owner of the CA certificate.
cacdOwnedBy :: Lens' CACertificateDescription (Maybe Text)
cacdOwnedBy = lens _cacdOwnedBy (\ s a -> s{_cacdOwnedBy = a});

-- | The CA certificate data, in PEM format.
cacdCertificatePem :: Lens' CACertificateDescription (Maybe Text)
cacdCertificatePem = lens _cacdCertificatePem (\ s a -> s{_cacdCertificatePem = a});

-- | The CA certificate ARN.
cacdCertificateARN :: Lens' CACertificateDescription (Maybe Text)
cacdCertificateARN = lens _cacdCertificateARN (\ s a -> s{_cacdCertificateARN = a});

-- | The CA certificate ID.
cacdCertificateId :: Lens' CACertificateDescription (Maybe Text)
cacdCertificateId = lens _cacdCertificateId (\ s a -> s{_cacdCertificateId = a});

-- | The date the CA certificate was created.
cacdCreationDate :: Lens' CACertificateDescription (Maybe UTCTime)
cacdCreationDate = lens _cacdCreationDate (\ s a -> s{_cacdCreationDate = a}) . mapping _Time;

instance FromJSON CACertificateDescription where
        parseJSON
          = withObject "CACertificateDescription"
              (\ x ->
                 CACertificateDescription' <$>
                   (x .:? "status") <*> (x .:? "ownedBy") <*>
                     (x .:? "certificatePem")
                     <*> (x .:? "certificateArn")
                     <*> (x .:? "certificateId")
                     <*> (x .:? "creationDate"))

instance Hashable CACertificateDescription

instance NFData CACertificateDescription

-- | Information about a certificate.
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
    { _cStatus         :: !(Maybe CertificateStatus)
    , _cCertificateARN :: !(Maybe Text)
    , _cCertificateId  :: !(Maybe Text)
    , _cCreationDate   :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus'
--
-- * 'cCertificateARN'
--
-- * 'cCertificateId'
--
-- * 'cCreationDate'
certificate
    :: Certificate
certificate =
    Certificate'
    { _cStatus = Nothing
    , _cCertificateARN = Nothing
    , _cCertificateId = Nothing
    , _cCreationDate = Nothing
    }

-- | The status of the certificate.
cStatus :: Lens' Certificate (Maybe CertificateStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The ARN of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a});

-- | The ID of the certificate.
cCertificateId :: Lens' Certificate (Maybe Text)
cCertificateId = lens _cCertificateId (\ s a -> s{_cCertificateId = a});

-- | The date and time the certificate was created.
cCreationDate :: Lens' Certificate (Maybe UTCTime)
cCreationDate = lens _cCreationDate (\ s a -> s{_cCreationDate = a}) . mapping _Time;

instance FromJSON Certificate where
        parseJSON
          = withObject "Certificate"
              (\ x ->
                 Certificate' <$>
                   (x .:? "status") <*> (x .:? "certificateArn") <*>
                     (x .:? "certificateId")
                     <*> (x .:? "creationDate"))

instance Hashable Certificate

instance NFData Certificate

-- | Describes a certificate.
--
-- /See:/ 'certificateDescription' smart constructor.
data CertificateDescription = CertificateDescription'
    { _cdStatus           :: !(Maybe CertificateStatus)
    , _cdOwnedBy          :: !(Maybe Text)
    , _cdLastModifiedDate :: !(Maybe POSIX)
    , _cdCaCertificateId  :: !(Maybe Text)
    , _cdPreviousOwnedBy  :: !(Maybe Text)
    , _cdCertificatePem   :: !(Maybe Text)
    , _cdCertificateARN   :: !(Maybe Text)
    , _cdCertificateId    :: !(Maybe Text)
    , _cdCreationDate     :: !(Maybe POSIX)
    , _cdTransferData     :: !(Maybe TransferData)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CertificateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdStatus'
--
-- * 'cdOwnedBy'
--
-- * 'cdLastModifiedDate'
--
-- * 'cdCaCertificateId'
--
-- * 'cdPreviousOwnedBy'
--
-- * 'cdCertificatePem'
--
-- * 'cdCertificateARN'
--
-- * 'cdCertificateId'
--
-- * 'cdCreationDate'
--
-- * 'cdTransferData'
certificateDescription
    :: CertificateDescription
certificateDescription =
    CertificateDescription'
    { _cdStatus = Nothing
    , _cdOwnedBy = Nothing
    , _cdLastModifiedDate = Nothing
    , _cdCaCertificateId = Nothing
    , _cdPreviousOwnedBy = Nothing
    , _cdCertificatePem = Nothing
    , _cdCertificateARN = Nothing
    , _cdCertificateId = Nothing
    , _cdCreationDate = Nothing
    , _cdTransferData = Nothing
    }

-- | The status of the certificate.
cdStatus :: Lens' CertificateDescription (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a});

-- | The ID of the AWS account that owns the certificate.
cdOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdOwnedBy = lens _cdOwnedBy (\ s a -> s{_cdOwnedBy = a});

-- | The date and time the certificate was last modified.
cdLastModifiedDate :: Lens' CertificateDescription (Maybe UTCTime)
cdLastModifiedDate = lens _cdLastModifiedDate (\ s a -> s{_cdLastModifiedDate = a}) . mapping _Time;

-- | The certificate ID of the CA certificate used to sign this certificate.
cdCaCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCaCertificateId = lens _cdCaCertificateId (\ s a -> s{_cdCaCertificateId = a});

-- | The ID of the AWS account of the previous owner of the certificate.
cdPreviousOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdPreviousOwnedBy = lens _cdPreviousOwnedBy (\ s a -> s{_cdPreviousOwnedBy = a});

-- | The certificate data, in PEM format.
cdCertificatePem :: Lens' CertificateDescription (Maybe Text)
cdCertificatePem = lens _cdCertificatePem (\ s a -> s{_cdCertificatePem = a});

-- | The ARN of the certificate.
cdCertificateARN :: Lens' CertificateDescription (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\ s a -> s{_cdCertificateARN = a});

-- | The ID of the certificate.
cdCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCertificateId = lens _cdCertificateId (\ s a -> s{_cdCertificateId = a});

-- | The date and time the certificate was created.
cdCreationDate :: Lens' CertificateDescription (Maybe UTCTime)
cdCreationDate = lens _cdCreationDate (\ s a -> s{_cdCreationDate = a}) . mapping _Time;

-- | The transfer data.
cdTransferData :: Lens' CertificateDescription (Maybe TransferData)
cdTransferData = lens _cdTransferData (\ s a -> s{_cdTransferData = a});

instance FromJSON CertificateDescription where
        parseJSON
          = withObject "CertificateDescription"
              (\ x ->
                 CertificateDescription' <$>
                   (x .:? "status") <*> (x .:? "ownedBy") <*>
                     (x .:? "lastModifiedDate")
                     <*> (x .:? "caCertificateId")
                     <*> (x .:? "previousOwnedBy")
                     <*> (x .:? "certificatePem")
                     <*> (x .:? "certificateArn")
                     <*> (x .:? "certificateId")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "transferData"))

instance Hashable CertificateDescription

instance NFData CertificateDescription

-- | Describes an action that updates a CloudWatch alarm.
--
-- /See:/ 'cloudwatchAlarmAction' smart constructor.
data CloudwatchAlarmAction = CloudwatchAlarmAction'
    { _caaRoleARN     :: !Text
    , _caaAlarmName   :: !Text
    , _caaStateReason :: !Text
    , _caaStateValue  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudwatchAlarmAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caaRoleARN'
--
-- * 'caaAlarmName'
--
-- * 'caaStateReason'
--
-- * 'caaStateValue'
cloudwatchAlarmAction
    :: Text -- ^ 'caaRoleARN'
    -> Text -- ^ 'caaAlarmName'
    -> Text -- ^ 'caaStateReason'
    -> Text -- ^ 'caaStateValue'
    -> CloudwatchAlarmAction
cloudwatchAlarmAction pRoleARN_ pAlarmName_ pStateReason_ pStateValue_ =
    CloudwatchAlarmAction'
    { _caaRoleARN = pRoleARN_
    , _caaAlarmName = pAlarmName_
    , _caaStateReason = pStateReason_
    , _caaStateValue = pStateValue_
    }

-- | The IAM role that allows access to the CloudWatch alarm.
caaRoleARN :: Lens' CloudwatchAlarmAction Text
caaRoleARN = lens _caaRoleARN (\ s a -> s{_caaRoleARN = a});

-- | The CloudWatch alarm name.
caaAlarmName :: Lens' CloudwatchAlarmAction Text
caaAlarmName = lens _caaAlarmName (\ s a -> s{_caaAlarmName = a});

-- | The reason for the alarm change.
caaStateReason :: Lens' CloudwatchAlarmAction Text
caaStateReason = lens _caaStateReason (\ s a -> s{_caaStateReason = a});

-- | The value of the alarm state. Acceptable values are: OK, ALARM,
-- INSUFFICIENT_DATA.
caaStateValue :: Lens' CloudwatchAlarmAction Text
caaStateValue = lens _caaStateValue (\ s a -> s{_caaStateValue = a});

instance FromJSON CloudwatchAlarmAction where
        parseJSON
          = withObject "CloudwatchAlarmAction"
              (\ x ->
                 CloudwatchAlarmAction' <$>
                   (x .: "roleArn") <*> (x .: "alarmName") <*>
                     (x .: "stateReason")
                     <*> (x .: "stateValue"))

instance Hashable CloudwatchAlarmAction

instance NFData CloudwatchAlarmAction

instance ToJSON CloudwatchAlarmAction where
        toJSON CloudwatchAlarmAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _caaRoleARN),
                  Just ("alarmName" .= _caaAlarmName),
                  Just ("stateReason" .= _caaStateReason),
                  Just ("stateValue" .= _caaStateValue)])

-- | Describes an action that captures a CloudWatch metric.
--
-- /See:/ 'cloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
    { _cmaMetricTimestamp :: !(Maybe Text)
    , _cmaRoleARN         :: !Text
    , _cmaMetricNamespace :: !Text
    , _cmaMetricName      :: !Text
    , _cmaMetricValue     :: !Text
    , _cmaMetricUnit      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudwatchMetricAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmaMetricTimestamp'
--
-- * 'cmaRoleARN'
--
-- * 'cmaMetricNamespace'
--
-- * 'cmaMetricName'
--
-- * 'cmaMetricValue'
--
-- * 'cmaMetricUnit'
cloudwatchMetricAction
    :: Text -- ^ 'cmaRoleARN'
    -> Text -- ^ 'cmaMetricNamespace'
    -> Text -- ^ 'cmaMetricName'
    -> Text -- ^ 'cmaMetricValue'
    -> Text -- ^ 'cmaMetricUnit'
    -> CloudwatchMetricAction
cloudwatchMetricAction pRoleARN_ pMetricNamespace_ pMetricName_ pMetricValue_ pMetricUnit_ =
    CloudwatchMetricAction'
    { _cmaMetricTimestamp = Nothing
    , _cmaRoleARN = pRoleARN_
    , _cmaMetricNamespace = pMetricNamespace_
    , _cmaMetricName = pMetricName_
    , _cmaMetricValue = pMetricValue_
    , _cmaMetricUnit = pMetricUnit_
    }

-- | An optional
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
cmaMetricTimestamp :: Lens' CloudwatchMetricAction (Maybe Text)
cmaMetricTimestamp = lens _cmaMetricTimestamp (\ s a -> s{_cmaMetricTimestamp = a});

-- | The IAM role that allows access to the CloudWatch metric.
cmaRoleARN :: Lens' CloudwatchMetricAction Text
cmaRoleARN = lens _cmaRoleARN (\ s a -> s{_cmaRoleARN = a});

-- | The CloudWatch metric namespace name.
cmaMetricNamespace :: Lens' CloudwatchMetricAction Text
cmaMetricNamespace = lens _cmaMetricNamespace (\ s a -> s{_cmaMetricNamespace = a});

-- | The CloudWatch metric name.
cmaMetricName :: Lens' CloudwatchMetricAction Text
cmaMetricName = lens _cmaMetricName (\ s a -> s{_cmaMetricName = a});

-- | The CloudWatch metric value.
cmaMetricValue :: Lens' CloudwatchMetricAction Text
cmaMetricValue = lens _cmaMetricValue (\ s a -> s{_cmaMetricValue = a});

-- | The
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
-- supported by CloudWatch.
cmaMetricUnit :: Lens' CloudwatchMetricAction Text
cmaMetricUnit = lens _cmaMetricUnit (\ s a -> s{_cmaMetricUnit = a});

instance FromJSON CloudwatchMetricAction where
        parseJSON
          = withObject "CloudwatchMetricAction"
              (\ x ->
                 CloudwatchMetricAction' <$>
                   (x .:? "metricTimestamp") <*> (x .: "roleArn") <*>
                     (x .: "metricNamespace")
                     <*> (x .: "metricName")
                     <*> (x .: "metricValue")
                     <*> (x .: "metricUnit"))

instance Hashable CloudwatchMetricAction

instance NFData CloudwatchMetricAction

instance ToJSON CloudwatchMetricAction where
        toJSON CloudwatchMetricAction'{..}
          = object
              (catMaybes
                 [("metricTimestamp" .=) <$> _cmaMetricTimestamp,
                  Just ("roleArn" .= _cmaRoleARN),
                  Just ("metricNamespace" .= _cmaMetricNamespace),
                  Just ("metricName" .= _cmaMetricName),
                  Just ("metricValue" .= _cmaMetricValue),
                  Just ("metricUnit" .= _cmaMetricUnit)])

-- | Describes an action to write to a DynamoDB table.
--
-- The 'tableName', 'hashKeyField', and 'rangeKeyField' values must match
-- the values used when you created the table.
--
-- The 'hashKeyValue' and 'rangeKeyvalue' fields use a substitution
-- template syntax. These templates provide data at runtime. The syntax is
-- as follows: ${/sql-expression/}.
--
-- You can specify any valid expression in a WHERE or SELECT clause,
-- including JSON properties, comparisons, calculations, and functions. For
-- example, the following field uses the third level of the topic:
--
-- '\"hashKeyValue\": \"${topic(3)}\"'
--
-- The following field uses the timestamp:
--
-- '\"rangeKeyValue\": \"${timestamp()}\"'
--
-- /See:/ 'dynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
    { _ddaPayloadField  :: !(Maybe Text)
    , _ddaTableName     :: !Text
    , _ddaRoleARN       :: !Text
    , _ddaHashKeyField  :: !Text
    , _ddaHashKeyValue  :: !Text
    , _ddaRangeKeyField :: !Text
    , _ddaRangeKeyValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamoDBAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddaPayloadField'
--
-- * 'ddaTableName'
--
-- * 'ddaRoleARN'
--
-- * 'ddaHashKeyField'
--
-- * 'ddaHashKeyValue'
--
-- * 'ddaRangeKeyField'
--
-- * 'ddaRangeKeyValue'
dynamoDBAction
    :: Text -- ^ 'ddaTableName'
    -> Text -- ^ 'ddaRoleARN'
    -> Text -- ^ 'ddaHashKeyField'
    -> Text -- ^ 'ddaHashKeyValue'
    -> Text -- ^ 'ddaRangeKeyField'
    -> Text -- ^ 'ddaRangeKeyValue'
    -> DynamoDBAction
dynamoDBAction pTableName_ pRoleARN_ pHashKeyField_ pHashKeyValue_ pRangeKeyField_ pRangeKeyValue_ =
    DynamoDBAction'
    { _ddaPayloadField = Nothing
    , _ddaTableName = pTableName_
    , _ddaRoleARN = pRoleARN_
    , _ddaHashKeyField = pHashKeyField_
    , _ddaHashKeyValue = pHashKeyValue_
    , _ddaRangeKeyField = pRangeKeyField_
    , _ddaRangeKeyValue = pRangeKeyValue_
    }

-- | The action payload. This name can be customized.
ddaPayloadField :: Lens' DynamoDBAction (Maybe Text)
ddaPayloadField = lens _ddaPayloadField (\ s a -> s{_ddaPayloadField = a});

-- | The name of the DynamoDB table.
ddaTableName :: Lens' DynamoDBAction Text
ddaTableName = lens _ddaTableName (\ s a -> s{_ddaTableName = a});

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddaRoleARN :: Lens' DynamoDBAction Text
ddaRoleARN = lens _ddaRoleARN (\ s a -> s{_ddaRoleARN = a});

-- | The hash key name.
ddaHashKeyField :: Lens' DynamoDBAction Text
ddaHashKeyField = lens _ddaHashKeyField (\ s a -> s{_ddaHashKeyField = a});

-- | The hash key value.
ddaHashKeyValue :: Lens' DynamoDBAction Text
ddaHashKeyValue = lens _ddaHashKeyValue (\ s a -> s{_ddaHashKeyValue = a});

-- | The range key name.
ddaRangeKeyField :: Lens' DynamoDBAction Text
ddaRangeKeyField = lens _ddaRangeKeyField (\ s a -> s{_ddaRangeKeyField = a});

-- | The range key value.
ddaRangeKeyValue :: Lens' DynamoDBAction Text
ddaRangeKeyValue = lens _ddaRangeKeyValue (\ s a -> s{_ddaRangeKeyValue = a});

instance FromJSON DynamoDBAction where
        parseJSON
          = withObject "DynamoDBAction"
              (\ x ->
                 DynamoDBAction' <$>
                   (x .:? "payloadField") <*> (x .: "tableName") <*>
                     (x .: "roleArn")
                     <*> (x .: "hashKeyField")
                     <*> (x .: "hashKeyValue")
                     <*> (x .: "rangeKeyField")
                     <*> (x .: "rangeKeyValue"))

instance Hashable DynamoDBAction

instance NFData DynamoDBAction

instance ToJSON DynamoDBAction where
        toJSON DynamoDBAction'{..}
          = object
              (catMaybes
                 [("payloadField" .=) <$> _ddaPayloadField,
                  Just ("tableName" .= _ddaTableName),
                  Just ("roleArn" .= _ddaRoleARN),
                  Just ("hashKeyField" .= _ddaHashKeyField),
                  Just ("hashKeyValue" .= _ddaHashKeyValue),
                  Just ("rangeKeyField" .= _ddaRangeKeyField),
                  Just ("rangeKeyValue" .= _ddaRangeKeyValue)])

-- | Describes an action that writes data to an Amazon Elasticsearch Service;
-- domain.
--
-- /See:/ 'elasticsearchAction' smart constructor.
data ElasticsearchAction = ElasticsearchAction'
    { _eaRoleARN  :: !Text
    , _eaEndpoint :: !Text
    , _eaIndex    :: !Text
    , _eaType     :: !Text
    , _eaId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaRoleARN'
--
-- * 'eaEndpoint'
--
-- * 'eaIndex'
--
-- * 'eaType'
--
-- * 'eaId'
elasticsearchAction
    :: Text -- ^ 'eaRoleARN'
    -> Text -- ^ 'eaEndpoint'
    -> Text -- ^ 'eaIndex'
    -> Text -- ^ 'eaType'
    -> Text -- ^ 'eaId'
    -> ElasticsearchAction
elasticsearchAction pRoleARN_ pEndpoint_ pIndex_ pType_ pId_ =
    ElasticsearchAction'
    { _eaRoleARN = pRoleARN_
    , _eaEndpoint = pEndpoint_
    , _eaIndex = pIndex_
    , _eaType = pType_
    , _eaId = pId_
    }

-- | The IAM role ARN that has access to Elasticsearch.
eaRoleARN :: Lens' ElasticsearchAction Text
eaRoleARN = lens _eaRoleARN (\ s a -> s{_eaRoleARN = a});

-- | The endpoint of your Elasticsearch domain.
eaEndpoint :: Lens' ElasticsearchAction Text
eaEndpoint = lens _eaEndpoint (\ s a -> s{_eaEndpoint = a});

-- | The Elasticsearch index where you want to store your data.
eaIndex :: Lens' ElasticsearchAction Text
eaIndex = lens _eaIndex (\ s a -> s{_eaIndex = a});

-- | The type of document you are storing.
eaType :: Lens' ElasticsearchAction Text
eaType = lens _eaType (\ s a -> s{_eaType = a});

-- | The unique identifier for the document you are storing.
eaId :: Lens' ElasticsearchAction Text
eaId = lens _eaId (\ s a -> s{_eaId = a});

instance FromJSON ElasticsearchAction where
        parseJSON
          = withObject "ElasticsearchAction"
              (\ x ->
                 ElasticsearchAction' <$>
                   (x .: "roleArn") <*> (x .: "endpoint") <*>
                     (x .: "index")
                     <*> (x .: "type")
                     <*> (x .: "id"))

instance Hashable ElasticsearchAction

instance NFData ElasticsearchAction

instance ToJSON ElasticsearchAction where
        toJSON ElasticsearchAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _eaRoleARN),
                  Just ("endpoint" .= _eaEndpoint),
                  Just ("index" .= _eaIndex), Just ("type" .= _eaType),
                  Just ("id" .= _eaId)])

-- | Describes an action that writes data to an Amazon Kinesis Firehose
-- stream.
--
-- /See:/ 'firehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
    { _faRoleARN            :: !Text
    , _faDeliveryStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FirehoseAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faRoleARN'
--
-- * 'faDeliveryStreamName'
firehoseAction
    :: Text -- ^ 'faRoleARN'
    -> Text -- ^ 'faDeliveryStreamName'
    -> FirehoseAction
firehoseAction pRoleARN_ pDeliveryStreamName_ =
    FirehoseAction'
    { _faRoleARN = pRoleARN_
    , _faDeliveryStreamName = pDeliveryStreamName_
    }

-- | The IAM role that grants access to the Amazon Kinesis Firehost stream.
faRoleARN :: Lens' FirehoseAction Text
faRoleARN = lens _faRoleARN (\ s a -> s{_faRoleARN = a});

-- | The delivery stream name.
faDeliveryStreamName :: Lens' FirehoseAction Text
faDeliveryStreamName = lens _faDeliveryStreamName (\ s a -> s{_faDeliveryStreamName = a});

instance FromJSON FirehoseAction where
        parseJSON
          = withObject "FirehoseAction"
              (\ x ->
                 FirehoseAction' <$>
                   (x .: "roleArn") <*> (x .: "deliveryStreamName"))

instance Hashable FirehoseAction

instance NFData FirehoseAction

instance ToJSON FirehoseAction where
        toJSON FirehoseAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _faRoleARN),
                  Just
                    ("deliveryStreamName" .= _faDeliveryStreamName)])

-- | Describes a key pair.
--
-- /See:/ 'keyPair' smart constructor.
data KeyPair = KeyPair'
    { _kpPrivateKey :: !(Maybe (Sensitive Text))
    , _kpPublicKey  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpPrivateKey'
--
-- * 'kpPublicKey'
keyPair
    :: KeyPair
keyPair =
    KeyPair'
    { _kpPrivateKey = Nothing
    , _kpPublicKey = Nothing
    }

-- | The private key.
kpPrivateKey :: Lens' KeyPair (Maybe Text)
kpPrivateKey = lens _kpPrivateKey (\ s a -> s{_kpPrivateKey = a}) . mapping _Sensitive;

-- | The public key.
kpPublicKey :: Lens' KeyPair (Maybe Text)
kpPublicKey = lens _kpPublicKey (\ s a -> s{_kpPublicKey = a});

instance FromJSON KeyPair where
        parseJSON
          = withObject "KeyPair"
              (\ x ->
                 KeyPair' <$>
                   (x .:? "PrivateKey") <*> (x .:? "PublicKey"))

instance Hashable KeyPair

instance NFData KeyPair

-- | Describes an action to write data to an Amazon Kinesis stream.
--
-- /See:/ 'kinesisAction' smart constructor.
data KinesisAction = KinesisAction'
    { _kaPartitionKey :: !(Maybe Text)
    , _kaRoleARN      :: !Text
    , _kaStreamName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KinesisAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kaPartitionKey'
--
-- * 'kaRoleARN'
--
-- * 'kaStreamName'
kinesisAction
    :: Text -- ^ 'kaRoleARN'
    -> Text -- ^ 'kaStreamName'
    -> KinesisAction
kinesisAction pRoleARN_ pStreamName_ =
    KinesisAction'
    { _kaPartitionKey = Nothing
    , _kaRoleARN = pRoleARN_
    , _kaStreamName = pStreamName_
    }

-- | The partition key.
kaPartitionKey :: Lens' KinesisAction (Maybe Text)
kaPartitionKey = lens _kaPartitionKey (\ s a -> s{_kaPartitionKey = a});

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
kaRoleARN :: Lens' KinesisAction Text
kaRoleARN = lens _kaRoleARN (\ s a -> s{_kaRoleARN = a});

-- | The name of the Amazon Kinesis stream.
kaStreamName :: Lens' KinesisAction Text
kaStreamName = lens _kaStreamName (\ s a -> s{_kaStreamName = a});

instance FromJSON KinesisAction where
        parseJSON
          = withObject "KinesisAction"
              (\ x ->
                 KinesisAction' <$>
                   (x .:? "partitionKey") <*> (x .: "roleArn") <*>
                     (x .: "streamName"))

instance Hashable KinesisAction

instance NFData KinesisAction

instance ToJSON KinesisAction where
        toJSON KinesisAction'{..}
          = object
              (catMaybes
                 [("partitionKey" .=) <$> _kaPartitionKey,
                  Just ("roleArn" .= _kaRoleARN),
                  Just ("streamName" .= _kaStreamName)])

-- | Describes an action to invoke a Lambda function.
--
-- /See:/ 'lambdaAction' smart constructor.
newtype LambdaAction = LambdaAction'
    { _laFunctionARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laFunctionARN'
lambdaAction
    :: Text -- ^ 'laFunctionARN'
    -> LambdaAction
lambdaAction pFunctionARN_ =
    LambdaAction'
    { _laFunctionARN = pFunctionARN_
    }

-- | The ARN of the Lambda function.
laFunctionARN :: Lens' LambdaAction Text
laFunctionARN = lens _laFunctionARN (\ s a -> s{_laFunctionARN = a});

instance FromJSON LambdaAction where
        parseJSON
          = withObject "LambdaAction"
              (\ x -> LambdaAction' <$> (x .: "functionArn"))

instance Hashable LambdaAction

instance NFData LambdaAction

instance ToJSON LambdaAction where
        toJSON LambdaAction'{..}
          = object
              (catMaybes [Just ("functionArn" .= _laFunctionARN)])

-- | Describes the logging options payload.
--
-- /See:/ 'loggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
    { _lopLogLevel :: !(Maybe LogLevel)
    , _lopRoleARN  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoggingOptionsPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopLogLevel'
--
-- * 'lopRoleARN'
loggingOptionsPayload
    :: Text -- ^ 'lopRoleARN'
    -> LoggingOptionsPayload
loggingOptionsPayload pRoleARN_ =
    LoggingOptionsPayload'
    { _lopLogLevel = Nothing
    , _lopRoleARN = pRoleARN_
    }

-- | The logging level.
lopLogLevel :: Lens' LoggingOptionsPayload (Maybe LogLevel)
lopLogLevel = lens _lopLogLevel (\ s a -> s{_lopLogLevel = a});

-- | The ARN of the IAM role that grants access.
lopRoleARN :: Lens' LoggingOptionsPayload Text
lopRoleARN = lens _lopRoleARN (\ s a -> s{_lopRoleARN = a});

instance Hashable LoggingOptionsPayload

instance NFData LoggingOptionsPayload

instance ToJSON LoggingOptionsPayload where
        toJSON LoggingOptionsPayload'{..}
          = object
              (catMaybes
                 [("logLevel" .=) <$> _lopLogLevel,
                  Just ("roleArn" .= _lopRoleARN)])

-- | Describes an AWS IoT policy.
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
    { _pPolicyName :: !(Maybe Text)
    , _pPolicyARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyName'
--
-- * 'pPolicyARN'
policy
    :: Policy
policy =
    Policy'
    { _pPolicyName = Nothing
    , _pPolicyARN = Nothing
    }

-- | The policy name.
pPolicyName :: Lens' Policy (Maybe Text)
pPolicyName = lens _pPolicyName (\ s a -> s{_pPolicyName = a});

-- | The policy ARN.
pPolicyARN :: Lens' Policy (Maybe Text)
pPolicyARN = lens _pPolicyARN (\ s a -> s{_pPolicyARN = a});

instance FromJSON Policy where
        parseJSON
          = withObject "Policy"
              (\ x ->
                 Policy' <$>
                   (x .:? "policyName") <*> (x .:? "policyArn"))

instance Hashable Policy

instance NFData Policy

-- | Describes a policy version.
--
-- /See:/ 'policyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
    { _pvVersionId        :: !(Maybe Text)
    , _pvCreateDate       :: !(Maybe POSIX)
    , _pvIsDefaultVersion :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvVersionId'
--
-- * 'pvCreateDate'
--
-- * 'pvIsDefaultVersion'
policyVersion
    :: PolicyVersion
policyVersion =
    PolicyVersion'
    { _pvVersionId = Nothing
    , _pvCreateDate = Nothing
    , _pvIsDefaultVersion = Nothing
    }

-- | The policy version ID.
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\ s a -> s{_pvVersionId = a});

-- | The date and time the policy was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\ s a -> s{_pvCreateDate = a}) . mapping _Time;

-- | Specifies whether the policy version is the default.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\ s a -> s{_pvIsDefaultVersion = a});

instance FromJSON PolicyVersion where
        parseJSON
          = withObject "PolicyVersion"
              (\ x ->
                 PolicyVersion' <$>
                   (x .:? "versionId") <*> (x .:? "createDate") <*>
                     (x .:? "isDefaultVersion"))

instance Hashable PolicyVersion

instance NFData PolicyVersion

-- | Describes an action to republish to another topic.
--
-- /See:/ 'republishAction' smart constructor.
data RepublishAction = RepublishAction'
    { _raRoleARN :: !Text
    , _raTopic   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RepublishAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raRoleARN'
--
-- * 'raTopic'
republishAction
    :: Text -- ^ 'raRoleARN'
    -> Text -- ^ 'raTopic'
    -> RepublishAction
republishAction pRoleARN_ pTopic_ =
    RepublishAction'
    { _raRoleARN = pRoleARN_
    , _raTopic = pTopic_
    }

-- | The ARN of the IAM role that grants access.
raRoleARN :: Lens' RepublishAction Text
raRoleARN = lens _raRoleARN (\ s a -> s{_raRoleARN = a});

-- | The name of the MQTT topic.
raTopic :: Lens' RepublishAction Text
raTopic = lens _raTopic (\ s a -> s{_raTopic = a});

instance FromJSON RepublishAction where
        parseJSON
          = withObject "RepublishAction"
              (\ x ->
                 RepublishAction' <$>
                   (x .: "roleArn") <*> (x .: "topic"))

instance Hashable RepublishAction

instance NFData RepublishAction

instance ToJSON RepublishAction where
        toJSON RepublishAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _raRoleARN),
                  Just ("topic" .= _raTopic)])

-- | Describes an action to write data to an Amazon S3 bucket.
--
-- /See:/ 's3Action' smart constructor.
data S3Action = S3Action'
    { _sRoleARN    :: !Text
    , _sBucketName :: !Text
    , _sKey        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sRoleARN'
--
-- * 'sBucketName'
--
-- * 'sKey'
s3Action
    :: Text -- ^ 'sRoleARN'
    -> Text -- ^ 'sBucketName'
    -> Text -- ^ 'sKey'
    -> S3Action
s3Action pRoleARN_ pBucketName_ pKey_ =
    S3Action'
    { _sRoleARN = pRoleARN_
    , _sBucketName = pBucketName_
    , _sKey = pKey_
    }

-- | The ARN of the IAM role that grants access.
sRoleARN :: Lens' S3Action Text
sRoleARN = lens _sRoleARN (\ s a -> s{_sRoleARN = a});

-- | The Amazon S3 bucket.
sBucketName :: Lens' S3Action Text
sBucketName = lens _sBucketName (\ s a -> s{_sBucketName = a});

-- | The object key.
sKey :: Lens' S3Action Text
sKey = lens _sKey (\ s a -> s{_sKey = a});

instance FromJSON S3Action where
        parseJSON
          = withObject "S3Action"
              (\ x ->
                 S3Action' <$>
                   (x .: "roleArn") <*> (x .: "bucketName") <*>
                     (x .: "key"))

instance Hashable S3Action

instance NFData S3Action

instance ToJSON S3Action where
        toJSON S3Action'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _sRoleARN),
                  Just ("bucketName" .= _sBucketName),
                  Just ("key" .= _sKey)])

-- | Describes an action to publish to an Amazon SNS topic.
--
-- /See:/ 'snsAction' smart constructor.
data SNSAction = SNSAction'
    { _snsaMessageFormat :: !(Maybe MessageFormat)
    , _snsaTargetARN     :: !Text
    , _snsaRoleARN       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SNSAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snsaMessageFormat'
--
-- * 'snsaTargetARN'
--
-- * 'snsaRoleARN'
snsAction
    :: Text -- ^ 'snsaTargetARN'
    -> Text -- ^ 'snsaRoleARN'
    -> SNSAction
snsAction pTargetARN_ pRoleARN_ =
    SNSAction'
    { _snsaMessageFormat = Nothing
    , _snsaTargetARN = pTargetARN_
    , _snsaRoleARN = pRoleARN_
    }

-- | The message format of the message to publish. Optional. Accepted values
-- are \"JSON\" and \"RAW\". The default value of the attribute is \"RAW\".
-- SNS uses this setting to determine if the payload should be parsed and
-- relevant platform-specific bits of the payload should be extracted. To
-- read more about SNS message formats, see
-- <http://docs.aws.amazon.com/sns/latest/dg/json-formats.html > refer to
-- their official documentation.
snsaMessageFormat :: Lens' SNSAction (Maybe MessageFormat)
snsaMessageFormat = lens _snsaMessageFormat (\ s a -> s{_snsaMessageFormat = a});

-- | The ARN of the SNS topic.
snsaTargetARN :: Lens' SNSAction Text
snsaTargetARN = lens _snsaTargetARN (\ s a -> s{_snsaTargetARN = a});

-- | The ARN of the IAM role that grants access.
snsaRoleARN :: Lens' SNSAction Text
snsaRoleARN = lens _snsaRoleARN (\ s a -> s{_snsaRoleARN = a});

instance FromJSON SNSAction where
        parseJSON
          = withObject "SNSAction"
              (\ x ->
                 SNSAction' <$>
                   (x .:? "messageFormat") <*> (x .: "targetArn") <*>
                     (x .: "roleArn"))

instance Hashable SNSAction

instance NFData SNSAction

instance ToJSON SNSAction where
        toJSON SNSAction'{..}
          = object
              (catMaybes
                 [("messageFormat" .=) <$> _snsaMessageFormat,
                  Just ("targetArn" .= _snsaTargetARN),
                  Just ("roleArn" .= _snsaRoleARN)])

-- | Describes an action to publish data to an Amazon SQS queue.
--
-- /See:/ 'sqsAction' smart constructor.
data SqsAction = SqsAction'
    { _saUseBase64 :: !(Maybe Bool)
    , _saRoleARN   :: !Text
    , _saQueueURL  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SqsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saUseBase64'
--
-- * 'saRoleARN'
--
-- * 'saQueueURL'
sqsAction
    :: Text -- ^ 'saRoleARN'
    -> Text -- ^ 'saQueueURL'
    -> SqsAction
sqsAction pRoleARN_ pQueueURL_ =
    SqsAction'
    { _saUseBase64 = Nothing
    , _saRoleARN = pRoleARN_
    , _saQueueURL = pQueueURL_
    }

-- | Specifies whether to use Base64 encoding.
saUseBase64 :: Lens' SqsAction (Maybe Bool)
saUseBase64 = lens _saUseBase64 (\ s a -> s{_saUseBase64 = a});

-- | The ARN of the IAM role that grants access.
saRoleARN :: Lens' SqsAction Text
saRoleARN = lens _saRoleARN (\ s a -> s{_saRoleARN = a});

-- | The URL of the Amazon SQS queue.
saQueueURL :: Lens' SqsAction Text
saQueueURL = lens _saQueueURL (\ s a -> s{_saQueueURL = a});

instance FromJSON SqsAction where
        parseJSON
          = withObject "SqsAction"
              (\ x ->
                 SqsAction' <$>
                   (x .:? "useBase64") <*> (x .: "roleArn") <*>
                     (x .: "queueUrl"))

instance Hashable SqsAction

instance NFData SqsAction

instance ToJSON SqsAction where
        toJSON SqsAction'{..}
          = object
              (catMaybes
                 [("useBase64" .=) <$> _saUseBase64,
                  Just ("roleArn" .= _saRoleARN),
                  Just ("queueUrl" .= _saQueueURL)])

-- | Describes a thing attribute.
--
-- /See:/ 'thingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
    { _taAttributes :: !(Maybe (Map Text Text))
    , _taThingName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThingAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taAttributes'
--
-- * 'taThingName'
thingAttribute
    :: ThingAttribute
thingAttribute =
    ThingAttribute'
    { _taAttributes = Nothing
    , _taThingName = Nothing
    }

-- | The attributes.
taAttributes :: Lens' ThingAttribute (HashMap Text Text)
taAttributes = lens _taAttributes (\ s a -> s{_taAttributes = a}) . _Default . _Map;

-- | The name of the thing.
taThingName :: Lens' ThingAttribute (Maybe Text)
taThingName = lens _taThingName (\ s a -> s{_taThingName = a});

instance FromJSON ThingAttribute where
        parseJSON
          = withObject "ThingAttribute"
              (\ x ->
                 ThingAttribute' <$>
                   (x .:? "attributes" .!= mempty) <*>
                     (x .:? "thingName"))

instance Hashable ThingAttribute

instance NFData ThingAttribute

-- | Describes a rule.
--
-- /See:/ 'topicRule' smart constructor.
data TopicRule = TopicRule'
    { _trCreatedAt        :: !(Maybe POSIX)
    , _trActions          :: !(Maybe [Action])
    , _trAwsIotSqlVersion :: !(Maybe Text)
    , _trRuleDisabled     :: !(Maybe Bool)
    , _trRuleName         :: !(Maybe Text)
    , _trSql              :: !(Maybe Text)
    , _trDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trCreatedAt'
--
-- * 'trActions'
--
-- * 'trAwsIotSqlVersion'
--
-- * 'trRuleDisabled'
--
-- * 'trRuleName'
--
-- * 'trSql'
--
-- * 'trDescription'
topicRule
    :: TopicRule
topicRule =
    TopicRule'
    { _trCreatedAt = Nothing
    , _trActions = Nothing
    , _trAwsIotSqlVersion = Nothing
    , _trRuleDisabled = Nothing
    , _trRuleName = Nothing
    , _trSql = Nothing
    , _trDescription = Nothing
    }

-- | The date and time the rule was created.
trCreatedAt :: Lens' TopicRule (Maybe UTCTime)
trCreatedAt = lens _trCreatedAt (\ s a -> s{_trCreatedAt = a}) . mapping _Time;

-- | The actions associated with the rule.
trActions :: Lens' TopicRule [Action]
trActions = lens _trActions (\ s a -> s{_trActions = a}) . _Default . _Coerce;

-- | The version of the SQL rules engine to use when evaluating the rule.
trAwsIotSqlVersion :: Lens' TopicRule (Maybe Text)
trAwsIotSqlVersion = lens _trAwsIotSqlVersion (\ s a -> s{_trAwsIotSqlVersion = a});

-- | Specifies whether the rule is disabled.
trRuleDisabled :: Lens' TopicRule (Maybe Bool)
trRuleDisabled = lens _trRuleDisabled (\ s a -> s{_trRuleDisabled = a});

-- | The name of the rule.
trRuleName :: Lens' TopicRule (Maybe Text)
trRuleName = lens _trRuleName (\ s a -> s{_trRuleName = a});

-- | The SQL statement used to query the topic. When using a SQL query with
-- multiple lines, be sure to escape the newline characters.
trSql :: Lens' TopicRule (Maybe Text)
trSql = lens _trSql (\ s a -> s{_trSql = a});

-- | The description of the rule.
trDescription :: Lens' TopicRule (Maybe Text)
trDescription = lens _trDescription (\ s a -> s{_trDescription = a});

instance FromJSON TopicRule where
        parseJSON
          = withObject "TopicRule"
              (\ x ->
                 TopicRule' <$>
                   (x .:? "createdAt") <*> (x .:? "actions" .!= mempty)
                     <*> (x .:? "awsIotSqlVersion")
                     <*> (x .:? "ruleDisabled")
                     <*> (x .:? "ruleName")
                     <*> (x .:? "sql")
                     <*> (x .:? "description"))

instance Hashable TopicRule

instance NFData TopicRule

-- | Describes a rule.
--
-- /See:/ 'topicRuleListItem' smart constructor.
data TopicRuleListItem = TopicRuleListItem'
    { _trliCreatedAt    :: !(Maybe POSIX)
    , _trliRuleDisabled :: !(Maybe Bool)
    , _trliRuleName     :: !(Maybe Text)
    , _trliRuleARN      :: !(Maybe Text)
    , _trliTopicPattern :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TopicRuleListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trliCreatedAt'
--
-- * 'trliRuleDisabled'
--
-- * 'trliRuleName'
--
-- * 'trliRuleARN'
--
-- * 'trliTopicPattern'
topicRuleListItem
    :: TopicRuleListItem
topicRuleListItem =
    TopicRuleListItem'
    { _trliCreatedAt = Nothing
    , _trliRuleDisabled = Nothing
    , _trliRuleName = Nothing
    , _trliRuleARN = Nothing
    , _trliTopicPattern = Nothing
    }

-- | The date and time the rule was created.
trliCreatedAt :: Lens' TopicRuleListItem (Maybe UTCTime)
trliCreatedAt = lens _trliCreatedAt (\ s a -> s{_trliCreatedAt = a}) . mapping _Time;

-- | Specifies whether the rule is disabled.
trliRuleDisabled :: Lens' TopicRuleListItem (Maybe Bool)
trliRuleDisabled = lens _trliRuleDisabled (\ s a -> s{_trliRuleDisabled = a});

-- | The name of the rule.
trliRuleName :: Lens' TopicRuleListItem (Maybe Text)
trliRuleName = lens _trliRuleName (\ s a -> s{_trliRuleName = a});

-- | The rule ARN.
trliRuleARN :: Lens' TopicRuleListItem (Maybe Text)
trliRuleARN = lens _trliRuleARN (\ s a -> s{_trliRuleARN = a});

-- | The pattern for the topic names that apply.
trliTopicPattern :: Lens' TopicRuleListItem (Maybe Text)
trliTopicPattern = lens _trliTopicPattern (\ s a -> s{_trliTopicPattern = a});

instance FromJSON TopicRuleListItem where
        parseJSON
          = withObject "TopicRuleListItem"
              (\ x ->
                 TopicRuleListItem' <$>
                   (x .:? "createdAt") <*> (x .:? "ruleDisabled") <*>
                     (x .:? "ruleName")
                     <*> (x .:? "ruleArn")
                     <*> (x .:? "topicPattern"))

instance Hashable TopicRuleListItem

instance NFData TopicRuleListItem

-- | Describes a rule.
--
-- /See:/ 'topicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
    { _trpAwsIotSqlVersion :: !(Maybe Text)
    , _trpRuleDisabled     :: !(Maybe Bool)
    , _trpDescription      :: !(Maybe Text)
    , _trpSql              :: !Text
    , _trpActions          :: ![Action]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TopicRulePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpAwsIotSqlVersion'
--
-- * 'trpRuleDisabled'
--
-- * 'trpDescription'
--
-- * 'trpSql'
--
-- * 'trpActions'
topicRulePayload
    :: Text -- ^ 'trpSql'
    -> TopicRulePayload
topicRulePayload pSql_ =
    TopicRulePayload'
    { _trpAwsIotSqlVersion = Nothing
    , _trpRuleDisabled = Nothing
    , _trpDescription = Nothing
    , _trpSql = pSql_
    , _trpActions = mempty
    }

-- | The version of the SQL rules engine to use when evaluating the rule.
trpAwsIotSqlVersion :: Lens' TopicRulePayload (Maybe Text)
trpAwsIotSqlVersion = lens _trpAwsIotSqlVersion (\ s a -> s{_trpAwsIotSqlVersion = a});

-- | Specifies whether the rule is disabled.
trpRuleDisabled :: Lens' TopicRulePayload (Maybe Bool)
trpRuleDisabled = lens _trpRuleDisabled (\ s a -> s{_trpRuleDisabled = a});

-- | The description of the rule.
trpDescription :: Lens' TopicRulePayload (Maybe Text)
trpDescription = lens _trpDescription (\ s a -> s{_trpDescription = a});

-- | The SQL statement used to query the topic. For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/iot-rules.html#aws-iot-sql-reference AWS IoT SQL Reference>
-- in the /AWS IoT Developer Guide/.
trpSql :: Lens' TopicRulePayload Text
trpSql = lens _trpSql (\ s a -> s{_trpSql = a});

-- | The actions associated with the rule.
trpActions :: Lens' TopicRulePayload [Action]
trpActions = lens _trpActions (\ s a -> s{_trpActions = a}) . _Coerce;

instance Hashable TopicRulePayload

instance NFData TopicRulePayload

instance ToJSON TopicRulePayload where
        toJSON TopicRulePayload'{..}
          = object
              (catMaybes
                 [("awsIotSqlVersion" .=) <$> _trpAwsIotSqlVersion,
                  ("ruleDisabled" .=) <$> _trpRuleDisabled,
                  ("description" .=) <$> _trpDescription,
                  Just ("sql" .= _trpSql),
                  Just ("actions" .= _trpActions)])

-- | Data used to transfer a certificate to an AWS account.
--
-- /See:/ 'transferData' smart constructor.
data TransferData = TransferData'
    { _tdTransferDate    :: !(Maybe POSIX)
    , _tdAcceptDate      :: !(Maybe POSIX)
    , _tdTransferMessage :: !(Maybe Text)
    , _tdRejectDate      :: !(Maybe POSIX)
    , _tdRejectReason    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TransferData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdTransferDate'
--
-- * 'tdAcceptDate'
--
-- * 'tdTransferMessage'
--
-- * 'tdRejectDate'
--
-- * 'tdRejectReason'
transferData
    :: TransferData
transferData =
    TransferData'
    { _tdTransferDate = Nothing
    , _tdAcceptDate = Nothing
    , _tdTransferMessage = Nothing
    , _tdRejectDate = Nothing
    , _tdRejectReason = Nothing
    }

-- | The date the transfer took place.
tdTransferDate :: Lens' TransferData (Maybe UTCTime)
tdTransferDate = lens _tdTransferDate (\ s a -> s{_tdTransferDate = a}) . mapping _Time;

-- | The date the transfer was accepted.
tdAcceptDate :: Lens' TransferData (Maybe UTCTime)
tdAcceptDate = lens _tdAcceptDate (\ s a -> s{_tdAcceptDate = a}) . mapping _Time;

-- | The transfer message.
tdTransferMessage :: Lens' TransferData (Maybe Text)
tdTransferMessage = lens _tdTransferMessage (\ s a -> s{_tdTransferMessage = a});

-- | The date the transfer was rejected.
tdRejectDate :: Lens' TransferData (Maybe UTCTime)
tdRejectDate = lens _tdRejectDate (\ s a -> s{_tdRejectDate = a}) . mapping _Time;

-- | The reason why the transfer was rejected.
tdRejectReason :: Lens' TransferData (Maybe Text)
tdRejectReason = lens _tdRejectReason (\ s a -> s{_tdRejectReason = a});

instance FromJSON TransferData where
        parseJSON
          = withObject "TransferData"
              (\ x ->
                 TransferData' <$>
                   (x .:? "transferDate") <*> (x .:? "acceptDate") <*>
                     (x .:? "transferMessage")
                     <*> (x .:? "rejectDate")
                     <*> (x .:? "rejectReason"))

instance Hashable TransferData

instance NFData TransferData
