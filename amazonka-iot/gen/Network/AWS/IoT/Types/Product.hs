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
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
    { _aCloudwatchMetric :: !(Maybe CloudwatchMetricAction)
    , _aDynamoDBv2       :: !(Maybe DynamoDBv2Action)
    , _aCloudwatchAlarm  :: !(Maybe CloudwatchAlarmAction)
    , _aSns              :: !(Maybe SNSAction)
    , _aDynamoDB         :: !(Maybe DynamoDBAction)
    , _aFirehose         :: !(Maybe FirehoseAction)
    , _aLambda           :: !(Maybe LambdaAction)
    , _aSalesforce       :: !(Maybe SalesforceAction)
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
-- * 'aCloudwatchMetric' - Capture a CloudWatch metric.
--
-- * 'aDynamoDBv2' - Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
--
-- * 'aCloudwatchAlarm' - Change the state of a CloudWatch alarm.
--
-- * 'aSns' - Publish to an Amazon SNS topic.
--
-- * 'aDynamoDB' - Write to a DynamoDB table.
--
-- * 'aFirehose' - Write to an Amazon Kinesis Firehose stream.
--
-- * 'aLambda' - Invoke a Lambda function.
--
-- * 'aSalesforce' - Send a message to a Salesforce IoT Cloud Input Stream.
--
-- * 'aKinesis' - Write data to an Amazon Kinesis stream.
--
-- * 'aS3' - Write to an Amazon S3 bucket.
--
-- * 'aElasticsearch' - Write data to an Amazon Elasticsearch Service domain.
--
-- * 'aRepublish' - Publish to another MQTT topic.
--
-- * 'aSqs' - Publish to an Amazon SQS queue.
action
    :: Action
action =
    Action'
    { _aCloudwatchMetric = Nothing
    , _aDynamoDBv2 = Nothing
    , _aCloudwatchAlarm = Nothing
    , _aSns = Nothing
    , _aDynamoDB = Nothing
    , _aFirehose = Nothing
    , _aLambda = Nothing
    , _aSalesforce = Nothing
    , _aKinesis = Nothing
    , _aS3 = Nothing
    , _aElasticsearch = Nothing
    , _aRepublish = Nothing
    , _aSqs = Nothing
    }

-- | Capture a CloudWatch metric.
aCloudwatchMetric :: Lens' Action (Maybe CloudwatchMetricAction)
aCloudwatchMetric = lens _aCloudwatchMetric (\ s a -> s{_aCloudwatchMetric = a});

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
aDynamoDBv2 :: Lens' Action (Maybe DynamoDBv2Action)
aDynamoDBv2 = lens _aDynamoDBv2 (\ s a -> s{_aDynamoDBv2 = a});

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

-- | Send a message to a Salesforce IoT Cloud Input Stream.
aSalesforce :: Lens' Action (Maybe SalesforceAction)
aSalesforce = lens _aSalesforce (\ s a -> s{_aSalesforce = a});

-- | Write data to an Amazon Kinesis stream.
aKinesis :: Lens' Action (Maybe KinesisAction)
aKinesis = lens _aKinesis (\ s a -> s{_aKinesis = a});

-- | Write to an Amazon S3 bucket.
aS3 :: Lens' Action (Maybe S3Action)
aS3 = lens _aS3 (\ s a -> s{_aS3 = a});

-- | Write data to an Amazon Elasticsearch Service domain.
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
                   (x .:? "cloudwatchMetric") <*> (x .:? "dynamoDBv2")
                     <*> (x .:? "cloudwatchAlarm")
                     <*> (x .:? "sns")
                     <*> (x .:? "dynamoDB")
                     <*> (x .:? "firehose")
                     <*> (x .:? "lambda")
                     <*> (x .:? "salesforce")
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
                  ("dynamoDBv2" .=) <$> _aDynamoDBv2,
                  ("cloudwatchAlarm" .=) <$> _aCloudwatchAlarm,
                  ("sns" .=) <$> _aSns, ("dynamoDB" .=) <$> _aDynamoDB,
                  ("firehose" .=) <$> _aFirehose,
                  ("lambda" .=) <$> _aLambda,
                  ("salesforce" .=) <$> _aSalesforce,
                  ("kinesis" .=) <$> _aKinesis, ("s3" .=) <$> _aS3,
                  ("elasticsearch" .=) <$> _aElasticsearch,
                  ("republish" .=) <$> _aRepublish,
                  ("sqs" .=) <$> _aSqs])

-- | The attribute payload.
--
--
--
-- /See:/ 'attributePayload' smart constructor.
data AttributePayload = AttributePayload'
    { _apAttributes :: !(Maybe (Map Text Text))
    , _apMerge      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttributePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAttributes' - A JSON string containing up to three key-value pair in JSON format. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
--
-- * 'apMerge' - Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them. To remove an attribute, call @UpdateThing@ with an empty attribute value.
attributePayload
    :: AttributePayload
attributePayload =
    AttributePayload'
    { _apAttributes = Nothing
    , _apMerge = Nothing
    }

-- | A JSON string containing up to three key-value pair in JSON format. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
apAttributes :: Lens' AttributePayload (HashMap Text Text)
apAttributes = lens _apAttributes (\ s a -> s{_apAttributes = a}) . _Default . _Map;

-- | Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them. To remove an attribute, call @UpdateThing@ with an empty attribute value.
apMerge :: Lens' AttributePayload (Maybe Bool)
apMerge = lens _apMerge (\ s a -> s{_apMerge = a});

instance Hashable AttributePayload

instance NFData AttributePayload

instance ToJSON AttributePayload where
        toJSON AttributePayload'{..}
          = object
              (catMaybes
                 [("attributes" .=) <$> _apAttributes,
                  ("merge" .=) <$> _apMerge])

-- | A CA certificate.
--
--
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
-- * 'cacStatus' - The status of the CA certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'cacCertificateARN' - The ARN of the CA certificate.
--
-- * 'cacCertificateId' - The ID of the CA certificate.
--
-- * 'cacCreationDate' - The date the CA certificate was created.
cACertificate
    :: CACertificate
cACertificate =
    CACertificate'
    { _cacStatus = Nothing
    , _cacCertificateARN = Nothing
    , _cacCertificateId = Nothing
    , _cacCreationDate = Nothing
    }

-- | The status of the CA certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
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
--
--
-- /See:/ 'cACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
    { _cacdStatus                 :: !(Maybe CACertificateStatus)
    , _cacdOwnedBy                :: !(Maybe Text)
    , _cacdCertificatePem         :: !(Maybe Text)
    , _cacdCertificateARN         :: !(Maybe Text)
    , _cacdCertificateId          :: !(Maybe Text)
    , _cacdAutoRegistrationStatus :: !(Maybe AutoRegistrationStatus)
    , _cacdCreationDate           :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CACertificateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacdStatus' - The status of a CA certificate.
--
-- * 'cacdOwnedBy' - The owner of the CA certificate.
--
-- * 'cacdCertificatePem' - The CA certificate data, in PEM format.
--
-- * 'cacdCertificateARN' - The CA certificate ARN.
--
-- * 'cacdCertificateId' - The CA certificate ID.
--
-- * 'cacdAutoRegistrationStatus' - Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
--
-- * 'cacdCreationDate' - The date the CA certificate was created.
cACertificateDescription
    :: CACertificateDescription
cACertificateDescription =
    CACertificateDescription'
    { _cacdStatus = Nothing
    , _cacdOwnedBy = Nothing
    , _cacdCertificatePem = Nothing
    , _cacdCertificateARN = Nothing
    , _cacdCertificateId = Nothing
    , _cacdAutoRegistrationStatus = Nothing
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

-- | Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
cacdAutoRegistrationStatus :: Lens' CACertificateDescription (Maybe AutoRegistrationStatus)
cacdAutoRegistrationStatus = lens _cacdAutoRegistrationStatus (\ s a -> s{_cacdAutoRegistrationStatus = a});

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
                     <*> (x .:? "autoRegistrationStatus")
                     <*> (x .:? "creationDate"))

instance Hashable CACertificateDescription

instance NFData CACertificateDescription

-- | Information about a certificate.
--
--
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
-- * 'cStatus' - The status of the certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'cCertificateARN' - The ARN of the certificate.
--
-- * 'cCertificateId' - The ID of the certificate.
--
-- * 'cCreationDate' - The date and time the certificate was created.
certificate
    :: Certificate
certificate =
    Certificate'
    { _cStatus = Nothing
    , _cCertificateARN = Nothing
    , _cCertificateId = Nothing
    , _cCreationDate = Nothing
    }

-- | The status of the certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
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
--
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
-- * 'cdStatus' - The status of the certificate.
--
-- * 'cdOwnedBy' - The ID of the AWS account that owns the certificate.
--
-- * 'cdLastModifiedDate' - The date and time the certificate was last modified.
--
-- * 'cdCaCertificateId' - The certificate ID of the CA certificate used to sign this certificate.
--
-- * 'cdPreviousOwnedBy' - The ID of the AWS account of the previous owner of the certificate.
--
-- * 'cdCertificatePem' - The certificate data, in PEM format.
--
-- * 'cdCertificateARN' - The ARN of the certificate.
--
-- * 'cdCertificateId' - The ID of the certificate.
--
-- * 'cdCreationDate' - The date and time the certificate was created.
--
-- * 'cdTransferData' - The transfer data.
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
--
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
-- * 'caaRoleARN' - The IAM role that allows access to the CloudWatch alarm.
--
-- * 'caaAlarmName' - The CloudWatch alarm name.
--
-- * 'caaStateReason' - The reason for the alarm change.
--
-- * 'caaStateValue' - The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
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

-- | The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
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
--
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
-- * 'cmaMetricTimestamp' - An optional <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
--
-- * 'cmaRoleARN' - The IAM role that allows access to the CloudWatch metric.
--
-- * 'cmaMetricNamespace' - The CloudWatch metric namespace name.
--
-- * 'cmaMetricName' - The CloudWatch metric name.
--
-- * 'cmaMetricValue' - The CloudWatch metric value.
--
-- * 'cmaMetricUnit' - The <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
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

-- | An optional <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
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

-- | The <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
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
--
-- The @tableName@ , @hashKeyField@ , and @rangeKeyField@ values must match the values used when you created the table.
--
-- The @hashKeyValue@ and @rangeKeyvalue@ fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${/sql-expression/ }.
--
-- You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:
--
-- @"hashKeyValue": "${topic(3)}"@
--
-- The following field uses the timestamp:
--
-- @"rangeKeyValue": "${timestamp()}"@
--
--
-- /See:/ 'dynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
    { _ddbaHashKeyType   :: !(Maybe DynamoKeyType)
    , _ddbaOperation     :: !(Maybe Text)
    , _ddbaRangeKeyType  :: !(Maybe DynamoKeyType)
    , _ddbaPayloadField  :: !(Maybe Text)
    , _ddbaRangeKeyField :: !(Maybe Text)
    , _ddbaRangeKeyValue :: !(Maybe Text)
    , _ddbaTableName     :: !Text
    , _ddbaRoleARN       :: !Text
    , _ddbaHashKeyField  :: !Text
    , _ddbaHashKeyValue  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamoDBAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbaHashKeyType' - The hash key type. Valid values are "STRING" or "NUMBER"
--
-- * 'ddbaOperation' - The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
--
-- * 'ddbaRangeKeyType' - The range key type. Valid values are "STRING" or "NUMBER"
--
-- * 'ddbaPayloadField' - The action payload. This name can be customized.
--
-- * 'ddbaRangeKeyField' - The range key name.
--
-- * 'ddbaRangeKeyValue' - The range key value.
--
-- * 'ddbaTableName' - The name of the DynamoDB table.
--
-- * 'ddbaRoleARN' - The ARN of the IAM role that grants access to the DynamoDB table.
--
-- * 'ddbaHashKeyField' - The hash key name.
--
-- * 'ddbaHashKeyValue' - The hash key value.
dynamoDBAction
    :: Text -- ^ 'ddbaTableName'
    -> Text -- ^ 'ddbaRoleARN'
    -> Text -- ^ 'ddbaHashKeyField'
    -> Text -- ^ 'ddbaHashKeyValue'
    -> DynamoDBAction
dynamoDBAction pTableName_ pRoleARN_ pHashKeyField_ pHashKeyValue_ =
    DynamoDBAction'
    { _ddbaHashKeyType = Nothing
    , _ddbaOperation = Nothing
    , _ddbaRangeKeyType = Nothing
    , _ddbaPayloadField = Nothing
    , _ddbaRangeKeyField = Nothing
    , _ddbaRangeKeyValue = Nothing
    , _ddbaTableName = pTableName_
    , _ddbaRoleARN = pRoleARN_
    , _ddbaHashKeyField = pHashKeyField_
    , _ddbaHashKeyValue = pHashKeyValue_
    }

-- | The hash key type. Valid values are "STRING" or "NUMBER"
ddbaHashKeyType :: Lens' DynamoDBAction (Maybe DynamoKeyType)
ddbaHashKeyType = lens _ddbaHashKeyType (\ s a -> s{_ddbaHashKeyType = a});

-- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
ddbaOperation :: Lens' DynamoDBAction (Maybe Text)
ddbaOperation = lens _ddbaOperation (\ s a -> s{_ddbaOperation = a});

-- | The range key type. Valid values are "STRING" or "NUMBER"
ddbaRangeKeyType :: Lens' DynamoDBAction (Maybe DynamoKeyType)
ddbaRangeKeyType = lens _ddbaRangeKeyType (\ s a -> s{_ddbaRangeKeyType = a});

-- | The action payload. This name can be customized.
ddbaPayloadField :: Lens' DynamoDBAction (Maybe Text)
ddbaPayloadField = lens _ddbaPayloadField (\ s a -> s{_ddbaPayloadField = a});

-- | The range key name.
ddbaRangeKeyField :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyField = lens _ddbaRangeKeyField (\ s a -> s{_ddbaRangeKeyField = a});

-- | The range key value.
ddbaRangeKeyValue :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyValue = lens _ddbaRangeKeyValue (\ s a -> s{_ddbaRangeKeyValue = a});

-- | The name of the DynamoDB table.
ddbaTableName :: Lens' DynamoDBAction Text
ddbaTableName = lens _ddbaTableName (\ s a -> s{_ddbaTableName = a});

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddbaRoleARN :: Lens' DynamoDBAction Text
ddbaRoleARN = lens _ddbaRoleARN (\ s a -> s{_ddbaRoleARN = a});

-- | The hash key name.
ddbaHashKeyField :: Lens' DynamoDBAction Text
ddbaHashKeyField = lens _ddbaHashKeyField (\ s a -> s{_ddbaHashKeyField = a});

-- | The hash key value.
ddbaHashKeyValue :: Lens' DynamoDBAction Text
ddbaHashKeyValue = lens _ddbaHashKeyValue (\ s a -> s{_ddbaHashKeyValue = a});

instance FromJSON DynamoDBAction where
        parseJSON
          = withObject "DynamoDBAction"
              (\ x ->
                 DynamoDBAction' <$>
                   (x .:? "hashKeyType") <*> (x .:? "operation") <*>
                     (x .:? "rangeKeyType")
                     <*> (x .:? "payloadField")
                     <*> (x .:? "rangeKeyField")
                     <*> (x .:? "rangeKeyValue")
                     <*> (x .: "tableName")
                     <*> (x .: "roleArn")
                     <*> (x .: "hashKeyField")
                     <*> (x .: "hashKeyValue"))

instance Hashable DynamoDBAction

instance NFData DynamoDBAction

instance ToJSON DynamoDBAction where
        toJSON DynamoDBAction'{..}
          = object
              (catMaybes
                 [("hashKeyType" .=) <$> _ddbaHashKeyType,
                  ("operation" .=) <$> _ddbaOperation,
                  ("rangeKeyType" .=) <$> _ddbaRangeKeyType,
                  ("payloadField" .=) <$> _ddbaPayloadField,
                  ("rangeKeyField" .=) <$> _ddbaRangeKeyField,
                  ("rangeKeyValue" .=) <$> _ddbaRangeKeyValue,
                  Just ("tableName" .= _ddbaTableName),
                  Just ("roleArn" .= _ddbaRoleARN),
                  Just ("hashKeyField" .= _ddbaHashKeyField),
                  Just ("hashKeyValue" .= _ddbaHashKeyValue)])

-- | Describes an action to write to a DynamoDB table.
--
--
-- This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.
--
--
-- /See:/ 'dynamoDBv2Action' smart constructor.
data DynamoDBv2Action = DynamoDBv2Action'
    { _ddaPutItem :: !(Maybe PutItemInput)
    , _ddaRoleARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamoDBv2Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddaPutItem' - Specifies the DynamoDB table to which the message data will be written. For example: @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@  Each attribute in the message payload will be written to a separate column in the DynamoDB database.
--
-- * 'ddaRoleARN' - The ARN of the IAM role that grants access to the DynamoDB table.
dynamoDBv2Action
    :: DynamoDBv2Action
dynamoDBv2Action =
    DynamoDBv2Action'
    { _ddaPutItem = Nothing
    , _ddaRoleARN = Nothing
    }

-- | Specifies the DynamoDB table to which the message data will be written. For example: @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@  Each attribute in the message payload will be written to a separate column in the DynamoDB database.
ddaPutItem :: Lens' DynamoDBv2Action (Maybe PutItemInput)
ddaPutItem = lens _ddaPutItem (\ s a -> s{_ddaPutItem = a});

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddaRoleARN :: Lens' DynamoDBv2Action (Maybe Text)
ddaRoleARN = lens _ddaRoleARN (\ s a -> s{_ddaRoleARN = a});

instance FromJSON DynamoDBv2Action where
        parseJSON
          = withObject "DynamoDBv2Action"
              (\ x ->
                 DynamoDBv2Action' <$>
                   (x .:? "putItem") <*> (x .:? "roleArn"))

instance Hashable DynamoDBv2Action

instance NFData DynamoDBv2Action

instance ToJSON DynamoDBv2Action where
        toJSON DynamoDBv2Action'{..}
          = object
              (catMaybes
                 [("putItem" .=) <$> _ddaPutItem,
                  ("roleArn" .=) <$> _ddaRoleARN])

-- | Describes an action that writes data to an Amazon Elasticsearch Service domain.
--
--
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
-- * 'eaRoleARN' - The IAM role ARN that has access to Elasticsearch.
--
-- * 'eaEndpoint' - The endpoint of your Elasticsearch domain.
--
-- * 'eaIndex' - The Elasticsearch index where you want to store your data.
--
-- * 'eaType' - The type of document you are storing.
--
-- * 'eaId' - The unique identifier for the document you are storing.
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

-- | Describes an action that writes data to an Amazon Kinesis Firehose stream.
--
--
--
-- /See:/ 'firehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
    { _faSeparator          :: !(Maybe Text)
    , _faRoleARN            :: !Text
    , _faDeliveryStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FirehoseAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faSeparator' - A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
--
-- * 'faRoleARN' - The IAM role that grants access to the Amazon Kinesis Firehost stream.
--
-- * 'faDeliveryStreamName' - The delivery stream name.
firehoseAction
    :: Text -- ^ 'faRoleARN'
    -> Text -- ^ 'faDeliveryStreamName'
    -> FirehoseAction
firehoseAction pRoleARN_ pDeliveryStreamName_ =
    FirehoseAction'
    { _faSeparator = Nothing
    , _faRoleARN = pRoleARN_
    , _faDeliveryStreamName = pDeliveryStreamName_
    }

-- | A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
faSeparator :: Lens' FirehoseAction (Maybe Text)
faSeparator = lens _faSeparator (\ s a -> s{_faSeparator = a});

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
                   (x .:? "separator") <*> (x .: "roleArn") <*>
                     (x .: "deliveryStreamName"))

instance Hashable FirehoseAction

instance NFData FirehoseAction

instance ToJSON FirehoseAction where
        toJSON FirehoseAction'{..}
          = object
              (catMaybes
                 [("separator" .=) <$> _faSeparator,
                  Just ("roleArn" .= _faRoleARN),
                  Just
                    ("deliveryStreamName" .= _faDeliveryStreamName)])

-- | Describes a key pair.
--
--
--
-- /See:/ 'keyPair' smart constructor.
data KeyPair = KeyPair'
    { _kpPrivateKey :: !(Maybe (Sensitive Text))
    , _kpPublicKey  :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpPrivateKey' - The private key.
--
-- * 'kpPublicKey' - The public key.
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
--
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
-- * 'kaPartitionKey' - The partition key.
--
-- * 'kaRoleARN' - The ARN of the IAM role that grants access to the Amazon Kinesis stream.
--
-- * 'kaStreamName' - The name of the Amazon Kinesis stream.
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
--
--
-- /See:/ 'lambdaAction' smart constructor.
newtype LambdaAction = LambdaAction'
    { _laFunctionARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laFunctionARN' - The ARN of the Lambda function.
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
--
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
-- * 'lopLogLevel' - The logging level.
--
-- * 'lopRoleARN' - The ARN of the IAM role that grants access.
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

-- | A certificate that has been transfered but not yet accepted.
--
--
--
-- /See:/ 'outgoingCertificate' smart constructor.
data OutgoingCertificate = OutgoingCertificate'
    { _ocTransferDate    :: !(Maybe POSIX)
    , _ocCertificateARN  :: !(Maybe Text)
    , _ocCertificateId   :: !(Maybe Text)
    , _ocTransferredTo   :: !(Maybe Text)
    , _ocCreationDate    :: !(Maybe POSIX)
    , _ocTransferMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OutgoingCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocTransferDate' - The date the transfer was initiated.
--
-- * 'ocCertificateARN' - The certificate ARN.
--
-- * 'ocCertificateId' - The certificate ID.
--
-- * 'ocTransferredTo' - The AWS account to which the transfer was made.
--
-- * 'ocCreationDate' - The certificate creation date.
--
-- * 'ocTransferMessage' - The transfer message.
outgoingCertificate
    :: OutgoingCertificate
outgoingCertificate =
    OutgoingCertificate'
    { _ocTransferDate = Nothing
    , _ocCertificateARN = Nothing
    , _ocCertificateId = Nothing
    , _ocTransferredTo = Nothing
    , _ocCreationDate = Nothing
    , _ocTransferMessage = Nothing
    }

-- | The date the transfer was initiated.
ocTransferDate :: Lens' OutgoingCertificate (Maybe UTCTime)
ocTransferDate = lens _ocTransferDate (\ s a -> s{_ocTransferDate = a}) . mapping _Time;

-- | The certificate ARN.
ocCertificateARN :: Lens' OutgoingCertificate (Maybe Text)
ocCertificateARN = lens _ocCertificateARN (\ s a -> s{_ocCertificateARN = a});

-- | The certificate ID.
ocCertificateId :: Lens' OutgoingCertificate (Maybe Text)
ocCertificateId = lens _ocCertificateId (\ s a -> s{_ocCertificateId = a});

-- | The AWS account to which the transfer was made.
ocTransferredTo :: Lens' OutgoingCertificate (Maybe Text)
ocTransferredTo = lens _ocTransferredTo (\ s a -> s{_ocTransferredTo = a});

-- | The certificate creation date.
ocCreationDate :: Lens' OutgoingCertificate (Maybe UTCTime)
ocCreationDate = lens _ocCreationDate (\ s a -> s{_ocCreationDate = a}) . mapping _Time;

-- | The transfer message.
ocTransferMessage :: Lens' OutgoingCertificate (Maybe Text)
ocTransferMessage = lens _ocTransferMessage (\ s a -> s{_ocTransferMessage = a});

instance FromJSON OutgoingCertificate where
        parseJSON
          = withObject "OutgoingCertificate"
              (\ x ->
                 OutgoingCertificate' <$>
                   (x .:? "transferDate") <*> (x .:? "certificateArn")
                     <*> (x .:? "certificateId")
                     <*> (x .:? "transferredTo")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "transferMessage"))

instance Hashable OutgoingCertificate

instance NFData OutgoingCertificate

-- | Describes an AWS IoT policy.
--
--
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
-- * 'pPolicyName' - The policy name.
--
-- * 'pPolicyARN' - The policy ARN.
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
--
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
-- * 'pvVersionId' - The policy version ID.
--
-- * 'pvCreateDate' - The date and time the policy was created.
--
-- * 'pvIsDefaultVersion' - Specifies whether the policy version is the default.
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

-- | The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.
--
--
--
-- /See:/ 'putItemInput' smart constructor.
newtype PutItemInput = PutItemInput'
    { _piiTableName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutItemInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piiTableName' - The table where the message data will be written
putItemInput
    :: Text -- ^ 'piiTableName'
    -> PutItemInput
putItemInput pTableName_ =
    PutItemInput'
    { _piiTableName = pTableName_
    }

-- | The table where the message data will be written
piiTableName :: Lens' PutItemInput Text
piiTableName = lens _piiTableName (\ s a -> s{_piiTableName = a});

instance FromJSON PutItemInput where
        parseJSON
          = withObject "PutItemInput"
              (\ x -> PutItemInput' <$> (x .: "tableName"))

instance Hashable PutItemInput

instance NFData PutItemInput

instance ToJSON PutItemInput where
        toJSON PutItemInput'{..}
          = object
              (catMaybes [Just ("tableName" .= _piiTableName)])

-- | Describes an action to republish to another topic.
--
--
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
-- * 'raRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'raTopic' - The name of the MQTT topic.
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
--
--
-- /See:/ 's3Action' smart constructor.
data S3Action = S3Action'
    { _sCannedACL  :: !(Maybe CannedAccessControlList)
    , _sRoleARN    :: !Text
    , _sBucketName :: !Text
    , _sKey        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCannedACL' - The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
--
-- * 'sRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'sBucketName' - The Amazon S3 bucket.
--
-- * 'sKey' - The object key.
s3Action
    :: Text -- ^ 'sRoleARN'
    -> Text -- ^ 'sBucketName'
    -> Text -- ^ 'sKey'
    -> S3Action
s3Action pRoleARN_ pBucketName_ pKey_ =
    S3Action'
    { _sCannedACL = Nothing
    , _sRoleARN = pRoleARN_
    , _sBucketName = pBucketName_
    , _sKey = pKey_
    }

-- | The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
sCannedACL :: Lens' S3Action (Maybe CannedAccessControlList)
sCannedACL = lens _sCannedACL (\ s a -> s{_sCannedACL = a});

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
                   (x .:? "cannedAcl") <*> (x .: "roleArn") <*>
                     (x .: "bucketName")
                     <*> (x .: "key"))

instance Hashable S3Action

instance NFData S3Action

instance ToJSON S3Action where
        toJSON S3Action'{..}
          = object
              (catMaybes
                 [("cannedAcl" .=) <$> _sCannedACL,
                  Just ("roleArn" .= _sRoleARN),
                  Just ("bucketName" .= _sBucketName),
                  Just ("key" .= _sKey)])

-- | Describes an action to publish to an Amazon SNS topic.
--
--
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
-- * 'snsaMessageFormat' - The message format of the message to publish. Optional. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <http://docs.aws.amazon.com/sns/latest/dg/json-formats.html http://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
--
-- * 'snsaTargetARN' - The ARN of the SNS topic.
--
-- * 'snsaRoleARN' - The ARN of the IAM role that grants access.
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

-- | The message format of the message to publish. Optional. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <http://docs.aws.amazon.com/sns/latest/dg/json-formats.html http://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
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

-- | Describes an action to write a message to a Salesforce IoT Cloud Input Stream.
--
--
--
-- /See:/ 'salesforceAction' smart constructor.
data SalesforceAction = SalesforceAction'
    { _saToken :: !Text
    , _saUrl   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SalesforceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saToken' - The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- * 'saUrl' - The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
salesforceAction
    :: Text -- ^ 'saToken'
    -> Text -- ^ 'saUrl'
    -> SalesforceAction
salesforceAction pToken_ pUrl_ =
    SalesforceAction'
    { _saToken = pToken_
    , _saUrl = pUrl_
    }

-- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saToken :: Lens' SalesforceAction Text
saToken = lens _saToken (\ s a -> s{_saToken = a});

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saUrl :: Lens' SalesforceAction Text
saUrl = lens _saUrl (\ s a -> s{_saUrl = a});

instance FromJSON SalesforceAction where
        parseJSON
          = withObject "SalesforceAction"
              (\ x ->
                 SalesforceAction' <$>
                   (x .: "token") <*> (x .: "url"))

instance Hashable SalesforceAction

instance NFData SalesforceAction

instance ToJSON SalesforceAction where
        toJSON SalesforceAction'{..}
          = object
              (catMaybes
                 [Just ("token" .= _saToken), Just ("url" .= _saUrl)])

-- | Describes an action to publish data to an Amazon SQS queue.
--
--
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
-- * 'saUseBase64' - Specifies whether to use Base64 encoding.
--
-- * 'saRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'saQueueURL' - The URL of the Amazon SQS queue.
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

-- | The properties of the thing, including thing name, thing type name, and a list of thing attributes.
--
--
--
-- /See:/ 'thingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
    { _taThingTypeName :: !(Maybe Text)
    , _taAttributes    :: !(Maybe (Map Text Text))
    , _taVersion       :: !(Maybe Integer)
    , _taThingName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThingAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taThingTypeName' - The name of the thing type, if the thing has been associated with a type.
--
-- * 'taAttributes' - A list of thing attributes which are name-value pairs.
--
-- * 'taVersion' - The version of the thing record in the registry.
--
-- * 'taThingName' - The name of the thing.
thingAttribute
    :: ThingAttribute
thingAttribute =
    ThingAttribute'
    { _taThingTypeName = Nothing
    , _taAttributes = Nothing
    , _taVersion = Nothing
    , _taThingName = Nothing
    }

-- | The name of the thing type, if the thing has been associated with a type.
taThingTypeName :: Lens' ThingAttribute (Maybe Text)
taThingTypeName = lens _taThingTypeName (\ s a -> s{_taThingTypeName = a});

-- | A list of thing attributes which are name-value pairs.
taAttributes :: Lens' ThingAttribute (HashMap Text Text)
taAttributes = lens _taAttributes (\ s a -> s{_taAttributes = a}) . _Default . _Map;

-- | The version of the thing record in the registry.
taVersion :: Lens' ThingAttribute (Maybe Integer)
taVersion = lens _taVersion (\ s a -> s{_taVersion = a});

-- | The name of the thing.
taThingName :: Lens' ThingAttribute (Maybe Text)
taThingName = lens _taThingName (\ s a -> s{_taThingName = a});

instance FromJSON ThingAttribute where
        parseJSON
          = withObject "ThingAttribute"
              (\ x ->
                 ThingAttribute' <$>
                   (x .:? "thingTypeName") <*>
                     (x .:? "attributes" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "thingName"))

instance Hashable ThingAttribute

instance NFData ThingAttribute

-- | The definition of the thing type, including thing type name and description.
--
--
--
-- /See:/ 'thingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
    { _ttdThingTypeProperties :: !(Maybe ThingTypeProperties)
    , _ttdThingTypeName       :: !(Maybe Text)
    , _ttdThingTypeMetadata   :: !(Maybe ThingTypeMetadata)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThingTypeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttdThingTypeProperties' - The ThingTypeProperties for the thing type.
--
-- * 'ttdThingTypeName' - The name of the thing type.
--
-- * 'ttdThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
thingTypeDefinition
    :: ThingTypeDefinition
thingTypeDefinition =
    ThingTypeDefinition'
    { _ttdThingTypeProperties = Nothing
    , _ttdThingTypeName = Nothing
    , _ttdThingTypeMetadata = Nothing
    }

-- | The ThingTypeProperties for the thing type.
ttdThingTypeProperties :: Lens' ThingTypeDefinition (Maybe ThingTypeProperties)
ttdThingTypeProperties = lens _ttdThingTypeProperties (\ s a -> s{_ttdThingTypeProperties = a});

-- | The name of the thing type.
ttdThingTypeName :: Lens' ThingTypeDefinition (Maybe Text)
ttdThingTypeName = lens _ttdThingTypeName (\ s a -> s{_ttdThingTypeName = a});

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
ttdThingTypeMetadata :: Lens' ThingTypeDefinition (Maybe ThingTypeMetadata)
ttdThingTypeMetadata = lens _ttdThingTypeMetadata (\ s a -> s{_ttdThingTypeMetadata = a});

instance FromJSON ThingTypeDefinition where
        parseJSON
          = withObject "ThingTypeDefinition"
              (\ x ->
                 ThingTypeDefinition' <$>
                   (x .:? "thingTypeProperties") <*>
                     (x .:? "thingTypeName")
                     <*> (x .:? "thingTypeMetadata"))

instance Hashable ThingTypeDefinition

instance NFData ThingTypeDefinition

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.
--
--
--
-- /See:/ 'thingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
    { _ttmDeprecationDate :: !(Maybe POSIX)
    , _ttmCreationDate    :: !(Maybe POSIX)
    , _ttmDeprecated      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThingTypeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttmDeprecationDate' - The date and time when the thing type was deprecated.
--
-- * 'ttmCreationDate' - The date and time when the thing type was created.
--
-- * 'ttmDeprecated' - Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
thingTypeMetadata
    :: ThingTypeMetadata
thingTypeMetadata =
    ThingTypeMetadata'
    { _ttmDeprecationDate = Nothing
    , _ttmCreationDate = Nothing
    , _ttmDeprecated = Nothing
    }

-- | The date and time when the thing type was deprecated.
ttmDeprecationDate :: Lens' ThingTypeMetadata (Maybe UTCTime)
ttmDeprecationDate = lens _ttmDeprecationDate (\ s a -> s{_ttmDeprecationDate = a}) . mapping _Time;

-- | The date and time when the thing type was created.
ttmCreationDate :: Lens' ThingTypeMetadata (Maybe UTCTime)
ttmCreationDate = lens _ttmCreationDate (\ s a -> s{_ttmCreationDate = a}) . mapping _Time;

-- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
ttmDeprecated :: Lens' ThingTypeMetadata (Maybe Bool)
ttmDeprecated = lens _ttmDeprecated (\ s a -> s{_ttmDeprecated = a});

instance FromJSON ThingTypeMetadata where
        parseJSON
          = withObject "ThingTypeMetadata"
              (\ x ->
                 ThingTypeMetadata' <$>
                   (x .:? "deprecationDate") <*> (x .:? "creationDate")
                     <*> (x .:? "deprecated"))

instance Hashable ThingTypeMetadata

instance NFData ThingTypeMetadata

-- | The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.
--
--
--
-- /See:/ 'thingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
    { _ttpSearchableAttributes :: !(Maybe [Text])
    , _ttpThingTypeDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThingTypeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttpSearchableAttributes' - A list of searchable thing attribute names.
--
-- * 'ttpThingTypeDescription' - The description of the thing type.
thingTypeProperties
    :: ThingTypeProperties
thingTypeProperties =
    ThingTypeProperties'
    { _ttpSearchableAttributes = Nothing
    , _ttpThingTypeDescription = Nothing
    }

-- | A list of searchable thing attribute names.
ttpSearchableAttributes :: Lens' ThingTypeProperties [Text]
ttpSearchableAttributes = lens _ttpSearchableAttributes (\ s a -> s{_ttpSearchableAttributes = a}) . _Default . _Coerce;

-- | The description of the thing type.
ttpThingTypeDescription :: Lens' ThingTypeProperties (Maybe Text)
ttpThingTypeDescription = lens _ttpThingTypeDescription (\ s a -> s{_ttpThingTypeDescription = a});

instance FromJSON ThingTypeProperties where
        parseJSON
          = withObject "ThingTypeProperties"
              (\ x ->
                 ThingTypeProperties' <$>
                   (x .:? "searchableAttributes" .!= mempty) <*>
                     (x .:? "thingTypeDescription"))

instance Hashable ThingTypeProperties

instance NFData ThingTypeProperties

instance ToJSON ThingTypeProperties where
        toJSON ThingTypeProperties'{..}
          = object
              (catMaybes
                 [("searchableAttributes" .=) <$>
                    _ttpSearchableAttributes,
                  ("thingTypeDescription" .=) <$>
                    _ttpThingTypeDescription])

-- | Describes a rule.
--
--
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
-- * 'trCreatedAt' - The date and time the rule was created.
--
-- * 'trActions' - The actions associated with the rule.
--
-- * 'trAwsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- * 'trRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'trRuleName' - The name of the rule.
--
-- * 'trSql' - The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
--
-- * 'trDescription' - The description of the rule.
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

-- | The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
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
--
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
-- * 'trliCreatedAt' - The date and time the rule was created.
--
-- * 'trliRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'trliRuleName' - The name of the rule.
--
-- * 'trliRuleARN' - The rule ARN.
--
-- * 'trliTopicPattern' - The pattern for the topic names that apply.
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
--
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
-- * 'trpAwsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- * 'trpRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'trpDescription' - The description of the rule.
--
-- * 'trpSql' - The SQL statement used to query the topic. For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/iot-rules.html#aws-iot-sql-reference AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
--
-- * 'trpActions' - The actions associated with the rule.
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

-- | The SQL statement used to query the topic. For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/iot-rules.html#aws-iot-sql-reference AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
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
--
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
-- * 'tdTransferDate' - The date the transfer took place.
--
-- * 'tdAcceptDate' - The date the transfer was accepted.
--
-- * 'tdTransferMessage' - The transfer message.
--
-- * 'tdRejectDate' - The date the transfer was rejected.
--
-- * 'tdRejectReason' - The reason why the transfer was rejected.
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
