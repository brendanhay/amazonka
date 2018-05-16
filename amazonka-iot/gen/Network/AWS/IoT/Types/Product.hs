{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Product where

import Network.AWS.IoT.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

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
  , _aIotAnalytics     :: !(Maybe IotAnalyticsAction)
  , _aLambda           :: !(Maybe LambdaAction)
  , _aSalesforce       :: !(Maybe SalesforceAction)
  , _aKinesis          :: !(Maybe KinesisAction)
  , _aS3               :: !(Maybe S3Action)
  , _aElasticsearch    :: !(Maybe ElasticsearchAction)
  , _aRepublish        :: !(Maybe RepublishAction)
  , _aSqs              :: !(Maybe SqsAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'aIotAnalytics' - Sends message data to an AWS IoT Analytics channel.
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
    , _aIotAnalytics = Nothing
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
aCloudwatchMetric = lens _aCloudwatchMetric (\ s a -> s{_aCloudwatchMetric = a})

-- | Write to a DynamoDB table. This is a new version of the DynamoDB action. It allows you to write each attribute in an MQTT message payload into a separate DynamoDB column.
aDynamoDBv2 :: Lens' Action (Maybe DynamoDBv2Action)
aDynamoDBv2 = lens _aDynamoDBv2 (\ s a -> s{_aDynamoDBv2 = a})

-- | Change the state of a CloudWatch alarm.
aCloudwatchAlarm :: Lens' Action (Maybe CloudwatchAlarmAction)
aCloudwatchAlarm = lens _aCloudwatchAlarm (\ s a -> s{_aCloudwatchAlarm = a})

-- | Publish to an Amazon SNS topic.
aSns :: Lens' Action (Maybe SNSAction)
aSns = lens _aSns (\ s a -> s{_aSns = a})

-- | Write to a DynamoDB table.
aDynamoDB :: Lens' Action (Maybe DynamoDBAction)
aDynamoDB = lens _aDynamoDB (\ s a -> s{_aDynamoDB = a})

-- | Write to an Amazon Kinesis Firehose stream.
aFirehose :: Lens' Action (Maybe FirehoseAction)
aFirehose = lens _aFirehose (\ s a -> s{_aFirehose = a})

-- | Sends message data to an AWS IoT Analytics channel.
aIotAnalytics :: Lens' Action (Maybe IotAnalyticsAction)
aIotAnalytics = lens _aIotAnalytics (\ s a -> s{_aIotAnalytics = a})

-- | Invoke a Lambda function.
aLambda :: Lens' Action (Maybe LambdaAction)
aLambda = lens _aLambda (\ s a -> s{_aLambda = a})

-- | Send a message to a Salesforce IoT Cloud Input Stream.
aSalesforce :: Lens' Action (Maybe SalesforceAction)
aSalesforce = lens _aSalesforce (\ s a -> s{_aSalesforce = a})

-- | Write data to an Amazon Kinesis stream.
aKinesis :: Lens' Action (Maybe KinesisAction)
aKinesis = lens _aKinesis (\ s a -> s{_aKinesis = a})

-- | Write to an Amazon S3 bucket.
aS3 :: Lens' Action (Maybe S3Action)
aS3 = lens _aS3 (\ s a -> s{_aS3 = a})

-- | Write data to an Amazon Elasticsearch Service domain.
aElasticsearch :: Lens' Action (Maybe ElasticsearchAction)
aElasticsearch = lens _aElasticsearch (\ s a -> s{_aElasticsearch = a})

-- | Publish to another MQTT topic.
aRepublish :: Lens' Action (Maybe RepublishAction)
aRepublish = lens _aRepublish (\ s a -> s{_aRepublish = a})

-- | Publish to an Amazon SQS queue.
aSqs :: Lens' Action (Maybe SqsAction)
aSqs = lens _aSqs (\ s a -> s{_aSqs = a})

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
                     <*> (x .:? "iotAnalytics")
                     <*> (x .:? "lambda")
                     <*> (x .:? "salesforce")
                     <*> (x .:? "kinesis")
                     <*> (x .:? "s3")
                     <*> (x .:? "elasticsearch")
                     <*> (x .:? "republish")
                     <*> (x .:? "sqs"))

instance Hashable Action where

instance NFData Action where

instance ToJSON Action where
        toJSON Action'{..}
          = object
              (catMaybes
                 [("cloudwatchMetric" .=) <$> _aCloudwatchMetric,
                  ("dynamoDBv2" .=) <$> _aDynamoDBv2,
                  ("cloudwatchAlarm" .=) <$> _aCloudwatchAlarm,
                  ("sns" .=) <$> _aSns, ("dynamoDB" .=) <$> _aDynamoDB,
                  ("firehose" .=) <$> _aFirehose,
                  ("iotAnalytics" .=) <$> _aIotAnalytics,
                  ("lambda" .=) <$> _aLambda,
                  ("salesforce" .=) <$> _aSalesforce,
                  ("kinesis" .=) <$> _aKinesis, ("s3" .=) <$> _aS3,
                  ("elasticsearch" .=) <$> _aElasticsearch,
                  ("republish" .=) <$> _aRepublish,
                  ("sqs" .=) <$> _aSqs])

-- | Contains information that allowed the authorization.
--
--
--
-- /See:/ 'allowed' smart constructor.
newtype Allowed = Allowed'
  { _aPolicies :: Maybe [Policy]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Allowed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aPolicies' - A list of policies that allowed the authentication.
allowed
    :: Allowed
allowed = Allowed' {_aPolicies = Nothing}


-- | A list of policies that allowed the authentication.
aPolicies :: Lens' Allowed [Policy]
aPolicies = lens _aPolicies (\ s a -> s{_aPolicies = a}) . _Default . _Coerce

instance FromJSON Allowed where
        parseJSON
          = withObject "Allowed"
              (\ x -> Allowed' <$> (x .:? "policies" .!= mempty))

instance Hashable Allowed where

instance NFData Allowed where

-- | The attribute payload.
--
--
--
-- /See:/ 'attributePayload' smart constructor.
data AttributePayload = AttributePayload'
  { _apAttributes :: !(Maybe (Map Text Text))
  , _apMerge      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  AttributePayload' {_apAttributes = Nothing, _apMerge = Nothing}


-- | A JSON string containing up to three key-value pair in JSON format. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
apAttributes :: Lens' AttributePayload (HashMap Text Text)
apAttributes = lens _apAttributes (\ s a -> s{_apAttributes = a}) . _Default . _Map

-- | Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them. To remove an attribute, call @UpdateThing@ with an empty attribute value.
apMerge :: Lens' AttributePayload (Maybe Bool)
apMerge = lens _apMerge (\ s a -> s{_apMerge = a})

instance FromJSON AttributePayload where
        parseJSON
          = withObject "AttributePayload"
              (\ x ->
                 AttributePayload' <$>
                   (x .:? "attributes" .!= mempty) <*> (x .:? "merge"))

instance Hashable AttributePayload where

instance NFData AttributePayload where

instance ToJSON AttributePayload where
        toJSON AttributePayload'{..}
          = object
              (catMaybes
                 [("attributes" .=) <$> _apAttributes,
                  ("merge" .=) <$> _apMerge])

-- | A collection of authorization information.
--
--
--
-- /See:/ 'authInfo' smart constructor.
data AuthInfo = AuthInfo'
  { _aiResources  :: !(Maybe [Text])
  , _aiActionType :: !(Maybe ActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiResources' - The resources for which the principal is being authorized to perform the specified action.
--
-- * 'aiActionType' - The type of action for which the principal is being authorized.
authInfo
    :: AuthInfo
authInfo = AuthInfo' {_aiResources = Nothing, _aiActionType = Nothing}


-- | The resources for which the principal is being authorized to perform the specified action.
aiResources :: Lens' AuthInfo [Text]
aiResources = lens _aiResources (\ s a -> s{_aiResources = a}) . _Default . _Coerce

-- | The type of action for which the principal is being authorized.
aiActionType :: Lens' AuthInfo (Maybe ActionType)
aiActionType = lens _aiActionType (\ s a -> s{_aiActionType = a})

instance FromJSON AuthInfo where
        parseJSON
          = withObject "AuthInfo"
              (\ x ->
                 AuthInfo' <$>
                   (x .:? "resources" .!= mempty) <*>
                     (x .:? "actionType"))

instance Hashable AuthInfo where

instance NFData AuthInfo where

instance ToJSON AuthInfo where
        toJSON AuthInfo'{..}
          = object
              (catMaybes
                 [("resources" .=) <$> _aiResources,
                  ("actionType" .=) <$> _aiActionType])

-- | The authorizer result.
--
--
--
-- /See:/ 'authResult' smart constructor.
data AuthResult = AuthResult'
  { _arDenied               :: !(Maybe Denied)
  , _arAuthDecision         :: !(Maybe AuthDecision)
  , _arAllowed              :: !(Maybe Allowed)
  , _arMissingContextValues :: !(Maybe [Text])
  , _arAuthInfo             :: !(Maybe AuthInfo)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arDenied' - The policies and statements that denied the specified action.
--
-- * 'arAuthDecision' - The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
--
-- * 'arAllowed' - The policies and statements that allowed the specified action.
--
-- * 'arMissingContextValues' - Contains any missing context values found while evaluating policy.
--
-- * 'arAuthInfo' - Authorization information.
authResult
    :: AuthResult
authResult =
  AuthResult'
    { _arDenied = Nothing
    , _arAuthDecision = Nothing
    , _arAllowed = Nothing
    , _arMissingContextValues = Nothing
    , _arAuthInfo = Nothing
    }


-- | The policies and statements that denied the specified action.
arDenied :: Lens' AuthResult (Maybe Denied)
arDenied = lens _arDenied (\ s a -> s{_arDenied = a})

-- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
arAuthDecision :: Lens' AuthResult (Maybe AuthDecision)
arAuthDecision = lens _arAuthDecision (\ s a -> s{_arAuthDecision = a})

-- | The policies and statements that allowed the specified action.
arAllowed :: Lens' AuthResult (Maybe Allowed)
arAllowed = lens _arAllowed (\ s a -> s{_arAllowed = a})

-- | Contains any missing context values found while evaluating policy.
arMissingContextValues :: Lens' AuthResult [Text]
arMissingContextValues = lens _arMissingContextValues (\ s a -> s{_arMissingContextValues = a}) . _Default . _Coerce

-- | Authorization information.
arAuthInfo :: Lens' AuthResult (Maybe AuthInfo)
arAuthInfo = lens _arAuthInfo (\ s a -> s{_arAuthInfo = a})

instance FromJSON AuthResult where
        parseJSON
          = withObject "AuthResult"
              (\ x ->
                 AuthResult' <$>
                   (x .:? "denied") <*> (x .:? "authDecision") <*>
                     (x .:? "allowed")
                     <*> (x .:? "missingContextValues" .!= mempty)
                     <*> (x .:? "authInfo"))

instance Hashable AuthResult where

instance NFData AuthResult where

-- | The authorizer description.
--
--
--
-- /See:/ 'authorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { _adStatus                 :: !(Maybe AuthorizerStatus)
  , _adLastModifiedDate       :: !(Maybe POSIX)
  , _adAuthorizerName         :: !(Maybe Text)
  , _adAuthorizerFunctionARN  :: !(Maybe Text)
  , _adAuthorizerARN          :: !(Maybe Text)
  , _adCreationDate           :: !(Maybe POSIX)
  , _adTokenSigningPublicKeys :: !(Maybe (Map Text Text))
  , _adTokenKeyName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adStatus' - The status of the authorizer.
--
-- * 'adLastModifiedDate' - The UNIX timestamp of when the authorizer was last updated.
--
-- * 'adAuthorizerName' - The authorizer name.
--
-- * 'adAuthorizerFunctionARN' - The authorizer's Lambda function ARN.
--
-- * 'adAuthorizerARN' - The authorizer ARN.
--
-- * 'adCreationDate' - The UNIX timestamp of when the authorizer was created.
--
-- * 'adTokenSigningPublicKeys' - The public keys used to validate the token signature returned by your custom authentication service.
--
-- * 'adTokenKeyName' - The key used to extract the token from the HTTP headers.
authorizerDescription
    :: AuthorizerDescription
authorizerDescription =
  AuthorizerDescription'
    { _adStatus = Nothing
    , _adLastModifiedDate = Nothing
    , _adAuthorizerName = Nothing
    , _adAuthorizerFunctionARN = Nothing
    , _adAuthorizerARN = Nothing
    , _adCreationDate = Nothing
    , _adTokenSigningPublicKeys = Nothing
    , _adTokenKeyName = Nothing
    }


-- | The status of the authorizer.
adStatus :: Lens' AuthorizerDescription (Maybe AuthorizerStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a})

-- | The UNIX timestamp of when the authorizer was last updated.
adLastModifiedDate :: Lens' AuthorizerDescription (Maybe UTCTime)
adLastModifiedDate = lens _adLastModifiedDate (\ s a -> s{_adLastModifiedDate = a}) . mapping _Time

-- | The authorizer name.
adAuthorizerName :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerName = lens _adAuthorizerName (\ s a -> s{_adAuthorizerName = a})

-- | The authorizer's Lambda function ARN.
adAuthorizerFunctionARN :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerFunctionARN = lens _adAuthorizerFunctionARN (\ s a -> s{_adAuthorizerFunctionARN = a})

-- | The authorizer ARN.
adAuthorizerARN :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerARN = lens _adAuthorizerARN (\ s a -> s{_adAuthorizerARN = a})

-- | The UNIX timestamp of when the authorizer was created.
adCreationDate :: Lens' AuthorizerDescription (Maybe UTCTime)
adCreationDate = lens _adCreationDate (\ s a -> s{_adCreationDate = a}) . mapping _Time

-- | The public keys used to validate the token signature returned by your custom authentication service.
adTokenSigningPublicKeys :: Lens' AuthorizerDescription (HashMap Text Text)
adTokenSigningPublicKeys = lens _adTokenSigningPublicKeys (\ s a -> s{_adTokenSigningPublicKeys = a}) . _Default . _Map

-- | The key used to extract the token from the HTTP headers.
adTokenKeyName :: Lens' AuthorizerDescription (Maybe Text)
adTokenKeyName = lens _adTokenKeyName (\ s a -> s{_adTokenKeyName = a})

instance FromJSON AuthorizerDescription where
        parseJSON
          = withObject "AuthorizerDescription"
              (\ x ->
                 AuthorizerDescription' <$>
                   (x .:? "status") <*> (x .:? "lastModifiedDate") <*>
                     (x .:? "authorizerName")
                     <*> (x .:? "authorizerFunctionArn")
                     <*> (x .:? "authorizerArn")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "tokenSigningPublicKeys" .!= mempty)
                     <*> (x .:? "tokenKeyName"))

instance Hashable AuthorizerDescription where

instance NFData AuthorizerDescription where

-- | The authorizer summary.
--
--
--
-- /See:/ 'authorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { _asAuthorizerName :: !(Maybe Text)
  , _asAuthorizerARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAuthorizerName' - The authorizer name.
--
-- * 'asAuthorizerARN' - The authorizer ARN.
authorizerSummary
    :: AuthorizerSummary
authorizerSummary =
  AuthorizerSummary' {_asAuthorizerName = Nothing, _asAuthorizerARN = Nothing}


-- | The authorizer name.
asAuthorizerName :: Lens' AuthorizerSummary (Maybe Text)
asAuthorizerName = lens _asAuthorizerName (\ s a -> s{_asAuthorizerName = a})

-- | The authorizer ARN.
asAuthorizerARN :: Lens' AuthorizerSummary (Maybe Text)
asAuthorizerARN = lens _asAuthorizerARN (\ s a -> s{_asAuthorizerARN = a})

instance FromJSON AuthorizerSummary where
        parseJSON
          = withObject "AuthorizerSummary"
              (\ x ->
                 AuthorizerSummary' <$>
                   (x .:? "authorizerName") <*> (x .:? "authorizerArn"))

instance Hashable AuthorizerSummary where

instance NFData AuthorizerSummary where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
cacStatus = lens _cacStatus (\ s a -> s{_cacStatus = a})

-- | The ARN of the CA certificate.
cacCertificateARN :: Lens' CACertificate (Maybe Text)
cacCertificateARN = lens _cacCertificateARN (\ s a -> s{_cacCertificateARN = a})

-- | The ID of the CA certificate.
cacCertificateId :: Lens' CACertificate (Maybe Text)
cacCertificateId = lens _cacCertificateId (\ s a -> s{_cacCertificateId = a})

-- | The date the CA certificate was created.
cacCreationDate :: Lens' CACertificate (Maybe UTCTime)
cacCreationDate = lens _cacCreationDate (\ s a -> s{_cacCreationDate = a}) . mapping _Time

instance FromJSON CACertificate where
        parseJSON
          = withObject "CACertificate"
              (\ x ->
                 CACertificate' <$>
                   (x .:? "status") <*> (x .:? "certificateArn") <*>
                     (x .:? "certificateId")
                     <*> (x .:? "creationDate"))

instance Hashable CACertificate where

instance NFData CACertificate where

-- | Describes a CA certificate.
--
--
--
-- /See:/ 'cACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { _cacdStatus                 :: !(Maybe CACertificateStatus)
  , _cacdOwnedBy                :: !(Maybe Text)
  , _cacdLastModifiedDate       :: !(Maybe POSIX)
  , _cacdCertificatePem         :: !(Maybe Text)
  , _cacdCertificateARN         :: !(Maybe Text)
  , _cacdCertificateId          :: !(Maybe Text)
  , _cacdAutoRegistrationStatus :: !(Maybe AutoRegistrationStatus)
  , _cacdCreationDate           :: !(Maybe POSIX)
  , _cacdGenerationId           :: !(Maybe Text)
  , _cacdCustomerVersion        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CACertificateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacdStatus' - The status of a CA certificate.
--
-- * 'cacdOwnedBy' - The owner of the CA certificate.
--
-- * 'cacdLastModifiedDate' - The date the CA certificate was last modified.
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
--
-- * 'cacdGenerationId' - The generation ID of the CA certificate.
--
-- * 'cacdCustomerVersion' - The customer version of the CA certificate.
cACertificateDescription
    :: CACertificateDescription
cACertificateDescription =
  CACertificateDescription'
    { _cacdStatus = Nothing
    , _cacdOwnedBy = Nothing
    , _cacdLastModifiedDate = Nothing
    , _cacdCertificatePem = Nothing
    , _cacdCertificateARN = Nothing
    , _cacdCertificateId = Nothing
    , _cacdAutoRegistrationStatus = Nothing
    , _cacdCreationDate = Nothing
    , _cacdGenerationId = Nothing
    , _cacdCustomerVersion = Nothing
    }


-- | The status of a CA certificate.
cacdStatus :: Lens' CACertificateDescription (Maybe CACertificateStatus)
cacdStatus = lens _cacdStatus (\ s a -> s{_cacdStatus = a})

-- | The owner of the CA certificate.
cacdOwnedBy :: Lens' CACertificateDescription (Maybe Text)
cacdOwnedBy = lens _cacdOwnedBy (\ s a -> s{_cacdOwnedBy = a})

-- | The date the CA certificate was last modified.
cacdLastModifiedDate :: Lens' CACertificateDescription (Maybe UTCTime)
cacdLastModifiedDate = lens _cacdLastModifiedDate (\ s a -> s{_cacdLastModifiedDate = a}) . mapping _Time

-- | The CA certificate data, in PEM format.
cacdCertificatePem :: Lens' CACertificateDescription (Maybe Text)
cacdCertificatePem = lens _cacdCertificatePem (\ s a -> s{_cacdCertificatePem = a})

-- | The CA certificate ARN.
cacdCertificateARN :: Lens' CACertificateDescription (Maybe Text)
cacdCertificateARN = lens _cacdCertificateARN (\ s a -> s{_cacdCertificateARN = a})

-- | The CA certificate ID.
cacdCertificateId :: Lens' CACertificateDescription (Maybe Text)
cacdCertificateId = lens _cacdCertificateId (\ s a -> s{_cacdCertificateId = a})

-- | Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
cacdAutoRegistrationStatus :: Lens' CACertificateDescription (Maybe AutoRegistrationStatus)
cacdAutoRegistrationStatus = lens _cacdAutoRegistrationStatus (\ s a -> s{_cacdAutoRegistrationStatus = a})

-- | The date the CA certificate was created.
cacdCreationDate :: Lens' CACertificateDescription (Maybe UTCTime)
cacdCreationDate = lens _cacdCreationDate (\ s a -> s{_cacdCreationDate = a}) . mapping _Time

-- | The generation ID of the CA certificate.
cacdGenerationId :: Lens' CACertificateDescription (Maybe Text)
cacdGenerationId = lens _cacdGenerationId (\ s a -> s{_cacdGenerationId = a})

-- | The customer version of the CA certificate.
cacdCustomerVersion :: Lens' CACertificateDescription (Maybe Natural)
cacdCustomerVersion = lens _cacdCustomerVersion (\ s a -> s{_cacdCustomerVersion = a}) . mapping _Nat

instance FromJSON CACertificateDescription where
        parseJSON
          = withObject "CACertificateDescription"
              (\ x ->
                 CACertificateDescription' <$>
                   (x .:? "status") <*> (x .:? "ownedBy") <*>
                     (x .:? "lastModifiedDate")
                     <*> (x .:? "certificatePem")
                     <*> (x .:? "certificateArn")
                     <*> (x .:? "certificateId")
                     <*> (x .:? "autoRegistrationStatus")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "generationId")
                     <*> (x .:? "customerVersion"))

instance Hashable CACertificateDescription where

instance NFData CACertificateDescription where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'cCertificateARN' - The ARN of the certificate.
--
-- * 'cCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
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
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The ARN of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
cCertificateId :: Lens' Certificate (Maybe Text)
cCertificateId = lens _cCertificateId (\ s a -> s{_cCertificateId = a})

-- | The date and time the certificate was created.
cCreationDate :: Lens' Certificate (Maybe UTCTime)
cCreationDate = lens _cCreationDate (\ s a -> s{_cCreationDate = a}) . mapping _Time

instance FromJSON Certificate where
        parseJSON
          = withObject "Certificate"
              (\ x ->
                 Certificate' <$>
                   (x .:? "status") <*> (x .:? "certificateArn") <*>
                     (x .:? "certificateId")
                     <*> (x .:? "creationDate"))

instance Hashable Certificate where

instance NFData Certificate where

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
  , _cdGenerationId     :: !(Maybe Text)
  , _cdTransferData     :: !(Maybe TransferData)
  , _cdCustomerVersion  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'cdGenerationId' - The generation ID of the certificate.
--
-- * 'cdTransferData' - The transfer data.
--
-- * 'cdCustomerVersion' - The customer version of the certificate.
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
    , _cdGenerationId = Nothing
    , _cdTransferData = Nothing
    , _cdCustomerVersion = Nothing
    }


-- | The status of the certificate.
cdStatus :: Lens' CertificateDescription (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a})

-- | The ID of the AWS account that owns the certificate.
cdOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdOwnedBy = lens _cdOwnedBy (\ s a -> s{_cdOwnedBy = a})

-- | The date and time the certificate was last modified.
cdLastModifiedDate :: Lens' CertificateDescription (Maybe UTCTime)
cdLastModifiedDate = lens _cdLastModifiedDate (\ s a -> s{_cdLastModifiedDate = a}) . mapping _Time

-- | The certificate ID of the CA certificate used to sign this certificate.
cdCaCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCaCertificateId = lens _cdCaCertificateId (\ s a -> s{_cdCaCertificateId = a})

-- | The ID of the AWS account of the previous owner of the certificate.
cdPreviousOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdPreviousOwnedBy = lens _cdPreviousOwnedBy (\ s a -> s{_cdPreviousOwnedBy = a})

-- | The certificate data, in PEM format.
cdCertificatePem :: Lens' CertificateDescription (Maybe Text)
cdCertificatePem = lens _cdCertificatePem (\ s a -> s{_cdCertificatePem = a})

-- | The ARN of the certificate.
cdCertificateARN :: Lens' CertificateDescription (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\ s a -> s{_cdCertificateARN = a})

-- | The ID of the certificate.
cdCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCertificateId = lens _cdCertificateId (\ s a -> s{_cdCertificateId = a})

-- | The date and time the certificate was created.
cdCreationDate :: Lens' CertificateDescription (Maybe UTCTime)
cdCreationDate = lens _cdCreationDate (\ s a -> s{_cdCreationDate = a}) . mapping _Time

-- | The generation ID of the certificate.
cdGenerationId :: Lens' CertificateDescription (Maybe Text)
cdGenerationId = lens _cdGenerationId (\ s a -> s{_cdGenerationId = a})

-- | The transfer data.
cdTransferData :: Lens' CertificateDescription (Maybe TransferData)
cdTransferData = lens _cdTransferData (\ s a -> s{_cdTransferData = a})

-- | The customer version of the certificate.
cdCustomerVersion :: Lens' CertificateDescription (Maybe Natural)
cdCustomerVersion = lens _cdCustomerVersion (\ s a -> s{_cdCustomerVersion = a}) . mapping _Nat

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
                     <*> (x .:? "generationId")
                     <*> (x .:? "transferData")
                     <*> (x .:? "customerVersion"))

instance Hashable CertificateDescription where

instance NFData CertificateDescription where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
caaRoleARN = lens _caaRoleARN (\ s a -> s{_caaRoleARN = a})

-- | The CloudWatch alarm name.
caaAlarmName :: Lens' CloudwatchAlarmAction Text
caaAlarmName = lens _caaAlarmName (\ s a -> s{_caaAlarmName = a})

-- | The reason for the alarm change.
caaStateReason :: Lens' CloudwatchAlarmAction Text
caaStateReason = lens _caaStateReason (\ s a -> s{_caaStateReason = a})

-- | The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
caaStateValue :: Lens' CloudwatchAlarmAction Text
caaStateValue = lens _caaStateValue (\ s a -> s{_caaStateValue = a})

instance FromJSON CloudwatchAlarmAction where
        parseJSON
          = withObject "CloudwatchAlarmAction"
              (\ x ->
                 CloudwatchAlarmAction' <$>
                   (x .: "roleArn") <*> (x .: "alarmName") <*>
                     (x .: "stateReason")
                     <*> (x .: "stateValue"))

instance Hashable CloudwatchAlarmAction where

instance NFData CloudwatchAlarmAction where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
cmaMetricTimestamp = lens _cmaMetricTimestamp (\ s a -> s{_cmaMetricTimestamp = a})

-- | The IAM role that allows access to the CloudWatch metric.
cmaRoleARN :: Lens' CloudwatchMetricAction Text
cmaRoleARN = lens _cmaRoleARN (\ s a -> s{_cmaRoleARN = a})

-- | The CloudWatch metric namespace name.
cmaMetricNamespace :: Lens' CloudwatchMetricAction Text
cmaMetricNamespace = lens _cmaMetricNamespace (\ s a -> s{_cmaMetricNamespace = a})

-- | The CloudWatch metric name.
cmaMetricName :: Lens' CloudwatchMetricAction Text
cmaMetricName = lens _cmaMetricName (\ s a -> s{_cmaMetricName = a})

-- | The CloudWatch metric value.
cmaMetricValue :: Lens' CloudwatchMetricAction Text
cmaMetricValue = lens _cmaMetricValue (\ s a -> s{_cmaMetricValue = a})

-- | The <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
cmaMetricUnit :: Lens' CloudwatchMetricAction Text
cmaMetricUnit = lens _cmaMetricUnit (\ s a -> s{_cmaMetricUnit = a})

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

instance Hashable CloudwatchMetricAction where

instance NFData CloudwatchMetricAction where

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

-- | Describes the method to use when code signing a file.
--
--
--
-- /See:/ 'codeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { _csCustomCodeSigning :: !(Maybe CustomCodeSigning)
  , _csAwsSignerJobId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeSigning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCustomCodeSigning' - A custom method for code signing a file.
--
-- * 'csAwsSignerJobId' - The ID of the AWSSignerJob which was created to sign the file.
codeSigning
    :: CodeSigning
codeSigning =
  CodeSigning' {_csCustomCodeSigning = Nothing, _csAwsSignerJobId = Nothing}


-- | A custom method for code signing a file.
csCustomCodeSigning :: Lens' CodeSigning (Maybe CustomCodeSigning)
csCustomCodeSigning = lens _csCustomCodeSigning (\ s a -> s{_csCustomCodeSigning = a})

-- | The ID of the AWSSignerJob which was created to sign the file.
csAwsSignerJobId :: Lens' CodeSigning (Maybe Text)
csAwsSignerJobId = lens _csAwsSignerJobId (\ s a -> s{_csAwsSignerJobId = a})

instance FromJSON CodeSigning where
        parseJSON
          = withObject "CodeSigning"
              (\ x ->
                 CodeSigning' <$>
                   (x .:? "customCodeSigning") <*>
                     (x .:? "awsSignerJobId"))

instance Hashable CodeSigning where

instance NFData CodeSigning where

instance ToJSON CodeSigning where
        toJSON CodeSigning'{..}
          = object
              (catMaybes
                 [("customCodeSigning" .=) <$> _csCustomCodeSigning,
                  ("awsSignerJobId" .=) <$> _csAwsSignerJobId])

-- | Describes the certificate chain being used when code signing a file.
--
--
--
-- /See:/ 'codeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { _csccStream          :: !(Maybe Stream)
  , _csccCertificateName :: !(Maybe Text)
  , _csccInlineDocument  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeSigningCertificateChain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csccStream' - A stream of the certificate chain files.
--
-- * 'csccCertificateName' - The name of the certificate.
--
-- * 'csccInlineDocument' - A base64 encoded binary representation of the code signing certificate chain.
codeSigningCertificateChain
    :: CodeSigningCertificateChain
codeSigningCertificateChain =
  CodeSigningCertificateChain'
    { _csccStream = Nothing
    , _csccCertificateName = Nothing
    , _csccInlineDocument = Nothing
    }


-- | A stream of the certificate chain files.
csccStream :: Lens' CodeSigningCertificateChain (Maybe Stream)
csccStream = lens _csccStream (\ s a -> s{_csccStream = a})

-- | The name of the certificate.
csccCertificateName :: Lens' CodeSigningCertificateChain (Maybe Text)
csccCertificateName = lens _csccCertificateName (\ s a -> s{_csccCertificateName = a})

-- | A base64 encoded binary representation of the code signing certificate chain.
csccInlineDocument :: Lens' CodeSigningCertificateChain (Maybe Text)
csccInlineDocument = lens _csccInlineDocument (\ s a -> s{_csccInlineDocument = a})

instance FromJSON CodeSigningCertificateChain where
        parseJSON
          = withObject "CodeSigningCertificateChain"
              (\ x ->
                 CodeSigningCertificateChain' <$>
                   (x .:? "stream") <*> (x .:? "certificateName") <*>
                     (x .:? "inlineDocument"))

instance Hashable CodeSigningCertificateChain where

instance NFData CodeSigningCertificateChain where

instance ToJSON CodeSigningCertificateChain where
        toJSON CodeSigningCertificateChain'{..}
          = object
              (catMaybes
                 [("stream" .=) <$> _csccStream,
                  ("certificateName" .=) <$> _csccCertificateName,
                  ("inlineDocument" .=) <$> _csccInlineDocument])

-- | Describes the signature for a file.
--
--
--
-- /See:/ 'codeSigningSignature' smart constructor.
data CodeSigningSignature = CodeSigningSignature'
  { _cssStream         :: !(Maybe Stream)
  , _cssInlineDocument :: !(Maybe Base64)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeSigningSignature' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssStream' - A stream of the code signing signature.
--
-- * 'cssInlineDocument' - A base64 encoded binary representation of the code signing signature.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
codeSigningSignature
    :: CodeSigningSignature
codeSigningSignature =
  CodeSigningSignature' {_cssStream = Nothing, _cssInlineDocument = Nothing}


-- | A stream of the code signing signature.
cssStream :: Lens' CodeSigningSignature (Maybe Stream)
cssStream = lens _cssStream (\ s a -> s{_cssStream = a})

-- | A base64 encoded binary representation of the code signing signature.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
cssInlineDocument :: Lens' CodeSigningSignature (Maybe ByteString)
cssInlineDocument = lens _cssInlineDocument (\ s a -> s{_cssInlineDocument = a}) . mapping _Base64

instance FromJSON CodeSigningSignature where
        parseJSON
          = withObject "CodeSigningSignature"
              (\ x ->
                 CodeSigningSignature' <$>
                   (x .:? "stream") <*> (x .:? "inlineDocument"))

instance Hashable CodeSigningSignature where

instance NFData CodeSigningSignature where

instance ToJSON CodeSigningSignature where
        toJSON CodeSigningSignature'{..}
          = object
              (catMaybes
                 [("stream" .=) <$> _cssStream,
                  ("inlineDocument" .=) <$> _cssInlineDocument])

-- | Configuration.
--
--
--
-- /See:/ 'configuration' smart constructor.
newtype Configuration = Configuration'
  { _cEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEnabled' - True to enable the configuration.
configuration
    :: Configuration
configuration = Configuration' {_cEnabled = Nothing}


-- | True to enable the configuration.
cEnabled :: Lens' Configuration (Maybe Bool)
cEnabled = lens _cEnabled (\ s a -> s{_cEnabled = a})

instance FromJSON Configuration where
        parseJSON
          = withObject "Configuration"
              (\ x -> Configuration' <$> (x .:? "Enabled"))

instance Hashable Configuration where

instance NFData Configuration where

instance ToJSON Configuration where
        toJSON Configuration'{..}
          = object (catMaybes [("Enabled" .=) <$> _cEnabled])

-- | Describes a custom method used to code sign a file.
--
--
--
-- /See:/ 'customCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { _ccsSignature          :: !(Maybe CodeSigningSignature)
  , _ccsHashAlgorithm      :: !(Maybe Text)
  , _ccsCertificateChain   :: !(Maybe CodeSigningCertificateChain)
  , _ccsSignatureAlgorithm :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomCodeSigning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsSignature' - The signature for the file.
--
-- * 'ccsHashAlgorithm' - The hash algorithm used to code sign the file.
--
-- * 'ccsCertificateChain' - The certificate chain.
--
-- * 'ccsSignatureAlgorithm' - The signature algorithm used to code sign the file.
customCodeSigning
    :: CustomCodeSigning
customCodeSigning =
  CustomCodeSigning'
    { _ccsSignature = Nothing
    , _ccsHashAlgorithm = Nothing
    , _ccsCertificateChain = Nothing
    , _ccsSignatureAlgorithm = Nothing
    }


-- | The signature for the file.
ccsSignature :: Lens' CustomCodeSigning (Maybe CodeSigningSignature)
ccsSignature = lens _ccsSignature (\ s a -> s{_ccsSignature = a})

-- | The hash algorithm used to code sign the file.
ccsHashAlgorithm :: Lens' CustomCodeSigning (Maybe Text)
ccsHashAlgorithm = lens _ccsHashAlgorithm (\ s a -> s{_ccsHashAlgorithm = a})

-- | The certificate chain.
ccsCertificateChain :: Lens' CustomCodeSigning (Maybe CodeSigningCertificateChain)
ccsCertificateChain = lens _ccsCertificateChain (\ s a -> s{_ccsCertificateChain = a})

-- | The signature algorithm used to code sign the file.
ccsSignatureAlgorithm :: Lens' CustomCodeSigning (Maybe Text)
ccsSignatureAlgorithm = lens _ccsSignatureAlgorithm (\ s a -> s{_ccsSignatureAlgorithm = a})

instance FromJSON CustomCodeSigning where
        parseJSON
          = withObject "CustomCodeSigning"
              (\ x ->
                 CustomCodeSigning' <$>
                   (x .:? "signature") <*> (x .:? "hashAlgorithm") <*>
                     (x .:? "certificateChain")
                     <*> (x .:? "signatureAlgorithm"))

instance Hashable CustomCodeSigning where

instance NFData CustomCodeSigning where

instance ToJSON CustomCodeSigning where
        toJSON CustomCodeSigning'{..}
          = object
              (catMaybes
                 [("signature" .=) <$> _ccsSignature,
                  ("hashAlgorithm" .=) <$> _ccsHashAlgorithm,
                  ("certificateChain" .=) <$> _ccsCertificateChain,
                  ("signatureAlgorithm" .=) <$>
                    _ccsSignatureAlgorithm])

-- | Contains information that denied the authorization.
--
--
--
-- /See:/ 'denied' smart constructor.
data Denied = Denied'
  { _dImplicitDeny :: !(Maybe ImplicitDeny)
  , _dExplicitDeny :: !(Maybe ExplicitDeny)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Denied' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dImplicitDeny' - Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- * 'dExplicitDeny' - Information that explicitly denies the authorization.
denied
    :: Denied
denied = Denied' {_dImplicitDeny = Nothing, _dExplicitDeny = Nothing}


-- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
dImplicitDeny :: Lens' Denied (Maybe ImplicitDeny)
dImplicitDeny = lens _dImplicitDeny (\ s a -> s{_dImplicitDeny = a})

-- | Information that explicitly denies the authorization.
dExplicitDeny :: Lens' Denied (Maybe ExplicitDeny)
dExplicitDeny = lens _dExplicitDeny (\ s a -> s{_dExplicitDeny = a})

instance FromJSON Denied where
        parseJSON
          = withObject "Denied"
              (\ x ->
                 Denied' <$>
                   (x .:? "implicitDeny") <*> (x .:? "explicitDeny"))

instance Hashable Denied where

instance NFData Denied where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
ddbaHashKeyType = lens _ddbaHashKeyType (\ s a -> s{_ddbaHashKeyType = a})

-- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
ddbaOperation :: Lens' DynamoDBAction (Maybe Text)
ddbaOperation = lens _ddbaOperation (\ s a -> s{_ddbaOperation = a})

-- | The range key type. Valid values are "STRING" or "NUMBER"
ddbaRangeKeyType :: Lens' DynamoDBAction (Maybe DynamoKeyType)
ddbaRangeKeyType = lens _ddbaRangeKeyType (\ s a -> s{_ddbaRangeKeyType = a})

-- | The action payload. This name can be customized.
ddbaPayloadField :: Lens' DynamoDBAction (Maybe Text)
ddbaPayloadField = lens _ddbaPayloadField (\ s a -> s{_ddbaPayloadField = a})

-- | The range key name.
ddbaRangeKeyField :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyField = lens _ddbaRangeKeyField (\ s a -> s{_ddbaRangeKeyField = a})

-- | The range key value.
ddbaRangeKeyValue :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyValue = lens _ddbaRangeKeyValue (\ s a -> s{_ddbaRangeKeyValue = a})

-- | The name of the DynamoDB table.
ddbaTableName :: Lens' DynamoDBAction Text
ddbaTableName = lens _ddbaTableName (\ s a -> s{_ddbaTableName = a})

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddbaRoleARN :: Lens' DynamoDBAction Text
ddbaRoleARN = lens _ddbaRoleARN (\ s a -> s{_ddbaRoleARN = a})

-- | The hash key name.
ddbaHashKeyField :: Lens' DynamoDBAction Text
ddbaHashKeyField = lens _ddbaHashKeyField (\ s a -> s{_ddbaHashKeyField = a})

-- | The hash key value.
ddbaHashKeyValue :: Lens' DynamoDBAction Text
ddbaHashKeyValue = lens _ddbaHashKeyValue (\ s a -> s{_ddbaHashKeyValue = a})

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

instance Hashable DynamoDBAction where

instance NFData DynamoDBAction where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  DynamoDBv2Action' {_ddaPutItem = Nothing, _ddaRoleARN = Nothing}


-- | Specifies the DynamoDB table to which the message data will be written. For example: @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@  Each attribute in the message payload will be written to a separate column in the DynamoDB database.
ddaPutItem :: Lens' DynamoDBv2Action (Maybe PutItemInput)
ddaPutItem = lens _ddaPutItem (\ s a -> s{_ddaPutItem = a})

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddaRoleARN :: Lens' DynamoDBv2Action (Maybe Text)
ddaRoleARN = lens _ddaRoleARN (\ s a -> s{_ddaRoleARN = a})

instance FromJSON DynamoDBv2Action where
        parseJSON
          = withObject "DynamoDBv2Action"
              (\ x ->
                 DynamoDBv2Action' <$>
                   (x .:? "putItem") <*> (x .:? "roleArn"))

instance Hashable DynamoDBv2Action where

instance NFData DynamoDBv2Action where

instance ToJSON DynamoDBv2Action where
        toJSON DynamoDBv2Action'{..}
          = object
              (catMaybes
                 [("putItem" .=) <$> _ddaPutItem,
                  ("roleArn" .=) <$> _ddaRoleARN])

-- | The policy that has the effect on the authorization results.
--
--
--
-- /See:/ 'effectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { _epPolicyName     :: !(Maybe Text)
  , _epPolicyDocument :: !(Maybe Text)
  , _epPolicyARN      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EffectivePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPolicyName' - The policy name.
--
-- * 'epPolicyDocument' - The IAM policy document.
--
-- * 'epPolicyARN' - The policy ARN.
effectivePolicy
    :: EffectivePolicy
effectivePolicy =
  EffectivePolicy'
    { _epPolicyName = Nothing
    , _epPolicyDocument = Nothing
    , _epPolicyARN = Nothing
    }


-- | The policy name.
epPolicyName :: Lens' EffectivePolicy (Maybe Text)
epPolicyName = lens _epPolicyName (\ s a -> s{_epPolicyName = a})

-- | The IAM policy document.
epPolicyDocument :: Lens' EffectivePolicy (Maybe Text)
epPolicyDocument = lens _epPolicyDocument (\ s a -> s{_epPolicyDocument = a})

-- | The policy ARN.
epPolicyARN :: Lens' EffectivePolicy (Maybe Text)
epPolicyARN = lens _epPolicyARN (\ s a -> s{_epPolicyARN = a})

instance FromJSON EffectivePolicy where
        parseJSON
          = withObject "EffectivePolicy"
              (\ x ->
                 EffectivePolicy' <$>
                   (x .:? "policyName") <*> (x .:? "policyDocument") <*>
                     (x .:? "policyArn"))

instance Hashable EffectivePolicy where

instance NFData EffectivePolicy where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
eaRoleARN = lens _eaRoleARN (\ s a -> s{_eaRoleARN = a})

-- | The endpoint of your Elasticsearch domain.
eaEndpoint :: Lens' ElasticsearchAction Text
eaEndpoint = lens _eaEndpoint (\ s a -> s{_eaEndpoint = a})

-- | The Elasticsearch index where you want to store your data.
eaIndex :: Lens' ElasticsearchAction Text
eaIndex = lens _eaIndex (\ s a -> s{_eaIndex = a})

-- | The type of document you are storing.
eaType :: Lens' ElasticsearchAction Text
eaType = lens _eaType (\ s a -> s{_eaType = a})

-- | The unique identifier for the document you are storing.
eaId :: Lens' ElasticsearchAction Text
eaId = lens _eaId (\ s a -> s{_eaId = a})

instance FromJSON ElasticsearchAction where
        parseJSON
          = withObject "ElasticsearchAction"
              (\ x ->
                 ElasticsearchAction' <$>
                   (x .: "roleArn") <*> (x .: "endpoint") <*>
                     (x .: "index")
                     <*> (x .: "type")
                     <*> (x .: "id"))

instance Hashable ElasticsearchAction where

instance NFData ElasticsearchAction where

instance ToJSON ElasticsearchAction where
        toJSON ElasticsearchAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _eaRoleARN),
                  Just ("endpoint" .= _eaEndpoint),
                  Just ("index" .= _eaIndex), Just ("type" .= _eaType),
                  Just ("id" .= _eaId)])

-- | Error information.
--
--
--
-- /See:/ 'errorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { _eiCode    :: !(Maybe Text)
  , _eiMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiCode' - The error code.
--
-- * 'eiMessage' - The error message.
errorInfo
    :: ErrorInfo
errorInfo = ErrorInfo' {_eiCode = Nothing, _eiMessage = Nothing}


-- | The error code.
eiCode :: Lens' ErrorInfo (Maybe Text)
eiCode = lens _eiCode (\ s a -> s{_eiCode = a})

-- | The error message.
eiMessage :: Lens' ErrorInfo (Maybe Text)
eiMessage = lens _eiMessage (\ s a -> s{_eiMessage = a})

instance FromJSON ErrorInfo where
        parseJSON
          = withObject "ErrorInfo"
              (\ x ->
                 ErrorInfo' <$> (x .:? "code") <*> (x .:? "message"))

instance Hashable ErrorInfo where

instance NFData ErrorInfo where

-- | Information that explicitly denies authorization.
--
--
--
-- /See:/ 'explicitDeny' smart constructor.
newtype ExplicitDeny = ExplicitDeny'
  { _edPolicies :: Maybe [Policy]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExplicitDeny' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edPolicies' - The policies that denied the authorization.
explicitDeny
    :: ExplicitDeny
explicitDeny = ExplicitDeny' {_edPolicies = Nothing}


-- | The policies that denied the authorization.
edPolicies :: Lens' ExplicitDeny [Policy]
edPolicies = lens _edPolicies (\ s a -> s{_edPolicies = a}) . _Default . _Coerce

instance FromJSON ExplicitDeny where
        parseJSON
          = withObject "ExplicitDeny"
              (\ x ->
                 ExplicitDeny' <$> (x .:? "policies" .!= mempty))

instance Hashable ExplicitDeny where

instance NFData ExplicitDeny where

-- | Describes an action that writes data to an Amazon Kinesis Firehose stream.
--
--
--
-- /See:/ 'firehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { _faSeparator          :: !(Maybe Text)
  , _faRoleARN            :: !Text
  , _faDeliveryStreamName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FirehoseAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faSeparator' - A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
--
-- * 'faRoleARN' - The IAM role that grants access to the Amazon Kinesis Firehose stream.
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
faSeparator = lens _faSeparator (\ s a -> s{_faSeparator = a})

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
faRoleARN :: Lens' FirehoseAction Text
faRoleARN = lens _faRoleARN (\ s a -> s{_faRoleARN = a})

-- | The delivery stream name.
faDeliveryStreamName :: Lens' FirehoseAction Text
faDeliveryStreamName = lens _faDeliveryStreamName (\ s a -> s{_faDeliveryStreamName = a})

instance FromJSON FirehoseAction where
        parseJSON
          = withObject "FirehoseAction"
              (\ x ->
                 FirehoseAction' <$>
                   (x .:? "separator") <*> (x .: "roleArn") <*>
                     (x .: "deliveryStreamName"))

instance Hashable FirehoseAction where

instance NFData FirehoseAction where

instance ToJSON FirehoseAction where
        toJSON FirehoseAction'{..}
          = object
              (catMaybes
                 [("separator" .=) <$> _faSeparator,
                  Just ("roleArn" .= _faRoleARN),
                  Just
                    ("deliveryStreamName" .= _faDeliveryStreamName)])

-- | The name and ARN of a group.
--
--
--
-- /See:/ 'groupNameAndARN' smart constructor.
data GroupNameAndARN = GroupNameAndARN'
  { _gnaaGroupARN  :: !(Maybe Text)
  , _gnaaGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupNameAndARN' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnaaGroupARN' - The group ARN.
--
-- * 'gnaaGroupName' - The group name.
groupNameAndARN
    :: GroupNameAndARN
groupNameAndARN =
  GroupNameAndARN' {_gnaaGroupARN = Nothing, _gnaaGroupName = Nothing}


-- | The group ARN.
gnaaGroupARN :: Lens' GroupNameAndARN (Maybe Text)
gnaaGroupARN = lens _gnaaGroupARN (\ s a -> s{_gnaaGroupARN = a})

-- | The group name.
gnaaGroupName :: Lens' GroupNameAndARN (Maybe Text)
gnaaGroupName = lens _gnaaGroupName (\ s a -> s{_gnaaGroupName = a})

instance FromJSON GroupNameAndARN where
        parseJSON
          = withObject "GroupNameAndARN"
              (\ x ->
                 GroupNameAndARN' <$>
                   (x .:? "groupArn") <*> (x .:? "groupName"))

instance Hashable GroupNameAndARN where

instance NFData GroupNameAndARN where

-- | Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
--
--
-- /See:/ 'implicitDeny' smart constructor.
newtype ImplicitDeny = ImplicitDeny'
  { _idPolicies :: Maybe [Policy]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImplicitDeny' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idPolicies' - Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
implicitDeny
    :: ImplicitDeny
implicitDeny = ImplicitDeny' {_idPolicies = Nothing}


-- | Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
idPolicies :: Lens' ImplicitDeny [Policy]
idPolicies = lens _idPolicies (\ s a -> s{_idPolicies = a}) . _Default . _Coerce

instance FromJSON ImplicitDeny where
        parseJSON
          = withObject "ImplicitDeny"
              (\ x ->
                 ImplicitDeny' <$> (x .:? "policies" .!= mempty))

instance Hashable ImplicitDeny where

instance NFData ImplicitDeny where

-- | Sends message data to an AWS IoT Analytics channel.
--
--
--
-- /See:/ 'iotAnalyticsAction' smart constructor.
data IotAnalyticsAction = IotAnalyticsAction'
  { _iaaChannelARN  :: !(Maybe Text)
  , _iaaChannelName :: !(Maybe Text)
  , _iaaRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IotAnalyticsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaaChannelARN' - (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
--
-- * 'iaaChannelName' - The name of the IoT Analytics channel to which message data will be sent.
--
-- * 'iaaRoleARN' - The ARN of the role which has a policy that grants IoT permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
iotAnalyticsAction
    :: IotAnalyticsAction
iotAnalyticsAction =
  IotAnalyticsAction'
    {_iaaChannelARN = Nothing, _iaaChannelName = Nothing, _iaaRoleARN = Nothing}


-- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
iaaChannelARN :: Lens' IotAnalyticsAction (Maybe Text)
iaaChannelARN = lens _iaaChannelARN (\ s a -> s{_iaaChannelARN = a})

-- | The name of the IoT Analytics channel to which message data will be sent.
iaaChannelName :: Lens' IotAnalyticsAction (Maybe Text)
iaaChannelName = lens _iaaChannelName (\ s a -> s{_iaaChannelName = a})

-- | The ARN of the role which has a policy that grants IoT permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
iaaRoleARN :: Lens' IotAnalyticsAction (Maybe Text)
iaaRoleARN = lens _iaaRoleARN (\ s a -> s{_iaaRoleARN = a})

instance FromJSON IotAnalyticsAction where
        parseJSON
          = withObject "IotAnalyticsAction"
              (\ x ->
                 IotAnalyticsAction' <$>
                   (x .:? "channelArn") <*> (x .:? "channelName") <*>
                     (x .:? "roleArn"))

instance Hashable IotAnalyticsAction where

instance NFData IotAnalyticsAction where

instance ToJSON IotAnalyticsAction where
        toJSON IotAnalyticsAction'{..}
          = object
              (catMaybes
                 [("channelArn" .=) <$> _iaaChannelARN,
                  ("channelName" .=) <$> _iaaChannelName,
                  ("roleArn" .=) <$> _iaaRoleARN])

-- | The @Job@ object contains details about a job.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jobStatus                     :: !(Maybe JobStatus)
  , _jobJobExecutionsRolloutConfig :: !(Maybe JobExecutionsRolloutConfig)
  , _jobJobId                      :: !(Maybe Text)
  , _jobLastUpdatedAt              :: !(Maybe POSIX)
  , _jobJobARN                     :: !(Maybe Text)
  , _jobCreatedAt                  :: !(Maybe POSIX)
  , _jobDocumentParameters         :: !(Maybe (Map Text Text))
  , _jobJobProcessDetails          :: !(Maybe JobProcessDetails)
  , _jobPresignedURLConfig         :: !(Maybe PresignedURLConfig)
  , _jobTargets                    :: !(Maybe (List1 Text))
  , _jobCompletedAt                :: !(Maybe POSIX)
  , _jobComment                    :: !(Maybe Text)
  , _jobDescription                :: !(Maybe Text)
  , _jobTargetSelection            :: !(Maybe TargetSelection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobStatus' - The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , or @COMPLETED@ .
--
-- * 'jobJobExecutionsRolloutConfig' - Allows you to create a staged rollout of a job.
--
-- * 'jobJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jobLastUpdatedAt' - The time, in milliseconds since the epoch, when the job was last updated.
--
-- * 'jobJobARN' - An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
--
-- * 'jobCreatedAt' - The time, in milliseconds since the epoch, when the job was created.
--
-- * 'jobDocumentParameters' - The parameters specified for the job document.
--
-- * 'jobJobProcessDetails' - Details about the job process.
--
-- * 'jobPresignedURLConfig' - Configuration for pre-signed S3 URLs.
--
-- * 'jobTargets' - A list of IoT things and thing groups to which the job should be sent.
--
-- * 'jobCompletedAt' - The time, in milliseconds since the epoch, when the job was completed.
--
-- * 'jobComment' - If the job was updated, describes the reason for the update.
--
-- * 'jobDescription' - A short text description of the job.
--
-- * 'jobTargetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
job
    :: Job
job =
  Job'
    { _jobStatus = Nothing
    , _jobJobExecutionsRolloutConfig = Nothing
    , _jobJobId = Nothing
    , _jobLastUpdatedAt = Nothing
    , _jobJobARN = Nothing
    , _jobCreatedAt = Nothing
    , _jobDocumentParameters = Nothing
    , _jobJobProcessDetails = Nothing
    , _jobPresignedURLConfig = Nothing
    , _jobTargets = Nothing
    , _jobCompletedAt = Nothing
    , _jobComment = Nothing
    , _jobDescription = Nothing
    , _jobTargetSelection = Nothing
    }


-- | The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , or @COMPLETED@ .
jobStatus :: Lens' Job (Maybe JobStatus)
jobStatus = lens _jobStatus (\ s a -> s{_jobStatus = a})

-- | Allows you to create a staged rollout of a job.
jobJobExecutionsRolloutConfig :: Lens' Job (Maybe JobExecutionsRolloutConfig)
jobJobExecutionsRolloutConfig = lens _jobJobExecutionsRolloutConfig (\ s a -> s{_jobJobExecutionsRolloutConfig = a})

-- | The unique identifier you assigned to this job when it was created.
jobJobId :: Lens' Job (Maybe Text)
jobJobId = lens _jobJobId (\ s a -> s{_jobJobId = a})

-- | The time, in milliseconds since the epoch, when the job was last updated.
jobLastUpdatedAt :: Lens' Job (Maybe UTCTime)
jobLastUpdatedAt = lens _jobLastUpdatedAt (\ s a -> s{_jobLastUpdatedAt = a}) . mapping _Time

-- | An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
jobJobARN :: Lens' Job (Maybe Text)
jobJobARN = lens _jobJobARN (\ s a -> s{_jobJobARN = a})

-- | The time, in milliseconds since the epoch, when the job was created.
jobCreatedAt :: Lens' Job (Maybe UTCTime)
jobCreatedAt = lens _jobCreatedAt (\ s a -> s{_jobCreatedAt = a}) . mapping _Time

-- | The parameters specified for the job document.
jobDocumentParameters :: Lens' Job (HashMap Text Text)
jobDocumentParameters = lens _jobDocumentParameters (\ s a -> s{_jobDocumentParameters = a}) . _Default . _Map

-- | Details about the job process.
jobJobProcessDetails :: Lens' Job (Maybe JobProcessDetails)
jobJobProcessDetails = lens _jobJobProcessDetails (\ s a -> s{_jobJobProcessDetails = a})

-- | Configuration for pre-signed S3 URLs.
jobPresignedURLConfig :: Lens' Job (Maybe PresignedURLConfig)
jobPresignedURLConfig = lens _jobPresignedURLConfig (\ s a -> s{_jobPresignedURLConfig = a})

-- | A list of IoT things and thing groups to which the job should be sent.
jobTargets :: Lens' Job (Maybe (NonEmpty Text))
jobTargets = lens _jobTargets (\ s a -> s{_jobTargets = a}) . mapping _List1

-- | The time, in milliseconds since the epoch, when the job was completed.
jobCompletedAt :: Lens' Job (Maybe UTCTime)
jobCompletedAt = lens _jobCompletedAt (\ s a -> s{_jobCompletedAt = a}) . mapping _Time

-- | If the job was updated, describes the reason for the update.
jobComment :: Lens' Job (Maybe Text)
jobComment = lens _jobComment (\ s a -> s{_jobComment = a})

-- | A short text description of the job.
jobDescription :: Lens' Job (Maybe Text)
jobDescription = lens _jobDescription (\ s a -> s{_jobDescription = a})

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
jobTargetSelection :: Lens' Job (Maybe TargetSelection)
jobTargetSelection = lens _jobTargetSelection (\ s a -> s{_jobTargetSelection = a})

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "status") <*>
                     (x .:? "jobExecutionsRolloutConfig")
                     <*> (x .:? "jobId")
                     <*> (x .:? "lastUpdatedAt")
                     <*> (x .:? "jobArn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "documentParameters" .!= mempty)
                     <*> (x .:? "jobProcessDetails")
                     <*> (x .:? "presignedUrlConfig")
                     <*> (x .:? "targets")
                     <*> (x .:? "completedAt")
                     <*> (x .:? "comment")
                     <*> (x .:? "description")
                     <*> (x .:? "targetSelection"))

instance Hashable Job where

instance NFData Job where

-- | The job execution object represents the execution of a job on a particular device.
--
--
--
-- /See:/ 'jobExecution' smart constructor.
data JobExecution = JobExecution'
  { _jeStatus          :: !(Maybe JobExecutionStatus)
  , _jeJobId           :: !(Maybe Text)
  , _jeLastUpdatedAt   :: !(Maybe POSIX)
  , _jeQueuedAt        :: !(Maybe POSIX)
  , _jeStatusDetails   :: !(Maybe JobExecutionStatusDetails)
  , _jeThingARN        :: !(Maybe Text)
  , _jeExecutionNumber :: !(Maybe Integer)
  , _jeStartedAt       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jeStatus' - The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCESS, CANCELED, or REJECTED).
--
-- * 'jeJobId' - The unique identifier you assigned to the job when it was created.
--
-- * 'jeLastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- * 'jeQueuedAt' - The time, in milliseconds since the epoch, when the job execution was queued.
--
-- * 'jeStatusDetails' - A collection of name/value pairs that describe the status of the job execution.
--
-- * 'jeThingARN' - The ARN of the thing on which the job execution is running.
--
-- * 'jeExecutionNumber' - A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information.
--
-- * 'jeStartedAt' - The time, in milliseconds since the epoch, when the job execution started.
jobExecution
    :: JobExecution
jobExecution =
  JobExecution'
    { _jeStatus = Nothing
    , _jeJobId = Nothing
    , _jeLastUpdatedAt = Nothing
    , _jeQueuedAt = Nothing
    , _jeStatusDetails = Nothing
    , _jeThingARN = Nothing
    , _jeExecutionNumber = Nothing
    , _jeStartedAt = Nothing
    }


-- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCESS, CANCELED, or REJECTED).
jeStatus :: Lens' JobExecution (Maybe JobExecutionStatus)
jeStatus = lens _jeStatus (\ s a -> s{_jeStatus = a})

-- | The unique identifier you assigned to the job when it was created.
jeJobId :: Lens' JobExecution (Maybe Text)
jeJobId = lens _jeJobId (\ s a -> s{_jeJobId = a})

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
jeLastUpdatedAt :: Lens' JobExecution (Maybe UTCTime)
jeLastUpdatedAt = lens _jeLastUpdatedAt (\ s a -> s{_jeLastUpdatedAt = a}) . mapping _Time

-- | The time, in milliseconds since the epoch, when the job execution was queued.
jeQueuedAt :: Lens' JobExecution (Maybe UTCTime)
jeQueuedAt = lens _jeQueuedAt (\ s a -> s{_jeQueuedAt = a}) . mapping _Time

-- | A collection of name/value pairs that describe the status of the job execution.
jeStatusDetails :: Lens' JobExecution (Maybe JobExecutionStatusDetails)
jeStatusDetails = lens _jeStatusDetails (\ s a -> s{_jeStatusDetails = a})

-- | The ARN of the thing on which the job execution is running.
jeThingARN :: Lens' JobExecution (Maybe Text)
jeThingARN = lens _jeThingARN (\ s a -> s{_jeThingARN = a})

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information.
jeExecutionNumber :: Lens' JobExecution (Maybe Integer)
jeExecutionNumber = lens _jeExecutionNumber (\ s a -> s{_jeExecutionNumber = a})

-- | The time, in milliseconds since the epoch, when the job execution started.
jeStartedAt :: Lens' JobExecution (Maybe UTCTime)
jeStartedAt = lens _jeStartedAt (\ s a -> s{_jeStartedAt = a}) . mapping _Time

instance FromJSON JobExecution where
        parseJSON
          = withObject "JobExecution"
              (\ x ->
                 JobExecution' <$>
                   (x .:? "status") <*> (x .:? "jobId") <*>
                     (x .:? "lastUpdatedAt")
                     <*> (x .:? "queuedAt")
                     <*> (x .:? "statusDetails")
                     <*> (x .:? "thingArn")
                     <*> (x .:? "executionNumber")
                     <*> (x .:? "startedAt"))

instance Hashable JobExecution where

instance NFData JobExecution where

-- | Details of the job execution status.
--
--
--
-- /See:/ 'jobExecutionStatusDetails' smart constructor.
newtype JobExecutionStatusDetails = JobExecutionStatusDetails'
  { _jesdDetailsMap :: Maybe (Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesdDetailsMap' - The job execution status.
jobExecutionStatusDetails
    :: JobExecutionStatusDetails
jobExecutionStatusDetails =
  JobExecutionStatusDetails' {_jesdDetailsMap = Nothing}


-- | The job execution status.
jesdDetailsMap :: Lens' JobExecutionStatusDetails (HashMap Text Text)
jesdDetailsMap = lens _jesdDetailsMap (\ s a -> s{_jesdDetailsMap = a}) . _Default . _Map

instance FromJSON JobExecutionStatusDetails where
        parseJSON
          = withObject "JobExecutionStatusDetails"
              (\ x ->
                 JobExecutionStatusDetails' <$>
                   (x .:? "detailsMap" .!= mempty))

instance Hashable JobExecutionStatusDetails where

instance NFData JobExecutionStatusDetails where

-- | The job execution summary.
--
--
--
-- /See:/ 'jobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { _jesStatus          :: !(Maybe JobExecutionStatus)
  , _jesLastUpdatedAt   :: !(Maybe POSIX)
  , _jesQueuedAt        :: !(Maybe POSIX)
  , _jesExecutionNumber :: !(Maybe Integer)
  , _jesStartedAt       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesStatus' - The status of the job execution.
--
-- * 'jesLastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- * 'jesQueuedAt' - The time, in milliseconds since the epoch, when the job execution was queued.
--
-- * 'jesExecutionNumber' - A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
--
-- * 'jesStartedAt' - The time, in milliseconds since the epoch, when the job execution started.
jobExecutionSummary
    :: JobExecutionSummary
jobExecutionSummary =
  JobExecutionSummary'
    { _jesStatus = Nothing
    , _jesLastUpdatedAt = Nothing
    , _jesQueuedAt = Nothing
    , _jesExecutionNumber = Nothing
    , _jesStartedAt = Nothing
    }


-- | The status of the job execution.
jesStatus :: Lens' JobExecutionSummary (Maybe JobExecutionStatus)
jesStatus = lens _jesStatus (\ s a -> s{_jesStatus = a})

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
jesLastUpdatedAt :: Lens' JobExecutionSummary (Maybe UTCTime)
jesLastUpdatedAt = lens _jesLastUpdatedAt (\ s a -> s{_jesLastUpdatedAt = a}) . mapping _Time

-- | The time, in milliseconds since the epoch, when the job execution was queued.
jesQueuedAt :: Lens' JobExecutionSummary (Maybe UTCTime)
jesQueuedAt = lens _jesQueuedAt (\ s a -> s{_jesQueuedAt = a}) . mapping _Time

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
jesExecutionNumber :: Lens' JobExecutionSummary (Maybe Integer)
jesExecutionNumber = lens _jesExecutionNumber (\ s a -> s{_jesExecutionNumber = a})

-- | The time, in milliseconds since the epoch, when the job execution started.
jesStartedAt :: Lens' JobExecutionSummary (Maybe UTCTime)
jesStartedAt = lens _jesStartedAt (\ s a -> s{_jesStartedAt = a}) . mapping _Time

instance FromJSON JobExecutionSummary where
        parseJSON
          = withObject "JobExecutionSummary"
              (\ x ->
                 JobExecutionSummary' <$>
                   (x .:? "status") <*> (x .:? "lastUpdatedAt") <*>
                     (x .:? "queuedAt")
                     <*> (x .:? "executionNumber")
                     <*> (x .:? "startedAt"))

instance Hashable JobExecutionSummary where

instance NFData JobExecutionSummary where

-- | Contains a summary of information about job executions for a specific job.
--
--
--
-- /See:/ 'jobExecutionSummaryForJob' smart constructor.
data JobExecutionSummaryForJob = JobExecutionSummaryForJob'
  { _jesfjJobExecutionSummary :: !(Maybe JobExecutionSummary)
  , _jesfjThingARN            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionSummaryForJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesfjJobExecutionSummary' - Contains a subset of information about a job execution.
--
-- * 'jesfjThingARN' - The ARN of the thing on which the job execution is running.
jobExecutionSummaryForJob
    :: JobExecutionSummaryForJob
jobExecutionSummaryForJob =
  JobExecutionSummaryForJob'
    {_jesfjJobExecutionSummary = Nothing, _jesfjThingARN = Nothing}


-- | Contains a subset of information about a job execution.
jesfjJobExecutionSummary :: Lens' JobExecutionSummaryForJob (Maybe JobExecutionSummary)
jesfjJobExecutionSummary = lens _jesfjJobExecutionSummary (\ s a -> s{_jesfjJobExecutionSummary = a})

-- | The ARN of the thing on which the job execution is running.
jesfjThingARN :: Lens' JobExecutionSummaryForJob (Maybe Text)
jesfjThingARN = lens _jesfjThingARN (\ s a -> s{_jesfjThingARN = a})

instance FromJSON JobExecutionSummaryForJob where
        parseJSON
          = withObject "JobExecutionSummaryForJob"
              (\ x ->
                 JobExecutionSummaryForJob' <$>
                   (x .:? "jobExecutionSummary") <*> (x .:? "thingArn"))

instance Hashable JobExecutionSummaryForJob where

instance NFData JobExecutionSummaryForJob where

-- | The job execution summary for a thing.
--
--
--
-- /See:/ 'jobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { _jesftJobId               :: !(Maybe Text)
  , _jesftJobExecutionSummary :: !(Maybe JobExecutionSummary)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionSummaryForThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesftJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jesftJobExecutionSummary' - Contains a subset of information about a job execution.
jobExecutionSummaryForThing
    :: JobExecutionSummaryForThing
jobExecutionSummaryForThing =
  JobExecutionSummaryForThing'
    {_jesftJobId = Nothing, _jesftJobExecutionSummary = Nothing}


-- | The unique identifier you assigned to this job when it was created.
jesftJobId :: Lens' JobExecutionSummaryForThing (Maybe Text)
jesftJobId = lens _jesftJobId (\ s a -> s{_jesftJobId = a})

-- | Contains a subset of information about a job execution.
jesftJobExecutionSummary :: Lens' JobExecutionSummaryForThing (Maybe JobExecutionSummary)
jesftJobExecutionSummary = lens _jesftJobExecutionSummary (\ s a -> s{_jesftJobExecutionSummary = a})

instance FromJSON JobExecutionSummaryForThing where
        parseJSON
          = withObject "JobExecutionSummaryForThing"
              (\ x ->
                 JobExecutionSummaryForThing' <$>
                   (x .:? "jobId") <*> (x .:? "jobExecutionSummary"))

instance Hashable JobExecutionSummaryForThing where

instance NFData JobExecutionSummaryForThing where

-- | Allows you to create a staged rollout of a job.
--
--
--
-- /See:/ 'jobExecutionsRolloutConfig' smart constructor.
newtype JobExecutionsRolloutConfig = JobExecutionsRolloutConfig'
  { _jercMaximumPerMinute :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jercMaximumPerMinute' - The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
jobExecutionsRolloutConfig
    :: JobExecutionsRolloutConfig
jobExecutionsRolloutConfig =
  JobExecutionsRolloutConfig' {_jercMaximumPerMinute = Nothing}


-- | The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
jercMaximumPerMinute :: Lens' JobExecutionsRolloutConfig (Maybe Natural)
jercMaximumPerMinute = lens _jercMaximumPerMinute (\ s a -> s{_jercMaximumPerMinute = a}) . mapping _Nat

instance FromJSON JobExecutionsRolloutConfig where
        parseJSON
          = withObject "JobExecutionsRolloutConfig"
              (\ x ->
                 JobExecutionsRolloutConfig' <$>
                   (x .:? "maximumPerMinute"))

instance Hashable JobExecutionsRolloutConfig where

instance NFData JobExecutionsRolloutConfig where

instance ToJSON JobExecutionsRolloutConfig where
        toJSON JobExecutionsRolloutConfig'{..}
          = object
              (catMaybes
                 [("maximumPerMinute" .=) <$> _jercMaximumPerMinute])

-- | The job process details.
--
--
--
-- /See:/ 'jobProcessDetails' smart constructor.
data JobProcessDetails = JobProcessDetails'
  { _jpdNumberOfRemovedThings    :: !(Maybe Int)
  , _jpdNumberOfQueuedThings     :: !(Maybe Int)
  , _jpdNumberOfFailedThings     :: !(Maybe Int)
  , _jpdNumberOfSucceededThings  :: !(Maybe Int)
  , _jpdNumberOfInProgressThings :: !(Maybe Int)
  , _jpdNumberOfCanceledThings   :: !(Maybe Int)
  , _jpdNumberOfRejectedThings   :: !(Maybe Int)
  , _jpdProcessingTargets        :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobProcessDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jpdNumberOfRemovedThings' - The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
--
-- * 'jpdNumberOfQueuedThings' - The number of things that are awaiting execution of the job.
--
-- * 'jpdNumberOfFailedThings' - The number of things that failed executing the job.
--
-- * 'jpdNumberOfSucceededThings' - The number of things which successfully completed the job.
--
-- * 'jpdNumberOfInProgressThings' - The number of things currently executing the job.
--
-- * 'jpdNumberOfCanceledThings' - The number of things that cancelled the job.
--
-- * 'jpdNumberOfRejectedThings' - The number of things that rejected the job.
--
-- * 'jpdProcessingTargets' - The devices on which the job is executing.
jobProcessDetails
    :: JobProcessDetails
jobProcessDetails =
  JobProcessDetails'
    { _jpdNumberOfRemovedThings = Nothing
    , _jpdNumberOfQueuedThings = Nothing
    , _jpdNumberOfFailedThings = Nothing
    , _jpdNumberOfSucceededThings = Nothing
    , _jpdNumberOfInProgressThings = Nothing
    , _jpdNumberOfCanceledThings = Nothing
    , _jpdNumberOfRejectedThings = Nothing
    , _jpdProcessingTargets = Nothing
    }


-- | The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
jpdNumberOfRemovedThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfRemovedThings = lens _jpdNumberOfRemovedThings (\ s a -> s{_jpdNumberOfRemovedThings = a})

-- | The number of things that are awaiting execution of the job.
jpdNumberOfQueuedThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfQueuedThings = lens _jpdNumberOfQueuedThings (\ s a -> s{_jpdNumberOfQueuedThings = a})

-- | The number of things that failed executing the job.
jpdNumberOfFailedThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfFailedThings = lens _jpdNumberOfFailedThings (\ s a -> s{_jpdNumberOfFailedThings = a})

-- | The number of things which successfully completed the job.
jpdNumberOfSucceededThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfSucceededThings = lens _jpdNumberOfSucceededThings (\ s a -> s{_jpdNumberOfSucceededThings = a})

-- | The number of things currently executing the job.
jpdNumberOfInProgressThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfInProgressThings = lens _jpdNumberOfInProgressThings (\ s a -> s{_jpdNumberOfInProgressThings = a})

-- | The number of things that cancelled the job.
jpdNumberOfCanceledThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfCanceledThings = lens _jpdNumberOfCanceledThings (\ s a -> s{_jpdNumberOfCanceledThings = a})

-- | The number of things that rejected the job.
jpdNumberOfRejectedThings :: Lens' JobProcessDetails (Maybe Int)
jpdNumberOfRejectedThings = lens _jpdNumberOfRejectedThings (\ s a -> s{_jpdNumberOfRejectedThings = a})

-- | The devices on which the job is executing.
jpdProcessingTargets :: Lens' JobProcessDetails [Text]
jpdProcessingTargets = lens _jpdProcessingTargets (\ s a -> s{_jpdProcessingTargets = a}) . _Default . _Coerce

instance FromJSON JobProcessDetails where
        parseJSON
          = withObject "JobProcessDetails"
              (\ x ->
                 JobProcessDetails' <$>
                   (x .:? "numberOfRemovedThings") <*>
                     (x .:? "numberOfQueuedThings")
                     <*> (x .:? "numberOfFailedThings")
                     <*> (x .:? "numberOfSucceededThings")
                     <*> (x .:? "numberOfInProgressThings")
                     <*> (x .:? "numberOfCanceledThings")
                     <*> (x .:? "numberOfRejectedThings")
                     <*> (x .:? "processingTargets" .!= mempty))

instance Hashable JobProcessDetails where

instance NFData JobProcessDetails where

-- | The job summary.
--
--
--
-- /See:/ 'jobSummary' smart constructor.
data JobSummary = JobSummary'
  { _jsStatus          :: !(Maybe JobStatus)
  , _jsJobId           :: !(Maybe Text)
  , _jsLastUpdatedAt   :: !(Maybe POSIX)
  , _jsJobARN          :: !(Maybe Text)
  , _jsCreatedAt       :: !(Maybe POSIX)
  , _jsThingGroupId    :: !(Maybe Text)
  , _jsCompletedAt     :: !(Maybe POSIX)
  , _jsTargetSelection :: !(Maybe TargetSelection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsStatus' - The job summary status.
--
-- * 'jsJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jsLastUpdatedAt' - The time, in milliseconds since the epoch, when the job was last updated.
--
-- * 'jsJobARN' - The job ARN.
--
-- * 'jsCreatedAt' - The time, in milliseconds since the epoch, when the job was created.
--
-- * 'jsThingGroupId' - The ID of the thing group.
--
-- * 'jsCompletedAt' - The time, in milliseconds since the epoch, when the job completed.
--
-- * 'jsTargetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
jobSummary
    :: JobSummary
jobSummary =
  JobSummary'
    { _jsStatus = Nothing
    , _jsJobId = Nothing
    , _jsLastUpdatedAt = Nothing
    , _jsJobARN = Nothing
    , _jsCreatedAt = Nothing
    , _jsThingGroupId = Nothing
    , _jsCompletedAt = Nothing
    , _jsTargetSelection = Nothing
    }


-- | The job summary status.
jsStatus :: Lens' JobSummary (Maybe JobStatus)
jsStatus = lens _jsStatus (\ s a -> s{_jsStatus = a})

-- | The unique identifier you assigned to this job when it was created.
jsJobId :: Lens' JobSummary (Maybe Text)
jsJobId = lens _jsJobId (\ s a -> s{_jsJobId = a})

-- | The time, in milliseconds since the epoch, when the job was last updated.
jsLastUpdatedAt :: Lens' JobSummary (Maybe UTCTime)
jsLastUpdatedAt = lens _jsLastUpdatedAt (\ s a -> s{_jsLastUpdatedAt = a}) . mapping _Time

-- | The job ARN.
jsJobARN :: Lens' JobSummary (Maybe Text)
jsJobARN = lens _jsJobARN (\ s a -> s{_jsJobARN = a})

-- | The time, in milliseconds since the epoch, when the job was created.
jsCreatedAt :: Lens' JobSummary (Maybe UTCTime)
jsCreatedAt = lens _jsCreatedAt (\ s a -> s{_jsCreatedAt = a}) . mapping _Time

-- | The ID of the thing group.
jsThingGroupId :: Lens' JobSummary (Maybe Text)
jsThingGroupId = lens _jsThingGroupId (\ s a -> s{_jsThingGroupId = a})

-- | The time, in milliseconds since the epoch, when the job completed.
jsCompletedAt :: Lens' JobSummary (Maybe UTCTime)
jsCompletedAt = lens _jsCompletedAt (\ s a -> s{_jsCompletedAt = a}) . mapping _Time

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
jsTargetSelection :: Lens' JobSummary (Maybe TargetSelection)
jsTargetSelection = lens _jsTargetSelection (\ s a -> s{_jsTargetSelection = a})

instance FromJSON JobSummary where
        parseJSON
          = withObject "JobSummary"
              (\ x ->
                 JobSummary' <$>
                   (x .:? "status") <*> (x .:? "jobId") <*>
                     (x .:? "lastUpdatedAt")
                     <*> (x .:? "jobArn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "thingGroupId")
                     <*> (x .:? "completedAt")
                     <*> (x .:? "targetSelection"))

instance Hashable JobSummary where

instance NFData JobSummary where

-- | Describes a key pair.
--
--
--
-- /See:/ 'keyPair' smart constructor.
data KeyPair = KeyPair'
  { _kpPrivateKey :: !(Maybe (Sensitive Text))
  , _kpPublicKey  :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpPrivateKey' - The private key.
--
-- * 'kpPublicKey' - The public key.
keyPair
    :: KeyPair
keyPair = KeyPair' {_kpPrivateKey = Nothing, _kpPublicKey = Nothing}


-- | The private key.
kpPrivateKey :: Lens' KeyPair (Maybe Text)
kpPrivateKey = lens _kpPrivateKey (\ s a -> s{_kpPrivateKey = a}) . mapping _Sensitive

-- | The public key.
kpPublicKey :: Lens' KeyPair (Maybe Text)
kpPublicKey = lens _kpPublicKey (\ s a -> s{_kpPublicKey = a})

instance FromJSON KeyPair where
        parseJSON
          = withObject "KeyPair"
              (\ x ->
                 KeyPair' <$>
                   (x .:? "PrivateKey") <*> (x .:? "PublicKey"))

instance Hashable KeyPair where

instance NFData KeyPair where

-- | Describes an action to write data to an Amazon Kinesis stream.
--
--
--
-- /See:/ 'kinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { _kaPartitionKey :: !(Maybe Text)
  , _kaRoleARN      :: !Text
  , _kaStreamName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
kaPartitionKey = lens _kaPartitionKey (\ s a -> s{_kaPartitionKey = a})

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
kaRoleARN :: Lens' KinesisAction Text
kaRoleARN = lens _kaRoleARN (\ s a -> s{_kaRoleARN = a})

-- | The name of the Amazon Kinesis stream.
kaStreamName :: Lens' KinesisAction Text
kaStreamName = lens _kaStreamName (\ s a -> s{_kaStreamName = a})

instance FromJSON KinesisAction where
        parseJSON
          = withObject "KinesisAction"
              (\ x ->
                 KinesisAction' <$>
                   (x .:? "partitionKey") <*> (x .: "roleArn") <*>
                     (x .: "streamName"))

instance Hashable KinesisAction where

instance NFData KinesisAction where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laFunctionARN' - The ARN of the Lambda function.
lambdaAction
    :: Text -- ^ 'laFunctionARN'
    -> LambdaAction
lambdaAction pFunctionARN_ = LambdaAction' {_laFunctionARN = pFunctionARN_}


-- | The ARN of the Lambda function.
laFunctionARN :: Lens' LambdaAction Text
laFunctionARN = lens _laFunctionARN (\ s a -> s{_laFunctionARN = a})

instance FromJSON LambdaAction where
        parseJSON
          = withObject "LambdaAction"
              (\ x -> LambdaAction' <$> (x .: "functionArn"))

instance Hashable LambdaAction where

instance NFData LambdaAction where

instance ToJSON LambdaAction where
        toJSON LambdaAction'{..}
          = object
              (catMaybes [Just ("functionArn" .= _laFunctionARN)])

-- | A log target.
--
--
--
-- /See:/ 'logTarget' smart constructor.
data LogTarget = LogTarget'
  { _ltTargetName :: !(Maybe Text)
  , _ltTargetType :: !LogTargetType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltTargetName' - The target name.
--
-- * 'ltTargetType' - The target type.
logTarget
    :: LogTargetType -- ^ 'ltTargetType'
    -> LogTarget
logTarget pTargetType_ =
  LogTarget' {_ltTargetName = Nothing, _ltTargetType = pTargetType_}


-- | The target name.
ltTargetName :: Lens' LogTarget (Maybe Text)
ltTargetName = lens _ltTargetName (\ s a -> s{_ltTargetName = a})

-- | The target type.
ltTargetType :: Lens' LogTarget LogTargetType
ltTargetType = lens _ltTargetType (\ s a -> s{_ltTargetType = a})

instance FromJSON LogTarget where
        parseJSON
          = withObject "LogTarget"
              (\ x ->
                 LogTarget' <$>
                   (x .:? "targetName") <*> (x .: "targetType"))

instance Hashable LogTarget where

instance NFData LogTarget where

instance ToJSON LogTarget where
        toJSON LogTarget'{..}
          = object
              (catMaybes
                 [("targetName" .=) <$> _ltTargetName,
                  Just ("targetType" .= _ltTargetType)])

-- | The target configuration.
--
--
--
-- /See:/ 'logTargetConfiguration' smart constructor.
data LogTargetConfiguration = LogTargetConfiguration'
  { _ltcLogLevel  :: !(Maybe LogLevel)
  , _ltcLogTarget :: !(Maybe LogTarget)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogTargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcLogLevel' - The logging level.
--
-- * 'ltcLogTarget' - A log target
logTargetConfiguration
    :: LogTargetConfiguration
logTargetConfiguration =
  LogTargetConfiguration' {_ltcLogLevel = Nothing, _ltcLogTarget = Nothing}


-- | The logging level.
ltcLogLevel :: Lens' LogTargetConfiguration (Maybe LogLevel)
ltcLogLevel = lens _ltcLogLevel (\ s a -> s{_ltcLogLevel = a})

-- | A log target
ltcLogTarget :: Lens' LogTargetConfiguration (Maybe LogTarget)
ltcLogTarget = lens _ltcLogTarget (\ s a -> s{_ltcLogTarget = a})

instance FromJSON LogTargetConfiguration where
        parseJSON
          = withObject "LogTargetConfiguration"
              (\ x ->
                 LogTargetConfiguration' <$>
                   (x .:? "logLevel") <*> (x .:? "logTarget"))

instance Hashable LogTargetConfiguration where

instance NFData LogTargetConfiguration where

-- | Describes the logging options payload.
--
--
--
-- /See:/ 'loggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { _lopLogLevel :: !(Maybe LogLevel)
  , _lopRoleARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingOptionsPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopLogLevel' - The log level.
--
-- * 'lopRoleARN' - The ARN of the IAM role that grants access.
loggingOptionsPayload
    :: Text -- ^ 'lopRoleARN'
    -> LoggingOptionsPayload
loggingOptionsPayload pRoleARN_ =
  LoggingOptionsPayload' {_lopLogLevel = Nothing, _lopRoleARN = pRoleARN_}


-- | The log level.
lopLogLevel :: Lens' LoggingOptionsPayload (Maybe LogLevel)
lopLogLevel = lens _lopLogLevel (\ s a -> s{_lopLogLevel = a})

-- | The ARN of the IAM role that grants access.
lopRoleARN :: Lens' LoggingOptionsPayload Text
lopRoleARN = lens _lopRoleARN (\ s a -> s{_lopRoleARN = a})

instance Hashable LoggingOptionsPayload where

instance NFData LoggingOptionsPayload where

instance ToJSON LoggingOptionsPayload where
        toJSON LoggingOptionsPayload'{..}
          = object
              (catMaybes
                 [("logLevel" .=) <$> _lopLogLevel,
                  Just ("roleArn" .= _lopRoleARN)])

-- | Describes a file to be associated with an OTA update.
--
--
--
-- /See:/ 'oTAUpdateFile' smart constructor.
data OTAUpdateFile = OTAUpdateFile'
  { _otaufFileVersion :: !(Maybe Text)
  , _otaufAttributes  :: !(Maybe (Map Text Text))
  , _otaufFileSource  :: !(Maybe Stream)
  , _otaufCodeSigning :: !(Maybe CodeSigning)
  , _otaufFileName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OTAUpdateFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otaufFileVersion' - The file version.
--
-- * 'otaufAttributes' - A list of name/attribute pairs.
--
-- * 'otaufFileSource' - The source of the file.
--
-- * 'otaufCodeSigning' - The code signing method of the file.
--
-- * 'otaufFileName' - The name of the file.
oTAUpdateFile
    :: OTAUpdateFile
oTAUpdateFile =
  OTAUpdateFile'
    { _otaufFileVersion = Nothing
    , _otaufAttributes = Nothing
    , _otaufFileSource = Nothing
    , _otaufCodeSigning = Nothing
    , _otaufFileName = Nothing
    }


-- | The file version.
otaufFileVersion :: Lens' OTAUpdateFile (Maybe Text)
otaufFileVersion = lens _otaufFileVersion (\ s a -> s{_otaufFileVersion = a})

-- | A list of name/attribute pairs.
otaufAttributes :: Lens' OTAUpdateFile (HashMap Text Text)
otaufAttributes = lens _otaufAttributes (\ s a -> s{_otaufAttributes = a}) . _Default . _Map

-- | The source of the file.
otaufFileSource :: Lens' OTAUpdateFile (Maybe Stream)
otaufFileSource = lens _otaufFileSource (\ s a -> s{_otaufFileSource = a})

-- | The code signing method of the file.
otaufCodeSigning :: Lens' OTAUpdateFile (Maybe CodeSigning)
otaufCodeSigning = lens _otaufCodeSigning (\ s a -> s{_otaufCodeSigning = a})

-- | The name of the file.
otaufFileName :: Lens' OTAUpdateFile (Maybe Text)
otaufFileName = lens _otaufFileName (\ s a -> s{_otaufFileName = a})

instance FromJSON OTAUpdateFile where
        parseJSON
          = withObject "OTAUpdateFile"
              (\ x ->
                 OTAUpdateFile' <$>
                   (x .:? "fileVersion") <*>
                     (x .:? "attributes" .!= mempty)
                     <*> (x .:? "fileSource")
                     <*> (x .:? "codeSigning")
                     <*> (x .:? "fileName"))

instance Hashable OTAUpdateFile where

instance NFData OTAUpdateFile where

instance ToJSON OTAUpdateFile where
        toJSON OTAUpdateFile'{..}
          = object
              (catMaybes
                 [("fileVersion" .=) <$> _otaufFileVersion,
                  ("attributes" .=) <$> _otaufAttributes,
                  ("fileSource" .=) <$> _otaufFileSource,
                  ("codeSigning" .=) <$> _otaufCodeSigning,
                  ("fileName" .=) <$> _otaufFileName])

-- | Information about an OTA update.
--
--
--
-- /See:/ 'oTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'
  { _otauiLastModifiedDate     :: !(Maybe POSIX)
  , _otauiAwsIotJobId          :: !(Maybe Text)
  , _otauiOtaUpdateFiles       :: !(Maybe (List1 OTAUpdateFile))
  , _otauiOtaUpdateStatus      :: !(Maybe OTAUpdateStatus)
  , _otauiTargets              :: !(Maybe (List1 Text))
  , _otauiAwsIotJobARN         :: !(Maybe Text)
  , _otauiCreationDate         :: !(Maybe POSIX)
  , _otauiAdditionalParameters :: !(Maybe (Map Text Text))
  , _otauiOtaUpdateId          :: !(Maybe Text)
  , _otauiErrorInfo            :: !(Maybe ErrorInfo)
  , _otauiOtaUpdateARN         :: !(Maybe Text)
  , _otauiDescription          :: !(Maybe Text)
  , _otauiTargetSelection      :: !(Maybe TargetSelection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OTAUpdateInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otauiLastModifiedDate' - The date when the OTA update was last updated.
--
-- * 'otauiAwsIotJobId' - The AWS IoT job ID associated with the OTA update.
--
-- * 'otauiOtaUpdateFiles' - A list of files associated with the OTA update.
--
-- * 'otauiOtaUpdateStatus' - The status of the OTA update.
--
-- * 'otauiTargets' - The targets of the OTA update.
--
-- * 'otauiAwsIotJobARN' - The AWS IoT job ARN associated with the OTA update.
--
-- * 'otauiCreationDate' - The date when the OTA update was created.
--
-- * 'otauiAdditionalParameters' - A collection of name/value pairs
--
-- * 'otauiOtaUpdateId' - The OTA update ID.
--
-- * 'otauiErrorInfo' - Error information associated with the OTA update.
--
-- * 'otauiOtaUpdateARN' - The OTA update ARN.
--
-- * 'otauiDescription' - A description of the OTA update.
--
-- * 'otauiTargetSelection' - Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group.
oTAUpdateInfo
    :: OTAUpdateInfo
oTAUpdateInfo =
  OTAUpdateInfo'
    { _otauiLastModifiedDate = Nothing
    , _otauiAwsIotJobId = Nothing
    , _otauiOtaUpdateFiles = Nothing
    , _otauiOtaUpdateStatus = Nothing
    , _otauiTargets = Nothing
    , _otauiAwsIotJobARN = Nothing
    , _otauiCreationDate = Nothing
    , _otauiAdditionalParameters = Nothing
    , _otauiOtaUpdateId = Nothing
    , _otauiErrorInfo = Nothing
    , _otauiOtaUpdateARN = Nothing
    , _otauiDescription = Nothing
    , _otauiTargetSelection = Nothing
    }


-- | The date when the OTA update was last updated.
otauiLastModifiedDate :: Lens' OTAUpdateInfo (Maybe UTCTime)
otauiLastModifiedDate = lens _otauiLastModifiedDate (\ s a -> s{_otauiLastModifiedDate = a}) . mapping _Time

-- | The AWS IoT job ID associated with the OTA update.
otauiAwsIotJobId :: Lens' OTAUpdateInfo (Maybe Text)
otauiAwsIotJobId = lens _otauiAwsIotJobId (\ s a -> s{_otauiAwsIotJobId = a})

-- | A list of files associated with the OTA update.
otauiOtaUpdateFiles :: Lens' OTAUpdateInfo (Maybe (NonEmpty OTAUpdateFile))
otauiOtaUpdateFiles = lens _otauiOtaUpdateFiles (\ s a -> s{_otauiOtaUpdateFiles = a}) . mapping _List1

-- | The status of the OTA update.
otauiOtaUpdateStatus :: Lens' OTAUpdateInfo (Maybe OTAUpdateStatus)
otauiOtaUpdateStatus = lens _otauiOtaUpdateStatus (\ s a -> s{_otauiOtaUpdateStatus = a})

-- | The targets of the OTA update.
otauiTargets :: Lens' OTAUpdateInfo (Maybe (NonEmpty Text))
otauiTargets = lens _otauiTargets (\ s a -> s{_otauiTargets = a}) . mapping _List1

-- | The AWS IoT job ARN associated with the OTA update.
otauiAwsIotJobARN :: Lens' OTAUpdateInfo (Maybe Text)
otauiAwsIotJobARN = lens _otauiAwsIotJobARN (\ s a -> s{_otauiAwsIotJobARN = a})

-- | The date when the OTA update was created.
otauiCreationDate :: Lens' OTAUpdateInfo (Maybe UTCTime)
otauiCreationDate = lens _otauiCreationDate (\ s a -> s{_otauiCreationDate = a}) . mapping _Time

-- | A collection of name/value pairs
otauiAdditionalParameters :: Lens' OTAUpdateInfo (HashMap Text Text)
otauiAdditionalParameters = lens _otauiAdditionalParameters (\ s a -> s{_otauiAdditionalParameters = a}) . _Default . _Map

-- | The OTA update ID.
otauiOtaUpdateId :: Lens' OTAUpdateInfo (Maybe Text)
otauiOtaUpdateId = lens _otauiOtaUpdateId (\ s a -> s{_otauiOtaUpdateId = a})

-- | Error information associated with the OTA update.
otauiErrorInfo :: Lens' OTAUpdateInfo (Maybe ErrorInfo)
otauiErrorInfo = lens _otauiErrorInfo (\ s a -> s{_otauiErrorInfo = a})

-- | The OTA update ARN.
otauiOtaUpdateARN :: Lens' OTAUpdateInfo (Maybe Text)
otauiOtaUpdateARN = lens _otauiOtaUpdateARN (\ s a -> s{_otauiOtaUpdateARN = a})

-- | A description of the OTA update.
otauiDescription :: Lens' OTAUpdateInfo (Maybe Text)
otauiDescription = lens _otauiDescription (\ s a -> s{_otauiDescription = a})

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group.
otauiTargetSelection :: Lens' OTAUpdateInfo (Maybe TargetSelection)
otauiTargetSelection = lens _otauiTargetSelection (\ s a -> s{_otauiTargetSelection = a})

instance FromJSON OTAUpdateInfo where
        parseJSON
          = withObject "OTAUpdateInfo"
              (\ x ->
                 OTAUpdateInfo' <$>
                   (x .:? "lastModifiedDate") <*> (x .:? "awsIotJobId")
                     <*> (x .:? "otaUpdateFiles")
                     <*> (x .:? "otaUpdateStatus")
                     <*> (x .:? "targets")
                     <*> (x .:? "awsIotJobArn")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "additionalParameters" .!= mempty)
                     <*> (x .:? "otaUpdateId")
                     <*> (x .:? "errorInfo")
                     <*> (x .:? "otaUpdateArn")
                     <*> (x .:? "description")
                     <*> (x .:? "targetSelection"))

instance Hashable OTAUpdateInfo where

instance NFData OTAUpdateInfo where

-- | An OTA update summary.
--
--
--
-- /See:/ 'oTAUpdateSummary' smart constructor.
data OTAUpdateSummary = OTAUpdateSummary'
  { _otausCreationDate :: !(Maybe POSIX)
  , _otausOtaUpdateId  :: !(Maybe Text)
  , _otausOtaUpdateARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OTAUpdateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otausCreationDate' - The date when the OTA update was created.
--
-- * 'otausOtaUpdateId' - The OTA update ID.
--
-- * 'otausOtaUpdateARN' - The OTA update ARN.
oTAUpdateSummary
    :: OTAUpdateSummary
oTAUpdateSummary =
  OTAUpdateSummary'
    { _otausCreationDate = Nothing
    , _otausOtaUpdateId = Nothing
    , _otausOtaUpdateARN = Nothing
    }


-- | The date when the OTA update was created.
otausCreationDate :: Lens' OTAUpdateSummary (Maybe UTCTime)
otausCreationDate = lens _otausCreationDate (\ s a -> s{_otausCreationDate = a}) . mapping _Time

-- | The OTA update ID.
otausOtaUpdateId :: Lens' OTAUpdateSummary (Maybe Text)
otausOtaUpdateId = lens _otausOtaUpdateId (\ s a -> s{_otausOtaUpdateId = a})

-- | The OTA update ARN.
otausOtaUpdateARN :: Lens' OTAUpdateSummary (Maybe Text)
otausOtaUpdateARN = lens _otausOtaUpdateARN (\ s a -> s{_otausOtaUpdateARN = a})

instance FromJSON OTAUpdateSummary where
        parseJSON
          = withObject "OTAUpdateSummary"
              (\ x ->
                 OTAUpdateSummary' <$>
                   (x .:? "creationDate") <*> (x .:? "otaUpdateId") <*>
                     (x .:? "otaUpdateArn"))

instance Hashable OTAUpdateSummary where

instance NFData OTAUpdateSummary where

-- | A certificate that has been transferred but not yet accepted.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
ocTransferDate = lens _ocTransferDate (\ s a -> s{_ocTransferDate = a}) . mapping _Time

-- | The certificate ARN.
ocCertificateARN :: Lens' OutgoingCertificate (Maybe Text)
ocCertificateARN = lens _ocCertificateARN (\ s a -> s{_ocCertificateARN = a})

-- | The certificate ID.
ocCertificateId :: Lens' OutgoingCertificate (Maybe Text)
ocCertificateId = lens _ocCertificateId (\ s a -> s{_ocCertificateId = a})

-- | The AWS account to which the transfer was made.
ocTransferredTo :: Lens' OutgoingCertificate (Maybe Text)
ocTransferredTo = lens _ocTransferredTo (\ s a -> s{_ocTransferredTo = a})

-- | The certificate creation date.
ocCreationDate :: Lens' OutgoingCertificate (Maybe UTCTime)
ocCreationDate = lens _ocCreationDate (\ s a -> s{_ocCreationDate = a}) . mapping _Time

-- | The transfer message.
ocTransferMessage :: Lens' OutgoingCertificate (Maybe Text)
ocTransferMessage = lens _ocTransferMessage (\ s a -> s{_ocTransferMessage = a})

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

instance Hashable OutgoingCertificate where

instance NFData OutgoingCertificate where

-- | Describes an AWS IoT policy.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pPolicyName :: !(Maybe Text)
  , _pPolicyARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyName' - The policy name.
--
-- * 'pPolicyARN' - The policy ARN.
policy
    :: Policy
policy = Policy' {_pPolicyName = Nothing, _pPolicyARN = Nothing}


-- | The policy name.
pPolicyName :: Lens' Policy (Maybe Text)
pPolicyName = lens _pPolicyName (\ s a -> s{_pPolicyName = a})

-- | The policy ARN.
pPolicyARN :: Lens' Policy (Maybe Text)
pPolicyARN = lens _pPolicyARN (\ s a -> s{_pPolicyARN = a})

instance FromJSON Policy where
        parseJSON
          = withObject "Policy"
              (\ x ->
                 Policy' <$>
                   (x .:? "policyName") <*> (x .:? "policyArn"))

instance Hashable Policy where

instance NFData Policy where

-- | Describes a policy version.
--
--
--
-- /See:/ 'policyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { _pvVersionId        :: !(Maybe Text)
  , _pvCreateDate       :: !(Maybe POSIX)
  , _pvIsDefaultVersion :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
pvVersionId = lens _pvVersionId (\ s a -> s{_pvVersionId = a})

-- | The date and time the policy was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\ s a -> s{_pvCreateDate = a}) . mapping _Time

-- | Specifies whether the policy version is the default.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\ s a -> s{_pvIsDefaultVersion = a})

instance FromJSON PolicyVersion where
        parseJSON
          = withObject "PolicyVersion"
              (\ x ->
                 PolicyVersion' <$>
                   (x .:? "versionId") <*> (x .:? "createDate") <*>
                     (x .:? "isDefaultVersion"))

instance Hashable PolicyVersion where

instance NFData PolicyVersion where

-- | Configuration for pre-signed S3 URLs.
--
--
--
-- /See:/ 'presignedURLConfig' smart constructor.
data PresignedURLConfig = PresignedURLConfig'
  { _pucExpiresInSec :: !(Maybe Nat)
  , _pucRoleARN      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PresignedURLConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pucExpiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
--
-- * 'pucRoleARN' - The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
presignedURLConfig
    :: PresignedURLConfig
presignedURLConfig =
  PresignedURLConfig' {_pucExpiresInSec = Nothing, _pucRoleARN = Nothing}


-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
pucExpiresInSec :: Lens' PresignedURLConfig (Maybe Natural)
pucExpiresInSec = lens _pucExpiresInSec (\ s a -> s{_pucExpiresInSec = a}) . mapping _Nat

-- | The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
pucRoleARN :: Lens' PresignedURLConfig (Maybe Text)
pucRoleARN = lens _pucRoleARN (\ s a -> s{_pucRoleARN = a})

instance FromJSON PresignedURLConfig where
        parseJSON
          = withObject "PresignedURLConfig"
              (\ x ->
                 PresignedURLConfig' <$>
                   (x .:? "expiresInSec") <*> (x .:? "roleArn"))

instance Hashable PresignedURLConfig where

instance NFData PresignedURLConfig where

instance ToJSON PresignedURLConfig where
        toJSON PresignedURLConfig'{..}
          = object
              (catMaybes
                 [("expiresInSec" .=) <$> _pucExpiresInSec,
                  ("roleArn" .=) <$> _pucRoleARN])

-- | The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.
--
--
--
-- /See:/ 'putItemInput' smart constructor.
newtype PutItemInput = PutItemInput'
  { _piiTableName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutItemInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piiTableName' - The table where the message data will be written
putItemInput
    :: Text -- ^ 'piiTableName'
    -> PutItemInput
putItemInput pTableName_ = PutItemInput' {_piiTableName = pTableName_}


-- | The table where the message data will be written
piiTableName :: Lens' PutItemInput Text
piiTableName = lens _piiTableName (\ s a -> s{_piiTableName = a})

instance FromJSON PutItemInput where
        parseJSON
          = withObject "PutItemInput"
              (\ x -> PutItemInput' <$> (x .: "tableName"))

instance Hashable PutItemInput where

instance NFData PutItemInput where

instance ToJSON PutItemInput where
        toJSON PutItemInput'{..}
          = object
              (catMaybes [Just ("tableName" .= _piiTableName)])

-- | The registration configuration.
--
--
--
-- /See:/ 'registrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { _rcTemplateBody :: !(Maybe Text)
  , _rcRoleARN      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegistrationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcTemplateBody' - The template body.
--
-- * 'rcRoleARN' - The ARN of the role.
registrationConfig
    :: RegistrationConfig
registrationConfig =
  RegistrationConfig' {_rcTemplateBody = Nothing, _rcRoleARN = Nothing}


-- | The template body.
rcTemplateBody :: Lens' RegistrationConfig (Maybe Text)
rcTemplateBody = lens _rcTemplateBody (\ s a -> s{_rcTemplateBody = a})

-- | The ARN of the role.
rcRoleARN :: Lens' RegistrationConfig (Maybe Text)
rcRoleARN = lens _rcRoleARN (\ s a -> s{_rcRoleARN = a})

instance FromJSON RegistrationConfig where
        parseJSON
          = withObject "RegistrationConfig"
              (\ x ->
                 RegistrationConfig' <$>
                   (x .:? "templateBody") <*> (x .:? "roleArn"))

instance Hashable RegistrationConfig where

instance NFData RegistrationConfig where

instance ToJSON RegistrationConfig where
        toJSON RegistrationConfig'{..}
          = object
              (catMaybes
                 [("templateBody" .=) <$> _rcTemplateBody,
                  ("roleArn" .=) <$> _rcRoleARN])

-- | Describes an action to republish to another topic.
--
--
--
-- /See:/ 'republishAction' smart constructor.
data RepublishAction = RepublishAction'
  { _raRoleARN :: !Text
  , _raTopic   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  RepublishAction' {_raRoleARN = pRoleARN_, _raTopic = pTopic_}


-- | The ARN of the IAM role that grants access.
raRoleARN :: Lens' RepublishAction Text
raRoleARN = lens _raRoleARN (\ s a -> s{_raRoleARN = a})

-- | The name of the MQTT topic.
raTopic :: Lens' RepublishAction Text
raTopic = lens _raTopic (\ s a -> s{_raTopic = a})

instance FromJSON RepublishAction where
        parseJSON
          = withObject "RepublishAction"
              (\ x ->
                 RepublishAction' <$>
                   (x .: "roleArn") <*> (x .: "topic"))

instance Hashable RepublishAction where

instance NFData RepublishAction where

instance ToJSON RepublishAction where
        toJSON RepublishAction'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _raRoleARN),
                  Just ("topic" .= _raTopic)])

-- | Role alias description.
--
--
--
-- /See:/ 'roleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { _radRoleAliasARN              :: !(Maybe Text)
  , _radLastModifiedDate          :: !(Maybe POSIX)
  , _radRoleAlias                 :: !(Maybe Text)
  , _radOwner                     :: !(Maybe Text)
  , _radCreationDate              :: !(Maybe POSIX)
  , _radCredentialDurationSeconds :: !(Maybe Nat)
  , _radRoleARN                   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoleAliasDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radRoleAliasARN' - The ARN of the role alias.
--
-- * 'radLastModifiedDate' - The UNIX timestamp of when the role alias was last modified.
--
-- * 'radRoleAlias' - The role alias.
--
-- * 'radOwner' - The role alias owner.
--
-- * 'radCreationDate' - The UNIX timestamp of when the role alias was created.
--
-- * 'radCredentialDurationSeconds' - The number of seconds for which the credential is valid.
--
-- * 'radRoleARN' - The role ARN.
roleAliasDescription
    :: RoleAliasDescription
roleAliasDescription =
  RoleAliasDescription'
    { _radRoleAliasARN = Nothing
    , _radLastModifiedDate = Nothing
    , _radRoleAlias = Nothing
    , _radOwner = Nothing
    , _radCreationDate = Nothing
    , _radCredentialDurationSeconds = Nothing
    , _radRoleARN = Nothing
    }


-- | The ARN of the role alias.
radRoleAliasARN :: Lens' RoleAliasDescription (Maybe Text)
radRoleAliasARN = lens _radRoleAliasARN (\ s a -> s{_radRoleAliasARN = a})

-- | The UNIX timestamp of when the role alias was last modified.
radLastModifiedDate :: Lens' RoleAliasDescription (Maybe UTCTime)
radLastModifiedDate = lens _radLastModifiedDate (\ s a -> s{_radLastModifiedDate = a}) . mapping _Time

-- | The role alias.
radRoleAlias :: Lens' RoleAliasDescription (Maybe Text)
radRoleAlias = lens _radRoleAlias (\ s a -> s{_radRoleAlias = a})

-- | The role alias owner.
radOwner :: Lens' RoleAliasDescription (Maybe Text)
radOwner = lens _radOwner (\ s a -> s{_radOwner = a})

-- | The UNIX timestamp of when the role alias was created.
radCreationDate :: Lens' RoleAliasDescription (Maybe UTCTime)
radCreationDate = lens _radCreationDate (\ s a -> s{_radCreationDate = a}) . mapping _Time

-- | The number of seconds for which the credential is valid.
radCredentialDurationSeconds :: Lens' RoleAliasDescription (Maybe Natural)
radCredentialDurationSeconds = lens _radCredentialDurationSeconds (\ s a -> s{_radCredentialDurationSeconds = a}) . mapping _Nat

-- | The role ARN.
radRoleARN :: Lens' RoleAliasDescription (Maybe Text)
radRoleARN = lens _radRoleARN (\ s a -> s{_radRoleARN = a})

instance FromJSON RoleAliasDescription where
        parseJSON
          = withObject "RoleAliasDescription"
              (\ x ->
                 RoleAliasDescription' <$>
                   (x .:? "roleAliasArn") <*> (x .:? "lastModifiedDate")
                     <*> (x .:? "roleAlias")
                     <*> (x .:? "owner")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "credentialDurationSeconds")
                     <*> (x .:? "roleArn"))

instance Hashable RoleAliasDescription where

instance NFData RoleAliasDescription where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
sCannedACL = lens _sCannedACL (\ s a -> s{_sCannedACL = a})

-- | The ARN of the IAM role that grants access.
sRoleARN :: Lens' S3Action Text
sRoleARN = lens _sRoleARN (\ s a -> s{_sRoleARN = a})

-- | The Amazon S3 bucket.
sBucketName :: Lens' S3Action Text
sBucketName = lens _sBucketName (\ s a -> s{_sBucketName = a})

-- | The object key.
sKey :: Lens' S3Action Text
sKey = lens _sKey (\ s a -> s{_sKey = a})

instance FromJSON S3Action where
        parseJSON
          = withObject "S3Action"
              (\ x ->
                 S3Action' <$>
                   (x .:? "cannedAcl") <*> (x .: "roleArn") <*>
                     (x .: "bucketName")
                     <*> (x .: "key"))

instance Hashable S3Action where

instance NFData S3Action where

instance ToJSON S3Action where
        toJSON S3Action'{..}
          = object
              (catMaybes
                 [("cannedAcl" .=) <$> _sCannedACL,
                  Just ("roleArn" .= _sRoleARN),
                  Just ("bucketName" .= _sBucketName),
                  Just ("key" .= _sKey)])

-- | The location in S3 the contains the files to stream.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slVersion :: !(Maybe Text)
  , _slBucket  :: !Text
  , _slKey     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slVersion' - The file version.
--
-- * 'slBucket' - The S3 bucket that contains the file to stream.
--
-- * 'slKey' - The name of the file within the S3 bucket to stream.
s3Location
    :: Text -- ^ 'slBucket'
    -> Text -- ^ 'slKey'
    -> S3Location
s3Location pBucket_ pKey_ =
  S3Location' {_slVersion = Nothing, _slBucket = pBucket_, _slKey = pKey_}


-- | The file version.
slVersion :: Lens' S3Location (Maybe Text)
slVersion = lens _slVersion (\ s a -> s{_slVersion = a})

-- | The S3 bucket that contains the file to stream.
slBucket :: Lens' S3Location Text
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | The name of the file within the S3 bucket to stream.
slKey :: Lens' S3Location Text
slKey = lens _slKey (\ s a -> s{_slKey = a})

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "version") <*> (x .: "bucket") <*>
                     (x .: "key"))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("version" .=) <$> _slVersion,
                  Just ("bucket" .= _slBucket),
                  Just ("key" .= _slKey)])

-- | Describes an action to publish to an Amazon SNS topic.
--
--
--
-- /See:/ 'snsAction' smart constructor.
data SNSAction = SNSAction'
  { _snsaMessageFormat :: !(Maybe MessageFormat)
  , _snsaTargetARN     :: !Text
  , _snsaRoleARN       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
snsaMessageFormat = lens _snsaMessageFormat (\ s a -> s{_snsaMessageFormat = a})

-- | The ARN of the SNS topic.
snsaTargetARN :: Lens' SNSAction Text
snsaTargetARN = lens _snsaTargetARN (\ s a -> s{_snsaTargetARN = a})

-- | The ARN of the IAM role that grants access.
snsaRoleARN :: Lens' SNSAction Text
snsaRoleARN = lens _snsaRoleARN (\ s a -> s{_snsaRoleARN = a})

instance FromJSON SNSAction where
        parseJSON
          = withObject "SNSAction"
              (\ x ->
                 SNSAction' <$>
                   (x .:? "messageFormat") <*> (x .: "targetArn") <*>
                     (x .: "roleArn"))

instance Hashable SNSAction where

instance NFData SNSAction where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  SalesforceAction' {_saToken = pToken_, _saUrl = pUrl_}


-- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saToken :: Lens' SalesforceAction Text
saToken = lens _saToken (\ s a -> s{_saToken = a})

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saUrl :: Lens' SalesforceAction Text
saUrl = lens _saUrl (\ s a -> s{_saUrl = a})

instance FromJSON SalesforceAction where
        parseJSON
          = withObject "SalesforceAction"
              (\ x ->
                 SalesforceAction' <$>
                   (x .: "token") <*> (x .: "url"))

instance Hashable SalesforceAction where

instance NFData SalesforceAction where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
    {_saUseBase64 = Nothing, _saRoleARN = pRoleARN_, _saQueueURL = pQueueURL_}


-- | Specifies whether to use Base64 encoding.
saUseBase64 :: Lens' SqsAction (Maybe Bool)
saUseBase64 = lens _saUseBase64 (\ s a -> s{_saUseBase64 = a})

-- | The ARN of the IAM role that grants access.
saRoleARN :: Lens' SqsAction Text
saRoleARN = lens _saRoleARN (\ s a -> s{_saRoleARN = a})

-- | The URL of the Amazon SQS queue.
saQueueURL :: Lens' SqsAction Text
saQueueURL = lens _saQueueURL (\ s a -> s{_saQueueURL = a})

instance FromJSON SqsAction where
        parseJSON
          = withObject "SqsAction"
              (\ x ->
                 SqsAction' <$>
                   (x .:? "useBase64") <*> (x .: "roleArn") <*>
                     (x .: "queueUrl"))

instance Hashable SqsAction where

instance NFData SqsAction where

instance ToJSON SqsAction where
        toJSON SqsAction'{..}
          = object
              (catMaybes
                 [("useBase64" .=) <$> _saUseBase64,
                  Just ("roleArn" .= _saRoleARN),
                  Just ("queueUrl" .= _saQueueURL)])

-- | Describes a group of files that can be streamed.
--
--
--
-- /See:/ 'stream' smart constructor.
data Stream = Stream'
  { _sFileId   :: !(Maybe Nat)
  , _sStreamId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFileId' - The ID of a file associated with a stream.
--
-- * 'sStreamId' - The stream ID.
stream
    :: Stream
stream = Stream' {_sFileId = Nothing, _sStreamId = Nothing}


-- | The ID of a file associated with a stream.
sFileId :: Lens' Stream (Maybe Natural)
sFileId = lens _sFileId (\ s a -> s{_sFileId = a}) . mapping _Nat

-- | The stream ID.
sStreamId :: Lens' Stream (Maybe Text)
sStreamId = lens _sStreamId (\ s a -> s{_sStreamId = a})

instance FromJSON Stream where
        parseJSON
          = withObject "Stream"
              (\ x ->
                 Stream' <$> (x .:? "fileId") <*> (x .:? "streamId"))

instance Hashable Stream where

instance NFData Stream where

instance ToJSON Stream where
        toJSON Stream'{..}
          = object
              (catMaybes
                 [("fileId" .=) <$> _sFileId,
                  ("streamId" .=) <$> _sStreamId])

-- | Represents a file to stream.
--
--
--
-- /See:/ 'streamFile' smart constructor.
data StreamFile = StreamFile'
  { _sfS3Location :: !(Maybe S3Location)
  , _sfFileId     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfS3Location' - The location of the file in S3.
--
-- * 'sfFileId' - The file ID.
streamFile
    :: StreamFile
streamFile = StreamFile' {_sfS3Location = Nothing, _sfFileId = Nothing}


-- | The location of the file in S3.
sfS3Location :: Lens' StreamFile (Maybe S3Location)
sfS3Location = lens _sfS3Location (\ s a -> s{_sfS3Location = a})

-- | The file ID.
sfFileId :: Lens' StreamFile (Maybe Natural)
sfFileId = lens _sfFileId (\ s a -> s{_sfFileId = a}) . mapping _Nat

instance FromJSON StreamFile where
        parseJSON
          = withObject "StreamFile"
              (\ x ->
                 StreamFile' <$>
                   (x .:? "s3Location") <*> (x .:? "fileId"))

instance Hashable StreamFile where

instance NFData StreamFile where

instance ToJSON StreamFile where
        toJSON StreamFile'{..}
          = object
              (catMaybes
                 [("s3Location" .=) <$> _sfS3Location,
                  ("fileId" .=) <$> _sfFileId])

-- | Information about a stream.
--
--
--
-- /See:/ 'streamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { _siLastUpdatedAt :: !(Maybe POSIX)
  , _siCreatedAt     :: !(Maybe POSIX)
  , _siStreamVersion :: !(Maybe Nat)
  , _siStreamARN     :: !(Maybe Text)
  , _siFiles         :: !(Maybe (List1 StreamFile))
  , _siDescription   :: !(Maybe Text)
  , _siStreamId      :: !(Maybe Text)
  , _siRoleARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siLastUpdatedAt' - The date when the stream was last updated.
--
-- * 'siCreatedAt' - The date when the stream was created.
--
-- * 'siStreamVersion' - The stream version.
--
-- * 'siStreamARN' - The stream ARN.
--
-- * 'siFiles' - The files to stream.
--
-- * 'siDescription' - The description of the stream.
--
-- * 'siStreamId' - The stream ID.
--
-- * 'siRoleARN' - An IAM role AWS IoT assumes to access your S3 files.
streamInfo
    :: StreamInfo
streamInfo =
  StreamInfo'
    { _siLastUpdatedAt = Nothing
    , _siCreatedAt = Nothing
    , _siStreamVersion = Nothing
    , _siStreamARN = Nothing
    , _siFiles = Nothing
    , _siDescription = Nothing
    , _siStreamId = Nothing
    , _siRoleARN = Nothing
    }


-- | The date when the stream was last updated.
siLastUpdatedAt :: Lens' StreamInfo (Maybe UTCTime)
siLastUpdatedAt = lens _siLastUpdatedAt (\ s a -> s{_siLastUpdatedAt = a}) . mapping _Time

-- | The date when the stream was created.
siCreatedAt :: Lens' StreamInfo (Maybe UTCTime)
siCreatedAt = lens _siCreatedAt (\ s a -> s{_siCreatedAt = a}) . mapping _Time

-- | The stream version.
siStreamVersion :: Lens' StreamInfo (Maybe Natural)
siStreamVersion = lens _siStreamVersion (\ s a -> s{_siStreamVersion = a}) . mapping _Nat

-- | The stream ARN.
siStreamARN :: Lens' StreamInfo (Maybe Text)
siStreamARN = lens _siStreamARN (\ s a -> s{_siStreamARN = a})

-- | The files to stream.
siFiles :: Lens' StreamInfo (Maybe (NonEmpty StreamFile))
siFiles = lens _siFiles (\ s a -> s{_siFiles = a}) . mapping _List1

-- | The description of the stream.
siDescription :: Lens' StreamInfo (Maybe Text)
siDescription = lens _siDescription (\ s a -> s{_siDescription = a})

-- | The stream ID.
siStreamId :: Lens' StreamInfo (Maybe Text)
siStreamId = lens _siStreamId (\ s a -> s{_siStreamId = a})

-- | An IAM role AWS IoT assumes to access your S3 files.
siRoleARN :: Lens' StreamInfo (Maybe Text)
siRoleARN = lens _siRoleARN (\ s a -> s{_siRoleARN = a})

instance FromJSON StreamInfo where
        parseJSON
          = withObject "StreamInfo"
              (\ x ->
                 StreamInfo' <$>
                   (x .:? "lastUpdatedAt") <*> (x .:? "createdAt") <*>
                     (x .:? "streamVersion")
                     <*> (x .:? "streamArn")
                     <*> (x .:? "files")
                     <*> (x .:? "description")
                     <*> (x .:? "streamId")
                     <*> (x .:? "roleArn"))

instance Hashable StreamInfo where

instance NFData StreamInfo where

-- | A summary of a stream.
--
--
--
-- /See:/ 'streamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { _ssStreamVersion :: !(Maybe Nat)
  , _ssStreamARN     :: !(Maybe Text)
  , _ssDescription   :: !(Maybe Text)
  , _ssStreamId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamVersion' - The stream version.
--
-- * 'ssStreamARN' - The stream ARN.
--
-- * 'ssDescription' - A description of the stream.
--
-- * 'ssStreamId' - The stream ID.
streamSummary
    :: StreamSummary
streamSummary =
  StreamSummary'
    { _ssStreamVersion = Nothing
    , _ssStreamARN = Nothing
    , _ssDescription = Nothing
    , _ssStreamId = Nothing
    }


-- | The stream version.
ssStreamVersion :: Lens' StreamSummary (Maybe Natural)
ssStreamVersion = lens _ssStreamVersion (\ s a -> s{_ssStreamVersion = a}) . mapping _Nat

-- | The stream ARN.
ssStreamARN :: Lens' StreamSummary (Maybe Text)
ssStreamARN = lens _ssStreamARN (\ s a -> s{_ssStreamARN = a})

-- | A description of the stream.
ssDescription :: Lens' StreamSummary (Maybe Text)
ssDescription = lens _ssDescription (\ s a -> s{_ssDescription = a})

-- | The stream ID.
ssStreamId :: Lens' StreamSummary (Maybe Text)
ssStreamId = lens _ssStreamId (\ s a -> s{_ssStreamId = a})

instance FromJSON StreamSummary where
        parseJSON
          = withObject "StreamSummary"
              (\ x ->
                 StreamSummary' <$>
                   (x .:? "streamVersion") <*> (x .:? "streamArn") <*>
                     (x .:? "description")
                     <*> (x .:? "streamId"))

instance Hashable StreamSummary where

instance NFData StreamSummary where

-- | The properties of the thing, including thing name, thing type name, and a list of thing attributes.
--
--
--
-- /See:/ 'thingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { _taThingTypeName :: !(Maybe Text)
  , _taThingARN      :: !(Maybe Text)
  , _taAttributes    :: !(Maybe (Map Text Text))
  , _taVersion       :: !(Maybe Integer)
  , _taThingName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taThingTypeName' - The name of the thing type, if the thing has been associated with a type.
--
-- * 'taThingARN' - The thing ARN.
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
    , _taThingARN = Nothing
    , _taAttributes = Nothing
    , _taVersion = Nothing
    , _taThingName = Nothing
    }


-- | The name of the thing type, if the thing has been associated with a type.
taThingTypeName :: Lens' ThingAttribute (Maybe Text)
taThingTypeName = lens _taThingTypeName (\ s a -> s{_taThingTypeName = a})

-- | The thing ARN.
taThingARN :: Lens' ThingAttribute (Maybe Text)
taThingARN = lens _taThingARN (\ s a -> s{_taThingARN = a})

-- | A list of thing attributes which are name-value pairs.
taAttributes :: Lens' ThingAttribute (HashMap Text Text)
taAttributes = lens _taAttributes (\ s a -> s{_taAttributes = a}) . _Default . _Map

-- | The version of the thing record in the registry.
taVersion :: Lens' ThingAttribute (Maybe Integer)
taVersion = lens _taVersion (\ s a -> s{_taVersion = a})

-- | The name of the thing.
taThingName :: Lens' ThingAttribute (Maybe Text)
taThingName = lens _taThingName (\ s a -> s{_taThingName = a})

instance FromJSON ThingAttribute where
        parseJSON
          = withObject "ThingAttribute"
              (\ x ->
                 ThingAttribute' <$>
                   (x .:? "thingTypeName") <*> (x .:? "thingArn") <*>
                     (x .:? "attributes" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "thingName"))

instance Hashable ThingAttribute where

instance NFData ThingAttribute where

-- | The thing search index document.
--
--
--
-- /See:/ 'thingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { _tdThingGroupNames :: !(Maybe [Text])
  , _tdThingTypeName   :: !(Maybe Text)
  , _tdShadow          :: !(Maybe Text)
  , _tdAttributes      :: !(Maybe (Map Text Text))
  , _tdThingName       :: !(Maybe Text)
  , _tdThingId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdThingGroupNames' - Thing group names.
--
-- * 'tdThingTypeName' - The thing type name.
--
-- * 'tdShadow' - The shadow.
--
-- * 'tdAttributes' - The attributes.
--
-- * 'tdThingName' - The thing name.
--
-- * 'tdThingId' - The thing ID.
thingDocument
    :: ThingDocument
thingDocument =
  ThingDocument'
    { _tdThingGroupNames = Nothing
    , _tdThingTypeName = Nothing
    , _tdShadow = Nothing
    , _tdAttributes = Nothing
    , _tdThingName = Nothing
    , _tdThingId = Nothing
    }


-- | Thing group names.
tdThingGroupNames :: Lens' ThingDocument [Text]
tdThingGroupNames = lens _tdThingGroupNames (\ s a -> s{_tdThingGroupNames = a}) . _Default . _Coerce

-- | The thing type name.
tdThingTypeName :: Lens' ThingDocument (Maybe Text)
tdThingTypeName = lens _tdThingTypeName (\ s a -> s{_tdThingTypeName = a})

-- | The shadow.
tdShadow :: Lens' ThingDocument (Maybe Text)
tdShadow = lens _tdShadow (\ s a -> s{_tdShadow = a})

-- | The attributes.
tdAttributes :: Lens' ThingDocument (HashMap Text Text)
tdAttributes = lens _tdAttributes (\ s a -> s{_tdAttributes = a}) . _Default . _Map

-- | The thing name.
tdThingName :: Lens' ThingDocument (Maybe Text)
tdThingName = lens _tdThingName (\ s a -> s{_tdThingName = a})

-- | The thing ID.
tdThingId :: Lens' ThingDocument (Maybe Text)
tdThingId = lens _tdThingId (\ s a -> s{_tdThingId = a})

instance FromJSON ThingDocument where
        parseJSON
          = withObject "ThingDocument"
              (\ x ->
                 ThingDocument' <$>
                   (x .:? "thingGroupNames" .!= mempty) <*>
                     (x .:? "thingTypeName")
                     <*> (x .:? "shadow")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "thingName")
                     <*> (x .:? "thingId"))

instance Hashable ThingDocument where

instance NFData ThingDocument where

-- | Thing group metadata.
--
--
--
-- /See:/ 'thingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { _tgmRootToParentThingGroups :: !(Maybe [GroupNameAndARN])
  , _tgmParentGroupName         :: !(Maybe Text)
  , _tgmCreationDate            :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingGroupMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmRootToParentThingGroups' - The root parent thing group.
--
-- * 'tgmParentGroupName' - The parent thing group name.
--
-- * 'tgmCreationDate' - The UNIX timestamp of when the thing group was created.
thingGroupMetadata
    :: ThingGroupMetadata
thingGroupMetadata =
  ThingGroupMetadata'
    { _tgmRootToParentThingGroups = Nothing
    , _tgmParentGroupName = Nothing
    , _tgmCreationDate = Nothing
    }


-- | The root parent thing group.
tgmRootToParentThingGroups :: Lens' ThingGroupMetadata [GroupNameAndARN]
tgmRootToParentThingGroups = lens _tgmRootToParentThingGroups (\ s a -> s{_tgmRootToParentThingGroups = a}) . _Default . _Coerce

-- | The parent thing group name.
tgmParentGroupName :: Lens' ThingGroupMetadata (Maybe Text)
tgmParentGroupName = lens _tgmParentGroupName (\ s a -> s{_tgmParentGroupName = a})

-- | The UNIX timestamp of when the thing group was created.
tgmCreationDate :: Lens' ThingGroupMetadata (Maybe UTCTime)
tgmCreationDate = lens _tgmCreationDate (\ s a -> s{_tgmCreationDate = a}) . mapping _Time

instance FromJSON ThingGroupMetadata where
        parseJSON
          = withObject "ThingGroupMetadata"
              (\ x ->
                 ThingGroupMetadata' <$>
                   (x .:? "rootToParentThingGroups" .!= mempty) <*>
                     (x .:? "parentGroupName")
                     <*> (x .:? "creationDate"))

instance Hashable ThingGroupMetadata where

instance NFData ThingGroupMetadata where

-- | Thing group properties.
--
--
--
-- /See:/ 'thingGroupProperties' smart constructor.
data ThingGroupProperties = ThingGroupProperties'
  { _tgpAttributePayload      :: !(Maybe AttributePayload)
  , _tgpThingGroupDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingGroupProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgpAttributePayload' - The thing group attributes in JSON format.
--
-- * 'tgpThingGroupDescription' - The thing group description.
thingGroupProperties
    :: ThingGroupProperties
thingGroupProperties =
  ThingGroupProperties'
    {_tgpAttributePayload = Nothing, _tgpThingGroupDescription = Nothing}


-- | The thing group attributes in JSON format.
tgpAttributePayload :: Lens' ThingGroupProperties (Maybe AttributePayload)
tgpAttributePayload = lens _tgpAttributePayload (\ s a -> s{_tgpAttributePayload = a})

-- | The thing group description.
tgpThingGroupDescription :: Lens' ThingGroupProperties (Maybe Text)
tgpThingGroupDescription = lens _tgpThingGroupDescription (\ s a -> s{_tgpThingGroupDescription = a})

instance FromJSON ThingGroupProperties where
        parseJSON
          = withObject "ThingGroupProperties"
              (\ x ->
                 ThingGroupProperties' <$>
                   (x .:? "attributePayload") <*>
                     (x .:? "thingGroupDescription"))

instance Hashable ThingGroupProperties where

instance NFData ThingGroupProperties where

instance ToJSON ThingGroupProperties where
        toJSON ThingGroupProperties'{..}
          = object
              (catMaybes
                 [("attributePayload" .=) <$> _tgpAttributePayload,
                  ("thingGroupDescription" .=) <$>
                    _tgpThingGroupDescription])

-- | Thing indexing configuration.
--
--
--
-- /See:/ 'thingIndexingConfiguration' smart constructor.
newtype ThingIndexingConfiguration = ThingIndexingConfiguration'
  { _ticThingIndexingMode :: Maybe ThingIndexingMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingIndexingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ticThingIndexingMode' - Thing indexing mode. Valid values are:      * REGISTRY – Your thing index will contain only registry data.     * REGISTRY_AND_SHADOW - Your thing index will contain registry and shadow data.     * OFF - Thing indexing is disabled.
thingIndexingConfiguration
    :: ThingIndexingConfiguration
thingIndexingConfiguration =
  ThingIndexingConfiguration' {_ticThingIndexingMode = Nothing}


-- | Thing indexing mode. Valid values are:      * REGISTRY – Your thing index will contain only registry data.     * REGISTRY_AND_SHADOW - Your thing index will contain registry and shadow data.     * OFF - Thing indexing is disabled.
ticThingIndexingMode :: Lens' ThingIndexingConfiguration (Maybe ThingIndexingMode)
ticThingIndexingMode = lens _ticThingIndexingMode (\ s a -> s{_ticThingIndexingMode = a})

instance FromJSON ThingIndexingConfiguration where
        parseJSON
          = withObject "ThingIndexingConfiguration"
              (\ x ->
                 ThingIndexingConfiguration' <$>
                   (x .:? "thingIndexingMode"))

instance Hashable ThingIndexingConfiguration where

instance NFData ThingIndexingConfiguration where

instance ToJSON ThingIndexingConfiguration where
        toJSON ThingIndexingConfiguration'{..}
          = object
              (catMaybes
                 [("thingIndexingMode" .=) <$> _ticThingIndexingMode])

-- | The definition of the thing type, including thing type name and description.
--
--
--
-- /See:/ 'thingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { _ttdThingTypeProperties :: !(Maybe ThingTypeProperties)
  , _ttdThingTypeName       :: !(Maybe Text)
  , _ttdThingTypeMetadata   :: !(Maybe ThingTypeMetadata)
  , _ttdThingTypeARN        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ThingTypeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttdThingTypeProperties' - The ThingTypeProperties for the thing type.
--
-- * 'ttdThingTypeName' - The name of the thing type.
--
-- * 'ttdThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'ttdThingTypeARN' - The thing type ARN.
thingTypeDefinition
    :: ThingTypeDefinition
thingTypeDefinition =
  ThingTypeDefinition'
    { _ttdThingTypeProperties = Nothing
    , _ttdThingTypeName = Nothing
    , _ttdThingTypeMetadata = Nothing
    , _ttdThingTypeARN = Nothing
    }


-- | The ThingTypeProperties for the thing type.
ttdThingTypeProperties :: Lens' ThingTypeDefinition (Maybe ThingTypeProperties)
ttdThingTypeProperties = lens _ttdThingTypeProperties (\ s a -> s{_ttdThingTypeProperties = a})

-- | The name of the thing type.
ttdThingTypeName :: Lens' ThingTypeDefinition (Maybe Text)
ttdThingTypeName = lens _ttdThingTypeName (\ s a -> s{_ttdThingTypeName = a})

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
ttdThingTypeMetadata :: Lens' ThingTypeDefinition (Maybe ThingTypeMetadata)
ttdThingTypeMetadata = lens _ttdThingTypeMetadata (\ s a -> s{_ttdThingTypeMetadata = a})

-- | The thing type ARN.
ttdThingTypeARN :: Lens' ThingTypeDefinition (Maybe Text)
ttdThingTypeARN = lens _ttdThingTypeARN (\ s a -> s{_ttdThingTypeARN = a})

instance FromJSON ThingTypeDefinition where
        parseJSON
          = withObject "ThingTypeDefinition"
              (\ x ->
                 ThingTypeDefinition' <$>
                   (x .:? "thingTypeProperties") <*>
                     (x .:? "thingTypeName")
                     <*> (x .:? "thingTypeMetadata")
                     <*> (x .:? "thingTypeArn"))

instance Hashable ThingTypeDefinition where

instance NFData ThingTypeDefinition where

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.
--
--
--
-- /See:/ 'thingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { _ttmDeprecationDate :: !(Maybe POSIX)
  , _ttmCreationDate    :: !(Maybe POSIX)
  , _ttmDeprecated      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
ttmDeprecationDate = lens _ttmDeprecationDate (\ s a -> s{_ttmDeprecationDate = a}) . mapping _Time

-- | The date and time when the thing type was created.
ttmCreationDate :: Lens' ThingTypeMetadata (Maybe UTCTime)
ttmCreationDate = lens _ttmCreationDate (\ s a -> s{_ttmCreationDate = a}) . mapping _Time

-- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
ttmDeprecated :: Lens' ThingTypeMetadata (Maybe Bool)
ttmDeprecated = lens _ttmDeprecated (\ s a -> s{_ttmDeprecated = a})

instance FromJSON ThingTypeMetadata where
        parseJSON
          = withObject "ThingTypeMetadata"
              (\ x ->
                 ThingTypeMetadata' <$>
                   (x .:? "deprecationDate") <*> (x .:? "creationDate")
                     <*> (x .:? "deprecated"))

instance Hashable ThingTypeMetadata where

instance NFData ThingTypeMetadata where

-- | The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.
--
--
--
-- /See:/ 'thingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { _ttpSearchableAttributes :: !(Maybe [Text])
  , _ttpThingTypeDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
    {_ttpSearchableAttributes = Nothing, _ttpThingTypeDescription = Nothing}


-- | A list of searchable thing attribute names.
ttpSearchableAttributes :: Lens' ThingTypeProperties [Text]
ttpSearchableAttributes = lens _ttpSearchableAttributes (\ s a -> s{_ttpSearchableAttributes = a}) . _Default . _Coerce

-- | The description of the thing type.
ttpThingTypeDescription :: Lens' ThingTypeProperties (Maybe Text)
ttpThingTypeDescription = lens _ttpThingTypeDescription (\ s a -> s{_ttpThingTypeDescription = a})

instance FromJSON ThingTypeProperties where
        parseJSON
          = withObject "ThingTypeProperties"
              (\ x ->
                 ThingTypeProperties' <$>
                   (x .:? "searchableAttributes" .!= mempty) <*>
                     (x .:? "thingTypeDescription"))

instance Hashable ThingTypeProperties where

instance NFData ThingTypeProperties where

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
  , _trErrorAction      :: !(Maybe Action)
  , _trRuleDisabled     :: !(Maybe Bool)
  , _trRuleName         :: !(Maybe Text)
  , _trSql              :: !(Maybe Text)
  , _trDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'trErrorAction' - The action to perform when an error occurs.
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
    , _trErrorAction = Nothing
    , _trRuleDisabled = Nothing
    , _trRuleName = Nothing
    , _trSql = Nothing
    , _trDescription = Nothing
    }


-- | The date and time the rule was created.
trCreatedAt :: Lens' TopicRule (Maybe UTCTime)
trCreatedAt = lens _trCreatedAt (\ s a -> s{_trCreatedAt = a}) . mapping _Time

-- | The actions associated with the rule.
trActions :: Lens' TopicRule [Action]
trActions = lens _trActions (\ s a -> s{_trActions = a}) . _Default . _Coerce

-- | The version of the SQL rules engine to use when evaluating the rule.
trAwsIotSqlVersion :: Lens' TopicRule (Maybe Text)
trAwsIotSqlVersion = lens _trAwsIotSqlVersion (\ s a -> s{_trAwsIotSqlVersion = a})

-- | The action to perform when an error occurs.
trErrorAction :: Lens' TopicRule (Maybe Action)
trErrorAction = lens _trErrorAction (\ s a -> s{_trErrorAction = a})

-- | Specifies whether the rule is disabled.
trRuleDisabled :: Lens' TopicRule (Maybe Bool)
trRuleDisabled = lens _trRuleDisabled (\ s a -> s{_trRuleDisabled = a})

-- | The name of the rule.
trRuleName :: Lens' TopicRule (Maybe Text)
trRuleName = lens _trRuleName (\ s a -> s{_trRuleName = a})

-- | The SQL statement used to query the topic. When using a SQL query with multiple lines, be sure to escape the newline characters.
trSql :: Lens' TopicRule (Maybe Text)
trSql = lens _trSql (\ s a -> s{_trSql = a})

-- | The description of the rule.
trDescription :: Lens' TopicRule (Maybe Text)
trDescription = lens _trDescription (\ s a -> s{_trDescription = a})

instance FromJSON TopicRule where
        parseJSON
          = withObject "TopicRule"
              (\ x ->
                 TopicRule' <$>
                   (x .:? "createdAt") <*> (x .:? "actions" .!= mempty)
                     <*> (x .:? "awsIotSqlVersion")
                     <*> (x .:? "errorAction")
                     <*> (x .:? "ruleDisabled")
                     <*> (x .:? "ruleName")
                     <*> (x .:? "sql")
                     <*> (x .:? "description"))

instance Hashable TopicRule where

instance NFData TopicRule where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
trliCreatedAt = lens _trliCreatedAt (\ s a -> s{_trliCreatedAt = a}) . mapping _Time

-- | Specifies whether the rule is disabled.
trliRuleDisabled :: Lens' TopicRuleListItem (Maybe Bool)
trliRuleDisabled = lens _trliRuleDisabled (\ s a -> s{_trliRuleDisabled = a})

-- | The name of the rule.
trliRuleName :: Lens' TopicRuleListItem (Maybe Text)
trliRuleName = lens _trliRuleName (\ s a -> s{_trliRuleName = a})

-- | The rule ARN.
trliRuleARN :: Lens' TopicRuleListItem (Maybe Text)
trliRuleARN = lens _trliRuleARN (\ s a -> s{_trliRuleARN = a})

-- | The pattern for the topic names that apply.
trliTopicPattern :: Lens' TopicRuleListItem (Maybe Text)
trliTopicPattern = lens _trliTopicPattern (\ s a -> s{_trliTopicPattern = a})

instance FromJSON TopicRuleListItem where
        parseJSON
          = withObject "TopicRuleListItem"
              (\ x ->
                 TopicRuleListItem' <$>
                   (x .:? "createdAt") <*> (x .:? "ruleDisabled") <*>
                     (x .:? "ruleName")
                     <*> (x .:? "ruleArn")
                     <*> (x .:? "topicPattern"))

instance Hashable TopicRuleListItem where

instance NFData TopicRuleListItem where

-- | Describes a rule.
--
--
--
-- /See:/ 'topicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { _trpAwsIotSqlVersion :: !(Maybe Text)
  , _trpErrorAction      :: !(Maybe Action)
  , _trpRuleDisabled     :: !(Maybe Bool)
  , _trpDescription      :: !(Maybe Text)
  , _trpSql              :: !Text
  , _trpActions          :: ![Action]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TopicRulePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpAwsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- * 'trpErrorAction' - The action to take when an error occurs.
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
    , _trpErrorAction = Nothing
    , _trpRuleDisabled = Nothing
    , _trpDescription = Nothing
    , _trpSql = pSql_
    , _trpActions = mempty
    }


-- | The version of the SQL rules engine to use when evaluating the rule.
trpAwsIotSqlVersion :: Lens' TopicRulePayload (Maybe Text)
trpAwsIotSqlVersion = lens _trpAwsIotSqlVersion (\ s a -> s{_trpAwsIotSqlVersion = a})

-- | The action to take when an error occurs.
trpErrorAction :: Lens' TopicRulePayload (Maybe Action)
trpErrorAction = lens _trpErrorAction (\ s a -> s{_trpErrorAction = a})

-- | Specifies whether the rule is disabled.
trpRuleDisabled :: Lens' TopicRulePayload (Maybe Bool)
trpRuleDisabled = lens _trpRuleDisabled (\ s a -> s{_trpRuleDisabled = a})

-- | The description of the rule.
trpDescription :: Lens' TopicRulePayload (Maybe Text)
trpDescription = lens _trpDescription (\ s a -> s{_trpDescription = a})

-- | The SQL statement used to query the topic. For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/iot-rules.html#aws-iot-sql-reference AWS IoT SQL Reference> in the /AWS IoT Developer Guide/ .
trpSql :: Lens' TopicRulePayload Text
trpSql = lens _trpSql (\ s a -> s{_trpSql = a})

-- | The actions associated with the rule.
trpActions :: Lens' TopicRulePayload [Action]
trpActions = lens _trpActions (\ s a -> s{_trpActions = a}) . _Coerce

instance Hashable TopicRulePayload where

instance NFData TopicRulePayload where

instance ToJSON TopicRulePayload where
        toJSON TopicRulePayload'{..}
          = object
              (catMaybes
                 [("awsIotSqlVersion" .=) <$> _trpAwsIotSqlVersion,
                  ("errorAction" .=) <$> _trpErrorAction,
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
tdTransferDate = lens _tdTransferDate (\ s a -> s{_tdTransferDate = a}) . mapping _Time

-- | The date the transfer was accepted.
tdAcceptDate :: Lens' TransferData (Maybe UTCTime)
tdAcceptDate = lens _tdAcceptDate (\ s a -> s{_tdAcceptDate = a}) . mapping _Time

-- | The transfer message.
tdTransferMessage :: Lens' TransferData (Maybe Text)
tdTransferMessage = lens _tdTransferMessage (\ s a -> s{_tdTransferMessage = a})

-- | The date the transfer was rejected.
tdRejectDate :: Lens' TransferData (Maybe UTCTime)
tdRejectDate = lens _tdRejectDate (\ s a -> s{_tdRejectDate = a}) . mapping _Time

-- | The reason why the transfer was rejected.
tdRejectReason :: Lens' TransferData (Maybe Text)
tdRejectReason = lens _tdRejectReason (\ s a -> s{_tdRejectReason = a})

instance FromJSON TransferData where
        parseJSON
          = withObject "TransferData"
              (\ x ->
                 TransferData' <$>
                   (x .:? "transferDate") <*> (x .:? "acceptDate") <*>
                     (x .:? "transferMessage")
                     <*> (x .:? "rejectDate")
                     <*> (x .:? "rejectReason"))

instance Hashable TransferData where

instance NFData TransferData where
