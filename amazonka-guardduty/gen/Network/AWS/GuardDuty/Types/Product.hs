{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Product where

import Network.AWS.GuardDuty.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the AWS_API_CALL action described in this finding.
--
-- /See:/ 'awsAPICallAction' smart constructor.
data AWSAPICallAction = AWSAPICallAction'
  { _aacaRemoteIPDetails :: !(Maybe RemoteIPDetails)
  , _aacaCallerType      :: !(Maybe Text)
  , _aacaDomainDetails   :: !(Maybe DomainDetails)
  , _aacaServiceName     :: !(Maybe Text)
  , _aacaAPI             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AWSAPICallAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aacaRemoteIPDetails' - Remote IP information of the connection.
--
-- * 'aacaCallerType' - AWS API caller type.
--
-- * 'aacaDomainDetails' - Domain information for the AWS API call.
--
-- * 'aacaServiceName' - AWS service name whose API was invoked.
--
-- * 'aacaAPI' - AWS API name.
awsAPICallAction
    :: AWSAPICallAction
awsAPICallAction =
  AWSAPICallAction'
    { _aacaRemoteIPDetails = Nothing
    , _aacaCallerType = Nothing
    , _aacaDomainDetails = Nothing
    , _aacaServiceName = Nothing
    , _aacaAPI = Nothing
    }


-- | Remote IP information of the connection.
aacaRemoteIPDetails :: Lens' AWSAPICallAction (Maybe RemoteIPDetails)
aacaRemoteIPDetails = lens _aacaRemoteIPDetails (\ s a -> s{_aacaRemoteIPDetails = a})

-- | AWS API caller type.
aacaCallerType :: Lens' AWSAPICallAction (Maybe Text)
aacaCallerType = lens _aacaCallerType (\ s a -> s{_aacaCallerType = a})

-- | Domain information for the AWS API call.
aacaDomainDetails :: Lens' AWSAPICallAction (Maybe DomainDetails)
aacaDomainDetails = lens _aacaDomainDetails (\ s a -> s{_aacaDomainDetails = a})

-- | AWS service name whose API was invoked.
aacaServiceName :: Lens' AWSAPICallAction (Maybe Text)
aacaServiceName = lens _aacaServiceName (\ s a -> s{_aacaServiceName = a})

-- | AWS API name.
aacaAPI :: Lens' AWSAPICallAction (Maybe Text)
aacaAPI = lens _aacaAPI (\ s a -> s{_aacaAPI = a})

instance FromJSON AWSAPICallAction where
        parseJSON
          = withObject "AWSAPICallAction"
              (\ x ->
                 AWSAPICallAction' <$>
                   (x .:? "remoteIpDetails") <*> (x .:? "callerType")
                     <*> (x .:? "domainDetails")
                     <*> (x .:? "serviceName")
                     <*> (x .:? "api"))

instance Hashable AWSAPICallAction where

instance NFData AWSAPICallAction where

-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'accessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { _akdPrincipalId :: !(Maybe Text)
  , _akdUserName    :: !(Maybe Text)
  , _akdAccessKeyId :: !(Maybe Text)
  , _akdUserType    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessKeyDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akdPrincipalId' - The principal ID of the user.
--
-- * 'akdUserName' - The name of the user.
--
-- * 'akdAccessKeyId' - Access key ID of the user.
--
-- * 'akdUserType' - The type of the user.
accessKeyDetails
    :: AccessKeyDetails
accessKeyDetails =
  AccessKeyDetails'
    { _akdPrincipalId = Nothing
    , _akdUserName = Nothing
    , _akdAccessKeyId = Nothing
    , _akdUserType = Nothing
    }


-- | The principal ID of the user.
akdPrincipalId :: Lens' AccessKeyDetails (Maybe Text)
akdPrincipalId = lens _akdPrincipalId (\ s a -> s{_akdPrincipalId = a})

-- | The name of the user.
akdUserName :: Lens' AccessKeyDetails (Maybe Text)
akdUserName = lens _akdUserName (\ s a -> s{_akdUserName = a})

-- | Access key ID of the user.
akdAccessKeyId :: Lens' AccessKeyDetails (Maybe Text)
akdAccessKeyId = lens _akdAccessKeyId (\ s a -> s{_akdAccessKeyId = a})

-- | The type of the user.
akdUserType :: Lens' AccessKeyDetails (Maybe Text)
akdUserType = lens _akdUserType (\ s a -> s{_akdUserType = a})

instance FromJSON AccessKeyDetails where
        parseJSON
          = withObject "AccessKeyDetails"
              (\ x ->
                 AccessKeyDetails' <$>
                   (x .:? "principalId") <*> (x .:? "userName") <*>
                     (x .:? "accessKeyId")
                     <*> (x .:? "userType"))

instance Hashable AccessKeyDetails where

instance NFData AccessKeyDetails where

-- | An object containing the member's accountId and email address.
--
-- /See:/ 'accountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { _adEmail     :: !Text
  , _adAccountId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adEmail' - Member account's email address.
--
-- * 'adAccountId' - Member account ID.
accountDetail
    :: Text -- ^ 'adEmail'
    -> Text -- ^ 'adAccountId'
    -> AccountDetail
accountDetail pEmail_ pAccountId_ =
  AccountDetail' {_adEmail = pEmail_, _adAccountId = pAccountId_}


-- | Member account's email address.
adEmail :: Lens' AccountDetail Text
adEmail = lens _adEmail (\ s a -> s{_adEmail = a})

-- | Member account ID.
adAccountId :: Lens' AccountDetail Text
adAccountId = lens _adAccountId (\ s a -> s{_adAccountId = a})

instance Hashable AccountDetail where

instance NFData AccountDetail where

instance ToJSON AccountDetail where
        toJSON AccountDetail'{..}
          = object
              (catMaybes
                 [Just ("email" .= _adEmail),
                  Just ("accountId" .= _adAccountId)])

-- | Information about the activity described in a finding.
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aNetworkConnectionAction :: !(Maybe NetworkConnectionAction)
  , _aPortProbeAction         :: !(Maybe PortProbeAction)
  , _aActionType              :: !(Maybe Text)
  , _aDNSRequestAction        :: !(Maybe DNSRequestAction)
  , _aAWSAPICallAction        :: !(Maybe AWSAPICallAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aNetworkConnectionAction' - Information about the NETWORK_CONNECTION action described in this finding.
--
-- * 'aPortProbeAction' - Information about the PORT_PROBE action described in this finding.
--
-- * 'aActionType' - GuardDuty Finding activity type.
--
-- * 'aDNSRequestAction' - Information about the DNS_REQUEST action described in this finding.
--
-- * 'aAWSAPICallAction' - Information about the AWS_API_CALL action described in this finding.
action
    :: Action
action =
  Action'
    { _aNetworkConnectionAction = Nothing
    , _aPortProbeAction = Nothing
    , _aActionType = Nothing
    , _aDNSRequestAction = Nothing
    , _aAWSAPICallAction = Nothing
    }


-- | Information about the NETWORK_CONNECTION action described in this finding.
aNetworkConnectionAction :: Lens' Action (Maybe NetworkConnectionAction)
aNetworkConnectionAction = lens _aNetworkConnectionAction (\ s a -> s{_aNetworkConnectionAction = a})

-- | Information about the PORT_PROBE action described in this finding.
aPortProbeAction :: Lens' Action (Maybe PortProbeAction)
aPortProbeAction = lens _aPortProbeAction (\ s a -> s{_aPortProbeAction = a})

-- | GuardDuty Finding activity type.
aActionType :: Lens' Action (Maybe Text)
aActionType = lens _aActionType (\ s a -> s{_aActionType = a})

-- | Information about the DNS_REQUEST action described in this finding.
aDNSRequestAction :: Lens' Action (Maybe DNSRequestAction)
aDNSRequestAction = lens _aDNSRequestAction (\ s a -> s{_aDNSRequestAction = a})

-- | Information about the AWS_API_CALL action described in this finding.
aAWSAPICallAction :: Lens' Action (Maybe AWSAPICallAction)
aAWSAPICallAction = lens _aAWSAPICallAction (\ s a -> s{_aAWSAPICallAction = a})

instance FromJSON Action where
        parseJSON
          = withObject "Action"
              (\ x ->
                 Action' <$>
                   (x .:? "networkConnectionAction") <*>
                     (x .:? "portProbeAction")
                     <*> (x .:? "actionType")
                     <*> (x .:? "dnsRequestAction")
                     <*> (x .:? "awsApiCallAction"))

instance Hashable Action where

instance NFData Action where

-- | City information of the remote IP address.
--
-- /See:/ 'city' smart constructor.
newtype City = City'
  { _cCityName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'City' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCityName' - City name of the remote IP address.
city
    :: City
city = City' {_cCityName = Nothing}


-- | City name of the remote IP address.
cCityName :: Lens' City (Maybe Text)
cCityName = lens _cCityName (\ s a -> s{_cCityName = a})

instance FromJSON City where
        parseJSON
          = withObject "City"
              (\ x -> City' <$> (x .:? "cityName"))

instance Hashable City where

instance NFData City where

-- | Finding attribute (for example, accountId) for which conditions and values must be specified when querying findings.
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cEQ  :: !(Maybe [Text])
  , _cLte :: !(Maybe Int)
  , _cGT  :: !(Maybe Int)
  , _cNeq :: !(Maybe [Text])
  , _cLT  :: !(Maybe Int)
  , _cGte :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEQ' - Represents the equal condition to be applied to a single field when querying for findings.
--
-- * 'cLte' - Represents the less than equal condition to be applied to a single field when querying for findings.
--
-- * 'cGT' - Represents the greater than condition to be applied to a single field when querying for findings.
--
-- * 'cNeq' - Represents the not equal condition to be applied to a single field when querying for findings.
--
-- * 'cLT' - Represents the less than condition to be applied to a single field when querying for findings.
--
-- * 'cGte' - Represents the greater than equal condition to be applied to a single field when querying for findings.
condition
    :: Condition
condition =
  Condition'
    { _cEQ = Nothing
    , _cLte = Nothing
    , _cGT = Nothing
    , _cNeq = Nothing
    , _cLT = Nothing
    , _cGte = Nothing
    }


-- | Represents the equal condition to be applied to a single field when querying for findings.
cEQ :: Lens' Condition [Text]
cEQ = lens _cEQ (\ s a -> s{_cEQ = a}) . _Default . _Coerce

-- | Represents the less than equal condition to be applied to a single field when querying for findings.
cLte :: Lens' Condition (Maybe Int)
cLte = lens _cLte (\ s a -> s{_cLte = a})

-- | Represents the greater than condition to be applied to a single field when querying for findings.
cGT :: Lens' Condition (Maybe Int)
cGT = lens _cGT (\ s a -> s{_cGT = a})

-- | Represents the not equal condition to be applied to a single field when querying for findings.
cNeq :: Lens' Condition [Text]
cNeq = lens _cNeq (\ s a -> s{_cNeq = a}) . _Default . _Coerce

-- | Represents the less than condition to be applied to a single field when querying for findings.
cLT :: Lens' Condition (Maybe Int)
cLT = lens _cLT (\ s a -> s{_cLT = a})

-- | Represents the greater than equal condition to be applied to a single field when querying for findings.
cGte :: Lens' Condition (Maybe Int)
cGte = lens _cGte (\ s a -> s{_cGte = a})

instance FromJSON Condition where
        parseJSON
          = withObject "Condition"
              (\ x ->
                 Condition' <$>
                   (x .:? "eq" .!= mempty) <*> (x .:? "lte") <*>
                     (x .:? "gt")
                     <*> (x .:? "neq" .!= mempty)
                     <*> (x .:? "lt")
                     <*> (x .:? "gte"))

instance Hashable Condition where

instance NFData Condition where

instance ToJSON Condition where
        toJSON Condition'{..}
          = object
              (catMaybes
                 [("eq" .=) <$> _cEQ, ("lte" .=) <$> _cLte,
                  ("gt" .=) <$> _cGT, ("neq" .=) <$> _cNeq,
                  ("lt" .=) <$> _cLT, ("gte" .=) <$> _cGte])

-- | Country information of the remote IP address.
--
-- /See:/ 'country' smart constructor.
data Country = Country'
  { _cCountryName :: !(Maybe Text)
  , _cCountryCode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Country' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCountryName' - Country name of the remote IP address.
--
-- * 'cCountryCode' - Country code of the remote IP address.
country
    :: Country
country = Country' {_cCountryName = Nothing, _cCountryCode = Nothing}


-- | Country name of the remote IP address.
cCountryName :: Lens' Country (Maybe Text)
cCountryName = lens _cCountryName (\ s a -> s{_cCountryName = a})

-- | Country code of the remote IP address.
cCountryCode :: Lens' Country (Maybe Text)
cCountryCode = lens _cCountryCode (\ s a -> s{_cCountryCode = a})

instance FromJSON Country where
        parseJSON
          = withObject "Country"
              (\ x ->
                 Country' <$>
                   (x .:? "countryName") <*> (x .:? "countryCode"))

instance Hashable Country where

instance NFData Country where

-- | Information about the DNS_REQUEST action described in this finding.
--
-- /See:/ 'dnsRequestAction' smart constructor.
newtype DNSRequestAction = DNSRequestAction'
  { _draDomain :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSRequestAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draDomain' - Domain information for the DNS request.
dnsRequestAction
    :: DNSRequestAction
dnsRequestAction = DNSRequestAction' {_draDomain = Nothing}


-- | Domain information for the DNS request.
draDomain :: Lens' DNSRequestAction (Maybe Text)
draDomain = lens _draDomain (\ s a -> s{_draDomain = a})

instance FromJSON DNSRequestAction where
        parseJSON
          = withObject "DNSRequestAction"
              (\ x -> DNSRequestAction' <$> (x .:? "domain"))

instance Hashable DNSRequestAction where

instance NFData DNSRequestAction where

-- | Domain information for the AWS API call.
--
-- /See:/ 'domainDetails' smart constructor.
data DomainDetails =
  DomainDetails'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainDetails' with the minimum fields required to make a request.
--
domainDetails
    :: DomainDetails
domainDetails = DomainDetails'


instance FromJSON DomainDetails where
        parseJSON
          = withObject "DomainDetails"
              (\ x -> pure DomainDetails')

instance Hashable DomainDetails where

instance NFData DomainDetails where

-- | Representation of a abnormal or suspicious activity.
--
-- /See:/ 'finding' smart constructor.
data Finding = Finding'
  { _fService       :: !(Maybe ServiceInfo)
  , _fConfidence    :: !(Maybe Double)
  , _fPartition     :: !(Maybe Text)
  , _fTitle         :: !(Maybe Text)
  , _fDescription   :: !(Maybe Text)
  , _fAccountId     :: !Text
  , _fSchemaVersion :: !Text
  , _fCreatedAt     :: !Text
  , _fResource      :: !Resource
  , _fSeverity      :: !Double
  , _fUpdatedAt     :: !Text
  , _fType          :: !Text
  , _fRegion        :: !Text
  , _fId            :: !Text
  , _fARN           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fService' - Additional information assigned to the generated finding by GuardDuty.
--
-- * 'fConfidence' - The confidence level of a finding.
--
-- * 'fPartition' - The AWS resource partition.
--
-- * 'fTitle' - The title of a finding.
--
-- * 'fDescription' - The description of a finding.
--
-- * 'fAccountId' - AWS account ID where the activity occurred that prompted GuardDuty to generate a finding.
--
-- * 'fSchemaVersion' - Findings' schema version.
--
-- * 'fCreatedAt' - The time stamp at which a finding was generated.
--
-- * 'fResource' - The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
--
-- * 'fSeverity' - The severity of a finding.
--
-- * 'fUpdatedAt' - The time stamp at which a finding was last updated.
--
-- * 'fType' - The type of a finding described by the action.
--
-- * 'fRegion' - The AWS region where the activity occurred that prompted GuardDuty to generate a finding.
--
-- * 'fId' - The identifier that corresponds to a finding described by the action.
--
-- * 'fARN' - The ARN of a finding described by the action.
finding
    :: Text -- ^ 'fAccountId'
    -> Text -- ^ 'fSchemaVersion'
    -> Text -- ^ 'fCreatedAt'
    -> Resource -- ^ 'fResource'
    -> Double -- ^ 'fSeverity'
    -> Text -- ^ 'fUpdatedAt'
    -> Text -- ^ 'fType'
    -> Text -- ^ 'fRegion'
    -> Text -- ^ 'fId'
    -> Text -- ^ 'fARN'
    -> Finding
finding pAccountId_ pSchemaVersion_ pCreatedAt_ pResource_ pSeverity_ pUpdatedAt_ pType_ pRegion_ pId_ pARN_ =
  Finding'
    { _fService = Nothing
    , _fConfidence = Nothing
    , _fPartition = Nothing
    , _fTitle = Nothing
    , _fDescription = Nothing
    , _fAccountId = pAccountId_
    , _fSchemaVersion = pSchemaVersion_
    , _fCreatedAt = pCreatedAt_
    , _fResource = pResource_
    , _fSeverity = pSeverity_
    , _fUpdatedAt = pUpdatedAt_
    , _fType = pType_
    , _fRegion = pRegion_
    , _fId = pId_
    , _fARN = pARN_
    }


-- | Additional information assigned to the generated finding by GuardDuty.
fService :: Lens' Finding (Maybe ServiceInfo)
fService = lens _fService (\ s a -> s{_fService = a})

-- | The confidence level of a finding.
fConfidence :: Lens' Finding (Maybe Double)
fConfidence = lens _fConfidence (\ s a -> s{_fConfidence = a})

-- | The AWS resource partition.
fPartition :: Lens' Finding (Maybe Text)
fPartition = lens _fPartition (\ s a -> s{_fPartition = a})

-- | The title of a finding.
fTitle :: Lens' Finding (Maybe Text)
fTitle = lens _fTitle (\ s a -> s{_fTitle = a})

-- | The description of a finding.
fDescription :: Lens' Finding (Maybe Text)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a})

-- | AWS account ID where the activity occurred that prompted GuardDuty to generate a finding.
fAccountId :: Lens' Finding Text
fAccountId = lens _fAccountId (\ s a -> s{_fAccountId = a})

-- | Findings' schema version.
fSchemaVersion :: Lens' Finding Text
fSchemaVersion = lens _fSchemaVersion (\ s a -> s{_fSchemaVersion = a})

-- | The time stamp at which a finding was generated.
fCreatedAt :: Lens' Finding Text
fCreatedAt = lens _fCreatedAt (\ s a -> s{_fCreatedAt = a})

-- | The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
fResource :: Lens' Finding Resource
fResource = lens _fResource (\ s a -> s{_fResource = a})

-- | The severity of a finding.
fSeverity :: Lens' Finding Double
fSeverity = lens _fSeverity (\ s a -> s{_fSeverity = a})

-- | The time stamp at which a finding was last updated.
fUpdatedAt :: Lens' Finding Text
fUpdatedAt = lens _fUpdatedAt (\ s a -> s{_fUpdatedAt = a})

-- | The type of a finding described by the action.
fType :: Lens' Finding Text
fType = lens _fType (\ s a -> s{_fType = a})

-- | The AWS region where the activity occurred that prompted GuardDuty to generate a finding.
fRegion :: Lens' Finding Text
fRegion = lens _fRegion (\ s a -> s{_fRegion = a})

-- | The identifier that corresponds to a finding described by the action.
fId :: Lens' Finding Text
fId = lens _fId (\ s a -> s{_fId = a})

-- | The ARN of a finding described by the action.
fARN :: Lens' Finding Text
fARN = lens _fARN (\ s a -> s{_fARN = a})

instance FromJSON Finding where
        parseJSON
          = withObject "Finding"
              (\ x ->
                 Finding' <$>
                   (x .:? "service") <*> (x .:? "confidence") <*>
                     (x .:? "partition")
                     <*> (x .:? "title")
                     <*> (x .:? "description")
                     <*> (x .: "accountId")
                     <*> (x .: "schemaVersion")
                     <*> (x .: "createdAt")
                     <*> (x .: "resource")
                     <*> (x .: "severity")
                     <*> (x .: "updatedAt")
                     <*> (x .: "type")
                     <*> (x .: "region")
                     <*> (x .: "id")
                     <*> (x .: "arn"))

instance Hashable Finding where

instance NFData Finding where

-- | Represents the criteria used for querying findings.
--
-- /See:/ 'findingCriteria' smart constructor.
newtype FindingCriteria = FindingCriteria'
  { _fcCriterion :: Maybe (Map Text Condition)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FindingCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcCriterion' - Represents a map of finding properties that match specified conditions and values when querying findings.
findingCriteria
    :: FindingCriteria
findingCriteria = FindingCriteria' {_fcCriterion = Nothing}


-- | Represents a map of finding properties that match specified conditions and values when querying findings.
fcCriterion :: Lens' FindingCriteria (HashMap Text Condition)
fcCriterion = lens _fcCriterion (\ s a -> s{_fcCriterion = a}) . _Default . _Map

instance FromJSON FindingCriteria where
        parseJSON
          = withObject "FindingCriteria"
              (\ x ->
                 FindingCriteria' <$> (x .:? "criterion" .!= mempty))

instance Hashable FindingCriteria where

instance NFData FindingCriteria where

instance ToJSON FindingCriteria where
        toJSON FindingCriteria'{..}
          = object
              (catMaybes [("criterion" .=) <$> _fcCriterion])

-- | Finding statistics object.
--
-- /See:/ 'findingStatistics' smart constructor.
newtype FindingStatistics = FindingStatistics'
  { _fsCountBySeverity :: Maybe (Map Text Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FindingStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsCountBySeverity' - Represents a map of severity to count statistic for a set of findings
findingStatistics
    :: FindingStatistics
findingStatistics = FindingStatistics' {_fsCountBySeverity = Nothing}


-- | Represents a map of severity to count statistic for a set of findings
fsCountBySeverity :: Lens' FindingStatistics (HashMap Text Int)
fsCountBySeverity = lens _fsCountBySeverity (\ s a -> s{_fsCountBySeverity = a}) . _Default . _Map

instance FromJSON FindingStatistics where
        parseJSON
          = withObject "FindingStatistics"
              (\ x ->
                 FindingStatistics' <$>
                   (x .:? "countBySeverity" .!= mempty))

instance Hashable FindingStatistics where

instance NFData FindingStatistics where

-- | Location information of the remote IP address.
--
-- /See:/ 'geoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { _glLat :: !(Maybe Double)
  , _glLon :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glLat' - Latitude information of remote IP address.
--
-- * 'glLon' - Longitude information of remote IP address.
geoLocation
    :: GeoLocation
geoLocation = GeoLocation' {_glLat = Nothing, _glLon = Nothing}


-- | Latitude information of remote IP address.
glLat :: Lens' GeoLocation (Maybe Double)
glLat = lens _glLat (\ s a -> s{_glLat = a})

-- | Longitude information of remote IP address.
glLon :: Lens' GeoLocation (Maybe Double)
glLon = lens _glLon (\ s a -> s{_glLon = a})

instance FromJSON GeoLocation where
        parseJSON
          = withObject "GeoLocation"
              (\ x ->
                 GeoLocation' <$> (x .:? "lat") <*> (x .:? "lon"))

instance Hashable GeoLocation where

instance NFData GeoLocation where

-- | The profile information of the EC2 instance.
--
-- /See:/ 'iamInstanceProfile' smart constructor.
data IAMInstanceProfile = IAMInstanceProfile'
  { _iapARN :: !(Maybe Text)
  , _iapId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapARN' - AWS EC2 instance profile ARN.
--
-- * 'iapId' - AWS EC2 instance profile ID.
iamInstanceProfile
    :: IAMInstanceProfile
iamInstanceProfile = IAMInstanceProfile' {_iapARN = Nothing, _iapId = Nothing}


-- | AWS EC2 instance profile ARN.
iapARN :: Lens' IAMInstanceProfile (Maybe Text)
iapARN = lens _iapARN (\ s a -> s{_iapARN = a})

-- | AWS EC2 instance profile ID.
iapId :: Lens' IAMInstanceProfile (Maybe Text)
iapId = lens _iapId (\ s a -> s{_iapId = a})

instance FromJSON IAMInstanceProfile where
        parseJSON
          = withObject "IAMInstanceProfile"
              (\ x ->
                 IAMInstanceProfile' <$>
                   (x .:? "arn") <*> (x .:? "id"))

instance Hashable IAMInstanceProfile where

instance NFData IAMInstanceProfile where

-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'instanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { _idInstanceId         :: !(Maybe Text)
  , _idPlatform           :: !(Maybe Text)
  , _idLaunchTime         :: !(Maybe Text)
  , _idNetworkInterfaces  :: !(Maybe [NetworkInterface])
  , _idInstanceType       :: !(Maybe Text)
  , _idAvailabilityZone   :: !(Maybe Text)
  , _idIAMInstanceProfile :: !(Maybe IAMInstanceProfile)
  , _idImageId            :: !(Maybe Text)
  , _idProductCodes       :: !(Maybe [ProductCode])
  , _idInstanceState      :: !(Maybe Text)
  , _idTags               :: !(Maybe [Tag])
  , _idImageDescription   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idInstanceId' - The ID of the EC2 instance.
--
-- * 'idPlatform' - The platform of the EC2 instance.
--
-- * 'idLaunchTime' - The launch time of the EC2 instance.
--
-- * 'idNetworkInterfaces' - The network interface information of the EC2 instance.
--
-- * 'idInstanceType' - The type of the EC2 instance.
--
-- * 'idAvailabilityZone' - The availability zone of the EC2 instance.
--
-- * 'idIAMInstanceProfile' - Undocumented member.
--
-- * 'idImageId' - The image ID of the EC2 instance.
--
-- * 'idProductCodes' - The product code of the EC2 instance.
--
-- * 'idInstanceState' - The state of the EC2 instance.
--
-- * 'idTags' - The tags of the EC2 instance.
--
-- * 'idImageDescription' - The image description of the EC2 instance.
instanceDetails
    :: InstanceDetails
instanceDetails =
  InstanceDetails'
    { _idInstanceId = Nothing
    , _idPlatform = Nothing
    , _idLaunchTime = Nothing
    , _idNetworkInterfaces = Nothing
    , _idInstanceType = Nothing
    , _idAvailabilityZone = Nothing
    , _idIAMInstanceProfile = Nothing
    , _idImageId = Nothing
    , _idProductCodes = Nothing
    , _idInstanceState = Nothing
    , _idTags = Nothing
    , _idImageDescription = Nothing
    }


-- | The ID of the EC2 instance.
idInstanceId :: Lens' InstanceDetails (Maybe Text)
idInstanceId = lens _idInstanceId (\ s a -> s{_idInstanceId = a})

-- | The platform of the EC2 instance.
idPlatform :: Lens' InstanceDetails (Maybe Text)
idPlatform = lens _idPlatform (\ s a -> s{_idPlatform = a})

-- | The launch time of the EC2 instance.
idLaunchTime :: Lens' InstanceDetails (Maybe Text)
idLaunchTime = lens _idLaunchTime (\ s a -> s{_idLaunchTime = a})

-- | The network interface information of the EC2 instance.
idNetworkInterfaces :: Lens' InstanceDetails [NetworkInterface]
idNetworkInterfaces = lens _idNetworkInterfaces (\ s a -> s{_idNetworkInterfaces = a}) . _Default . _Coerce

-- | The type of the EC2 instance.
idInstanceType :: Lens' InstanceDetails (Maybe Text)
idInstanceType = lens _idInstanceType (\ s a -> s{_idInstanceType = a})

-- | The availability zone of the EC2 instance.
idAvailabilityZone :: Lens' InstanceDetails (Maybe Text)
idAvailabilityZone = lens _idAvailabilityZone (\ s a -> s{_idAvailabilityZone = a})

-- | Undocumented member.
idIAMInstanceProfile :: Lens' InstanceDetails (Maybe IAMInstanceProfile)
idIAMInstanceProfile = lens _idIAMInstanceProfile (\ s a -> s{_idIAMInstanceProfile = a})

-- | The image ID of the EC2 instance.
idImageId :: Lens' InstanceDetails (Maybe Text)
idImageId = lens _idImageId (\ s a -> s{_idImageId = a})

-- | The product code of the EC2 instance.
idProductCodes :: Lens' InstanceDetails [ProductCode]
idProductCodes = lens _idProductCodes (\ s a -> s{_idProductCodes = a}) . _Default . _Coerce

-- | The state of the EC2 instance.
idInstanceState :: Lens' InstanceDetails (Maybe Text)
idInstanceState = lens _idInstanceState (\ s a -> s{_idInstanceState = a})

-- | The tags of the EC2 instance.
idTags :: Lens' InstanceDetails [Tag]
idTags = lens _idTags (\ s a -> s{_idTags = a}) . _Default . _Coerce

-- | The image description of the EC2 instance.
idImageDescription :: Lens' InstanceDetails (Maybe Text)
idImageDescription = lens _idImageDescription (\ s a -> s{_idImageDescription = a})

instance FromJSON InstanceDetails where
        parseJSON
          = withObject "InstanceDetails"
              (\ x ->
                 InstanceDetails' <$>
                   (x .:? "instanceId") <*> (x .:? "platform") <*>
                     (x .:? "launchTime")
                     <*> (x .:? "networkInterfaces" .!= mempty)
                     <*> (x .:? "instanceType")
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "iamInstanceProfile")
                     <*> (x .:? "imageId")
                     <*> (x .:? "productCodes" .!= mempty)
                     <*> (x .:? "instanceState")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "imageDescription"))

instance Hashable InstanceDetails where

instance NFData InstanceDetails where

-- | Invitation from an AWS account to become the current account's master.
--
-- /See:/ 'invitation' smart constructor.
data Invitation = Invitation'
  { _iInvitedAt          :: !(Maybe Text)
  , _iRelationshipStatus :: !(Maybe Text)
  , _iInvitationId       :: !(Maybe Text)
  , _iAccountId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Invitation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInvitedAt' - Timestamp at which the invitation was sent
--
-- * 'iRelationshipStatus' - The status of the relationship between the inviter and invitee accounts.
--
-- * 'iInvitationId' - This value is used to validate the inviter account to the member account.
--
-- * 'iAccountId' - Inviter account ID
invitation
    :: Invitation
invitation =
  Invitation'
    { _iInvitedAt = Nothing
    , _iRelationshipStatus = Nothing
    , _iInvitationId = Nothing
    , _iAccountId = Nothing
    }


-- | Timestamp at which the invitation was sent
iInvitedAt :: Lens' Invitation (Maybe Text)
iInvitedAt = lens _iInvitedAt (\ s a -> s{_iInvitedAt = a})

-- | The status of the relationship between the inviter and invitee accounts.
iRelationshipStatus :: Lens' Invitation (Maybe Text)
iRelationshipStatus = lens _iRelationshipStatus (\ s a -> s{_iRelationshipStatus = a})

-- | This value is used to validate the inviter account to the member account.
iInvitationId :: Lens' Invitation (Maybe Text)
iInvitationId = lens _iInvitationId (\ s a -> s{_iInvitationId = a})

-- | Inviter account ID
iAccountId :: Lens' Invitation (Maybe Text)
iAccountId = lens _iAccountId (\ s a -> s{_iAccountId = a})

instance FromJSON Invitation where
        parseJSON
          = withObject "Invitation"
              (\ x ->
                 Invitation' <$>
                   (x .:? "invitedAt") <*> (x .:? "relationshipStatus")
                     <*> (x .:? "invitationId")
                     <*> (x .:? "accountId"))

instance Hashable Invitation where

instance NFData Invitation where

-- | Local port information of the connection.
--
-- /See:/ 'localPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { _lpdPortName :: !(Maybe Text)
  , _lpdPort     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalPortDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpdPortName' - Port name of the local connection.
--
-- * 'lpdPort' - Port number of the local connection.
localPortDetails
    :: LocalPortDetails
localPortDetails =
  LocalPortDetails' {_lpdPortName = Nothing, _lpdPort = Nothing}


-- | Port name of the local connection.
lpdPortName :: Lens' LocalPortDetails (Maybe Text)
lpdPortName = lens _lpdPortName (\ s a -> s{_lpdPortName = a})

-- | Port number of the local connection.
lpdPort :: Lens' LocalPortDetails (Maybe Int)
lpdPort = lens _lpdPort (\ s a -> s{_lpdPort = a})

instance FromJSON LocalPortDetails where
        parseJSON
          = withObject "LocalPortDetails"
              (\ x ->
                 LocalPortDetails' <$>
                   (x .:? "portName") <*> (x .:? "port"))

instance Hashable LocalPortDetails where

instance NFData LocalPortDetails where

-- | Contains details about the master account.
--
-- /See:/ 'master' smart constructor.
data Master = Master'
  { _masInvitedAt          :: !(Maybe Text)
  , _masRelationshipStatus :: !(Maybe Text)
  , _masInvitationId       :: !(Maybe Text)
  , _masAccountId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Master' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'masInvitedAt' - Timestamp at which the invitation was sent
--
-- * 'masRelationshipStatus' - The status of the relationship between the master and member accounts.
--
-- * 'masInvitationId' - This value is used to validate the master account to the member account.
--
-- * 'masAccountId' - Master account ID
master
    :: Master
master =
  Master'
    { _masInvitedAt = Nothing
    , _masRelationshipStatus = Nothing
    , _masInvitationId = Nothing
    , _masAccountId = Nothing
    }


-- | Timestamp at which the invitation was sent
masInvitedAt :: Lens' Master (Maybe Text)
masInvitedAt = lens _masInvitedAt (\ s a -> s{_masInvitedAt = a})

-- | The status of the relationship between the master and member accounts.
masRelationshipStatus :: Lens' Master (Maybe Text)
masRelationshipStatus = lens _masRelationshipStatus (\ s a -> s{_masRelationshipStatus = a})

-- | This value is used to validate the master account to the member account.
masInvitationId :: Lens' Master (Maybe Text)
masInvitationId = lens _masInvitationId (\ s a -> s{_masInvitationId = a})

-- | Master account ID
masAccountId :: Lens' Master (Maybe Text)
masAccountId = lens _masAccountId (\ s a -> s{_masAccountId = a})

instance FromJSON Master where
        parseJSON
          = withObject "Master"
              (\ x ->
                 Master' <$>
                   (x .:? "invitedAt") <*> (x .:? "relationshipStatus")
                     <*> (x .:? "invitationId")
                     <*> (x .:? "accountId"))

instance Hashable Master where

instance NFData Master where

-- | Contains details about the member account.
--
-- /See:/ 'member' smart constructor.
data Member = Member'
  { _mInvitedAt          :: !(Maybe Text)
  , _mDetectorId         :: !(Maybe Text)
  , _mEmail              :: !Text
  , _mAccountId          :: !Text
  , _mMasterId           :: !Text
  , _mUpdatedAt          :: !Text
  , _mRelationshipStatus :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mInvitedAt' - Timestamp at which the invitation was sent
--
-- * 'mDetectorId' - Undocumented member.
--
-- * 'mEmail' - Member account's email address.
--
-- * 'mAccountId' - Undocumented member.
--
-- * 'mMasterId' - Undocumented member.
--
-- * 'mUpdatedAt' - Undocumented member.
--
-- * 'mRelationshipStatus' - The status of the relationship between the member and the master.
member
    :: Text -- ^ 'mEmail'
    -> Text -- ^ 'mAccountId'
    -> Text -- ^ 'mMasterId'
    -> Text -- ^ 'mUpdatedAt'
    -> Text -- ^ 'mRelationshipStatus'
    -> Member
member pEmail_ pAccountId_ pMasterId_ pUpdatedAt_ pRelationshipStatus_ =
  Member'
    { _mInvitedAt = Nothing
    , _mDetectorId = Nothing
    , _mEmail = pEmail_
    , _mAccountId = pAccountId_
    , _mMasterId = pMasterId_
    , _mUpdatedAt = pUpdatedAt_
    , _mRelationshipStatus = pRelationshipStatus_
    }


-- | Timestamp at which the invitation was sent
mInvitedAt :: Lens' Member (Maybe Text)
mInvitedAt = lens _mInvitedAt (\ s a -> s{_mInvitedAt = a})

-- | Undocumented member.
mDetectorId :: Lens' Member (Maybe Text)
mDetectorId = lens _mDetectorId (\ s a -> s{_mDetectorId = a})

-- | Member account's email address.
mEmail :: Lens' Member Text
mEmail = lens _mEmail (\ s a -> s{_mEmail = a})

-- | Undocumented member.
mAccountId :: Lens' Member Text
mAccountId = lens _mAccountId (\ s a -> s{_mAccountId = a})

-- | Undocumented member.
mMasterId :: Lens' Member Text
mMasterId = lens _mMasterId (\ s a -> s{_mMasterId = a})

-- | Undocumented member.
mUpdatedAt :: Lens' Member Text
mUpdatedAt = lens _mUpdatedAt (\ s a -> s{_mUpdatedAt = a})

-- | The status of the relationship between the member and the master.
mRelationshipStatus :: Lens' Member Text
mRelationshipStatus = lens _mRelationshipStatus (\ s a -> s{_mRelationshipStatus = a})

instance FromJSON Member where
        parseJSON
          = withObject "Member"
              (\ x ->
                 Member' <$>
                   (x .:? "invitedAt") <*> (x .:? "detectorId") <*>
                     (x .: "email")
                     <*> (x .: "accountId")
                     <*> (x .: "masterId")
                     <*> (x .: "updatedAt")
                     <*> (x .: "relationshipStatus"))

instance Hashable Member where

instance NFData Member where

-- | Information about the NETWORK_CONNECTION action described in this finding.
--
-- /See:/ 'networkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { _ncaRemoteIPDetails     :: !(Maybe RemoteIPDetails)
  , _ncaProtocol            :: !(Maybe Text)
  , _ncaRemotePortDetails   :: !(Maybe RemotePortDetails)
  , _ncaBlocked             :: !(Maybe Bool)
  , _ncaConnectionDirection :: !(Maybe Text)
  , _ncaLocalPortDetails    :: !(Maybe LocalPortDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkConnectionAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncaRemoteIPDetails' - Remote IP information of the connection.
--
-- * 'ncaProtocol' - Network connection protocol.
--
-- * 'ncaRemotePortDetails' - Remote port information of the connection.
--
-- * 'ncaBlocked' - Network connection blocked information.
--
-- * 'ncaConnectionDirection' - Network connection direction.
--
-- * 'ncaLocalPortDetails' - Local port information of the connection.
networkConnectionAction
    :: NetworkConnectionAction
networkConnectionAction =
  NetworkConnectionAction'
    { _ncaRemoteIPDetails = Nothing
    , _ncaProtocol = Nothing
    , _ncaRemotePortDetails = Nothing
    , _ncaBlocked = Nothing
    , _ncaConnectionDirection = Nothing
    , _ncaLocalPortDetails = Nothing
    }


-- | Remote IP information of the connection.
ncaRemoteIPDetails :: Lens' NetworkConnectionAction (Maybe RemoteIPDetails)
ncaRemoteIPDetails = lens _ncaRemoteIPDetails (\ s a -> s{_ncaRemoteIPDetails = a})

-- | Network connection protocol.
ncaProtocol :: Lens' NetworkConnectionAction (Maybe Text)
ncaProtocol = lens _ncaProtocol (\ s a -> s{_ncaProtocol = a})

-- | Remote port information of the connection.
ncaRemotePortDetails :: Lens' NetworkConnectionAction (Maybe RemotePortDetails)
ncaRemotePortDetails = lens _ncaRemotePortDetails (\ s a -> s{_ncaRemotePortDetails = a})

-- | Network connection blocked information.
ncaBlocked :: Lens' NetworkConnectionAction (Maybe Bool)
ncaBlocked = lens _ncaBlocked (\ s a -> s{_ncaBlocked = a})

-- | Network connection direction.
ncaConnectionDirection :: Lens' NetworkConnectionAction (Maybe Text)
ncaConnectionDirection = lens _ncaConnectionDirection (\ s a -> s{_ncaConnectionDirection = a})

-- | Local port information of the connection.
ncaLocalPortDetails :: Lens' NetworkConnectionAction (Maybe LocalPortDetails)
ncaLocalPortDetails = lens _ncaLocalPortDetails (\ s a -> s{_ncaLocalPortDetails = a})

instance FromJSON NetworkConnectionAction where
        parseJSON
          = withObject "NetworkConnectionAction"
              (\ x ->
                 NetworkConnectionAction' <$>
                   (x .:? "remoteIpDetails") <*> (x .:? "protocol") <*>
                     (x .:? "remotePortDetails")
                     <*> (x .:? "blocked")
                     <*> (x .:? "connectionDirection")
                     <*> (x .:? "localPortDetails"))

instance Hashable NetworkConnectionAction where

instance NFData NetworkConnectionAction where

-- | The network interface information of the EC2 instance.
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niPrivateIPAddresses :: !(Maybe [PrivateIPAddressDetails])
  , _niPublicDNSName      :: !(Maybe Text)
  , _niSecurityGroups     :: !(Maybe [SecurityGroup])
  , _niVPCId              :: !(Maybe Text)
  , _niNetworkInterfaceId :: !(Maybe Text)
  , _niSubnetId           :: !(Maybe Text)
  , _niPrivateIPAddress   :: !(Maybe Text)
  , _niPublicIP           :: !(Maybe Text)
  , _niPrivateDNSName     :: !(Maybe Text)
  , _niIPv6Addresses      :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niPrivateIPAddresses' - Other private IP address information of the EC2 instance.
--
-- * 'niPublicDNSName' - Public DNS name of the EC2 instance.
--
-- * 'niSecurityGroups' - Security groups associated with the EC2 instance.
--
-- * 'niVPCId' - The VPC ID of the EC2 instance.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface
--
-- * 'niSubnetId' - The subnet ID of the EC2 instance.
--
-- * 'niPrivateIPAddress' - Private IP address of the EC2 instance.
--
-- * 'niPublicIP' - Public IP address of the EC2 instance.
--
-- * 'niPrivateDNSName' - Private DNS name of the EC2 instance.
--
-- * 'niIPv6Addresses' - A list of EC2 instance IPv6 address information.
networkInterface
    :: NetworkInterface
networkInterface =
  NetworkInterface'
    { _niPrivateIPAddresses = Nothing
    , _niPublicDNSName = Nothing
    , _niSecurityGroups = Nothing
    , _niVPCId = Nothing
    , _niNetworkInterfaceId = Nothing
    , _niSubnetId = Nothing
    , _niPrivateIPAddress = Nothing
    , _niPublicIP = Nothing
    , _niPrivateDNSName = Nothing
    , _niIPv6Addresses = Nothing
    }


-- | Other private IP address information of the EC2 instance.
niPrivateIPAddresses :: Lens' NetworkInterface [PrivateIPAddressDetails]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\ s a -> s{_niPrivateIPAddresses = a}) . _Default . _Coerce

-- | Public DNS name of the EC2 instance.
niPublicDNSName :: Lens' NetworkInterface (Maybe Text)
niPublicDNSName = lens _niPublicDNSName (\ s a -> s{_niPublicDNSName = a})

-- | Security groups associated with the EC2 instance.
niSecurityGroups :: Lens' NetworkInterface [SecurityGroup]
niSecurityGroups = lens _niSecurityGroups (\ s a -> s{_niSecurityGroups = a}) . _Default . _Coerce

-- | The VPC ID of the EC2 instance.
niVPCId :: Lens' NetworkInterface (Maybe Text)
niVPCId = lens _niVPCId (\ s a -> s{_niVPCId = a})

-- | The ID of the network interface
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\ s a -> s{_niNetworkInterfaceId = a})

-- | The subnet ID of the EC2 instance.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\ s a -> s{_niSubnetId = a})

-- | Private IP address of the EC2 instance.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\ s a -> s{_niPrivateIPAddress = a})

-- | Public IP address of the EC2 instance.
niPublicIP :: Lens' NetworkInterface (Maybe Text)
niPublicIP = lens _niPublicIP (\ s a -> s{_niPublicIP = a})

-- | Private DNS name of the EC2 instance.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\ s a -> s{_niPrivateDNSName = a})

-- | A list of EC2 instance IPv6 address information.
niIPv6Addresses :: Lens' NetworkInterface [Text]
niIPv6Addresses = lens _niIPv6Addresses (\ s a -> s{_niIPv6Addresses = a}) . _Default . _Coerce

instance FromJSON NetworkInterface where
        parseJSON
          = withObject "NetworkInterface"
              (\ x ->
                 NetworkInterface' <$>
                   (x .:? "privateIpAddresses" .!= mempty) <*>
                     (x .:? "publicDnsName")
                     <*> (x .:? "securityGroups" .!= mempty)
                     <*> (x .:? "vpcId")
                     <*> (x .:? "networkInterfaceId")
                     <*> (x .:? "subnetId")
                     <*> (x .:? "privateIpAddress")
                     <*> (x .:? "publicIp")
                     <*> (x .:? "privateDnsName")
                     <*> (x .:? "ipv6Addresses" .!= mempty))

instance Hashable NetworkInterface where

instance NFData NetworkInterface where

-- | ISP Organization information of the remote IP address.
--
-- /See:/ 'organization' smart constructor.
data Organization = Organization'
  { _oOrg    :: !(Maybe Text)
  , _oASNOrg :: !(Maybe Text)
  , _oASN    :: !(Maybe Text)
  , _oIsp    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOrg' - Name of the internet provider.
--
-- * 'oASNOrg' - Organization that registered this ASN.
--
-- * 'oASN' - Autonomous system number of the internet provider of the remote IP address.
--
-- * 'oIsp' - ISP information for the internet provider.
organization
    :: Organization
organization =
  Organization'
    {_oOrg = Nothing, _oASNOrg = Nothing, _oASN = Nothing, _oIsp = Nothing}


-- | Name of the internet provider.
oOrg :: Lens' Organization (Maybe Text)
oOrg = lens _oOrg (\ s a -> s{_oOrg = a})

-- | Organization that registered this ASN.
oASNOrg :: Lens' Organization (Maybe Text)
oASNOrg = lens _oASNOrg (\ s a -> s{_oASNOrg = a})

-- | Autonomous system number of the internet provider of the remote IP address.
oASN :: Lens' Organization (Maybe Text)
oASN = lens _oASN (\ s a -> s{_oASN = a})

-- | ISP information for the internet provider.
oIsp :: Lens' Organization (Maybe Text)
oIsp = lens _oIsp (\ s a -> s{_oIsp = a})

instance FromJSON Organization where
        parseJSON
          = withObject "Organization"
              (\ x ->
                 Organization' <$>
                   (x .:? "org") <*> (x .:? "asnOrg") <*> (x .:? "asn")
                     <*> (x .:? "isp"))

instance Hashable Organization where

instance NFData Organization where

-- | Information about the PORT_PROBE action described in this finding.
--
-- /See:/ 'portProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { _ppaPortProbeDetails :: !(Maybe [PortProbeDetail])
  , _ppaBlocked          :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortProbeAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppaPortProbeDetails' - A list of port probe details objects.
--
-- * 'ppaBlocked' - Port probe blocked information.
portProbeAction
    :: PortProbeAction
portProbeAction =
  PortProbeAction' {_ppaPortProbeDetails = Nothing, _ppaBlocked = Nothing}


-- | A list of port probe details objects.
ppaPortProbeDetails :: Lens' PortProbeAction [PortProbeDetail]
ppaPortProbeDetails = lens _ppaPortProbeDetails (\ s a -> s{_ppaPortProbeDetails = a}) . _Default . _Coerce

-- | Port probe blocked information.
ppaBlocked :: Lens' PortProbeAction (Maybe Bool)
ppaBlocked = lens _ppaBlocked (\ s a -> s{_ppaBlocked = a})

instance FromJSON PortProbeAction where
        parseJSON
          = withObject "PortProbeAction"
              (\ x ->
                 PortProbeAction' <$>
                   (x .:? "portProbeDetails" .!= mempty) <*>
                     (x .:? "blocked"))

instance Hashable PortProbeAction where

instance NFData PortProbeAction where

-- | Details about the port probe finding.
--
-- /See:/ 'portProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { _ppdRemoteIPDetails  :: !(Maybe RemoteIPDetails)
  , _ppdLocalPortDetails :: !(Maybe LocalPortDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortProbeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdRemoteIPDetails' - Remote IP information of the connection.
--
-- * 'ppdLocalPortDetails' - Local port information of the connection.
portProbeDetail
    :: PortProbeDetail
portProbeDetail =
  PortProbeDetail'
    {_ppdRemoteIPDetails = Nothing, _ppdLocalPortDetails = Nothing}


-- | Remote IP information of the connection.
ppdRemoteIPDetails :: Lens' PortProbeDetail (Maybe RemoteIPDetails)
ppdRemoteIPDetails = lens _ppdRemoteIPDetails (\ s a -> s{_ppdRemoteIPDetails = a})

-- | Local port information of the connection.
ppdLocalPortDetails :: Lens' PortProbeDetail (Maybe LocalPortDetails)
ppdLocalPortDetails = lens _ppdLocalPortDetails (\ s a -> s{_ppdLocalPortDetails = a})

instance FromJSON PortProbeDetail where
        parseJSON
          = withObject "PortProbeDetail"
              (\ x ->
                 PortProbeDetail' <$>
                   (x .:? "remoteIpDetails") <*>
                     (x .:? "localPortDetails"))

instance Hashable PortProbeDetail where

instance NFData PortProbeDetail where

-- | Other private IP address information of the EC2 instance.
--
-- /See:/ 'privateIPAddressDetails' smart constructor.
data PrivateIPAddressDetails = PrivateIPAddressDetails'
  { _piadPrivateIPAddress :: !(Maybe Text)
  , _piadPrivateDNSName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PrivateIPAddressDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadPrivateIPAddress' - Private IP address of the EC2 instance.
--
-- * 'piadPrivateDNSName' - Private DNS name of the EC2 instance.
privateIPAddressDetails
    :: PrivateIPAddressDetails
privateIPAddressDetails =
  PrivateIPAddressDetails'
    {_piadPrivateIPAddress = Nothing, _piadPrivateDNSName = Nothing}


-- | Private IP address of the EC2 instance.
piadPrivateIPAddress :: Lens' PrivateIPAddressDetails (Maybe Text)
piadPrivateIPAddress = lens _piadPrivateIPAddress (\ s a -> s{_piadPrivateIPAddress = a})

-- | Private DNS name of the EC2 instance.
piadPrivateDNSName :: Lens' PrivateIPAddressDetails (Maybe Text)
piadPrivateDNSName = lens _piadPrivateDNSName (\ s a -> s{_piadPrivateDNSName = a})

instance FromJSON PrivateIPAddressDetails where
        parseJSON
          = withObject "PrivateIPAddressDetails"
              (\ x ->
                 PrivateIPAddressDetails' <$>
                   (x .:? "privateIpAddress") <*>
                     (x .:? "privateDnsName"))

instance Hashable PrivateIPAddressDetails where

instance NFData PrivateIPAddressDetails where

-- | The product code of the EC2 instance.
--
-- /See:/ 'productCode' smart constructor.
data ProductCode = ProductCode'
  { _pcProductType :: !(Maybe Text)
  , _pcCode        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcProductType' - Product code type.
--
-- * 'pcCode' - Product code information.
productCode
    :: ProductCode
productCode = ProductCode' {_pcProductType = Nothing, _pcCode = Nothing}


-- | Product code type.
pcProductType :: Lens' ProductCode (Maybe Text)
pcProductType = lens _pcProductType (\ s a -> s{_pcProductType = a})

-- | Product code information.
pcCode :: Lens' ProductCode (Maybe Text)
pcCode = lens _pcCode (\ s a -> s{_pcCode = a})

instance FromJSON ProductCode where
        parseJSON
          = withObject "ProductCode"
              (\ x ->
                 ProductCode' <$>
                   (x .:? "productType") <*> (x .:? "code"))

instance Hashable ProductCode where

instance NFData ProductCode where

-- | Remote IP information of the connection.
--
-- /See:/ 'remoteIPDetails' smart constructor.
data RemoteIPDetails = RemoteIPDetails'
  { _ridCountry      :: !(Maybe Country)
  , _ridCity         :: !(Maybe City)
  , _ridIPAddressV4  :: !(Maybe Text)
  , _ridGeoLocation  :: !(Maybe GeoLocation)
  , _ridOrganization :: !(Maybe Organization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoteIPDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ridCountry' - Country code of the remote IP address.
--
-- * 'ridCity' - City information of the remote IP address.
--
-- * 'ridIPAddressV4' - IPV4 remote address of the connection.
--
-- * 'ridGeoLocation' - Location information of the remote IP address.
--
-- * 'ridOrganization' - ISP Organization information of the remote IP address.
remoteIPDetails
    :: RemoteIPDetails
remoteIPDetails =
  RemoteIPDetails'
    { _ridCountry = Nothing
    , _ridCity = Nothing
    , _ridIPAddressV4 = Nothing
    , _ridGeoLocation = Nothing
    , _ridOrganization = Nothing
    }


-- | Country code of the remote IP address.
ridCountry :: Lens' RemoteIPDetails (Maybe Country)
ridCountry = lens _ridCountry (\ s a -> s{_ridCountry = a})

-- | City information of the remote IP address.
ridCity :: Lens' RemoteIPDetails (Maybe City)
ridCity = lens _ridCity (\ s a -> s{_ridCity = a})

-- | IPV4 remote address of the connection.
ridIPAddressV4 :: Lens' RemoteIPDetails (Maybe Text)
ridIPAddressV4 = lens _ridIPAddressV4 (\ s a -> s{_ridIPAddressV4 = a})

-- | Location information of the remote IP address.
ridGeoLocation :: Lens' RemoteIPDetails (Maybe GeoLocation)
ridGeoLocation = lens _ridGeoLocation (\ s a -> s{_ridGeoLocation = a})

-- | ISP Organization information of the remote IP address.
ridOrganization :: Lens' RemoteIPDetails (Maybe Organization)
ridOrganization = lens _ridOrganization (\ s a -> s{_ridOrganization = a})

instance FromJSON RemoteIPDetails where
        parseJSON
          = withObject "RemoteIPDetails"
              (\ x ->
                 RemoteIPDetails' <$>
                   (x .:? "country") <*> (x .:? "city") <*>
                     (x .:? "ipAddressV4")
                     <*> (x .:? "geoLocation")
                     <*> (x .:? "organization"))

instance Hashable RemoteIPDetails where

instance NFData RemoteIPDetails where

-- | Remote port information of the connection.
--
-- /See:/ 'remotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { _rpdPortName :: !(Maybe Text)
  , _rpdPort     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemotePortDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpdPortName' - Port name of the remote connection.
--
-- * 'rpdPort' - Port number of the remote connection.
remotePortDetails
    :: RemotePortDetails
remotePortDetails =
  RemotePortDetails' {_rpdPortName = Nothing, _rpdPort = Nothing}


-- | Port name of the remote connection.
rpdPortName :: Lens' RemotePortDetails (Maybe Text)
rpdPortName = lens _rpdPortName (\ s a -> s{_rpdPortName = a})

-- | Port number of the remote connection.
rpdPort :: Lens' RemotePortDetails (Maybe Int)
rpdPort = lens _rpdPort (\ s a -> s{_rpdPort = a})

instance FromJSON RemotePortDetails where
        parseJSON
          = withObject "RemotePortDetails"
              (\ x ->
                 RemotePortDetails' <$>
                   (x .:? "portName") <*> (x .:? "port"))

instance Hashable RemotePortDetails where

instance NFData RemotePortDetails where

-- | The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceType     :: !(Maybe Text)
  , _rInstanceDetails  :: !(Maybe InstanceDetails)
  , _rAccessKeyDetails :: !(Maybe AccessKeyDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceType' - The type of the AWS resource.
--
-- * 'rInstanceDetails' - Undocumented member.
--
-- * 'rAccessKeyDetails' - Undocumented member.
resource
    :: Resource
resource =
  Resource'
    { _rResourceType = Nothing
    , _rInstanceDetails = Nothing
    , _rAccessKeyDetails = Nothing
    }


-- | The type of the AWS resource.
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a})

-- | Undocumented member.
rInstanceDetails :: Lens' Resource (Maybe InstanceDetails)
rInstanceDetails = lens _rInstanceDetails (\ s a -> s{_rInstanceDetails = a})

-- | Undocumented member.
rAccessKeyDetails :: Lens' Resource (Maybe AccessKeyDetails)
rAccessKeyDetails = lens _rAccessKeyDetails (\ s a -> s{_rAccessKeyDetails = a})

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "resourceType") <*> (x .:? "instanceDetails")
                     <*> (x .:? "accessKeyDetails"))

instance Hashable Resource where

instance NFData Resource where

-- | Security groups associated with the EC2 instance.
--
-- /See:/ 'securityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { _sgGroupId   :: !(Maybe Text)
  , _sgGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgGroupId' - EC2 instance's security group ID.
--
-- * 'sgGroupName' - EC2 instance's security group name.
securityGroup
    :: SecurityGroup
securityGroup = SecurityGroup' {_sgGroupId = Nothing, _sgGroupName = Nothing}


-- | EC2 instance's security group ID.
sgGroupId :: Lens' SecurityGroup (Maybe Text)
sgGroupId = lens _sgGroupId (\ s a -> s{_sgGroupId = a})

-- | EC2 instance's security group name.
sgGroupName :: Lens' SecurityGroup (Maybe Text)
sgGroupName = lens _sgGroupName (\ s a -> s{_sgGroupName = a})

instance FromJSON SecurityGroup where
        parseJSON
          = withObject "SecurityGroup"
              (\ x ->
                 SecurityGroup' <$>
                   (x .:? "groupId") <*> (x .:? "groupName"))

instance Hashable SecurityGroup where

instance NFData SecurityGroup where

-- | Additional information assigned to the generated finding by GuardDuty.
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _siCount          :: !(Maybe Int)
  , _siEventFirstSeen :: !(Maybe Text)
  , _siAction         :: !(Maybe Action)
  , _siDetectorId     :: !(Maybe Text)
  , _siServiceName    :: !(Maybe Text)
  , _siUserFeedback   :: !(Maybe Text)
  , _siEventLastSeen  :: !(Maybe Text)
  , _siResourceRole   :: !(Maybe Text)
  , _siArchived       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siCount' - Total count of the occurrences of this finding type.
--
-- * 'siEventFirstSeen' - First seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- * 'siAction' - Information about the activity described in a finding.
--
-- * 'siDetectorId' - Detector ID for the GuardDuty service.
--
-- * 'siServiceName' - The name of the AWS service (GuardDuty) that generated a finding.
--
-- * 'siUserFeedback' - Feedback left about the finding.
--
-- * 'siEventLastSeen' - Last seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- * 'siResourceRole' - Resource role information for this finding.
--
-- * 'siArchived' - Indicates whether this finding is archived.
serviceInfo
    :: ServiceInfo
serviceInfo =
  ServiceInfo'
    { _siCount = Nothing
    , _siEventFirstSeen = Nothing
    , _siAction = Nothing
    , _siDetectorId = Nothing
    , _siServiceName = Nothing
    , _siUserFeedback = Nothing
    , _siEventLastSeen = Nothing
    , _siResourceRole = Nothing
    , _siArchived = Nothing
    }


-- | Total count of the occurrences of this finding type.
siCount :: Lens' ServiceInfo (Maybe Int)
siCount = lens _siCount (\ s a -> s{_siCount = a})

-- | First seen timestamp of the activity that prompted GuardDuty to generate this finding.
siEventFirstSeen :: Lens' ServiceInfo (Maybe Text)
siEventFirstSeen = lens _siEventFirstSeen (\ s a -> s{_siEventFirstSeen = a})

-- | Information about the activity described in a finding.
siAction :: Lens' ServiceInfo (Maybe Action)
siAction = lens _siAction (\ s a -> s{_siAction = a})

-- | Detector ID for the GuardDuty service.
siDetectorId :: Lens' ServiceInfo (Maybe Text)
siDetectorId = lens _siDetectorId (\ s a -> s{_siDetectorId = a})

-- | The name of the AWS service (GuardDuty) that generated a finding.
siServiceName :: Lens' ServiceInfo (Maybe Text)
siServiceName = lens _siServiceName (\ s a -> s{_siServiceName = a})

-- | Feedback left about the finding.
siUserFeedback :: Lens' ServiceInfo (Maybe Text)
siUserFeedback = lens _siUserFeedback (\ s a -> s{_siUserFeedback = a})

-- | Last seen timestamp of the activity that prompted GuardDuty to generate this finding.
siEventLastSeen :: Lens' ServiceInfo (Maybe Text)
siEventLastSeen = lens _siEventLastSeen (\ s a -> s{_siEventLastSeen = a})

-- | Resource role information for this finding.
siResourceRole :: Lens' ServiceInfo (Maybe Text)
siResourceRole = lens _siResourceRole (\ s a -> s{_siResourceRole = a})

-- | Indicates whether this finding is archived.
siArchived :: Lens' ServiceInfo (Maybe Bool)
siArchived = lens _siArchived (\ s a -> s{_siArchived = a})

instance FromJSON ServiceInfo where
        parseJSON
          = withObject "ServiceInfo"
              (\ x ->
                 ServiceInfo' <$>
                   (x .:? "count") <*> (x .:? "eventFirstSeen") <*>
                     (x .:? "action")
                     <*> (x .:? "detectorId")
                     <*> (x .:? "serviceName")
                     <*> (x .:? "userFeedback")
                     <*> (x .:? "eventLastSeen")
                     <*> (x .:? "resourceRole")
                     <*> (x .:? "archived"))

instance Hashable ServiceInfo where

instance NFData ServiceInfo where

-- | Represents the criteria used for sorting findings.
--
-- /See:/ 'sortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { _scOrderBy       :: !(Maybe OrderBy)
  , _scAttributeName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scOrderBy' - Order by which the sorted findings are to be displayed.
--
-- * 'scAttributeName' - Represents the finding attribute (for example, accountId) by which to sort findings.
sortCriteria
    :: SortCriteria
sortCriteria = SortCriteria' {_scOrderBy = Nothing, _scAttributeName = Nothing}


-- | Order by which the sorted findings are to be displayed.
scOrderBy :: Lens' SortCriteria (Maybe OrderBy)
scOrderBy = lens _scOrderBy (\ s a -> s{_scOrderBy = a})

-- | Represents the finding attribute (for example, accountId) by which to sort findings.
scAttributeName :: Lens' SortCriteria (Maybe Text)
scAttributeName = lens _scAttributeName (\ s a -> s{_scAttributeName = a})

instance Hashable SortCriteria where

instance NFData SortCriteria where

instance ToJSON SortCriteria where
        toJSON SortCriteria'{..}
          = object
              (catMaybes
                 [("orderBy" .=) <$> _scOrderBy,
                  ("attributeName" .=) <$> _scAttributeName])

-- | A tag of the EC2 instance.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - EC2 instance tag value.
--
-- * 'tagKey' - EC2 instance tag key.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | EC2 instance tag value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | EC2 instance tag key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Tag where

instance NFData Tag where

-- | An object containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- /See:/ 'unprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { _uaAccountId :: !Text
  , _uaResult    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAccountId' - AWS Account ID.
--
-- * 'uaResult' - A reason why the account hasn't been processed.
unprocessedAccount
    :: Text -- ^ 'uaAccountId'
    -> Text -- ^ 'uaResult'
    -> UnprocessedAccount
unprocessedAccount pAccountId_ pResult_ =
  UnprocessedAccount' {_uaAccountId = pAccountId_, _uaResult = pResult_}


-- | AWS Account ID.
uaAccountId :: Lens' UnprocessedAccount Text
uaAccountId = lens _uaAccountId (\ s a -> s{_uaAccountId = a})

-- | A reason why the account hasn't been processed.
uaResult :: Lens' UnprocessedAccount Text
uaResult = lens _uaResult (\ s a -> s{_uaResult = a})

instance FromJSON UnprocessedAccount where
        parseJSON
          = withObject "UnprocessedAccount"
              (\ x ->
                 UnprocessedAccount' <$>
                   (x .: "accountId") <*> (x .: "result"))

instance Hashable UnprocessedAccount where

instance NFData UnprocessedAccount where
