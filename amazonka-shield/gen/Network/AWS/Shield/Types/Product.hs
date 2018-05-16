{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.Sum

-- | The details of a DDoS attack.
--
--
--
-- /See:/ 'attackDetail' smart constructor.
data AttackDetail = AttackDetail'
  { _adAttackId         :: !(Maybe Text)
  , _adStartTime        :: !(Maybe POSIX)
  , _adSubResources     :: !(Maybe [SubResourceSummary])
  , _adMitigations      :: !(Maybe [Mitigation])
  , _adAttackProperties :: !(Maybe [AttackProperty])
  , _adAttackCounters   :: !(Maybe [SummarizedCounter])
  , _adResourceARN      :: !(Maybe Text)
  , _adEndTime          :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttackDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttackId' - The unique identifier (ID) of the attack.
--
-- * 'adStartTime' - The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'adSubResources' - If applicable, additional detail about the resource being attacked, for example, IP address or URL.
--
-- * 'adMitigations' - List of mitigation actions taken for the attack.
--
-- * 'adAttackProperties' - The array of 'AttackProperty' objects.
--
-- * 'adAttackCounters' - List of counters that describe the attack for the specified time period.
--
-- * 'adResourceARN' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- * 'adEndTime' - The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
attackDetail
    :: AttackDetail
attackDetail =
  AttackDetail'
    { _adAttackId = Nothing
    , _adStartTime = Nothing
    , _adSubResources = Nothing
    , _adMitigations = Nothing
    , _adAttackProperties = Nothing
    , _adAttackCounters = Nothing
    , _adResourceARN = Nothing
    , _adEndTime = Nothing
    }


-- | The unique identifier (ID) of the attack.
adAttackId :: Lens' AttackDetail (Maybe Text)
adAttackId = lens _adAttackId (\ s a -> s{_adAttackId = a})

-- | The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
adStartTime :: Lens' AttackDetail (Maybe UTCTime)
adStartTime = lens _adStartTime (\ s a -> s{_adStartTime = a}) . mapping _Time

-- | If applicable, additional detail about the resource being attacked, for example, IP address or URL.
adSubResources :: Lens' AttackDetail [SubResourceSummary]
adSubResources = lens _adSubResources (\ s a -> s{_adSubResources = a}) . _Default . _Coerce

-- | List of mitigation actions taken for the attack.
adMitigations :: Lens' AttackDetail [Mitigation]
adMitigations = lens _adMitigations (\ s a -> s{_adMitigations = a}) . _Default . _Coerce

-- | The array of 'AttackProperty' objects.
adAttackProperties :: Lens' AttackDetail [AttackProperty]
adAttackProperties = lens _adAttackProperties (\ s a -> s{_adAttackProperties = a}) . _Default . _Coerce

-- | List of counters that describe the attack for the specified time period.
adAttackCounters :: Lens' AttackDetail [SummarizedCounter]
adAttackCounters = lens _adAttackCounters (\ s a -> s{_adAttackCounters = a}) . _Default . _Coerce

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
adResourceARN :: Lens' AttackDetail (Maybe Text)
adResourceARN = lens _adResourceARN (\ s a -> s{_adResourceARN = a})

-- | The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
adEndTime :: Lens' AttackDetail (Maybe UTCTime)
adEndTime = lens _adEndTime (\ s a -> s{_adEndTime = a}) . mapping _Time

instance FromJSON AttackDetail where
        parseJSON
          = withObject "AttackDetail"
              (\ x ->
                 AttackDetail' <$>
                   (x .:? "AttackId") <*> (x .:? "StartTime") <*>
                     (x .:? "SubResources" .!= mempty)
                     <*> (x .:? "Mitigations" .!= mempty)
                     <*> (x .:? "AttackProperties" .!= mempty)
                     <*> (x .:? "AttackCounters" .!= mempty)
                     <*> (x .:? "ResourceArn")
                     <*> (x .:? "EndTime"))

instance Hashable AttackDetail where

instance NFData AttackDetail where

-- | Details of the described attack.
--
--
--
-- /See:/ 'attackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { _apAttackLayer              :: !(Maybe AttackLayer)
  , _apTopContributors          :: !(Maybe [Contributor])
  , _apAttackPropertyIdentifier :: !(Maybe AttackPropertyIdentifier)
  , _apTotal                    :: !(Maybe Integer)
  , _apUnit                     :: !(Maybe Unit)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttackProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAttackLayer' - The type of DDoS event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- * 'apTopContributors' - The array of 'Contributor' objects that includes the top five contributors to an attack.
--
-- * 'apAttackPropertyIdentifier' - Defines the DDoS attack property information that is provided.
--
-- * 'apTotal' - The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
--
-- * 'apUnit' - The unit of the @Value@ of the contributions.
attackProperty
    :: AttackProperty
attackProperty =
  AttackProperty'
    { _apAttackLayer = Nothing
    , _apTopContributors = Nothing
    , _apAttackPropertyIdentifier = Nothing
    , _apTotal = Nothing
    , _apUnit = Nothing
    }


-- | The type of DDoS event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
apAttackLayer :: Lens' AttackProperty (Maybe AttackLayer)
apAttackLayer = lens _apAttackLayer (\ s a -> s{_apAttackLayer = a})

-- | The array of 'Contributor' objects that includes the top five contributors to an attack.
apTopContributors :: Lens' AttackProperty [Contributor]
apTopContributors = lens _apTopContributors (\ s a -> s{_apTopContributors = a}) . _Default . _Coerce

-- | Defines the DDoS attack property information that is provided.
apAttackPropertyIdentifier :: Lens' AttackProperty (Maybe AttackPropertyIdentifier)
apAttackPropertyIdentifier = lens _apAttackPropertyIdentifier (\ s a -> s{_apAttackPropertyIdentifier = a})

-- | The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
apTotal :: Lens' AttackProperty (Maybe Integer)
apTotal = lens _apTotal (\ s a -> s{_apTotal = a})

-- | The unit of the @Value@ of the contributions.
apUnit :: Lens' AttackProperty (Maybe Unit)
apUnit = lens _apUnit (\ s a -> s{_apUnit = a})

instance FromJSON AttackProperty where
        parseJSON
          = withObject "AttackProperty"
              (\ x ->
                 AttackProperty' <$>
                   (x .:? "AttackLayer") <*>
                     (x .:? "TopContributors" .!= mempty)
                     <*> (x .:? "AttackPropertyIdentifier")
                     <*> (x .:? "Total")
                     <*> (x .:? "Unit"))

instance Hashable AttackProperty where

instance NFData AttackProperty where

-- | Summarizes all DDoS attacks for a specified time period.
--
--
--
-- /See:/ 'attackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { _asAttackVectors :: !(Maybe [AttackVectorDescription])
  , _asAttackId      :: !(Maybe Text)
  , _asStartTime     :: !(Maybe POSIX)
  , _asResourceARN   :: !(Maybe Text)
  , _asEndTime       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAttackVectors' - The list of attacks for a specified time period.
--
-- * 'asAttackId' - The unique identifier (ID) of the attack.
--
-- * 'asStartTime' - The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'asResourceARN' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- * 'asEndTime' - The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
attackSummary
    :: AttackSummary
attackSummary =
  AttackSummary'
    { _asAttackVectors = Nothing
    , _asAttackId = Nothing
    , _asStartTime = Nothing
    , _asResourceARN = Nothing
    , _asEndTime = Nothing
    }


-- | The list of attacks for a specified time period.
asAttackVectors :: Lens' AttackSummary [AttackVectorDescription]
asAttackVectors = lens _asAttackVectors (\ s a -> s{_asAttackVectors = a}) . _Default . _Coerce

-- | The unique identifier (ID) of the attack.
asAttackId :: Lens' AttackSummary (Maybe Text)
asAttackId = lens _asAttackId (\ s a -> s{_asAttackId = a})

-- | The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
asStartTime :: Lens' AttackSummary (Maybe UTCTime)
asStartTime = lens _asStartTime (\ s a -> s{_asStartTime = a}) . mapping _Time

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
asResourceARN :: Lens' AttackSummary (Maybe Text)
asResourceARN = lens _asResourceARN (\ s a -> s{_asResourceARN = a})

-- | The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
asEndTime :: Lens' AttackSummary (Maybe UTCTime)
asEndTime = lens _asEndTime (\ s a -> s{_asEndTime = a}) . mapping _Time

instance FromJSON AttackSummary where
        parseJSON
          = withObject "AttackSummary"
              (\ x ->
                 AttackSummary' <$>
                   (x .:? "AttackVectors" .!= mempty) <*>
                     (x .:? "AttackId")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "ResourceArn")
                     <*> (x .:? "EndTime"))

instance Hashable AttackSummary where

instance NFData AttackSummary where

-- | Describes the attack.
--
--
--
-- /See:/ 'attackVectorDescription' smart constructor.
newtype AttackVectorDescription = AttackVectorDescription'
  { _avdVectorType :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttackVectorDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdVectorType' - The attack type. Valid values:     * UDP_TRAFFIC     * UDP_FRAGMENT     * GENERIC_UDP_REFLECTION     * DNS_REFLECTION     * NTP_REFLECTION     * CHARGEN_REFLECTION     * SSDP_REFLECTION     * PORT_MAPPER     * RIP_REFLECTION     * SNMP_REFLECTION     * MSSQL_REFLECTION     * NET_BIOS_REFLECTION     * SYN_FLOOD     * ACK_FLOOD     * REQUEST_FLOOD
attackVectorDescription
    :: Text -- ^ 'avdVectorType'
    -> AttackVectorDescription
attackVectorDescription pVectorType_ =
  AttackVectorDescription' {_avdVectorType = pVectorType_}


-- | The attack type. Valid values:     * UDP_TRAFFIC     * UDP_FRAGMENT     * GENERIC_UDP_REFLECTION     * DNS_REFLECTION     * NTP_REFLECTION     * CHARGEN_REFLECTION     * SSDP_REFLECTION     * PORT_MAPPER     * RIP_REFLECTION     * SNMP_REFLECTION     * MSSQL_REFLECTION     * NET_BIOS_REFLECTION     * SYN_FLOOD     * ACK_FLOOD     * REQUEST_FLOOD
avdVectorType :: Lens' AttackVectorDescription Text
avdVectorType = lens _avdVectorType (\ s a -> s{_avdVectorType = a})

instance FromJSON AttackVectorDescription where
        parseJSON
          = withObject "AttackVectorDescription"
              (\ x ->
                 AttackVectorDescription' <$> (x .: "VectorType"))

instance Hashable AttackVectorDescription where

instance NFData AttackVectorDescription where

-- | A contributor to the attack and their contribution.
--
--
--
-- /See:/ 'contributor' smart constructor.
data Contributor = Contributor'
  { _cValue :: !(Maybe Integer)
  , _cName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Contributor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cValue' - The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
--
-- * 'cName' - The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
contributor
    :: Contributor
contributor = Contributor' {_cValue = Nothing, _cName = Nothing}


-- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
cValue :: Lens' Contributor (Maybe Integer)
cValue = lens _cValue (\ s a -> s{_cValue = a})

-- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
cName :: Lens' Contributor (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromJSON Contributor where
        parseJSON
          = withObject "Contributor"
              (\ x ->
                 Contributor' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable Contributor where

instance NFData Contributor where

-- | The mitigation applied to a DDoS attack.
--
--
--
-- /See:/ 'mitigation' smart constructor.
newtype Mitigation = Mitigation'
  { _mMitigationName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mitigation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMitigationName' - The name of the mitigation taken for this attack.
mitigation
    :: Mitigation
mitigation = Mitigation' {_mMitigationName = Nothing}


-- | The name of the mitigation taken for this attack.
mMitigationName :: Lens' Mitigation (Maybe Text)
mMitigationName = lens _mMitigationName (\ s a -> s{_mMitigationName = a})

instance FromJSON Mitigation where
        parseJSON
          = withObject "Mitigation"
              (\ x -> Mitigation' <$> (x .:? "MitigationName"))

instance Hashable Mitigation where

instance NFData Mitigation where

-- | An object that represents a resource that is under DDoS protection.
--
--
--
-- /See:/ 'protection' smart constructor.
data Protection = Protection'
  { _pResourceARN :: !(Maybe Text)
  , _pName        :: !(Maybe Text)
  , _pId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Protection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pResourceARN' - The ARN (Amazon Resource Name) of the AWS resource that is protected.
--
-- * 'pName' - The friendly name of the protection. For example, @My CloudFront distributions@ .
--
-- * 'pId' - The unique identifier (ID) of the protection.
protection
    :: Protection
protection =
  Protection' {_pResourceARN = Nothing, _pName = Nothing, _pId = Nothing}


-- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
pResourceARN :: Lens' Protection (Maybe Text)
pResourceARN = lens _pResourceARN (\ s a -> s{_pResourceARN = a})

-- | The friendly name of the protection. For example, @My CloudFront distributions@ .
pName :: Lens' Protection (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | The unique identifier (ID) of the protection.
pId :: Lens' Protection (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

instance FromJSON Protection where
        parseJSON
          = withObject "Protection"
              (\ x ->
                 Protection' <$>
                   (x .:? "ResourceArn") <*> (x .:? "Name") <*>
                     (x .:? "Id"))

instance Hashable Protection where

instance NFData Protection where

-- | The attack information for the specified SubResource.
--
--
--
-- /See:/ 'subResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { _srsCounters      :: !(Maybe [SummarizedCounter])
  , _srsAttackVectors :: !(Maybe [SummarizedAttackVector])
  , _srsId            :: !(Maybe Text)
  , _srsType          :: !(Maybe SubResourceType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubResourceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsCounters' - The counters that describe the details of the attack.
--
-- * 'srsAttackVectors' - The list of attack types and associated counters.
--
-- * 'srsId' - The unique identifier (ID) of the @SubResource@ .
--
-- * 'srsType' - The @SubResource@ type.
subResourceSummary
    :: SubResourceSummary
subResourceSummary =
  SubResourceSummary'
    { _srsCounters = Nothing
    , _srsAttackVectors = Nothing
    , _srsId = Nothing
    , _srsType = Nothing
    }


-- | The counters that describe the details of the attack.
srsCounters :: Lens' SubResourceSummary [SummarizedCounter]
srsCounters = lens _srsCounters (\ s a -> s{_srsCounters = a}) . _Default . _Coerce

-- | The list of attack types and associated counters.
srsAttackVectors :: Lens' SubResourceSummary [SummarizedAttackVector]
srsAttackVectors = lens _srsAttackVectors (\ s a -> s{_srsAttackVectors = a}) . _Default . _Coerce

-- | The unique identifier (ID) of the @SubResource@ .
srsId :: Lens' SubResourceSummary (Maybe Text)
srsId = lens _srsId (\ s a -> s{_srsId = a})

-- | The @SubResource@ type.
srsType :: Lens' SubResourceSummary (Maybe SubResourceType)
srsType = lens _srsType (\ s a -> s{_srsType = a})

instance FromJSON SubResourceSummary where
        parseJSON
          = withObject "SubResourceSummary"
              (\ x ->
                 SubResourceSummary' <$>
                   (x .:? "Counters" .!= mempty) <*>
                     (x .:? "AttackVectors" .!= mempty)
                     <*> (x .:? "Id")
                     <*> (x .:? "Type"))

instance Hashable SubResourceSummary where

instance NFData SubResourceSummary where

-- | Information about the AWS Shield Advanced subscription for an account.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sTimeCommitmentInSeconds :: !(Maybe Nat)
  , _sStartTime               :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTimeCommitmentInSeconds' - The length, in seconds, of the AWS Shield Advanced subscription for the account.
--
-- * 'sStartTime' - The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
subscription
    :: Subscription
subscription =
  Subscription' {_sTimeCommitmentInSeconds = Nothing, _sStartTime = Nothing}


-- | The length, in seconds, of the AWS Shield Advanced subscription for the account.
sTimeCommitmentInSeconds :: Lens' Subscription (Maybe Natural)
sTimeCommitmentInSeconds = lens _sTimeCommitmentInSeconds (\ s a -> s{_sTimeCommitmentInSeconds = a}) . mapping _Nat

-- | The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
sStartTime :: Lens' Subscription (Maybe UTCTime)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . mapping _Time

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "TimeCommitmentInSeconds") <*>
                     (x .:? "StartTime"))

instance Hashable Subscription where

instance NFData Subscription where

-- | A summary of information about the attack.
--
--
--
-- /See:/ 'summarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { _savVectorCounters :: !(Maybe [SummarizedCounter])
  , _savVectorType     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SummarizedAttackVector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'savVectorCounters' - The list of counters that describe the details of the attack.
--
-- * 'savVectorType' - The attack type, for example, SNMP reflection or SYN flood.
summarizedAttackVector
    :: Text -- ^ 'savVectorType'
    -> SummarizedAttackVector
summarizedAttackVector pVectorType_ =
  SummarizedAttackVector'
    {_savVectorCounters = Nothing, _savVectorType = pVectorType_}


-- | The list of counters that describe the details of the attack.
savVectorCounters :: Lens' SummarizedAttackVector [SummarizedCounter]
savVectorCounters = lens _savVectorCounters (\ s a -> s{_savVectorCounters = a}) . _Default . _Coerce

-- | The attack type, for example, SNMP reflection or SYN flood.
savVectorType :: Lens' SummarizedAttackVector Text
savVectorType = lens _savVectorType (\ s a -> s{_savVectorType = a})

instance FromJSON SummarizedAttackVector where
        parseJSON
          = withObject "SummarizedAttackVector"
              (\ x ->
                 SummarizedAttackVector' <$>
                   (x .:? "VectorCounters" .!= mempty) <*>
                     (x .: "VectorType"))

instance Hashable SummarizedAttackVector where

instance NFData SummarizedAttackVector where

-- | The counter that describes a DDoS attack.
--
--
--
-- /See:/ 'summarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { _scMax     :: !(Maybe Double)
  , _scAverage :: !(Maybe Double)
  , _scN       :: !(Maybe Int)
  , _scName    :: !(Maybe Text)
  , _scSum     :: !(Maybe Double)
  , _scUnit    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SummarizedCounter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMax' - The maximum value of the counter for a specified time period.
--
-- * 'scAverage' - The average value of the counter for a specified time period.
--
-- * 'scN' - The number of counters for a specified time period.
--
-- * 'scName' - The counter name.
--
-- * 'scSum' - The total of counter values for a specified time period.
--
-- * 'scUnit' - The unit of the counters.
summarizedCounter
    :: SummarizedCounter
summarizedCounter =
  SummarizedCounter'
    { _scMax = Nothing
    , _scAverage = Nothing
    , _scN = Nothing
    , _scName = Nothing
    , _scSum = Nothing
    , _scUnit = Nothing
    }


-- | The maximum value of the counter for a specified time period.
scMax :: Lens' SummarizedCounter (Maybe Double)
scMax = lens _scMax (\ s a -> s{_scMax = a})

-- | The average value of the counter for a specified time period.
scAverage :: Lens' SummarizedCounter (Maybe Double)
scAverage = lens _scAverage (\ s a -> s{_scAverage = a})

-- | The number of counters for a specified time period.
scN :: Lens' SummarizedCounter (Maybe Int)
scN = lens _scN (\ s a -> s{_scN = a})

-- | The counter name.
scName :: Lens' SummarizedCounter (Maybe Text)
scName = lens _scName (\ s a -> s{_scName = a})

-- | The total of counter values for a specified time period.
scSum :: Lens' SummarizedCounter (Maybe Double)
scSum = lens _scSum (\ s a -> s{_scSum = a})

-- | The unit of the counters.
scUnit :: Lens' SummarizedCounter (Maybe Text)
scUnit = lens _scUnit (\ s a -> s{_scUnit = a})

instance FromJSON SummarizedCounter where
        parseJSON
          = withObject "SummarizedCounter"
              (\ x ->
                 SummarizedCounter' <$>
                   (x .:? "Max") <*> (x .:? "Average") <*> (x .:? "N")
                     <*> (x .:? "Name")
                     <*> (x .:? "Sum")
                     <*> (x .:? "Unit"))

instance Hashable SummarizedCounter where

instance NFData SummarizedCounter where

-- | The time range.
--
--
--
-- /See:/ 'timeRange' smart constructor.
data TimeRange = TimeRange'
  { _trFromInclusive :: !(Maybe POSIX)
  , _trToExclusive   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trFromInclusive' - The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'trToExclusive' - The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
timeRange
    :: TimeRange
timeRange = TimeRange' {_trFromInclusive = Nothing, _trToExclusive = Nothing}


-- | The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
trFromInclusive :: Lens' TimeRange (Maybe UTCTime)
trFromInclusive = lens _trFromInclusive (\ s a -> s{_trFromInclusive = a}) . mapping _Time

-- | The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
trToExclusive :: Lens' TimeRange (Maybe UTCTime)
trToExclusive = lens _trToExclusive (\ s a -> s{_trToExclusive = a}) . mapping _Time

instance Hashable TimeRange where

instance NFData TimeRange where

instance ToJSON TimeRange where
        toJSON TimeRange'{..}
          = object
              (catMaybes
                 [("FromInclusive" .=) <$> _trFromInclusive,
                  ("ToExclusive" .=) <$> _trToExclusive])
