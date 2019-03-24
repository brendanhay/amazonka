{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.Sum

-- | An alias for an edge.
--
--
--
-- /See:/ 'alias' smart constructor.
data Alias = Alias'
  { _aNames :: !(Maybe [Text])
  , _aName  :: !(Maybe Text)
  , _aType  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aNames' - A list of names for the alias, including the canonical name.
--
-- * 'aName' - The canonical name of the alias.
--
-- * 'aType' - The type of the alias.
alias
    :: Alias
alias = Alias' {_aNames = Nothing, _aName = Nothing, _aType = Nothing}


-- | A list of names for the alias, including the canonical name.
aNames :: Lens' Alias [Text]
aNames = lens _aNames (\ s a -> s{_aNames = a}) . _Default . _Coerce

-- | The canonical name of the alias.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The type of the alias.
aType :: Lens' Alias (Maybe Text)
aType = lens _aType (\ s a -> s{_aType = a})

instance FromJSON Alias where
        parseJSON
          = withObject "Alias"
              (\ x ->
                 Alias' <$>
                   (x .:? "Names" .!= mempty) <*> (x .:? "Name") <*>
                     (x .:? "Type"))

instance Hashable Alias where

instance NFData Alias where

-- | Value of a segment annotation. Has one of three value types: Number, Boolean or String.
--
--
--
-- /See:/ 'annotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { _avNumberValue  :: !(Maybe Double)
  , _avStringValue  :: !(Maybe Text)
  , _avBooleanValue :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnnotationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avNumberValue' - Value for a Number annotation.
--
-- * 'avStringValue' - Value for a String annotation.
--
-- * 'avBooleanValue' - Value for a Boolean annotation.
annotationValue
    :: AnnotationValue
annotationValue =
  AnnotationValue'
    { _avNumberValue = Nothing
    , _avStringValue = Nothing
    , _avBooleanValue = Nothing
    }


-- | Value for a Number annotation.
avNumberValue :: Lens' AnnotationValue (Maybe Double)
avNumberValue = lens _avNumberValue (\ s a -> s{_avNumberValue = a})

-- | Value for a String annotation.
avStringValue :: Lens' AnnotationValue (Maybe Text)
avStringValue = lens _avStringValue (\ s a -> s{_avStringValue = a})

-- | Value for a Boolean annotation.
avBooleanValue :: Lens' AnnotationValue (Maybe Bool)
avBooleanValue = lens _avBooleanValue (\ s a -> s{_avBooleanValue = a})

instance FromJSON AnnotationValue where
        parseJSON
          = withObject "AnnotationValue"
              (\ x ->
                 AnnotationValue' <$>
                   (x .:? "NumberValue") <*> (x .:? "StringValue") <*>
                     (x .:? "BooleanValue"))

instance Hashable AnnotationValue where

instance NFData AnnotationValue where

-- | A list of availability zones corresponding to the segments in a trace.
--
--
--
-- /See:/ 'availabilityZoneDetail' smart constructor.
newtype AvailabilityZoneDetail = AvailabilityZoneDetail'
  { _azdName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZoneDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azdName' - The name of a corresponding availability zone.
availabilityZoneDetail
    :: AvailabilityZoneDetail
availabilityZoneDetail = AvailabilityZoneDetail' {_azdName = Nothing}


-- | The name of a corresponding availability zone.
azdName :: Lens' AvailabilityZoneDetail (Maybe Text)
azdName = lens _azdName (\ s a -> s{_azdName = a})

instance FromJSON AvailabilityZoneDetail where
        parseJSON
          = withObject "AvailabilityZoneDetail"
              (\ x -> AvailabilityZoneDetail' <$> (x .:? "Name"))

instance Hashable AvailabilityZoneDetail where

instance NFData AvailabilityZoneDetail where

-- |
--
--
--
-- /See:/ 'backendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { _bceOtherCount             :: !(Maybe Int)
  , _bceTimeoutCount           :: !(Maybe Int)
  , _bceHTTPCode5XXCount       :: !(Maybe Int)
  , _bceConnectionRefusedCount :: !(Maybe Int)
  , _bceHTTPCode4XXCount       :: !(Maybe Int)
  , _bceUnknownHostCount       :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackendConnectionErrors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bceOtherCount' -
--
-- * 'bceTimeoutCount' -
--
-- * 'bceHTTPCode5XXCount' -
--
-- * 'bceConnectionRefusedCount' -
--
-- * 'bceHTTPCode4XXCount' -
--
-- * 'bceUnknownHostCount' -
backendConnectionErrors
    :: BackendConnectionErrors
backendConnectionErrors =
  BackendConnectionErrors'
    { _bceOtherCount = Nothing
    , _bceTimeoutCount = Nothing
    , _bceHTTPCode5XXCount = Nothing
    , _bceConnectionRefusedCount = Nothing
    , _bceHTTPCode4XXCount = Nothing
    , _bceUnknownHostCount = Nothing
    }


-- |
bceOtherCount :: Lens' BackendConnectionErrors (Maybe Int)
bceOtherCount = lens _bceOtherCount (\ s a -> s{_bceOtherCount = a})

-- |
bceTimeoutCount :: Lens' BackendConnectionErrors (Maybe Int)
bceTimeoutCount = lens _bceTimeoutCount (\ s a -> s{_bceTimeoutCount = a})

-- |
bceHTTPCode5XXCount :: Lens' BackendConnectionErrors (Maybe Int)
bceHTTPCode5XXCount = lens _bceHTTPCode5XXCount (\ s a -> s{_bceHTTPCode5XXCount = a})

-- |
bceConnectionRefusedCount :: Lens' BackendConnectionErrors (Maybe Int)
bceConnectionRefusedCount = lens _bceConnectionRefusedCount (\ s a -> s{_bceConnectionRefusedCount = a})

-- |
bceHTTPCode4XXCount :: Lens' BackendConnectionErrors (Maybe Int)
bceHTTPCode4XXCount = lens _bceHTTPCode4XXCount (\ s a -> s{_bceHTTPCode4XXCount = a})

-- |
bceUnknownHostCount :: Lens' BackendConnectionErrors (Maybe Int)
bceUnknownHostCount = lens _bceUnknownHostCount (\ s a -> s{_bceUnknownHostCount = a})

instance Hashable BackendConnectionErrors where

instance NFData BackendConnectionErrors where

instance ToJSON BackendConnectionErrors where
        toJSON BackendConnectionErrors'{..}
          = object
              (catMaybes
                 [("OtherCount" .=) <$> _bceOtherCount,
                  ("TimeoutCount" .=) <$> _bceTimeoutCount,
                  ("HTTPCode5XXCount" .=) <$> _bceHTTPCode5XXCount,
                  ("ConnectionRefusedCount" .=) <$>
                    _bceConnectionRefusedCount,
                  ("HTTPCode4XXCount" .=) <$> _bceHTTPCode4XXCount,
                  ("UnknownHostCount" .=) <$> _bceUnknownHostCount])

-- | Information about a connection between two services.
--
--
--
-- /See:/ 'edge' smart constructor.
data Edge = Edge'
  { _eStartTime             :: !(Maybe POSIX)
  , _eAliases               :: !(Maybe [Alias])
  , _eResponseTimeHistogram :: !(Maybe [HistogramEntry])
  , _eReferenceId           :: !(Maybe Int)
  , _eEndTime               :: !(Maybe POSIX)
  , _eSummaryStatistics     :: !(Maybe EdgeStatistics)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Edge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStartTime' - The start time of the first segment on the edge.
--
-- * 'eAliases' - Aliases for the edge.
--
-- * 'eResponseTimeHistogram' - A histogram that maps the spread of client response times on an edge.
--
-- * 'eReferenceId' - Identifier of the edge. Unique within a service map.
--
-- * 'eEndTime' - The end time of the last segment on the edge.
--
-- * 'eSummaryStatistics' - Response statistics for segments on the edge.
edge
    :: Edge
edge =
  Edge'
    { _eStartTime = Nothing
    , _eAliases = Nothing
    , _eResponseTimeHistogram = Nothing
    , _eReferenceId = Nothing
    , _eEndTime = Nothing
    , _eSummaryStatistics = Nothing
    }


-- | The start time of the first segment on the edge.
eStartTime :: Lens' Edge (Maybe UTCTime)
eStartTime = lens _eStartTime (\ s a -> s{_eStartTime = a}) . mapping _Time

-- | Aliases for the edge.
eAliases :: Lens' Edge [Alias]
eAliases = lens _eAliases (\ s a -> s{_eAliases = a}) . _Default . _Coerce

-- | A histogram that maps the spread of client response times on an edge.
eResponseTimeHistogram :: Lens' Edge [HistogramEntry]
eResponseTimeHistogram = lens _eResponseTimeHistogram (\ s a -> s{_eResponseTimeHistogram = a}) . _Default . _Coerce

-- | Identifier of the edge. Unique within a service map.
eReferenceId :: Lens' Edge (Maybe Int)
eReferenceId = lens _eReferenceId (\ s a -> s{_eReferenceId = a})

-- | The end time of the last segment on the edge.
eEndTime :: Lens' Edge (Maybe UTCTime)
eEndTime = lens _eEndTime (\ s a -> s{_eEndTime = a}) . mapping _Time

-- | Response statistics for segments on the edge.
eSummaryStatistics :: Lens' Edge (Maybe EdgeStatistics)
eSummaryStatistics = lens _eSummaryStatistics (\ s a -> s{_eSummaryStatistics = a})

instance FromJSON Edge where
        parseJSON
          = withObject "Edge"
              (\ x ->
                 Edge' <$>
                   (x .:? "StartTime") <*> (x .:? "Aliases" .!= mempty)
                     <*> (x .:? "ResponseTimeHistogram" .!= mempty)
                     <*> (x .:? "ReferenceId")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "SummaryStatistics"))

instance Hashable Edge where

instance NFData Edge where

-- | Response statistics for an edge.
--
--
--
-- /See:/ 'edgeStatistics' smart constructor.
data EdgeStatistics = EdgeStatistics'
  { _esFaultStatistics   :: !(Maybe FaultStatistics)
  , _esOKCount           :: !(Maybe Integer)
  , _esTotalResponseTime :: !(Maybe Double)
  , _esErrorStatistics   :: !(Maybe ErrorStatistics)
  , _esTotalCount        :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EdgeStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esFaultStatistics' - Information about requests that failed with a 5xx Server Error status code.
--
-- * 'esOKCount' - The number of requests that completed with a 2xx Success status code.
--
-- * 'esTotalResponseTime' - The aggregate response time of completed requests.
--
-- * 'esErrorStatistics' - Information about requests that failed with a 4xx Client Error status code.
--
-- * 'esTotalCount' - The total number of completed requests.
edgeStatistics
    :: EdgeStatistics
edgeStatistics =
  EdgeStatistics'
    { _esFaultStatistics = Nothing
    , _esOKCount = Nothing
    , _esTotalResponseTime = Nothing
    , _esErrorStatistics = Nothing
    , _esTotalCount = Nothing
    }


-- | Information about requests that failed with a 5xx Server Error status code.
esFaultStatistics :: Lens' EdgeStatistics (Maybe FaultStatistics)
esFaultStatistics = lens _esFaultStatistics (\ s a -> s{_esFaultStatistics = a})

-- | The number of requests that completed with a 2xx Success status code.
esOKCount :: Lens' EdgeStatistics (Maybe Integer)
esOKCount = lens _esOKCount (\ s a -> s{_esOKCount = a})

-- | The aggregate response time of completed requests.
esTotalResponseTime :: Lens' EdgeStatistics (Maybe Double)
esTotalResponseTime = lens _esTotalResponseTime (\ s a -> s{_esTotalResponseTime = a})

-- | Information about requests that failed with a 4xx Client Error status code.
esErrorStatistics :: Lens' EdgeStatistics (Maybe ErrorStatistics)
esErrorStatistics = lens _esErrorStatistics (\ s a -> s{_esErrorStatistics = a})

-- | The total number of completed requests.
esTotalCount :: Lens' EdgeStatistics (Maybe Integer)
esTotalCount = lens _esTotalCount (\ s a -> s{_esTotalCount = a})

instance FromJSON EdgeStatistics where
        parseJSON
          = withObject "EdgeStatistics"
              (\ x ->
                 EdgeStatistics' <$>
                   (x .:? "FaultStatistics") <*> (x .:? "OkCount") <*>
                     (x .:? "TotalResponseTime")
                     <*> (x .:? "ErrorStatistics")
                     <*> (x .:? "TotalCount"))

instance Hashable EdgeStatistics where

instance NFData EdgeStatistics where

-- | A configuration document that specifies encryption configuration settings.
--
--
--
-- /See:/ 'encryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { _ecStatus :: !(Maybe EncryptionStatus)
  , _ecKeyId  :: !(Maybe Text)
  , _ecType   :: !(Maybe EncryptionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecStatus' - The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
--
-- * 'ecKeyId' - The ID of the customer master key (CMK) used for encryption, if applicable.
--
-- * 'ecType' - The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
encryptionConfig
    :: EncryptionConfig
encryptionConfig =
  EncryptionConfig' {_ecStatus = Nothing, _ecKeyId = Nothing, _ecType = Nothing}


-- | The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
ecStatus :: Lens' EncryptionConfig (Maybe EncryptionStatus)
ecStatus = lens _ecStatus (\ s a -> s{_ecStatus = a})

-- | The ID of the customer master key (CMK) used for encryption, if applicable.
ecKeyId :: Lens' EncryptionConfig (Maybe Text)
ecKeyId = lens _ecKeyId (\ s a -> s{_ecKeyId = a})

-- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
ecType :: Lens' EncryptionConfig (Maybe EncryptionType)
ecType = lens _ecType (\ s a -> s{_ecType = a})

instance FromJSON EncryptionConfig where
        parseJSON
          = withObject "EncryptionConfig"
              (\ x ->
                 EncryptionConfig' <$>
                   (x .:? "Status") <*> (x .:? "KeyId") <*>
                     (x .:? "Type"))

instance Hashable EncryptionConfig where

instance NFData EncryptionConfig where

-- | The root cause of a trace summary error.
--
--
--
-- /See:/ 'errorRootCause' smart constructor.
newtype ErrorRootCause = ErrorRootCause'
  { _ercServices :: Maybe [ErrorRootCauseService]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorRootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ercServices' - A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
errorRootCause
    :: ErrorRootCause
errorRootCause = ErrorRootCause' {_ercServices = Nothing}


-- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
ercServices :: Lens' ErrorRootCause [ErrorRootCauseService]
ercServices = lens _ercServices (\ s a -> s{_ercServices = a}) . _Default . _Coerce

instance FromJSON ErrorRootCause where
        parseJSON
          = withObject "ErrorRootCause"
              (\ x ->
                 ErrorRootCause' <$> (x .:? "Services" .!= mempty))

instance Hashable ErrorRootCause where

instance NFData ErrorRootCause where

-- | A collection of segments and corresponding subsegments associated to a trace summary error.
--
--
--
-- /See:/ 'errorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { _erceExceptions :: !(Maybe [RootCauseException])
  , _erceRemote     :: !(Maybe Bool)
  , _erceName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erceExceptions' - The types and messages of the exceptions.
--
-- * 'erceRemote' - A flag that denotes a remote subsegment.
--
-- * 'erceName' - The name of the entity.
errorRootCauseEntity
    :: ErrorRootCauseEntity
errorRootCauseEntity =
  ErrorRootCauseEntity'
    {_erceExceptions = Nothing, _erceRemote = Nothing, _erceName = Nothing}


-- | The types and messages of the exceptions.
erceExceptions :: Lens' ErrorRootCauseEntity [RootCauseException]
erceExceptions = lens _erceExceptions (\ s a -> s{_erceExceptions = a}) . _Default . _Coerce

-- | A flag that denotes a remote subsegment.
erceRemote :: Lens' ErrorRootCauseEntity (Maybe Bool)
erceRemote = lens _erceRemote (\ s a -> s{_erceRemote = a})

-- | The name of the entity.
erceName :: Lens' ErrorRootCauseEntity (Maybe Text)
erceName = lens _erceName (\ s a -> s{_erceName = a})

instance FromJSON ErrorRootCauseEntity where
        parseJSON
          = withObject "ErrorRootCauseEntity"
              (\ x ->
                 ErrorRootCauseEntity' <$>
                   (x .:? "Exceptions" .!= mempty) <*> (x .:? "Remote")
                     <*> (x .:? "Name"))

instance Hashable ErrorRootCauseEntity where

instance NFData ErrorRootCauseEntity where

-- | A collection of fields identifying the services in a trace summary error.
--
--
--
-- /See:/ 'errorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { _ercsEntityPath :: !(Maybe [ErrorRootCauseEntity])
  , _ercsAccountId  :: !(Maybe Text)
  , _ercsNames      :: !(Maybe [Text])
  , _ercsName       :: !(Maybe Text)
  , _ercsInferred   :: !(Maybe Bool)
  , _ercsType       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorRootCauseService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ercsEntityPath' - The path of root cause entities found on the service.
--
-- * 'ercsAccountId' - The account ID associated to the service.
--
-- * 'ercsNames' - A collection of associated service names.
--
-- * 'ercsName' - The service name.
--
-- * 'ercsInferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- * 'ercsType' - The type associated to the service.
errorRootCauseService
    :: ErrorRootCauseService
errorRootCauseService =
  ErrorRootCauseService'
    { _ercsEntityPath = Nothing
    , _ercsAccountId = Nothing
    , _ercsNames = Nothing
    , _ercsName = Nothing
    , _ercsInferred = Nothing
    , _ercsType = Nothing
    }


-- | The path of root cause entities found on the service.
ercsEntityPath :: Lens' ErrorRootCauseService [ErrorRootCauseEntity]
ercsEntityPath = lens _ercsEntityPath (\ s a -> s{_ercsEntityPath = a}) . _Default . _Coerce

-- | The account ID associated to the service.
ercsAccountId :: Lens' ErrorRootCauseService (Maybe Text)
ercsAccountId = lens _ercsAccountId (\ s a -> s{_ercsAccountId = a})

-- | A collection of associated service names.
ercsNames :: Lens' ErrorRootCauseService [Text]
ercsNames = lens _ercsNames (\ s a -> s{_ercsNames = a}) . _Default . _Coerce

-- | The service name.
ercsName :: Lens' ErrorRootCauseService (Maybe Text)
ercsName = lens _ercsName (\ s a -> s{_ercsName = a})

-- | A Boolean value indicating if the service is inferred from the trace.
ercsInferred :: Lens' ErrorRootCauseService (Maybe Bool)
ercsInferred = lens _ercsInferred (\ s a -> s{_ercsInferred = a})

-- | The type associated to the service.
ercsType :: Lens' ErrorRootCauseService (Maybe Text)
ercsType = lens _ercsType (\ s a -> s{_ercsType = a})

instance FromJSON ErrorRootCauseService where
        parseJSON
          = withObject "ErrorRootCauseService"
              (\ x ->
                 ErrorRootCauseService' <$>
                   (x .:? "EntityPath" .!= mempty) <*>
                     (x .:? "AccountId")
                     <*> (x .:? "Names" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Inferred")
                     <*> (x .:? "Type"))

instance Hashable ErrorRootCauseService where

instance NFData ErrorRootCauseService where

-- | Information about requests that failed with a 4xx Client Error status code.
--
--
--
-- /See:/ 'errorStatistics' smart constructor.
data ErrorStatistics = ErrorStatistics'
  { _eOtherCount    :: !(Maybe Integer)
  , _eThrottleCount :: !(Maybe Integer)
  , _eTotalCount    :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eOtherCount' - The number of requests that failed with untracked 4xx Client Error status codes.
--
-- * 'eThrottleCount' - The number of requests that failed with a 419 throttling status code.
--
-- * 'eTotalCount' - The total number of requests that failed with a 4xx Client Error status code.
errorStatistics
    :: ErrorStatistics
errorStatistics =
  ErrorStatistics'
    {_eOtherCount = Nothing, _eThrottleCount = Nothing, _eTotalCount = Nothing}


-- | The number of requests that failed with untracked 4xx Client Error status codes.
eOtherCount :: Lens' ErrorStatistics (Maybe Integer)
eOtherCount = lens _eOtherCount (\ s a -> s{_eOtherCount = a})

-- | The number of requests that failed with a 419 throttling status code.
eThrottleCount :: Lens' ErrorStatistics (Maybe Integer)
eThrottleCount = lens _eThrottleCount (\ s a -> s{_eThrottleCount = a})

-- | The total number of requests that failed with a 4xx Client Error status code.
eTotalCount :: Lens' ErrorStatistics (Maybe Integer)
eTotalCount = lens _eTotalCount (\ s a -> s{_eTotalCount = a})

instance FromJSON ErrorStatistics where
        parseJSON
          = withObject "ErrorStatistics"
              (\ x ->
                 ErrorStatistics' <$>
                   (x .:? "OtherCount") <*> (x .:? "ThrottleCount") <*>
                     (x .:? "TotalCount"))

instance Hashable ErrorStatistics where

instance NFData ErrorStatistics where

-- | The root cause information for a trace summary fault.
--
--
--
-- /See:/ 'faultRootCause' smart constructor.
newtype FaultRootCause = FaultRootCause'
  { _frcServices :: Maybe [FaultRootCauseService]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaultRootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frcServices' - A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
faultRootCause
    :: FaultRootCause
faultRootCause = FaultRootCause' {_frcServices = Nothing}


-- | A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
frcServices :: Lens' FaultRootCause [FaultRootCauseService]
frcServices = lens _frcServices (\ s a -> s{_frcServices = a}) . _Default . _Coerce

instance FromJSON FaultRootCause where
        parseJSON
          = withObject "FaultRootCause"
              (\ x ->
                 FaultRootCause' <$> (x .:? "Services" .!= mempty))

instance Hashable FaultRootCause where

instance NFData FaultRootCause where

-- | A collection of segments and corresponding subsegments associated to a trace summary fault error.
--
--
--
-- /See:/ 'faultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { _frceExceptions :: !(Maybe [RootCauseException])
  , _frceRemote     :: !(Maybe Bool)
  , _frceName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaultRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frceExceptions' - The types and messages of the exceptions.
--
-- * 'frceRemote' - A flag that denotes a remote subsegment.
--
-- * 'frceName' - The name of the entity.
faultRootCauseEntity
    :: FaultRootCauseEntity
faultRootCauseEntity =
  FaultRootCauseEntity'
    {_frceExceptions = Nothing, _frceRemote = Nothing, _frceName = Nothing}


-- | The types and messages of the exceptions.
frceExceptions :: Lens' FaultRootCauseEntity [RootCauseException]
frceExceptions = lens _frceExceptions (\ s a -> s{_frceExceptions = a}) . _Default . _Coerce

-- | A flag that denotes a remote subsegment.
frceRemote :: Lens' FaultRootCauseEntity (Maybe Bool)
frceRemote = lens _frceRemote (\ s a -> s{_frceRemote = a})

-- | The name of the entity.
frceName :: Lens' FaultRootCauseEntity (Maybe Text)
frceName = lens _frceName (\ s a -> s{_frceName = a})

instance FromJSON FaultRootCauseEntity where
        parseJSON
          = withObject "FaultRootCauseEntity"
              (\ x ->
                 FaultRootCauseEntity' <$>
                   (x .:? "Exceptions" .!= mempty) <*> (x .:? "Remote")
                     <*> (x .:? "Name"))

instance Hashable FaultRootCauseEntity where

instance NFData FaultRootCauseEntity where

-- | A collection of fields identifying the services in a trace summary fault.
--
--
--
-- /See:/ 'faultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { _frcsEntityPath :: !(Maybe [FaultRootCauseEntity])
  , _frcsAccountId  :: !(Maybe Text)
  , _frcsNames      :: !(Maybe [Text])
  , _frcsName       :: !(Maybe Text)
  , _frcsInferred   :: !(Maybe Bool)
  , _frcsType       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaultRootCauseService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frcsEntityPath' - The path of root cause entities found on the service.
--
-- * 'frcsAccountId' - The account ID associated to the service.
--
-- * 'frcsNames' - A collection of associated service names.
--
-- * 'frcsName' - The service name.
--
-- * 'frcsInferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- * 'frcsType' - The type associated to the service.
faultRootCauseService
    :: FaultRootCauseService
faultRootCauseService =
  FaultRootCauseService'
    { _frcsEntityPath = Nothing
    , _frcsAccountId = Nothing
    , _frcsNames = Nothing
    , _frcsName = Nothing
    , _frcsInferred = Nothing
    , _frcsType = Nothing
    }


-- | The path of root cause entities found on the service.
frcsEntityPath :: Lens' FaultRootCauseService [FaultRootCauseEntity]
frcsEntityPath = lens _frcsEntityPath (\ s a -> s{_frcsEntityPath = a}) . _Default . _Coerce

-- | The account ID associated to the service.
frcsAccountId :: Lens' FaultRootCauseService (Maybe Text)
frcsAccountId = lens _frcsAccountId (\ s a -> s{_frcsAccountId = a})

-- | A collection of associated service names.
frcsNames :: Lens' FaultRootCauseService [Text]
frcsNames = lens _frcsNames (\ s a -> s{_frcsNames = a}) . _Default . _Coerce

-- | The service name.
frcsName :: Lens' FaultRootCauseService (Maybe Text)
frcsName = lens _frcsName (\ s a -> s{_frcsName = a})

-- | A Boolean value indicating if the service is inferred from the trace.
frcsInferred :: Lens' FaultRootCauseService (Maybe Bool)
frcsInferred = lens _frcsInferred (\ s a -> s{_frcsInferred = a})

-- | The type associated to the service.
frcsType :: Lens' FaultRootCauseService (Maybe Text)
frcsType = lens _frcsType (\ s a -> s{_frcsType = a})

instance FromJSON FaultRootCauseService where
        parseJSON
          = withObject "FaultRootCauseService"
              (\ x ->
                 FaultRootCauseService' <$>
                   (x .:? "EntityPath" .!= mempty) <*>
                     (x .:? "AccountId")
                     <*> (x .:? "Names" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Inferred")
                     <*> (x .:? "Type"))

instance Hashable FaultRootCauseService where

instance NFData FaultRootCauseService where

-- | Information about requests that failed with a 5xx Server Error status code.
--
--
--
-- /See:/ 'faultStatistics' smart constructor.
data FaultStatistics = FaultStatistics'
  { _fsOtherCount :: !(Maybe Integer)
  , _fsTotalCount :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaultStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsOtherCount' - The number of requests that failed with untracked 5xx Server Error status codes.
--
-- * 'fsTotalCount' - The total number of requests that failed with a 5xx Server Error status code.
faultStatistics
    :: FaultStatistics
faultStatistics =
  FaultStatistics' {_fsOtherCount = Nothing, _fsTotalCount = Nothing}


-- | The number of requests that failed with untracked 5xx Server Error status codes.
fsOtherCount :: Lens' FaultStatistics (Maybe Integer)
fsOtherCount = lens _fsOtherCount (\ s a -> s{_fsOtherCount = a})

-- | The total number of requests that failed with a 5xx Server Error status code.
fsTotalCount :: Lens' FaultStatistics (Maybe Integer)
fsTotalCount = lens _fsTotalCount (\ s a -> s{_fsTotalCount = a})

instance FromJSON FaultStatistics where
        parseJSON
          = withObject "FaultStatistics"
              (\ x ->
                 FaultStatistics' <$>
                   (x .:? "OtherCount") <*> (x .:? "TotalCount"))

instance Hashable FaultStatistics where

instance NFData FaultStatistics where

-- | Details and metadata for a group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gFilterExpression :: !(Maybe Text)
  , _gGroupARN         :: !(Maybe Text)
  , _gGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gFilterExpression' - The filter expression defining the parameters to include traces.
--
-- * 'gGroupARN' - The ARN of the group generated based on the GroupName.
--
-- * 'gGroupName' - The unique case-sensitive name of the group.
group'
    :: Group
group' =
  Group'
    {_gFilterExpression = Nothing, _gGroupARN = Nothing, _gGroupName = Nothing}


-- | The filter expression defining the parameters to include traces.
gFilterExpression :: Lens' Group (Maybe Text)
gFilterExpression = lens _gFilterExpression (\ s a -> s{_gFilterExpression = a})

-- | The ARN of the group generated based on the GroupName.
gGroupARN :: Lens' Group (Maybe Text)
gGroupARN = lens _gGroupARN (\ s a -> s{_gGroupARN = a})

-- | The unique case-sensitive name of the group.
gGroupName :: Lens' Group (Maybe Text)
gGroupName = lens _gGroupName (\ s a -> s{_gGroupName = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "FilterExpression") <*> (x .:? "GroupARN") <*>
                     (x .:? "GroupName"))

instance Hashable Group where

instance NFData Group where

-- | Details for a group without metadata.
--
--
--
-- /See:/ 'groupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { _gsFilterExpression :: !(Maybe Text)
  , _gsGroupARN         :: !(Maybe Text)
  , _gsGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsFilterExpression' - The filter expression defining the parameters to include traces.
--
-- * 'gsGroupARN' - The ARN of the group generated based on the GroupName.
--
-- * 'gsGroupName' - The unique case-sensitive name of the group.
groupSummary
    :: GroupSummary
groupSummary =
  GroupSummary'
    { _gsFilterExpression = Nothing
    , _gsGroupARN = Nothing
    , _gsGroupName = Nothing
    }


-- | The filter expression defining the parameters to include traces.
gsFilterExpression :: Lens' GroupSummary (Maybe Text)
gsFilterExpression = lens _gsFilterExpression (\ s a -> s{_gsFilterExpression = a})

-- | The ARN of the group generated based on the GroupName.
gsGroupARN :: Lens' GroupSummary (Maybe Text)
gsGroupARN = lens _gsGroupARN (\ s a -> s{_gsGroupARN = a})

-- | The unique case-sensitive name of the group.
gsGroupName :: Lens' GroupSummary (Maybe Text)
gsGroupName = lens _gsGroupName (\ s a -> s{_gsGroupName = a})

instance FromJSON GroupSummary where
        parseJSON
          = withObject "GroupSummary"
              (\ x ->
                 GroupSummary' <$>
                   (x .:? "FilterExpression") <*> (x .:? "GroupARN") <*>
                     (x .:? "GroupName"))

instance Hashable GroupSummary where

instance NFData GroupSummary where

-- | Information about an HTTP request.
--
--
--
-- /See:/ 'hTTP' smart constructor.
data HTTP = HTTP'
  { _httpHTTPMethod :: !(Maybe Text)
  , _httpHTTPStatus :: !(Maybe Int)
  , _httpClientIP   :: !(Maybe Text)
  , _httpUserAgent  :: !(Maybe Text)
  , _httpHTTPURL    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HTTP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpHTTPMethod' - The request method.
--
-- * 'httpHTTPStatus' - The response status.
--
-- * 'httpClientIP' - The IP address of the requestor.
--
-- * 'httpUserAgent' - The request's user agent string.
--
-- * 'httpHTTPURL' - The request URL.
hTTP
    :: HTTP
hTTP =
  HTTP'
    { _httpHTTPMethod = Nothing
    , _httpHTTPStatus = Nothing
    , _httpClientIP = Nothing
    , _httpUserAgent = Nothing
    , _httpHTTPURL = Nothing
    }


-- | The request method.
httpHTTPMethod :: Lens' HTTP (Maybe Text)
httpHTTPMethod = lens _httpHTTPMethod (\ s a -> s{_httpHTTPMethod = a})

-- | The response status.
httpHTTPStatus :: Lens' HTTP (Maybe Int)
httpHTTPStatus = lens _httpHTTPStatus (\ s a -> s{_httpHTTPStatus = a})

-- | The IP address of the requestor.
httpClientIP :: Lens' HTTP (Maybe Text)
httpClientIP = lens _httpClientIP (\ s a -> s{_httpClientIP = a})

-- | The request's user agent string.
httpUserAgent :: Lens' HTTP (Maybe Text)
httpUserAgent = lens _httpUserAgent (\ s a -> s{_httpUserAgent = a})

-- | The request URL.
httpHTTPURL :: Lens' HTTP (Maybe Text)
httpHTTPURL = lens _httpHTTPURL (\ s a -> s{_httpHTTPURL = a})

instance FromJSON HTTP where
        parseJSON
          = withObject "HTTP"
              (\ x ->
                 HTTP' <$>
                   (x .:? "HttpMethod") <*> (x .:? "HttpStatus") <*>
                     (x .:? "ClientIp")
                     <*> (x .:? "UserAgent")
                     <*> (x .:? "HttpURL"))

instance Hashable HTTP where

instance NFData HTTP where

-- | An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.
--
--
--
-- /See:/ 'histogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { _heCount :: !(Maybe Int)
  , _heValue :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistogramEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heCount' - The prevalence of the entry.
--
-- * 'heValue' - The value of the entry.
histogramEntry
    :: HistogramEntry
histogramEntry = HistogramEntry' {_heCount = Nothing, _heValue = Nothing}


-- | The prevalence of the entry.
heCount :: Lens' HistogramEntry (Maybe Int)
heCount = lens _heCount (\ s a -> s{_heCount = a})

-- | The value of the entry.
heValue :: Lens' HistogramEntry (Maybe Double)
heValue = lens _heValue (\ s a -> s{_heValue = a})

instance FromJSON HistogramEntry where
        parseJSON
          = withObject "HistogramEntry"
              (\ x ->
                 HistogramEntry' <$>
                   (x .:? "Count") <*> (x .:? "Value"))

instance Hashable HistogramEntry where

instance NFData HistogramEntry where

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
--
--
-- /See:/ 'instanceIdDetail' smart constructor.
newtype InstanceIdDetail = InstanceIdDetail'
  { _iidId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceIdDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iidId' - The ID of a corresponding EC2 instance.
instanceIdDetail
    :: InstanceIdDetail
instanceIdDetail = InstanceIdDetail' {_iidId = Nothing}


-- | The ID of a corresponding EC2 instance.
iidId :: Lens' InstanceIdDetail (Maybe Text)
iidId = lens _iidId (\ s a -> s{_iidId = a})

instance FromJSON InstanceIdDetail where
        parseJSON
          = withObject "InstanceIdDetail"
              (\ x -> InstanceIdDetail' <$> (x .:? "Id"))

instance Hashable InstanceIdDetail where

instance NFData InstanceIdDetail where

-- | A list of resources ARNs corresponding to the segments in a trace.
--
--
--
-- /See:/ 'resourceARNDetail' smart constructor.
newtype ResourceARNDetail = ResourceARNDetail'
  { _radARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceARNDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radARN' - The ARN of a corresponding resource.
resourceARNDetail
    :: ResourceARNDetail
resourceARNDetail = ResourceARNDetail' {_radARN = Nothing}


-- | The ARN of a corresponding resource.
radARN :: Lens' ResourceARNDetail (Maybe Text)
radARN = lens _radARN (\ s a -> s{_radARN = a})

instance FromJSON ResourceARNDetail where
        parseJSON
          = withObject "ResourceARNDetail"
              (\ x -> ResourceARNDetail' <$> (x .:? "ARN"))

instance Hashable ResourceARNDetail where

instance NFData ResourceARNDetail where

-- | The root cause information for a response time warning.
--
--
--
-- /See:/ 'responseTimeRootCause' smart constructor.
newtype ResponseTimeRootCause = ResponseTimeRootCause'
  { _rtrcServices :: Maybe [ResponseTimeRootCauseService]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResponseTimeRootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrcServices' - A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
responseTimeRootCause
    :: ResponseTimeRootCause
responseTimeRootCause = ResponseTimeRootCause' {_rtrcServices = Nothing}


-- | A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
rtrcServices :: Lens' ResponseTimeRootCause [ResponseTimeRootCauseService]
rtrcServices = lens _rtrcServices (\ s a -> s{_rtrcServices = a}) . _Default . _Coerce

instance FromJSON ResponseTimeRootCause where
        parseJSON
          = withObject "ResponseTimeRootCause"
              (\ x ->
                 ResponseTimeRootCause' <$>
                   (x .:? "Services" .!= mempty))

instance Hashable ResponseTimeRootCause where

instance NFData ResponseTimeRootCause where

-- | A collection of segments and corresponding subsegments associated to a response time warning.
--
--
--
-- /See:/ 'responseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { _rtrceRemote   :: !(Maybe Bool)
  , _rtrceCoverage :: !(Maybe Double)
  , _rtrceName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResponseTimeRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrceRemote' - A flag that denotes a remote subsegment.
--
-- * 'rtrceCoverage' - The types and messages of the exceptions.
--
-- * 'rtrceName' - The name of the entity.
responseTimeRootCauseEntity
    :: ResponseTimeRootCauseEntity
responseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    {_rtrceRemote = Nothing, _rtrceCoverage = Nothing, _rtrceName = Nothing}


-- | A flag that denotes a remote subsegment.
rtrceRemote :: Lens' ResponseTimeRootCauseEntity (Maybe Bool)
rtrceRemote = lens _rtrceRemote (\ s a -> s{_rtrceRemote = a})

-- | The types and messages of the exceptions.
rtrceCoverage :: Lens' ResponseTimeRootCauseEntity (Maybe Double)
rtrceCoverage = lens _rtrceCoverage (\ s a -> s{_rtrceCoverage = a})

-- | The name of the entity.
rtrceName :: Lens' ResponseTimeRootCauseEntity (Maybe Text)
rtrceName = lens _rtrceName (\ s a -> s{_rtrceName = a})

instance FromJSON ResponseTimeRootCauseEntity where
        parseJSON
          = withObject "ResponseTimeRootCauseEntity"
              (\ x ->
                 ResponseTimeRootCauseEntity' <$>
                   (x .:? "Remote") <*> (x .:? "Coverage") <*>
                     (x .:? "Name"))

instance Hashable ResponseTimeRootCauseEntity where

instance NFData ResponseTimeRootCauseEntity where

-- | A collection of fields identifying the service in a response time warning.
--
--
--
-- /See:/ 'responseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { _rtrcsEntityPath :: !(Maybe [ResponseTimeRootCauseEntity])
  , _rtrcsAccountId  :: !(Maybe Text)
  , _rtrcsNames      :: !(Maybe [Text])
  , _rtrcsName       :: !(Maybe Text)
  , _rtrcsInferred   :: !(Maybe Bool)
  , _rtrcsType       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResponseTimeRootCauseService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrcsEntityPath' - The path of root cause entities found on the service.
--
-- * 'rtrcsAccountId' - The account ID associated to the service.
--
-- * 'rtrcsNames' - A collection of associated service names.
--
-- * 'rtrcsName' - The service name.
--
-- * 'rtrcsInferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- * 'rtrcsType' - The type associated to the service.
responseTimeRootCauseService
    :: ResponseTimeRootCauseService
responseTimeRootCauseService =
  ResponseTimeRootCauseService'
    { _rtrcsEntityPath = Nothing
    , _rtrcsAccountId = Nothing
    , _rtrcsNames = Nothing
    , _rtrcsName = Nothing
    , _rtrcsInferred = Nothing
    , _rtrcsType = Nothing
    }


-- | The path of root cause entities found on the service.
rtrcsEntityPath :: Lens' ResponseTimeRootCauseService [ResponseTimeRootCauseEntity]
rtrcsEntityPath = lens _rtrcsEntityPath (\ s a -> s{_rtrcsEntityPath = a}) . _Default . _Coerce

-- | The account ID associated to the service.
rtrcsAccountId :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsAccountId = lens _rtrcsAccountId (\ s a -> s{_rtrcsAccountId = a})

-- | A collection of associated service names.
rtrcsNames :: Lens' ResponseTimeRootCauseService [Text]
rtrcsNames = lens _rtrcsNames (\ s a -> s{_rtrcsNames = a}) . _Default . _Coerce

-- | The service name.
rtrcsName :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsName = lens _rtrcsName (\ s a -> s{_rtrcsName = a})

-- | A Boolean value indicating if the service is inferred from the trace.
rtrcsInferred :: Lens' ResponseTimeRootCauseService (Maybe Bool)
rtrcsInferred = lens _rtrcsInferred (\ s a -> s{_rtrcsInferred = a})

-- | The type associated to the service.
rtrcsType :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsType = lens _rtrcsType (\ s a -> s{_rtrcsType = a})

instance FromJSON ResponseTimeRootCauseService where
        parseJSON
          = withObject "ResponseTimeRootCauseService"
              (\ x ->
                 ResponseTimeRootCauseService' <$>
                   (x .:? "EntityPath" .!= mempty) <*>
                     (x .:? "AccountId")
                     <*> (x .:? "Names" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Inferred")
                     <*> (x .:? "Type"))

instance Hashable ResponseTimeRootCauseService where

instance NFData ResponseTimeRootCauseService where

-- | The exception associated with a root cause.
--
--
--
-- /See:/ 'rootCauseException' smart constructor.
data RootCauseException = RootCauseException'
  { _rceName    :: !(Maybe Text)
  , _rceMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RootCauseException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rceName' - The name of the exception.
--
-- * 'rceMessage' - The message of the exception.
rootCauseException
    :: RootCauseException
rootCauseException =
  RootCauseException' {_rceName = Nothing, _rceMessage = Nothing}


-- | The name of the exception.
rceName :: Lens' RootCauseException (Maybe Text)
rceName = lens _rceName (\ s a -> s{_rceName = a})

-- | The message of the exception.
rceMessage :: Lens' RootCauseException (Maybe Text)
rceMessage = lens _rceMessage (\ s a -> s{_rceMessage = a})

instance FromJSON RootCauseException where
        parseJSON
          = withObject "RootCauseException"
              (\ x ->
                 RootCauseException' <$>
                   (x .:? "Name") <*> (x .:? "Message"))

instance Hashable RootCauseException where

instance NFData RootCauseException where

-- | A sampling rule that services use to decide whether to instrument a request. Rule fields can match properties of the service, or properties of a request. The service can ignore rules that don't match its properties.
--
--
--
-- /See:/ 'samplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { _srRuleName      :: !(Maybe Text)
  , _srAttributes    :: !(Maybe (Map Text Text))
  , _srRuleARN       :: !(Maybe Text)
  , _srResourceARN   :: !Text
  , _srPriority      :: !Nat
  , _srFixedRate     :: !Double
  , _srReservoirSize :: !Nat
  , _srServiceName   :: !Text
  , _srServiceType   :: !Text
  , _srHost          :: !Text
  , _srHTTPMethod    :: !Text
  , _srURLPath       :: !Text
  , _srVersion       :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srRuleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'srAttributes' - Matches attributes derived from the request.
--
-- * 'srRuleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'srResourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- * 'srPriority' - The priority of the sampling rule.
--
-- * 'srFixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- * 'srReservoirSize' - A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- * 'srServiceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- * 'srServiceType' - Matches the @origin@ that the service uses to identify its type in segments.
--
-- * 'srHost' - Matches the hostname from a request URL.
--
-- * 'srHTTPMethod' - Matches the HTTP method of a request.
--
-- * 'srURLPath' - Matches the path from a request URL.
--
-- * 'srVersion' - The version of the sampling rule format (@1@ ).
samplingRule
    :: Text -- ^ 'srResourceARN'
    -> Natural -- ^ 'srPriority'
    -> Double -- ^ 'srFixedRate'
    -> Natural -- ^ 'srReservoirSize'
    -> Text -- ^ 'srServiceName'
    -> Text -- ^ 'srServiceType'
    -> Text -- ^ 'srHost'
    -> Text -- ^ 'srHTTPMethod'
    -> Text -- ^ 'srURLPath'
    -> Natural -- ^ 'srVersion'
    -> SamplingRule
samplingRule pResourceARN_ pPriority_ pFixedRate_ pReservoirSize_ pServiceName_ pServiceType_ pHost_ pHTTPMethod_ pURLPath_ pVersion_ =
  SamplingRule'
    { _srRuleName = Nothing
    , _srAttributes = Nothing
    , _srRuleARN = Nothing
    , _srResourceARN = pResourceARN_
    , _srPriority = _Nat # pPriority_
    , _srFixedRate = pFixedRate_
    , _srReservoirSize = _Nat # pReservoirSize_
    , _srServiceName = pServiceName_
    , _srServiceType = pServiceType_
    , _srHost = pHost_
    , _srHTTPMethod = pHTTPMethod_
    , _srURLPath = pURLPath_
    , _srVersion = _Nat # pVersion_
    }


-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
srRuleName :: Lens' SamplingRule (Maybe Text)
srRuleName = lens _srRuleName (\ s a -> s{_srRuleName = a})

-- | Matches attributes derived from the request.
srAttributes :: Lens' SamplingRule (HashMap Text Text)
srAttributes = lens _srAttributes (\ s a -> s{_srAttributes = a}) . _Default . _Map

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
srRuleARN :: Lens' SamplingRule (Maybe Text)
srRuleARN = lens _srRuleARN (\ s a -> s{_srRuleARN = a})

-- | Matches the ARN of the AWS resource on which the service runs.
srResourceARN :: Lens' SamplingRule Text
srResourceARN = lens _srResourceARN (\ s a -> s{_srResourceARN = a})

-- | The priority of the sampling rule.
srPriority :: Lens' SamplingRule Natural
srPriority = lens _srPriority (\ s a -> s{_srPriority = a}) . _Nat

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
srFixedRate :: Lens' SamplingRule Double
srFixedRate = lens _srFixedRate (\ s a -> s{_srFixedRate = a})

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
srReservoirSize :: Lens' SamplingRule Natural
srReservoirSize = lens _srReservoirSize (\ s a -> s{_srReservoirSize = a}) . _Nat

-- | Matches the @name@ that the service uses to identify itself in segments.
srServiceName :: Lens' SamplingRule Text
srServiceName = lens _srServiceName (\ s a -> s{_srServiceName = a})

-- | Matches the @origin@ that the service uses to identify its type in segments.
srServiceType :: Lens' SamplingRule Text
srServiceType = lens _srServiceType (\ s a -> s{_srServiceType = a})

-- | Matches the hostname from a request URL.
srHost :: Lens' SamplingRule Text
srHost = lens _srHost (\ s a -> s{_srHost = a})

-- | Matches the HTTP method of a request.
srHTTPMethod :: Lens' SamplingRule Text
srHTTPMethod = lens _srHTTPMethod (\ s a -> s{_srHTTPMethod = a})

-- | Matches the path from a request URL.
srURLPath :: Lens' SamplingRule Text
srURLPath = lens _srURLPath (\ s a -> s{_srURLPath = a})

-- | The version of the sampling rule format (@1@ ).
srVersion :: Lens' SamplingRule Natural
srVersion = lens _srVersion (\ s a -> s{_srVersion = a}) . _Nat

instance FromJSON SamplingRule where
        parseJSON
          = withObject "SamplingRule"
              (\ x ->
                 SamplingRule' <$>
                   (x .:? "RuleName") <*>
                     (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "RuleARN")
                     <*> (x .: "ResourceARN")
                     <*> (x .: "Priority")
                     <*> (x .: "FixedRate")
                     <*> (x .: "ReservoirSize")
                     <*> (x .: "ServiceName")
                     <*> (x .: "ServiceType")
                     <*> (x .: "Host")
                     <*> (x .: "HTTPMethod")
                     <*> (x .: "URLPath")
                     <*> (x .: "Version"))

instance Hashable SamplingRule where

instance NFData SamplingRule where

instance ToJSON SamplingRule where
        toJSON SamplingRule'{..}
          = object
              (catMaybes
                 [("RuleName" .=) <$> _srRuleName,
                  ("Attributes" .=) <$> _srAttributes,
                  ("RuleARN" .=) <$> _srRuleARN,
                  Just ("ResourceARN" .= _srResourceARN),
                  Just ("Priority" .= _srPriority),
                  Just ("FixedRate" .= _srFixedRate),
                  Just ("ReservoirSize" .= _srReservoirSize),
                  Just ("ServiceName" .= _srServiceName),
                  Just ("ServiceType" .= _srServiceType),
                  Just ("Host" .= _srHost),
                  Just ("HTTPMethod" .= _srHTTPMethod),
                  Just ("URLPath" .= _srURLPath),
                  Just ("Version" .= _srVersion)])

-- | A 'SamplingRule' and its metadata.
--
--
--
-- /See:/ 'samplingRuleRecord' smart constructor.
data SamplingRuleRecord = SamplingRuleRecord'
  { _srrModifiedAt   :: !(Maybe POSIX)
  , _srrSamplingRule :: !(Maybe SamplingRule)
  , _srrCreatedAt    :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingRuleRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrModifiedAt' - When the rule was last modified.
--
-- * 'srrSamplingRule' - The sampling rule.
--
-- * 'srrCreatedAt' - When the rule was created.
samplingRuleRecord
    :: SamplingRuleRecord
samplingRuleRecord =
  SamplingRuleRecord'
    { _srrModifiedAt = Nothing
    , _srrSamplingRule = Nothing
    , _srrCreatedAt = Nothing
    }


-- | When the rule was last modified.
srrModifiedAt :: Lens' SamplingRuleRecord (Maybe UTCTime)
srrModifiedAt = lens _srrModifiedAt (\ s a -> s{_srrModifiedAt = a}) . mapping _Time

-- | The sampling rule.
srrSamplingRule :: Lens' SamplingRuleRecord (Maybe SamplingRule)
srrSamplingRule = lens _srrSamplingRule (\ s a -> s{_srrSamplingRule = a})

-- | When the rule was created.
srrCreatedAt :: Lens' SamplingRuleRecord (Maybe UTCTime)
srrCreatedAt = lens _srrCreatedAt (\ s a -> s{_srrCreatedAt = a}) . mapping _Time

instance FromJSON SamplingRuleRecord where
        parseJSON
          = withObject "SamplingRuleRecord"
              (\ x ->
                 SamplingRuleRecord' <$>
                   (x .:? "ModifiedAt") <*> (x .:? "SamplingRule") <*>
                     (x .:? "CreatedAt"))

instance Hashable SamplingRuleRecord where

instance NFData SamplingRuleRecord where

-- | A document specifying changes to a sampling rule's configuration.
--
--
--
-- /See:/ 'samplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { _sruHTTPMethod    :: !(Maybe Text)
  , _sruPriority      :: !(Maybe Int)
  , _sruRuleName      :: !(Maybe Text)
  , _sruReservoirSize :: !(Maybe Int)
  , _sruFixedRate     :: !(Maybe Double)
  , _sruResourceARN   :: !(Maybe Text)
  , _sruAttributes    :: !(Maybe (Map Text Text))
  , _sruServiceName   :: !(Maybe Text)
  , _sruServiceType   :: !(Maybe Text)
  , _sruHost          :: !(Maybe Text)
  , _sruRuleARN       :: !(Maybe Text)
  , _sruURLPath       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingRuleUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sruHTTPMethod' - Matches the HTTP method of a request.
--
-- * 'sruPriority' - The priority of the sampling rule.
--
-- * 'sruRuleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'sruReservoirSize' - A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- * 'sruFixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- * 'sruResourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- * 'sruAttributes' - Matches attributes derived from the request.
--
-- * 'sruServiceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- * 'sruServiceType' - Matches the @origin@ that the service uses to identify its type in segments.
--
-- * 'sruHost' - Matches the hostname from a request URL.
--
-- * 'sruRuleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'sruURLPath' - Matches the path from a request URL.
samplingRuleUpdate
    :: SamplingRuleUpdate
samplingRuleUpdate =
  SamplingRuleUpdate'
    { _sruHTTPMethod = Nothing
    , _sruPriority = Nothing
    , _sruRuleName = Nothing
    , _sruReservoirSize = Nothing
    , _sruFixedRate = Nothing
    , _sruResourceARN = Nothing
    , _sruAttributes = Nothing
    , _sruServiceName = Nothing
    , _sruServiceType = Nothing
    , _sruHost = Nothing
    , _sruRuleARN = Nothing
    , _sruURLPath = Nothing
    }


-- | Matches the HTTP method of a request.
sruHTTPMethod :: Lens' SamplingRuleUpdate (Maybe Text)
sruHTTPMethod = lens _sruHTTPMethod (\ s a -> s{_sruHTTPMethod = a})

-- | The priority of the sampling rule.
sruPriority :: Lens' SamplingRuleUpdate (Maybe Int)
sruPriority = lens _sruPriority (\ s a -> s{_sruPriority = a})

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
sruRuleName :: Lens' SamplingRuleUpdate (Maybe Text)
sruRuleName = lens _sruRuleName (\ s a -> s{_sruRuleName = a})

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
sruReservoirSize :: Lens' SamplingRuleUpdate (Maybe Int)
sruReservoirSize = lens _sruReservoirSize (\ s a -> s{_sruReservoirSize = a})

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
sruFixedRate :: Lens' SamplingRuleUpdate (Maybe Double)
sruFixedRate = lens _sruFixedRate (\ s a -> s{_sruFixedRate = a})

-- | Matches the ARN of the AWS resource on which the service runs.
sruResourceARN :: Lens' SamplingRuleUpdate (Maybe Text)
sruResourceARN = lens _sruResourceARN (\ s a -> s{_sruResourceARN = a})

-- | Matches attributes derived from the request.
sruAttributes :: Lens' SamplingRuleUpdate (HashMap Text Text)
sruAttributes = lens _sruAttributes (\ s a -> s{_sruAttributes = a}) . _Default . _Map

-- | Matches the @name@ that the service uses to identify itself in segments.
sruServiceName :: Lens' SamplingRuleUpdate (Maybe Text)
sruServiceName = lens _sruServiceName (\ s a -> s{_sruServiceName = a})

-- | Matches the @origin@ that the service uses to identify its type in segments.
sruServiceType :: Lens' SamplingRuleUpdate (Maybe Text)
sruServiceType = lens _sruServiceType (\ s a -> s{_sruServiceType = a})

-- | Matches the hostname from a request URL.
sruHost :: Lens' SamplingRuleUpdate (Maybe Text)
sruHost = lens _sruHost (\ s a -> s{_sruHost = a})

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
sruRuleARN :: Lens' SamplingRuleUpdate (Maybe Text)
sruRuleARN = lens _sruRuleARN (\ s a -> s{_sruRuleARN = a})

-- | Matches the path from a request URL.
sruURLPath :: Lens' SamplingRuleUpdate (Maybe Text)
sruURLPath = lens _sruURLPath (\ s a -> s{_sruURLPath = a})

instance Hashable SamplingRuleUpdate where

instance NFData SamplingRuleUpdate where

instance ToJSON SamplingRuleUpdate where
        toJSON SamplingRuleUpdate'{..}
          = object
              (catMaybes
                 [("HTTPMethod" .=) <$> _sruHTTPMethod,
                  ("Priority" .=) <$> _sruPriority,
                  ("RuleName" .=) <$> _sruRuleName,
                  ("ReservoirSize" .=) <$> _sruReservoirSize,
                  ("FixedRate" .=) <$> _sruFixedRate,
                  ("ResourceARN" .=) <$> _sruResourceARN,
                  ("Attributes" .=) <$> _sruAttributes,
                  ("ServiceName" .=) <$> _sruServiceName,
                  ("ServiceType" .=) <$> _sruServiceType,
                  ("Host" .=) <$> _sruHost,
                  ("RuleARN" .=) <$> _sruRuleARN,
                  ("URLPath" .=) <$> _sruURLPath])

-- | Aggregated request sampling data for a sampling rule across all services for a 10 second window.
--
--
--
-- /See:/ 'samplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { _sssRequestCount :: !(Maybe Int)
  , _sssBorrowCount  :: !(Maybe Int)
  , _sssRuleName     :: !(Maybe Text)
  , _sssTimestamp    :: !(Maybe POSIX)
  , _sssSampledCount :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingStatisticSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssRequestCount' - The number of requests that matched the rule.
--
-- * 'sssBorrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- * 'sssRuleName' - The name of the sampling rule.
--
-- * 'sssTimestamp' - The start time of the reporting window.
--
-- * 'sssSampledCount' - The number of requests recorded.
samplingStatisticSummary
    :: SamplingStatisticSummary
samplingStatisticSummary =
  SamplingStatisticSummary'
    { _sssRequestCount = Nothing
    , _sssBorrowCount = Nothing
    , _sssRuleName = Nothing
    , _sssTimestamp = Nothing
    , _sssSampledCount = Nothing
    }


-- | The number of requests that matched the rule.
sssRequestCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssRequestCount = lens _sssRequestCount (\ s a -> s{_sssRequestCount = a})

-- | The number of requests recorded with borrowed reservoir quota.
sssBorrowCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssBorrowCount = lens _sssBorrowCount (\ s a -> s{_sssBorrowCount = a})

-- | The name of the sampling rule.
sssRuleName :: Lens' SamplingStatisticSummary (Maybe Text)
sssRuleName = lens _sssRuleName (\ s a -> s{_sssRuleName = a})

-- | The start time of the reporting window.
sssTimestamp :: Lens' SamplingStatisticSummary (Maybe UTCTime)
sssTimestamp = lens _sssTimestamp (\ s a -> s{_sssTimestamp = a}) . mapping _Time

-- | The number of requests recorded.
sssSampledCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssSampledCount = lens _sssSampledCount (\ s a -> s{_sssSampledCount = a})

instance FromJSON SamplingStatisticSummary where
        parseJSON
          = withObject "SamplingStatisticSummary"
              (\ x ->
                 SamplingStatisticSummary' <$>
                   (x .:? "RequestCount") <*> (x .:? "BorrowCount") <*>
                     (x .:? "RuleName")
                     <*> (x .:? "Timestamp")
                     <*> (x .:? "SampledCount"))

instance Hashable SamplingStatisticSummary where

instance NFData SamplingStatisticSummary where

-- | Request sampling results for a single rule from a service. Results are for the last 10 seconds unless the service has been assigned a longer reporting interval after a previous call to 'GetSamplingTargets' .
--
--
--
-- /See:/ 'samplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { _ssdBorrowCount  :: !(Maybe Nat)
  , _ssdRuleName     :: !Text
  , _ssdClientId     :: !Text
  , _ssdTimestamp    :: !POSIX
  , _ssdRequestCount :: !Nat
  , _ssdSampledCount :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingStatisticsDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdBorrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- * 'ssdRuleName' - The name of the sampling rule.
--
-- * 'ssdClientId' - A unique identifier for the service in hexadecimal.
--
-- * 'ssdTimestamp' - The current time.
--
-- * 'ssdRequestCount' - The number of requests that matched the rule.
--
-- * 'ssdSampledCount' - The number of requests recorded.
samplingStatisticsDocument
    :: Text -- ^ 'ssdRuleName'
    -> Text -- ^ 'ssdClientId'
    -> UTCTime -- ^ 'ssdTimestamp'
    -> Natural -- ^ 'ssdRequestCount'
    -> Natural -- ^ 'ssdSampledCount'
    -> SamplingStatisticsDocument
samplingStatisticsDocument pRuleName_ pClientId_ pTimestamp_ pRequestCount_ pSampledCount_ =
  SamplingStatisticsDocument'
    { _ssdBorrowCount = Nothing
    , _ssdRuleName = pRuleName_
    , _ssdClientId = pClientId_
    , _ssdTimestamp = _Time # pTimestamp_
    , _ssdRequestCount = _Nat # pRequestCount_
    , _ssdSampledCount = _Nat # pSampledCount_
    }


-- | The number of requests recorded with borrowed reservoir quota.
ssdBorrowCount :: Lens' SamplingStatisticsDocument (Maybe Natural)
ssdBorrowCount = lens _ssdBorrowCount (\ s a -> s{_ssdBorrowCount = a}) . mapping _Nat

-- | The name of the sampling rule.
ssdRuleName :: Lens' SamplingStatisticsDocument Text
ssdRuleName = lens _ssdRuleName (\ s a -> s{_ssdRuleName = a})

-- | A unique identifier for the service in hexadecimal.
ssdClientId :: Lens' SamplingStatisticsDocument Text
ssdClientId = lens _ssdClientId (\ s a -> s{_ssdClientId = a})

-- | The current time.
ssdTimestamp :: Lens' SamplingStatisticsDocument UTCTime
ssdTimestamp = lens _ssdTimestamp (\ s a -> s{_ssdTimestamp = a}) . _Time

-- | The number of requests that matched the rule.
ssdRequestCount :: Lens' SamplingStatisticsDocument Natural
ssdRequestCount = lens _ssdRequestCount (\ s a -> s{_ssdRequestCount = a}) . _Nat

-- | The number of requests recorded.
ssdSampledCount :: Lens' SamplingStatisticsDocument Natural
ssdSampledCount = lens _ssdSampledCount (\ s a -> s{_ssdSampledCount = a}) . _Nat

instance Hashable SamplingStatisticsDocument where

instance NFData SamplingStatisticsDocument where

instance ToJSON SamplingStatisticsDocument where
        toJSON SamplingStatisticsDocument'{..}
          = object
              (catMaybes
                 [("BorrowCount" .=) <$> _ssdBorrowCount,
                  Just ("RuleName" .= _ssdRuleName),
                  Just ("ClientID" .= _ssdClientId),
                  Just ("Timestamp" .= _ssdTimestamp),
                  Just ("RequestCount" .= _ssdRequestCount),
                  Just ("SampledCount" .= _ssdSampledCount)])

-- | Temporary changes to a sampling rule configuration. To meet the global sampling target for a rule, X-Ray calculates a new reservoir for each service based on the recent sampling results of all services that called 'GetSamplingTargets' .
--
--
--
-- /See:/ 'samplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { _stdReservoirQuota    :: !(Maybe Int)
  , _stdRuleName          :: !(Maybe Text)
  , _stdFixedRate         :: !(Maybe Double)
  , _stdInterval          :: !(Maybe Int)
  , _stdReservoirQuotaTTL :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SamplingTargetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdReservoirQuota' - The number of requests per second that X-Ray allocated this service.
--
-- * 'stdRuleName' - The name of the sampling rule.
--
-- * 'stdFixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- * 'stdInterval' - The number of seconds for the service to wait before getting sampling targets again.
--
-- * 'stdReservoirQuotaTTL' - When the reservoir quota expires.
samplingTargetDocument
    :: SamplingTargetDocument
samplingTargetDocument =
  SamplingTargetDocument'
    { _stdReservoirQuota = Nothing
    , _stdRuleName = Nothing
    , _stdFixedRate = Nothing
    , _stdInterval = Nothing
    , _stdReservoirQuotaTTL = Nothing
    }


-- | The number of requests per second that X-Ray allocated this service.
stdReservoirQuota :: Lens' SamplingTargetDocument (Maybe Int)
stdReservoirQuota = lens _stdReservoirQuota (\ s a -> s{_stdReservoirQuota = a})

-- | The name of the sampling rule.
stdRuleName :: Lens' SamplingTargetDocument (Maybe Text)
stdRuleName = lens _stdRuleName (\ s a -> s{_stdRuleName = a})

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
stdFixedRate :: Lens' SamplingTargetDocument (Maybe Double)
stdFixedRate = lens _stdFixedRate (\ s a -> s{_stdFixedRate = a})

-- | The number of seconds for the service to wait before getting sampling targets again.
stdInterval :: Lens' SamplingTargetDocument (Maybe Int)
stdInterval = lens _stdInterval (\ s a -> s{_stdInterval = a})

-- | When the reservoir quota expires.
stdReservoirQuotaTTL :: Lens' SamplingTargetDocument (Maybe UTCTime)
stdReservoirQuotaTTL = lens _stdReservoirQuotaTTL (\ s a -> s{_stdReservoirQuotaTTL = a}) . mapping _Time

instance FromJSON SamplingTargetDocument where
        parseJSON
          = withObject "SamplingTargetDocument"
              (\ x ->
                 SamplingTargetDocument' <$>
                   (x .:? "ReservoirQuota") <*> (x .:? "RuleName") <*>
                     (x .:? "FixedRate")
                     <*> (x .:? "Interval")
                     <*> (x .:? "ReservoirQuotaTTL"))

instance Hashable SamplingTargetDocument where

instance NFData SamplingTargetDocument where

-- | A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with 'PutTraceSegments' , or an @inferred@ segment for a downstream service, generated from a subsegment sent by the service that called it.
--
--
-- For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
--
--
-- /See:/ 'segment' smart constructor.
data Segment = Segment'
  { _sDocument :: !(Maybe Text)
  , _sId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Segment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDocument' - The segment document.
--
-- * 'sId' - The segment's ID.
segment
    :: Segment
segment = Segment' {_sDocument = Nothing, _sId = Nothing}


-- | The segment document.
sDocument :: Lens' Segment (Maybe Text)
sDocument = lens _sDocument (\ s a -> s{_sDocument = a})

-- | The segment's ID.
sId :: Lens' Segment (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a})

instance FromJSON Segment where
        parseJSON
          = withObject "Segment"
              (\ x ->
                 Segment' <$> (x .:? "Document") <*> (x .:? "Id"))

instance Hashable Segment where

instance NFData Segment where

-- |
--
--
--
-- /See:/ 'serviceId' smart constructor.
data ServiceId = ServiceId'
  { _siAccountId :: !(Maybe Text)
  , _siNames     :: !(Maybe [Text])
  , _siName      :: !(Maybe Text)
  , _siType      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siAccountId' -
--
-- * 'siNames' -
--
-- * 'siName' -
--
-- * 'siType' -
serviceId
    :: ServiceId
serviceId =
  ServiceId'
    { _siAccountId = Nothing
    , _siNames = Nothing
    , _siName = Nothing
    , _siType = Nothing
    }


-- |
siAccountId :: Lens' ServiceId (Maybe Text)
siAccountId = lens _siAccountId (\ s a -> s{_siAccountId = a})

-- |
siNames :: Lens' ServiceId [Text]
siNames = lens _siNames (\ s a -> s{_siNames = a}) . _Default . _Coerce

-- |
siName :: Lens' ServiceId (Maybe Text)
siName = lens _siName (\ s a -> s{_siName = a})

-- |
siType :: Lens' ServiceId (Maybe Text)
siType = lens _siType (\ s a -> s{_siType = a})

instance FromJSON ServiceId where
        parseJSON
          = withObject "ServiceId"
              (\ x ->
                 ServiceId' <$>
                   (x .:? "AccountId") <*> (x .:? "Names" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Type"))

instance Hashable ServiceId where

instance NFData ServiceId where

-- | Information about an application that processed requests, users that made requests, or downstream services, resources and applications that an application used.
--
--
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _sState                 :: !(Maybe Text)
  , _sStartTime             :: !(Maybe POSIX)
  , _sRoot                  :: !(Maybe Bool)
  , _sResponseTimeHistogram :: !(Maybe [HistogramEntry])
  , _sDurationHistogram     :: !(Maybe [HistogramEntry])
  , _sReferenceId           :: !(Maybe Int)
  , _sAccountId             :: !(Maybe Text)
  , _sNames                 :: !(Maybe [Text])
  , _sName                  :: !(Maybe Text)
  , _sEndTime               :: !(Maybe POSIX)
  , _sType                  :: !(Maybe Text)
  , _sEdges                 :: !(Maybe [Edge])
  , _sSummaryStatistics     :: !(Maybe ServiceStatistics)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sState' - The service's state.
--
-- * 'sStartTime' - The start time of the first segment that the service generated.
--
-- * 'sRoot' - Indicates that the service was the first service to process a request.
--
-- * 'sResponseTimeHistogram' - A histogram that maps the spread of service response times.
--
-- * 'sDurationHistogram' - A histogram that maps the spread of service durations.
--
-- * 'sReferenceId' - Identifier for the service. Unique within the service map.
--
-- * 'sAccountId' - Identifier of the AWS account in which the service runs.
--
-- * 'sNames' - A list of names for the service, including the canonical name.
--
-- * 'sName' - The canonical name of the service.
--
-- * 'sEndTime' - The end time of the last segment that the service generated.
--
-- * 'sType' - The type of service.     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for a application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.     * @client@ - Represents the clients that sent requests to a root service.     * @remote@ - A downstream service of indeterminate type.
--
-- * 'sEdges' - Connections to downstream services.
--
-- * 'sSummaryStatistics' - Aggregated statistics for the service.
serviceInfo
    :: ServiceInfo
serviceInfo =
  ServiceInfo'
    { _sState = Nothing
    , _sStartTime = Nothing
    , _sRoot = Nothing
    , _sResponseTimeHistogram = Nothing
    , _sDurationHistogram = Nothing
    , _sReferenceId = Nothing
    , _sAccountId = Nothing
    , _sNames = Nothing
    , _sName = Nothing
    , _sEndTime = Nothing
    , _sType = Nothing
    , _sEdges = Nothing
    , _sSummaryStatistics = Nothing
    }


-- | The service's state.
sState :: Lens' ServiceInfo (Maybe Text)
sState = lens _sState (\ s a -> s{_sState = a})

-- | The start time of the first segment that the service generated.
sStartTime :: Lens' ServiceInfo (Maybe UTCTime)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . mapping _Time

-- | Indicates that the service was the first service to process a request.
sRoot :: Lens' ServiceInfo (Maybe Bool)
sRoot = lens _sRoot (\ s a -> s{_sRoot = a})

-- | A histogram that maps the spread of service response times.
sResponseTimeHistogram :: Lens' ServiceInfo [HistogramEntry]
sResponseTimeHistogram = lens _sResponseTimeHistogram (\ s a -> s{_sResponseTimeHistogram = a}) . _Default . _Coerce

-- | A histogram that maps the spread of service durations.
sDurationHistogram :: Lens' ServiceInfo [HistogramEntry]
sDurationHistogram = lens _sDurationHistogram (\ s a -> s{_sDurationHistogram = a}) . _Default . _Coerce

-- | Identifier for the service. Unique within the service map.
sReferenceId :: Lens' ServiceInfo (Maybe Int)
sReferenceId = lens _sReferenceId (\ s a -> s{_sReferenceId = a})

-- | Identifier of the AWS account in which the service runs.
sAccountId :: Lens' ServiceInfo (Maybe Text)
sAccountId = lens _sAccountId (\ s a -> s{_sAccountId = a})

-- | A list of names for the service, including the canonical name.
sNames :: Lens' ServiceInfo [Text]
sNames = lens _sNames (\ s a -> s{_sNames = a}) . _Default . _Coerce

-- | The canonical name of the service.
sName :: Lens' ServiceInfo (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The end time of the last segment that the service generated.
sEndTime :: Lens' ServiceInfo (Maybe UTCTime)
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a}) . mapping _Time

-- | The type of service.     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for a application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.     * @client@ - Represents the clients that sent requests to a root service.     * @remote@ - A downstream service of indeterminate type.
sType :: Lens' ServiceInfo (Maybe Text)
sType = lens _sType (\ s a -> s{_sType = a})

-- | Connections to downstream services.
sEdges :: Lens' ServiceInfo [Edge]
sEdges = lens _sEdges (\ s a -> s{_sEdges = a}) . _Default . _Coerce

-- | Aggregated statistics for the service.
sSummaryStatistics :: Lens' ServiceInfo (Maybe ServiceStatistics)
sSummaryStatistics = lens _sSummaryStatistics (\ s a -> s{_sSummaryStatistics = a})

instance FromJSON ServiceInfo where
        parseJSON
          = withObject "ServiceInfo"
              (\ x ->
                 ServiceInfo' <$>
                   (x .:? "State") <*> (x .:? "StartTime") <*>
                     (x .:? "Root")
                     <*> (x .:? "ResponseTimeHistogram" .!= mempty)
                     <*> (x .:? "DurationHistogram" .!= mempty)
                     <*> (x .:? "ReferenceId")
                     <*> (x .:? "AccountId")
                     <*> (x .:? "Names" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "Type")
                     <*> (x .:? "Edges" .!= mempty)
                     <*> (x .:? "SummaryStatistics"))

instance Hashable ServiceInfo where

instance NFData ServiceInfo where

-- | Response statistics for a service.
--
--
--
-- /See:/ 'serviceStatistics' smart constructor.
data ServiceStatistics = ServiceStatistics'
  { _ssFaultStatistics   :: !(Maybe FaultStatistics)
  , _ssOKCount           :: !(Maybe Integer)
  , _ssTotalResponseTime :: !(Maybe Double)
  , _ssErrorStatistics   :: !(Maybe ErrorStatistics)
  , _ssTotalCount        :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssFaultStatistics' - Information about requests that failed with a 5xx Server Error status code.
--
-- * 'ssOKCount' - The number of requests that completed with a 2xx Success status code.
--
-- * 'ssTotalResponseTime' - The aggregate response time of completed requests.
--
-- * 'ssErrorStatistics' - Information about requests that failed with a 4xx Client Error status code.
--
-- * 'ssTotalCount' - The total number of completed requests.
serviceStatistics
    :: ServiceStatistics
serviceStatistics =
  ServiceStatistics'
    { _ssFaultStatistics = Nothing
    , _ssOKCount = Nothing
    , _ssTotalResponseTime = Nothing
    , _ssErrorStatistics = Nothing
    , _ssTotalCount = Nothing
    }


-- | Information about requests that failed with a 5xx Server Error status code.
ssFaultStatistics :: Lens' ServiceStatistics (Maybe FaultStatistics)
ssFaultStatistics = lens _ssFaultStatistics (\ s a -> s{_ssFaultStatistics = a})

-- | The number of requests that completed with a 2xx Success status code.
ssOKCount :: Lens' ServiceStatistics (Maybe Integer)
ssOKCount = lens _ssOKCount (\ s a -> s{_ssOKCount = a})

-- | The aggregate response time of completed requests.
ssTotalResponseTime :: Lens' ServiceStatistics (Maybe Double)
ssTotalResponseTime = lens _ssTotalResponseTime (\ s a -> s{_ssTotalResponseTime = a})

-- | Information about requests that failed with a 4xx Client Error status code.
ssErrorStatistics :: Lens' ServiceStatistics (Maybe ErrorStatistics)
ssErrorStatistics = lens _ssErrorStatistics (\ s a -> s{_ssErrorStatistics = a})

-- | The total number of completed requests.
ssTotalCount :: Lens' ServiceStatistics (Maybe Integer)
ssTotalCount = lens _ssTotalCount (\ s a -> s{_ssTotalCount = a})

instance FromJSON ServiceStatistics where
        parseJSON
          = withObject "ServiceStatistics"
              (\ x ->
                 ServiceStatistics' <$>
                   (x .:? "FaultStatistics") <*> (x .:? "OkCount") <*>
                     (x .:? "TotalResponseTime")
                     <*> (x .:? "ErrorStatistics")
                     <*> (x .:? "TotalCount"))

instance Hashable ServiceStatistics where

instance NFData ServiceStatistics where

-- |
--
--
--
-- /See:/ 'telemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { _trSegmentsReceivedCount   :: !(Maybe Int)
  , _trSegmentsSentCount       :: !(Maybe Int)
  , _trSegmentsSpilloverCount  :: !(Maybe Int)
  , _trSegmentsRejectedCount   :: !(Maybe Int)
  , _trBackendConnectionErrors :: !(Maybe BackendConnectionErrors)
  , _trTimestamp               :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TelemetryRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trSegmentsReceivedCount' -
--
-- * 'trSegmentsSentCount' -
--
-- * 'trSegmentsSpilloverCount' -
--
-- * 'trSegmentsRejectedCount' -
--
-- * 'trBackendConnectionErrors' -
--
-- * 'trTimestamp' -
telemetryRecord
    :: UTCTime -- ^ 'trTimestamp'
    -> TelemetryRecord
telemetryRecord pTimestamp_ =
  TelemetryRecord'
    { _trSegmentsReceivedCount = Nothing
    , _trSegmentsSentCount = Nothing
    , _trSegmentsSpilloverCount = Nothing
    , _trSegmentsRejectedCount = Nothing
    , _trBackendConnectionErrors = Nothing
    , _trTimestamp = _Time # pTimestamp_
    }


-- |
trSegmentsReceivedCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsReceivedCount = lens _trSegmentsReceivedCount (\ s a -> s{_trSegmentsReceivedCount = a})

-- |
trSegmentsSentCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsSentCount = lens _trSegmentsSentCount (\ s a -> s{_trSegmentsSentCount = a})

-- |
trSegmentsSpilloverCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsSpilloverCount = lens _trSegmentsSpilloverCount (\ s a -> s{_trSegmentsSpilloverCount = a})

-- |
trSegmentsRejectedCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsRejectedCount = lens _trSegmentsRejectedCount (\ s a -> s{_trSegmentsRejectedCount = a})

-- |
trBackendConnectionErrors :: Lens' TelemetryRecord (Maybe BackendConnectionErrors)
trBackendConnectionErrors = lens _trBackendConnectionErrors (\ s a -> s{_trBackendConnectionErrors = a})

-- |
trTimestamp :: Lens' TelemetryRecord UTCTime
trTimestamp = lens _trTimestamp (\ s a -> s{_trTimestamp = a}) . _Time

instance Hashable TelemetryRecord where

instance NFData TelemetryRecord where

instance ToJSON TelemetryRecord where
        toJSON TelemetryRecord'{..}
          = object
              (catMaybes
                 [("SegmentsReceivedCount" .=) <$>
                    _trSegmentsReceivedCount,
                  ("SegmentsSentCount" .=) <$> _trSegmentsSentCount,
                  ("SegmentsSpilloverCount" .=) <$>
                    _trSegmentsSpilloverCount,
                  ("SegmentsRejectedCount" .=) <$>
                    _trSegmentsRejectedCount,
                  ("BackendConnectionErrors" .=) <$>
                    _trBackendConnectionErrors,
                  Just ("Timestamp" .= _trTimestamp)])

-- | A collection of segment documents with matching trace IDs.
--
--
--
-- /See:/ 'trace' smart constructor.
data Trace = Trace'
  { _tId       :: !(Maybe Text)
  , _tSegments :: !(Maybe [Segment])
  , _tDuration :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Trace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tId' - The unique identifier for the request that generated the trace's segments and subsegments.
--
-- * 'tSegments' - Segment documents for the segments and subsegments that comprise the trace.
--
-- * 'tDuration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
trace
    :: Trace
trace = Trace' {_tId = Nothing, _tSegments = Nothing, _tDuration = Nothing}


-- | The unique identifier for the request that generated the trace's segments and subsegments.
tId :: Lens' Trace (Maybe Text)
tId = lens _tId (\ s a -> s{_tId = a})

-- | Segment documents for the segments and subsegments that comprise the trace.
tSegments :: Lens' Trace [Segment]
tSegments = lens _tSegments (\ s a -> s{_tSegments = a}) . _Default . _Coerce

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
tDuration :: Lens' Trace (Maybe Double)
tDuration = lens _tDuration (\ s a -> s{_tDuration = a})

instance FromJSON Trace where
        parseJSON
          = withObject "Trace"
              (\ x ->
                 Trace' <$>
                   (x .:? "Id") <*> (x .:? "Segments" .!= mempty) <*>
                     (x .:? "Duration"))

instance Hashable Trace where

instance NFData Trace where

-- | Metadata generated from the segment documents in a trace.
--
--
--
-- /See:/ 'traceSummary' smart constructor.
data TraceSummary = TraceSummary'
  { _tsAnnotations            :: !(Maybe (Map Text [ValueWithServiceIds]))
  , _tsHasThrottle            :: !(Maybe Bool)
  , _tsUsers                  :: !(Maybe [TraceUser])
  , _tsEntryPoint             :: !(Maybe ServiceId)
  , _tsHasFault               :: !(Maybe Bool)
  , _tsServiceIds             :: !(Maybe [ServiceId])
  , _tsIsPartial              :: !(Maybe Bool)
  , _tsErrorRootCauses        :: !(Maybe [ErrorRootCause])
  , _tsResourceARNs           :: !(Maybe [ResourceARNDetail])
  , _tsAvailabilityZones      :: !(Maybe [AvailabilityZoneDetail])
  , _tsInstanceIds            :: !(Maybe [InstanceIdDetail])
  , _tsResponseTimeRootCauses :: !(Maybe [ResponseTimeRootCause])
  , _tsHasError               :: !(Maybe Bool)
  , _tsId                     :: !(Maybe Text)
  , _tsHTTP                   :: !(Maybe HTTP)
  , _tsRevision               :: !(Maybe Int)
  , _tsDuration               :: !(Maybe Double)
  , _tsFaultRootCauses        :: !(Maybe [FaultRootCause])
  , _tsResponseTime           :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TraceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsAnnotations' - Annotations from the trace's segment documents.
--
-- * 'tsHasThrottle' - One or more of the segment documents has a 429 throttling error.
--
-- * 'tsUsers' - Users from the trace's segment documents.
--
-- * 'tsEntryPoint' - The root of a trace.
--
-- * 'tsHasFault' - One or more of the segment documents has a 500 series error.
--
-- * 'tsServiceIds' - Service IDs from the trace's segment documents.
--
-- * 'tsIsPartial' - One or more of the segment documents is in progress.
--
-- * 'tsErrorRootCauses' - A collection of ErrorRootCause structures corresponding to the trace segments.
--
-- * 'tsResourceARNs' - A list of resource ARNs for any resource corresponding to the trace segments.
--
-- * 'tsAvailabilityZones' - A list of availability zones for any zone corresponding to the trace segments.
--
-- * 'tsInstanceIds' - A list of EC2 instance IDs for any instance corresponding to the trace segments.
--
-- * 'tsResponseTimeRootCauses' - A collection of ResponseTimeRootCause structures corresponding to the trace segments.
--
-- * 'tsHasError' - One or more of the segment documents has a 400 series error.
--
-- * 'tsId' - The unique identifier for the request that generated the trace's segments and subsegments.
--
-- * 'tsHTTP' - Information about the HTTP request served by the trace.
--
-- * 'tsRevision' - The revision number of a trace.
--
-- * 'tsDuration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- * 'tsFaultRootCauses' - A collection of FaultRootCause structures corresponding to the the trace segments.
--
-- * 'tsResponseTime' - The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
traceSummary
    :: TraceSummary
traceSummary =
  TraceSummary'
    { _tsAnnotations = Nothing
    , _tsHasThrottle = Nothing
    , _tsUsers = Nothing
    , _tsEntryPoint = Nothing
    , _tsHasFault = Nothing
    , _tsServiceIds = Nothing
    , _tsIsPartial = Nothing
    , _tsErrorRootCauses = Nothing
    , _tsResourceARNs = Nothing
    , _tsAvailabilityZones = Nothing
    , _tsInstanceIds = Nothing
    , _tsResponseTimeRootCauses = Nothing
    , _tsHasError = Nothing
    , _tsId = Nothing
    , _tsHTTP = Nothing
    , _tsRevision = Nothing
    , _tsDuration = Nothing
    , _tsFaultRootCauses = Nothing
    , _tsResponseTime = Nothing
    }


-- | Annotations from the trace's segment documents.
tsAnnotations :: Lens' TraceSummary (HashMap Text [ValueWithServiceIds])
tsAnnotations = lens _tsAnnotations (\ s a -> s{_tsAnnotations = a}) . _Default . _Map

-- | One or more of the segment documents has a 429 throttling error.
tsHasThrottle :: Lens' TraceSummary (Maybe Bool)
tsHasThrottle = lens _tsHasThrottle (\ s a -> s{_tsHasThrottle = a})

-- | Users from the trace's segment documents.
tsUsers :: Lens' TraceSummary [TraceUser]
tsUsers = lens _tsUsers (\ s a -> s{_tsUsers = a}) . _Default . _Coerce

-- | The root of a trace.
tsEntryPoint :: Lens' TraceSummary (Maybe ServiceId)
tsEntryPoint = lens _tsEntryPoint (\ s a -> s{_tsEntryPoint = a})

-- | One or more of the segment documents has a 500 series error.
tsHasFault :: Lens' TraceSummary (Maybe Bool)
tsHasFault = lens _tsHasFault (\ s a -> s{_tsHasFault = a})

-- | Service IDs from the trace's segment documents.
tsServiceIds :: Lens' TraceSummary [ServiceId]
tsServiceIds = lens _tsServiceIds (\ s a -> s{_tsServiceIds = a}) . _Default . _Coerce

-- | One or more of the segment documents is in progress.
tsIsPartial :: Lens' TraceSummary (Maybe Bool)
tsIsPartial = lens _tsIsPartial (\ s a -> s{_tsIsPartial = a})

-- | A collection of ErrorRootCause structures corresponding to the trace segments.
tsErrorRootCauses :: Lens' TraceSummary [ErrorRootCause]
tsErrorRootCauses = lens _tsErrorRootCauses (\ s a -> s{_tsErrorRootCauses = a}) . _Default . _Coerce

-- | A list of resource ARNs for any resource corresponding to the trace segments.
tsResourceARNs :: Lens' TraceSummary [ResourceARNDetail]
tsResourceARNs = lens _tsResourceARNs (\ s a -> s{_tsResourceARNs = a}) . _Default . _Coerce

-- | A list of availability zones for any zone corresponding to the trace segments.
tsAvailabilityZones :: Lens' TraceSummary [AvailabilityZoneDetail]
tsAvailabilityZones = lens _tsAvailabilityZones (\ s a -> s{_tsAvailabilityZones = a}) . _Default . _Coerce

-- | A list of EC2 instance IDs for any instance corresponding to the trace segments.
tsInstanceIds :: Lens' TraceSummary [InstanceIdDetail]
tsInstanceIds = lens _tsInstanceIds (\ s a -> s{_tsInstanceIds = a}) . _Default . _Coerce

-- | A collection of ResponseTimeRootCause structures corresponding to the trace segments.
tsResponseTimeRootCauses :: Lens' TraceSummary [ResponseTimeRootCause]
tsResponseTimeRootCauses = lens _tsResponseTimeRootCauses (\ s a -> s{_tsResponseTimeRootCauses = a}) . _Default . _Coerce

-- | One or more of the segment documents has a 400 series error.
tsHasError :: Lens' TraceSummary (Maybe Bool)
tsHasError = lens _tsHasError (\ s a -> s{_tsHasError = a})

-- | The unique identifier for the request that generated the trace's segments and subsegments.
tsId :: Lens' TraceSummary (Maybe Text)
tsId = lens _tsId (\ s a -> s{_tsId = a})

-- | Information about the HTTP request served by the trace.
tsHTTP :: Lens' TraceSummary (Maybe HTTP)
tsHTTP = lens _tsHTTP (\ s a -> s{_tsHTTP = a})

-- | The revision number of a trace.
tsRevision :: Lens' TraceSummary (Maybe Int)
tsRevision = lens _tsRevision (\ s a -> s{_tsRevision = a})

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
tsDuration :: Lens' TraceSummary (Maybe Double)
tsDuration = lens _tsDuration (\ s a -> s{_tsDuration = a})

-- | A collection of FaultRootCause structures corresponding to the the trace segments.
tsFaultRootCauses :: Lens' TraceSummary [FaultRootCause]
tsFaultRootCauses = lens _tsFaultRootCauses (\ s a -> s{_tsFaultRootCauses = a}) . _Default . _Coerce

-- | The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
tsResponseTime :: Lens' TraceSummary (Maybe Double)
tsResponseTime = lens _tsResponseTime (\ s a -> s{_tsResponseTime = a})

instance FromJSON TraceSummary where
        parseJSON
          = withObject "TraceSummary"
              (\ x ->
                 TraceSummary' <$>
                   (x .:? "Annotations" .!= mempty) <*>
                     (x .:? "HasThrottle")
                     <*> (x .:? "Users" .!= mempty)
                     <*> (x .:? "EntryPoint")
                     <*> (x .:? "HasFault")
                     <*> (x .:? "ServiceIds" .!= mempty)
                     <*> (x .:? "IsPartial")
                     <*> (x .:? "ErrorRootCauses" .!= mempty)
                     <*> (x .:? "ResourceARNs" .!= mempty)
                     <*> (x .:? "AvailabilityZones" .!= mempty)
                     <*> (x .:? "InstanceIds" .!= mempty)
                     <*> (x .:? "ResponseTimeRootCauses" .!= mempty)
                     <*> (x .:? "HasError")
                     <*> (x .:? "Id")
                     <*> (x .:? "Http")
                     <*> (x .:? "Revision")
                     <*> (x .:? "Duration")
                     <*> (x .:? "FaultRootCauses" .!= mempty)
                     <*> (x .:? "ResponseTime"))

instance Hashable TraceSummary where

instance NFData TraceSummary where

-- | Information about a user recorded in segment documents.
--
--
--
-- /See:/ 'traceUser' smart constructor.
data TraceUser = TraceUser'
  { _tuServiceIds :: !(Maybe [ServiceId])
  , _tuUserName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TraceUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuServiceIds' - Services that the user's request hit.
--
-- * 'tuUserName' - The user's name.
traceUser
    :: TraceUser
traceUser = TraceUser' {_tuServiceIds = Nothing, _tuUserName = Nothing}


-- | Services that the user's request hit.
tuServiceIds :: Lens' TraceUser [ServiceId]
tuServiceIds = lens _tuServiceIds (\ s a -> s{_tuServiceIds = a}) . _Default . _Coerce

-- | The user's name.
tuUserName :: Lens' TraceUser (Maybe Text)
tuUserName = lens _tuUserName (\ s a -> s{_tuUserName = a})

instance FromJSON TraceUser where
        parseJSON
          = withObject "TraceUser"
              (\ x ->
                 TraceUser' <$>
                   (x .:? "ServiceIds" .!= mempty) <*>
                     (x .:? "UserName"))

instance Hashable TraceUser where

instance NFData TraceUser where

-- | Sampling statistics from a call to 'GetSamplingTargets' that X-Ray could not process.
--
--
--
-- /See:/ 'unprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { _usRuleName  :: !(Maybe Text)
  , _usErrorCode :: !(Maybe Text)
  , _usMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usRuleName' - The name of the sampling rule.
--
-- * 'usErrorCode' - The error code.
--
-- * 'usMessage' - The error message.
unprocessedStatistics
    :: UnprocessedStatistics
unprocessedStatistics =
  UnprocessedStatistics'
    {_usRuleName = Nothing, _usErrorCode = Nothing, _usMessage = Nothing}


-- | The name of the sampling rule.
usRuleName :: Lens' UnprocessedStatistics (Maybe Text)
usRuleName = lens _usRuleName (\ s a -> s{_usRuleName = a})

-- | The error code.
usErrorCode :: Lens' UnprocessedStatistics (Maybe Text)
usErrorCode = lens _usErrorCode (\ s a -> s{_usErrorCode = a})

-- | The error message.
usMessage :: Lens' UnprocessedStatistics (Maybe Text)
usMessage = lens _usMessage (\ s a -> s{_usMessage = a})

instance FromJSON UnprocessedStatistics where
        parseJSON
          = withObject "UnprocessedStatistics"
              (\ x ->
                 UnprocessedStatistics' <$>
                   (x .:? "RuleName") <*> (x .:? "ErrorCode") <*>
                     (x .:? "Message"))

instance Hashable UnprocessedStatistics where

instance NFData UnprocessedStatistics where

-- | Information about a segment that failed processing.
--
--
--
-- /See:/ 'unprocessedTraceSegment' smart constructor.
data UnprocessedTraceSegment = UnprocessedTraceSegment'
  { _utsErrorCode :: !(Maybe Text)
  , _utsId        :: !(Maybe Text)
  , _utsMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedTraceSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utsErrorCode' - The error that caused processing to fail.
--
-- * 'utsId' - The segment's ID.
--
-- * 'utsMessage' - The error message.
unprocessedTraceSegment
    :: UnprocessedTraceSegment
unprocessedTraceSegment =
  UnprocessedTraceSegment'
    {_utsErrorCode = Nothing, _utsId = Nothing, _utsMessage = Nothing}


-- | The error that caused processing to fail.
utsErrorCode :: Lens' UnprocessedTraceSegment (Maybe Text)
utsErrorCode = lens _utsErrorCode (\ s a -> s{_utsErrorCode = a})

-- | The segment's ID.
utsId :: Lens' UnprocessedTraceSegment (Maybe Text)
utsId = lens _utsId (\ s a -> s{_utsId = a})

-- | The error message.
utsMessage :: Lens' UnprocessedTraceSegment (Maybe Text)
utsMessage = lens _utsMessage (\ s a -> s{_utsMessage = a})

instance FromJSON UnprocessedTraceSegment where
        parseJSON
          = withObject "UnprocessedTraceSegment"
              (\ x ->
                 UnprocessedTraceSegment' <$>
                   (x .:? "ErrorCode") <*> (x .:? "Id") <*>
                     (x .:? "Message"))

instance Hashable UnprocessedTraceSegment where

instance NFData UnprocessedTraceSegment where

-- | Information about a segment annotation.
--
--
--
-- /See:/ 'valueWithServiceIds' smart constructor.
data ValueWithServiceIds = ValueWithServiceIds'
  { _vwsiServiceIds      :: !(Maybe [ServiceId])
  , _vwsiAnnotationValue :: !(Maybe AnnotationValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValueWithServiceIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vwsiServiceIds' - Services to which the annotation applies.
--
-- * 'vwsiAnnotationValue' - Values of the annotation.
valueWithServiceIds
    :: ValueWithServiceIds
valueWithServiceIds =
  ValueWithServiceIds'
    {_vwsiServiceIds = Nothing, _vwsiAnnotationValue = Nothing}


-- | Services to which the annotation applies.
vwsiServiceIds :: Lens' ValueWithServiceIds [ServiceId]
vwsiServiceIds = lens _vwsiServiceIds (\ s a -> s{_vwsiServiceIds = a}) . _Default . _Coerce

-- | Values of the annotation.
vwsiAnnotationValue :: Lens' ValueWithServiceIds (Maybe AnnotationValue)
vwsiAnnotationValue = lens _vwsiAnnotationValue (\ s a -> s{_vwsiAnnotationValue = a})

instance FromJSON ValueWithServiceIds where
        parseJSON
          = withObject "ValueWithServiceIds"
              (\ x ->
                 ValueWithServiceIds' <$>
                   (x .:? "ServiceIds" .!= mempty) <*>
                     (x .:? "AnnotationValue"))

instance Hashable ValueWithServiceIds where

instance NFData ValueWithServiceIds where
