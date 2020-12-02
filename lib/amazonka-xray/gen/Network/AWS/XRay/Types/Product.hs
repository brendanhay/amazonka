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
-- * 'ecStatus' - The encryption status. After modifying encryption configuration with 'PutEncryptionConfig' , the status can be @UPDATING@ for up to one hour before X-Ray starts encrypting data with the new key.
--
-- * 'ecKeyId' - The ID of the customer master key (CMK) used for encryption, if applicable.
--
-- * 'ecType' - The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
encryptionConfig
    :: EncryptionConfig
encryptionConfig =
  EncryptionConfig' {_ecStatus = Nothing, _ecKeyId = Nothing, _ecType = Nothing}


-- | The encryption status. After modifying encryption configuration with 'PutEncryptionConfig' , the status can be @UPDATING@ for up to one hour before X-Ray starts encrypting data with the new key.
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
  { _tsAnnotations  :: !(Maybe (Map Text [ValueWithServiceIds]))
  , _tsHasThrottle  :: !(Maybe Bool)
  , _tsUsers        :: !(Maybe [TraceUser])
  , _tsHasFault     :: !(Maybe Bool)
  , _tsServiceIds   :: !(Maybe [ServiceId])
  , _tsIsPartial    :: !(Maybe Bool)
  , _tsHasError     :: !(Maybe Bool)
  , _tsId           :: !(Maybe Text)
  , _tsHTTP         :: !(Maybe HTTP)
  , _tsDuration     :: !(Maybe Double)
  , _tsResponseTime :: !(Maybe Double)
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
-- * 'tsHasFault' - One or more of the segment documents has a 500 series error.
--
-- * 'tsServiceIds' - Service IDs from the trace's segment documents.
--
-- * 'tsIsPartial' - One or more of the segment documents is in progress.
--
-- * 'tsHasError' - One or more of the segment documents has a 400 series error.
--
-- * 'tsId' - The unique identifier for the request that generated the trace's segments and subsegments.
--
-- * 'tsHTTP' - Information about the HTTP request served by the trace.
--
-- * 'tsDuration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- * 'tsResponseTime' - The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
traceSummary
    :: TraceSummary
traceSummary =
  TraceSummary'
    { _tsAnnotations = Nothing
    , _tsHasThrottle = Nothing
    , _tsUsers = Nothing
    , _tsHasFault = Nothing
    , _tsServiceIds = Nothing
    , _tsIsPartial = Nothing
    , _tsHasError = Nothing
    , _tsId = Nothing
    , _tsHTTP = Nothing
    , _tsDuration = Nothing
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

-- | One or more of the segment documents has a 500 series error.
tsHasFault :: Lens' TraceSummary (Maybe Bool)
tsHasFault = lens _tsHasFault (\ s a -> s{_tsHasFault = a})

-- | Service IDs from the trace's segment documents.
tsServiceIds :: Lens' TraceSummary [ServiceId]
tsServiceIds = lens _tsServiceIds (\ s a -> s{_tsServiceIds = a}) . _Default . _Coerce

-- | One or more of the segment documents is in progress.
tsIsPartial :: Lens' TraceSummary (Maybe Bool)
tsIsPartial = lens _tsIsPartial (\ s a -> s{_tsIsPartial = a})

-- | One or more of the segment documents has a 400 series error.
tsHasError :: Lens' TraceSummary (Maybe Bool)
tsHasError = lens _tsHasError (\ s a -> s{_tsHasError = a})

-- | The unique identifier for the request that generated the trace's segments and subsegments.
tsId :: Lens' TraceSummary (Maybe Text)
tsId = lens _tsId (\ s a -> s{_tsId = a})

-- | Information about the HTTP request served by the trace.
tsHTTP :: Lens' TraceSummary (Maybe HTTP)
tsHTTP = lens _tsHTTP (\ s a -> s{_tsHTTP = a})

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
tsDuration :: Lens' TraceSummary (Maybe Double)
tsDuration = lens _tsDuration (\ s a -> s{_tsDuration = a})

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
                     <*> (x .:? "HasFault")
                     <*> (x .:? "ServiceIds" .!= mempty)
                     <*> (x .:? "IsPartial")
                     <*> (x .:? "HasError")
                     <*> (x .:? "Id")
                     <*> (x .:? "Http")
                     <*> (x .:? "Duration")
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
