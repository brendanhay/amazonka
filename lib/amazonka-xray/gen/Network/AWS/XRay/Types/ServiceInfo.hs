{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
--
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _sState :: !(Maybe Text),
    _sStartTime :: !(Maybe POSIX),
    _sRoot :: !(Maybe Bool),
    _sResponseTimeHistogram :: !(Maybe [HistogramEntry]),
    _sDurationHistogram :: !(Maybe [HistogramEntry]),
    _sReferenceId :: !(Maybe Int),
    _sAccountId :: !(Maybe Text),
    _sNames :: !(Maybe [Text]),
    _sName :: !(Maybe Text),
    _sEndTime :: !(Maybe POSIX),
    _sType :: !(Maybe Text),
    _sEdges :: !(Maybe [Edge]),
    _sSummaryStatistics :: !(Maybe ServiceStatistics)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'sType' - The type of service.     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for an application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.     * @client@ - Represents the clients that sent requests to a root service.     * @remote@ - A downstream service of indeterminate type.
--
-- * 'sEdges' - Connections to downstream services.
--
-- * 'sSummaryStatistics' - Aggregated statistics for the service.
serviceInfo ::
  ServiceInfo
serviceInfo =
  ServiceInfo'
    { _sState = Nothing,
      _sStartTime = Nothing,
      _sRoot = Nothing,
      _sResponseTimeHistogram = Nothing,
      _sDurationHistogram = Nothing,
      _sReferenceId = Nothing,
      _sAccountId = Nothing,
      _sNames = Nothing,
      _sName = Nothing,
      _sEndTime = Nothing,
      _sType = Nothing,
      _sEdges = Nothing,
      _sSummaryStatistics = Nothing
    }

-- | The service's state.
sState :: Lens' ServiceInfo (Maybe Text)
sState = lens _sState (\s a -> s {_sState = a})

-- | The start time of the first segment that the service generated.
sStartTime :: Lens' ServiceInfo (Maybe UTCTime)
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a}) . mapping _Time

-- | Indicates that the service was the first service to process a request.
sRoot :: Lens' ServiceInfo (Maybe Bool)
sRoot = lens _sRoot (\s a -> s {_sRoot = a})

-- | A histogram that maps the spread of service response times.
sResponseTimeHistogram :: Lens' ServiceInfo [HistogramEntry]
sResponseTimeHistogram = lens _sResponseTimeHistogram (\s a -> s {_sResponseTimeHistogram = a}) . _Default . _Coerce

-- | A histogram that maps the spread of service durations.
sDurationHistogram :: Lens' ServiceInfo [HistogramEntry]
sDurationHistogram = lens _sDurationHistogram (\s a -> s {_sDurationHistogram = a}) . _Default . _Coerce

-- | Identifier for the service. Unique within the service map.
sReferenceId :: Lens' ServiceInfo (Maybe Int)
sReferenceId = lens _sReferenceId (\s a -> s {_sReferenceId = a})

-- | Identifier of the AWS account in which the service runs.
sAccountId :: Lens' ServiceInfo (Maybe Text)
sAccountId = lens _sAccountId (\s a -> s {_sAccountId = a})

-- | A list of names for the service, including the canonical name.
sNames :: Lens' ServiceInfo [Text]
sNames = lens _sNames (\s a -> s {_sNames = a}) . _Default . _Coerce

-- | The canonical name of the service.
sName :: Lens' ServiceInfo (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The end time of the last segment that the service generated.
sEndTime :: Lens' ServiceInfo (Maybe UTCTime)
sEndTime = lens _sEndTime (\s a -> s {_sEndTime = a}) . mapping _Time

-- | The type of service.     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for an application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.     * @client@ - Represents the clients that sent requests to a root service.     * @remote@ - A downstream service of indeterminate type.
sType :: Lens' ServiceInfo (Maybe Text)
sType = lens _sType (\s a -> s {_sType = a})

-- | Connections to downstream services.
sEdges :: Lens' ServiceInfo [Edge]
sEdges = lens _sEdges (\s a -> s {_sEdges = a}) . _Default . _Coerce

-- | Aggregated statistics for the service.
sSummaryStatistics :: Lens' ServiceInfo (Maybe ServiceStatistics)
sSummaryStatistics = lens _sSummaryStatistics (\s a -> s {_sSummaryStatistics = a})

instance FromJSON ServiceInfo where
  parseJSON =
    withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            <$> (x .:? "State")
            <*> (x .:? "StartTime")
            <*> (x .:? "Root")
            <*> (x .:? "ResponseTimeHistogram" .!= mempty)
            <*> (x .:? "DurationHistogram" .!= mempty)
            <*> (x .:? "ReferenceId")
            <*> (x .:? "AccountId")
            <*> (x .:? "Names" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "EndTime")
            <*> (x .:? "Type")
            <*> (x .:? "Edges" .!= mempty)
            <*> (x .:? "SummaryStatistics")
      )

instance Hashable ServiceInfo

instance NFData ServiceInfo
