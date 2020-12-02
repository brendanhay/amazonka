{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EdgeStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EdgeStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultStatistics

-- | Response statistics for an edge.
--
--
--
-- /See:/ 'edgeStatistics' smart constructor.
data EdgeStatistics = EdgeStatistics'
  { _esFaultStatistics ::
      !(Maybe FaultStatistics),
    _esOKCount :: !(Maybe Integer),
    _esTotalResponseTime :: !(Maybe Double),
    _esErrorStatistics :: !(Maybe ErrorStatistics),
    _esTotalCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
edgeStatistics ::
  EdgeStatistics
edgeStatistics =
  EdgeStatistics'
    { _esFaultStatistics = Nothing,
      _esOKCount = Nothing,
      _esTotalResponseTime = Nothing,
      _esErrorStatistics = Nothing,
      _esTotalCount = Nothing
    }

-- | Information about requests that failed with a 5xx Server Error status code.
esFaultStatistics :: Lens' EdgeStatistics (Maybe FaultStatistics)
esFaultStatistics = lens _esFaultStatistics (\s a -> s {_esFaultStatistics = a})

-- | The number of requests that completed with a 2xx Success status code.
esOKCount :: Lens' EdgeStatistics (Maybe Integer)
esOKCount = lens _esOKCount (\s a -> s {_esOKCount = a})

-- | The aggregate response time of completed requests.
esTotalResponseTime :: Lens' EdgeStatistics (Maybe Double)
esTotalResponseTime = lens _esTotalResponseTime (\s a -> s {_esTotalResponseTime = a})

-- | Information about requests that failed with a 4xx Client Error status code.
esErrorStatistics :: Lens' EdgeStatistics (Maybe ErrorStatistics)
esErrorStatistics = lens _esErrorStatistics (\s a -> s {_esErrorStatistics = a})

-- | The total number of completed requests.
esTotalCount :: Lens' EdgeStatistics (Maybe Integer)
esTotalCount = lens _esTotalCount (\s a -> s {_esTotalCount = a})

instance FromJSON EdgeStatistics where
  parseJSON =
    withObject
      "EdgeStatistics"
      ( \x ->
          EdgeStatistics'
            <$> (x .:? "FaultStatistics")
            <*> (x .:? "OkCount")
            <*> (x .:? "TotalResponseTime")
            <*> (x .:? "ErrorStatistics")
            <*> (x .:? "TotalCount")
      )

instance Hashable EdgeStatistics

instance NFData EdgeStatistics
