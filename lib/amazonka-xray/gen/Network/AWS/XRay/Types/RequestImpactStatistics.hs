{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.RequestImpactStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.RequestImpactStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statistics that describe how the incident has impacted a service.
--
--
--
-- /See:/ 'requestImpactStatistics' smart constructor.
data RequestImpactStatistics = RequestImpactStatistics'
  { _risOKCount ::
      !(Maybe Integer),
    _risFaultCount :: !(Maybe Integer),
    _risTotalCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestImpactStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risOKCount' - The number of successful requests.
--
-- * 'risFaultCount' - The number of requests that have resulted in a fault,
--
-- * 'risTotalCount' - The total number of requests to the service.
requestImpactStatistics ::
  RequestImpactStatistics
requestImpactStatistics =
  RequestImpactStatistics'
    { _risOKCount = Nothing,
      _risFaultCount = Nothing,
      _risTotalCount = Nothing
    }

-- | The number of successful requests.
risOKCount :: Lens' RequestImpactStatistics (Maybe Integer)
risOKCount = lens _risOKCount (\s a -> s {_risOKCount = a})

-- | The number of requests that have resulted in a fault,
risFaultCount :: Lens' RequestImpactStatistics (Maybe Integer)
risFaultCount = lens _risFaultCount (\s a -> s {_risFaultCount = a})

-- | The total number of requests to the service.
risTotalCount :: Lens' RequestImpactStatistics (Maybe Integer)
risTotalCount = lens _risTotalCount (\s a -> s {_risTotalCount = a})

instance FromJSON RequestImpactStatistics where
  parseJSON =
    withObject
      "RequestImpactStatistics"
      ( \x ->
          RequestImpactStatistics'
            <$> (x .:? "OkCount")
            <*> (x .:? "FaultCount")
            <*> (x .:? "TotalCount")
      )

instance Hashable RequestImpactStatistics

instance NFData RequestImpactStatistics
