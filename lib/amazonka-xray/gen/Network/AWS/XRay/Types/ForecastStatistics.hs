{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ForecastStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ForecastStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The predicted high and low fault count. This is used to determine if a service has become anomalous and if an insight should be created.
--
--
--
-- /See:/ 'forecastStatistics' smart constructor.
data ForecastStatistics = ForecastStatistics'
  { _fsFaultCountLow ::
      !(Maybe Integer),
    _fsFaultCountHigh :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ForecastStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsFaultCountLow' - The lower limit of fault counts for a service.
--
-- * 'fsFaultCountHigh' - The upper limit of fault counts for a service.
forecastStatistics ::
  ForecastStatistics
forecastStatistics =
  ForecastStatistics'
    { _fsFaultCountLow = Nothing,
      _fsFaultCountHigh = Nothing
    }

-- | The lower limit of fault counts for a service.
fsFaultCountLow :: Lens' ForecastStatistics (Maybe Integer)
fsFaultCountLow = lens _fsFaultCountLow (\s a -> s {_fsFaultCountLow = a})

-- | The upper limit of fault counts for a service.
fsFaultCountHigh :: Lens' ForecastStatistics (Maybe Integer)
fsFaultCountHigh = lens _fsFaultCountHigh (\s a -> s {_fsFaultCountHigh = a})

instance FromJSON ForecastStatistics where
  parseJSON =
    withObject
      "ForecastStatistics"
      ( \x ->
          ForecastStatistics'
            <$> (x .:? "FaultCountLow") <*> (x .:? "FaultCountHigh")
      )

instance Hashable ForecastStatistics

instance NFData ForecastStatistics
