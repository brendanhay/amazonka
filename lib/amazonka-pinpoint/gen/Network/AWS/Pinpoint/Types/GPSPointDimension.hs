{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GPSPointDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSPointDimension where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.GPSCoordinates
import Network.AWS.Prelude

-- | Specifies GPS-based criteria for including or excluding endpoints from a segment.
--
--
--
-- /See:/ 'gPSPointDimension' smart constructor.
data GPSPointDimension = GPSPointDimension'
  { _gpspdRangeInKilometers ::
      !(Maybe Double),
    _gpspdCoordinates :: !GPSCoordinates
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GPSPointDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpspdRangeInKilometers' - The range, in kilometers, from the GPS coordinates.
--
-- * 'gpspdCoordinates' - The GPS coordinates to measure distance from.
gPSPointDimension ::
  -- | 'gpspdCoordinates'
  GPSCoordinates ->
  GPSPointDimension
gPSPointDimension pCoordinates_ =
  GPSPointDimension'
    { _gpspdRangeInKilometers = Nothing,
      _gpspdCoordinates = pCoordinates_
    }

-- | The range, in kilometers, from the GPS coordinates.
gpspdRangeInKilometers :: Lens' GPSPointDimension (Maybe Double)
gpspdRangeInKilometers = lens _gpspdRangeInKilometers (\s a -> s {_gpspdRangeInKilometers = a})

-- | The GPS coordinates to measure distance from.
gpspdCoordinates :: Lens' GPSPointDimension GPSCoordinates
gpspdCoordinates = lens _gpspdCoordinates (\s a -> s {_gpspdCoordinates = a})

instance FromJSON GPSPointDimension where
  parseJSON =
    withObject
      "GPSPointDimension"
      ( \x ->
          GPSPointDimension'
            <$> (x .:? "RangeInKilometers") <*> (x .: "Coordinates")
      )

instance Hashable GPSPointDimension

instance NFData GPSPointDimension

instance ToJSON GPSPointDimension where
  toJSON GPSPointDimension' {..} =
    object
      ( catMaybes
          [ ("RangeInKilometers" .=) <$> _gpspdRangeInKilometers,
            Just ("Coordinates" .= _gpspdCoordinates)
          ]
      )
