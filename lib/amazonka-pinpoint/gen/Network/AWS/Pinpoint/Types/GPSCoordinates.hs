{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GPSCoordinates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSCoordinates where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the GPS coordinates of a location.
--
--
--
-- /See:/ 'gPSCoordinates' smart constructor.
data GPSCoordinates = GPSCoordinates'
  { _gpscLatitude :: !Double,
    _gpscLongitude :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GPSCoordinates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpscLatitude' - The latitude coordinate of the location.
--
-- * 'gpscLongitude' - The longitude coordinate of the location.
gPSCoordinates ::
  -- | 'gpscLatitude'
  Double ->
  -- | 'gpscLongitude'
  Double ->
  GPSCoordinates
gPSCoordinates pLatitude_ pLongitude_ =
  GPSCoordinates'
    { _gpscLatitude = pLatitude_,
      _gpscLongitude = pLongitude_
    }

-- | The latitude coordinate of the location.
gpscLatitude :: Lens' GPSCoordinates Double
gpscLatitude = lens _gpscLatitude (\s a -> s {_gpscLatitude = a})

-- | The longitude coordinate of the location.
gpscLongitude :: Lens' GPSCoordinates Double
gpscLongitude = lens _gpscLongitude (\s a -> s {_gpscLongitude = a})

instance FromJSON GPSCoordinates where
  parseJSON =
    withObject
      "GPSCoordinates"
      ( \x ->
          GPSCoordinates' <$> (x .: "Latitude") <*> (x .: "Longitude")
      )

instance Hashable GPSCoordinates

instance NFData GPSCoordinates

instance ToJSON GPSCoordinates where
  toJSON GPSCoordinates' {..} =
    object
      ( catMaybes
          [ Just ("Latitude" .= _gpscLatitude),
            Just ("Longitude" .= _gpscLongitude)
          ]
      )
