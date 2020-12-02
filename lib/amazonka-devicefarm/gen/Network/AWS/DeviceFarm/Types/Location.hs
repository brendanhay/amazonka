{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example, 47.6204, -122.3491).
--
--
-- Elevation is currently not supported.
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lLatitude :: !Double,
    _lLongitude :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lLatitude' - The latitude.
--
-- * 'lLongitude' - The longitude.
location ::
  -- | 'lLatitude'
  Double ->
  -- | 'lLongitude'
  Double ->
  Location
location pLatitude_ pLongitude_ =
  Location' {_lLatitude = pLatitude_, _lLongitude = pLongitude_}

-- | The latitude.
lLatitude :: Lens' Location Double
lLatitude = lens _lLatitude (\s a -> s {_lLatitude = a})

-- | The longitude.
lLongitude :: Lens' Location Double
lLongitude = lens _lLongitude (\s a -> s {_lLongitude = a})

instance FromJSON Location where
  parseJSON =
    withObject
      "Location"
      (\x -> Location' <$> (x .: "latitude") <*> (x .: "longitude"))

instance Hashable Location

instance NFData Location

instance ToJSON Location where
  toJSON Location' {..} =
    object
      ( catMaybes
          [ Just ("latitude" .= _lLatitude),
            Just ("longitude" .= _lLongitude)
          ]
      )
