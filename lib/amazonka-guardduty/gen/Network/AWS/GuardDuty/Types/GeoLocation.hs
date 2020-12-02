{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.GeoLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.GeoLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the location of the remote IP address.
--
--
--
-- /See:/ 'geoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { _glLat :: !(Maybe Double),
    _glLon :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glLat' - The latitude information of the remote IP address.
--
-- * 'glLon' - The longitude information of the remote IP address.
geoLocation ::
  GeoLocation
geoLocation = GeoLocation' {_glLat = Nothing, _glLon = Nothing}

-- | The latitude information of the remote IP address.
glLat :: Lens' GeoLocation (Maybe Double)
glLat = lens _glLat (\s a -> s {_glLat = a})

-- | The longitude information of the remote IP address.
glLon :: Lens' GeoLocation (Maybe Double)
glLon = lens _glLon (\s a -> s {_glLon = a})

instance FromJSON GeoLocation where
  parseJSON =
    withObject
      "GeoLocation"
      (\x -> GeoLocation' <$> (x .:? "lat") <*> (x .:? "lon"))

instance Hashable GeoLocation

instance NFData GeoLocation
