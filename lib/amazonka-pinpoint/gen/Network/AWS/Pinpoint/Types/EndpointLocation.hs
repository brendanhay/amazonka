{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies geographic information about an endpoint.
--
--
--
-- /See:/ 'endpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { _elPostalCode ::
      !(Maybe Text),
    _elLatitude :: !(Maybe Double),
    _elCountry :: !(Maybe Text),
    _elCity :: !(Maybe Text),
    _elRegion :: !(Maybe Text),
    _elLongitude :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elPostalCode' - The postal or ZIP code for the area where the endpoint is located.
--
-- * 'elLatitude' - The latitude coordinate of the endpoint location, rounded to one decimal place.
--
-- * 'elCountry' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
--
-- * 'elCity' - The name of the city where the endpoint is located.
--
-- * 'elRegion' - The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
--
-- * 'elLongitude' - The longitude coordinate of the endpoint location, rounded to one decimal place.
endpointLocation ::
  EndpointLocation
endpointLocation =
  EndpointLocation'
    { _elPostalCode = Nothing,
      _elLatitude = Nothing,
      _elCountry = Nothing,
      _elCity = Nothing,
      _elRegion = Nothing,
      _elLongitude = Nothing
    }

-- | The postal or ZIP code for the area where the endpoint is located.
elPostalCode :: Lens' EndpointLocation (Maybe Text)
elPostalCode = lens _elPostalCode (\s a -> s {_elPostalCode = a})

-- | The latitude coordinate of the endpoint location, rounded to one decimal place.
elLatitude :: Lens' EndpointLocation (Maybe Double)
elLatitude = lens _elLatitude (\s a -> s {_elLatitude = a})

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
elCountry :: Lens' EndpointLocation (Maybe Text)
elCountry = lens _elCountry (\s a -> s {_elCountry = a})

-- | The name of the city where the endpoint is located.
elCity :: Lens' EndpointLocation (Maybe Text)
elCity = lens _elCity (\s a -> s {_elCity = a})

-- | The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
elRegion :: Lens' EndpointLocation (Maybe Text)
elRegion = lens _elRegion (\s a -> s {_elRegion = a})

-- | The longitude coordinate of the endpoint location, rounded to one decimal place.
elLongitude :: Lens' EndpointLocation (Maybe Double)
elLongitude = lens _elLongitude (\s a -> s {_elLongitude = a})

instance FromJSON EndpointLocation where
  parseJSON =
    withObject
      "EndpointLocation"
      ( \x ->
          EndpointLocation'
            <$> (x .:? "PostalCode")
            <*> (x .:? "Latitude")
            <*> (x .:? "Country")
            <*> (x .:? "City")
            <*> (x .:? "Region")
            <*> (x .:? "Longitude")
      )

instance Hashable EndpointLocation

instance NFData EndpointLocation

instance ToJSON EndpointLocation where
  toJSON EndpointLocation' {..} =
    object
      ( catMaybes
          [ ("PostalCode" .=) <$> _elPostalCode,
            ("Latitude" .=) <$> _elLatitude,
            ("Country" .=) <$> _elCountry,
            ("City" .=) <$> _elCity,
            ("Region" .=) <$> _elRegion,
            ("Longitude" .=) <$> _elLongitude
          ]
      )
