{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.GeoLocationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.GeoLocationDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified @geolocation@ code.
--
--
--
-- /See:/ 'geoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
  { _gldSubdivisionName ::
      !(Maybe Text),
    _gldSubdivisionCode :: !(Maybe Text),
    _gldCountryName :: !(Maybe Text),
    _gldCountryCode :: !(Maybe Text),
    _gldContinentCode :: !(Maybe Text),
    _gldContinentName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoLocationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldSubdivisionName' - The full name of the subdivision. Route 53 currently supports only states in the United States.
--
-- * 'gldSubdivisionCode' - The code for the subdivision. Route 53 currently supports only states in the United States.
--
-- * 'gldCountryName' - The name of the country.
--
-- * 'gldCountryCode' - The two-letter code for the country.
--
-- * 'gldContinentCode' - The two-letter code for the continent.
--
-- * 'gldContinentName' - The full name of the continent.
geoLocationDetails ::
  GeoLocationDetails
geoLocationDetails =
  GeoLocationDetails'
    { _gldSubdivisionName = Nothing,
      _gldSubdivisionCode = Nothing,
      _gldCountryName = Nothing,
      _gldCountryCode = Nothing,
      _gldContinentCode = Nothing,
      _gldContinentName = Nothing
    }

-- | The full name of the subdivision. Route 53 currently supports only states in the United States.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName = lens _gldSubdivisionName (\s a -> s {_gldSubdivisionName = a})

-- | The code for the subdivision. Route 53 currently supports only states in the United States.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode = lens _gldSubdivisionCode (\s a -> s {_gldSubdivisionCode = a})

-- | The name of the country.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\s a -> s {_gldCountryName = a})

-- | The two-letter code for the country.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\s a -> s {_gldCountryCode = a})

-- | The two-letter code for the continent.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode = lens _gldContinentCode (\s a -> s {_gldContinentCode = a})

-- | The full name of the continent.
gldContinentName :: Lens' GeoLocationDetails (Maybe Text)
gldContinentName = lens _gldContinentName (\s a -> s {_gldContinentName = a})

instance FromXML GeoLocationDetails where
  parseXML x =
    GeoLocationDetails'
      <$> (x .@? "SubdivisionName")
      <*> (x .@? "SubdivisionCode")
      <*> (x .@? "CountryName")
      <*> (x .@? "CountryCode")
      <*> (x .@? "ContinentCode")
      <*> (x .@? "ContinentName")

instance Hashable GeoLocationDetails

instance NFData GeoLocationDetails
