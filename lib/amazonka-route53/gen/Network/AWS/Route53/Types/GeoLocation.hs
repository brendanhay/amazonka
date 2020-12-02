{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.GeoLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.GeoLocation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a geographic location.
--
--
--
-- /See:/ 'geoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { _glSubdivisionCode ::
      !(Maybe Text),
    _glCountryCode :: !(Maybe Text),
    _glContinentCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSubdivisionCode' - For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.  If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
--
-- * 'glCountryCode' - For geolocation resource record sets, the two-letter code for a country. Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- * 'glContinentCode' - The two-letter code for the continent. Amazon Route 53 supports the following continent codes:     * __AF__ : Africa     * __AN__ : Antarctica     * __AS__ : Asia     * __EU__ : Europe     * __OC__ : Oceania     * __NA__ : North America     * __SA__ : South America Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
geoLocation ::
  GeoLocation
geoLocation =
  GeoLocation'
    { _glSubdivisionCode = Nothing,
      _glCountryCode = Nothing,
      _glContinentCode = Nothing
    }

-- | For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.  If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode = lens _glSubdivisionCode (\s a -> s {_glSubdivisionCode = a})

-- | For geolocation resource record sets, the two-letter code for a country. Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\s a -> s {_glCountryCode = a})

-- | The two-letter code for the continent. Amazon Route 53 supports the following continent codes:     * __AF__ : Africa     * __AN__ : Antarctica     * __AS__ : Asia     * __EU__ : Europe     * __OC__ : Oceania     * __NA__ : North America     * __SA__ : South America Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\s a -> s {_glContinentCode = a})

instance FromXML GeoLocation where
  parseXML x =
    GeoLocation'
      <$> (x .@? "SubdivisionCode")
      <*> (x .@? "CountryCode")
      <*> (x .@? "ContinentCode")

instance Hashable GeoLocation

instance NFData GeoLocation

instance ToXML GeoLocation where
  toXML GeoLocation' {..} =
    mconcat
      [ "SubdivisionCode" @= _glSubdivisionCode,
        "CountryCode" @= _glCountryCode,
        "ContinentCode" @= _glContinentCode
      ]
