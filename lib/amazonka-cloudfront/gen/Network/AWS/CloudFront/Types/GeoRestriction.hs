{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestriction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.GeoRestriction where

import Network.AWS.CloudFront.Types.GeoRestrictionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
--
--
--
-- /See:/ 'geoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
  { _grItems :: !(Maybe [Text]),
    _grRestrictionType :: !GeoRestrictionType,
    _grQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoRestriction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grItems' - A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ). The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country. CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
--
-- * 'grRestrictionType' - The method that you want to use to restrict distribution of your content by country:     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
--
-- * 'grQuantity' - When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
geoRestriction ::
  -- | 'grRestrictionType'
  GeoRestrictionType ->
  -- | 'grQuantity'
  Int ->
  GeoRestriction
geoRestriction pRestrictionType_ pQuantity_ =
  GeoRestriction'
    { _grItems = Nothing,
      _grRestrictionType = pRestrictionType_,
      _grQuantity = pQuantity_
    }

-- | A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ). The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country. CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
grItems :: Lens' GeoRestriction [Text]
grItems = lens _grItems (\s a -> s {_grItems = a}) . _Default . _Coerce

-- | The method that you want to use to restrict distribution of your content by country:     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
grRestrictionType :: Lens' GeoRestriction GeoRestrictionType
grRestrictionType = lens _grRestrictionType (\s a -> s {_grRestrictionType = a})

-- | When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
grQuantity :: Lens' GeoRestriction Int
grQuantity = lens _grQuantity (\s a -> s {_grQuantity = a})

instance FromXML GeoRestriction where
  parseXML x =
    GeoRestriction'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Location"))
      <*> (x .@ "RestrictionType")
      <*> (x .@ "Quantity")

instance Hashable GeoRestriction

instance NFData GeoRestriction

instance ToXML GeoRestriction where
  toXML GeoRestriction' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Location" <$> _grItems),
        "RestrictionType" @= _grRestrictionType,
        "Quantity" @= _grQuantity
      ]
