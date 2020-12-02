{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomErrorResponses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomErrorResponses where

import Network.AWS.CloudFront.Types.CustomErrorResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'customErrorResponses' smart constructor.
data CustomErrorResponses = CustomErrorResponses'
  { _cerItems ::
      !(Maybe [CustomErrorResponse]),
    _cerQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomErrorResponses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerItems' - A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- * 'cerQuantity' - The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
customErrorResponses ::
  -- | 'cerQuantity'
  Int ->
  CustomErrorResponses
customErrorResponses pQuantity_ =
  CustomErrorResponses'
    { _cerItems = Nothing,
      _cerQuantity = pQuantity_
    }

-- | A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
cerItems :: Lens' CustomErrorResponses [CustomErrorResponse]
cerItems = lens _cerItems (\s a -> s {_cerItems = a}) . _Default . _Coerce

-- | The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
cerQuantity :: Lens' CustomErrorResponses Int
cerQuantity = lens _cerQuantity (\s a -> s {_cerQuantity = a})

instance FromXML CustomErrorResponses where
  parseXML x =
    CustomErrorResponses'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "CustomErrorResponse")
          )
      <*> (x .@ "Quantity")

instance Hashable CustomErrorResponses

instance NFData CustomErrorResponses

instance ToXML CustomErrorResponses where
  toXML CustomErrorResponses' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "CustomErrorResponse" <$> _cerItems),
        "Quantity" @= _cerQuantity
      ]
