{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringCacheKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryStringCacheKeys where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
--
-- If you want to include query strings in the cache key, use @QueryStringsConfig@ in a cache policy. See @CachePolicy@ .
--
-- If you want to send query strings to the origin but not include them in the cache key, use @QueryStringsConfig@ in an origin request policy. See @OriginRequestPolicy@ .
--
-- A complex type that contains information about the query string parameters that you want CloudFront to use for caching for a cache behavior.
--
--
-- /See:/ 'queryStringCacheKeys' smart constructor.
data QueryStringCacheKeys = QueryStringCacheKeys'
  { _qsckItems ::
      !(Maybe [Text]),
    _qsckQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryStringCacheKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsckItems' - A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ .
--
-- * 'qsckQuantity' - The number of @whitelisted@ query string parameters for a cache behavior.
queryStringCacheKeys ::
  -- | 'qsckQuantity'
  Int ->
  QueryStringCacheKeys
queryStringCacheKeys pQuantity_ =
  QueryStringCacheKeys'
    { _qsckItems = Nothing,
      _qsckQuantity = pQuantity_
    }

-- | A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ .
qsckItems :: Lens' QueryStringCacheKeys [Text]
qsckItems = lens _qsckItems (\s a -> s {_qsckItems = a}) . _Default . _Coerce

-- | The number of @whitelisted@ query string parameters for a cache behavior.
qsckQuantity :: Lens' QueryStringCacheKeys Int
qsckQuantity = lens _qsckQuantity (\s a -> s {_qsckQuantity = a})

instance FromXML QueryStringCacheKeys where
  parseXML x =
    QueryStringCacheKeys'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Name"))
      <*> (x .@ "Quantity")

instance Hashable QueryStringCacheKeys

instance NFData QueryStringCacheKeys

instance ToXML QueryStringCacheKeys where
  toXML QueryStringCacheKeys' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Name" <$> _qsckItems),
        "Quantity" @= _qsckQuantity
      ]
