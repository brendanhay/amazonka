{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachedMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachedMethods where

import Network.AWS.CloudFront.Types.Method
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:
--
--
--     * CloudFront caches responses to @GET@ and @HEAD@ requests.
--
--     * CloudFront caches responses to @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--
--
-- If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly.
--
--
-- /See:/ 'cachedMethods' smart constructor.
data CachedMethods = CachedMethods'
  { _cmQuantity :: !Int,
    _cmItems :: ![Method]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmQuantity' - The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
--
-- * 'cmItems' - A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
cachedMethods ::
  -- | 'cmQuantity'
  Int ->
  CachedMethods
cachedMethods pQuantity_ =
  CachedMethods' {_cmQuantity = pQuantity_, _cmItems = mempty}

-- | The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
cmQuantity :: Lens' CachedMethods Int
cmQuantity = lens _cmQuantity (\s a -> s {_cmQuantity = a})

-- | A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
cmItems :: Lens' CachedMethods [Method]
cmItems = lens _cmItems (\s a -> s {_cmItems = a}) . _Coerce

instance FromXML CachedMethods where
  parseXML x =
    CachedMethods'
      <$> (x .@ "Quantity")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance Hashable CachedMethods

instance NFData CachedMethods

instance ToXML CachedMethods where
  toXML CachedMethods' {..} =
    mconcat
      ["Quantity" @= _cmQuantity, "Items" @= toXMLList "Method" _cmItems]
