{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.AllowedMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.AllowedMethods where

import Network.AWS.CloudFront.Types.CachedMethods
import Network.AWS.CloudFront.Types.Method
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:
--
--
--     * CloudFront forwards only @GET@ and @HEAD@ requests.
--
--     * CloudFront forwards only @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--     * CloudFront forwards @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests.
--
--
--
-- If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.
--
--
-- /See:/ 'allowedMethods' smart constructor.
data AllowedMethods = AllowedMethods'
  { _amCachedMethods ::
      !(Maybe CachedMethods),
    _amQuantity :: !Int,
    _amItems :: ![Method]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllowedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amCachedMethods' - Undocumented member.
--
-- * 'amQuantity' - The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
--
-- * 'amItems' - A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
allowedMethods ::
  -- | 'amQuantity'
  Int ->
  AllowedMethods
allowedMethods pQuantity_ =
  AllowedMethods'
    { _amCachedMethods = Nothing,
      _amQuantity = pQuantity_,
      _amItems = mempty
    }

-- | Undocumented member.
amCachedMethods :: Lens' AllowedMethods (Maybe CachedMethods)
amCachedMethods = lens _amCachedMethods (\s a -> s {_amCachedMethods = a})

-- | The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
amQuantity :: Lens' AllowedMethods Int
amQuantity = lens _amQuantity (\s a -> s {_amQuantity = a})

-- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
amItems :: Lens' AllowedMethods [Method]
amItems = lens _amItems (\s a -> s {_amItems = a}) . _Coerce

instance FromXML AllowedMethods where
  parseXML x =
    AllowedMethods'
      <$> (x .@? "CachedMethods")
      <*> (x .@ "Quantity")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance Hashable AllowedMethods

instance NFData AllowedMethods

instance ToXML AllowedMethods where
  toXML AllowedMethods' {..} =
    mconcat
      [ "CachedMethods" @= _amCachedMethods,
        "Quantity" @= _amQuantity,
        "Items" @= toXMLList "Method" _amItems
      ]
