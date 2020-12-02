{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginSSLProtocols
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginSSLProtocols where

import Network.AWS.CloudFront.Types.SSLProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin.
--
--
--
-- /See:/ 'originSSLProtocols' smart constructor.
data OriginSSLProtocols = OriginSSLProtocols'
  { _ospQuantity :: !Int,
    _ospItems :: ![SSLProtocol]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginSSLProtocols' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ospQuantity' - The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
--
-- * 'ospItems' - A list that contains allowed SSL/TLS protocols for this distribution.
originSSLProtocols ::
  -- | 'ospQuantity'
  Int ->
  OriginSSLProtocols
originSSLProtocols pQuantity_ =
  OriginSSLProtocols'
    { _ospQuantity = pQuantity_,
      _ospItems = mempty
    }

-- | The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
ospQuantity :: Lens' OriginSSLProtocols Int
ospQuantity = lens _ospQuantity (\s a -> s {_ospQuantity = a})

-- | A list that contains allowed SSL/TLS protocols for this distribution.
ospItems :: Lens' OriginSSLProtocols [SSLProtocol]
ospItems = lens _ospItems (\s a -> s {_ospItems = a}) . _Coerce

instance FromXML OriginSSLProtocols where
  parseXML x =
    OriginSSLProtocols'
      <$> (x .@ "Quantity")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList "SslProtocol")

instance Hashable OriginSSLProtocols

instance NFData OriginSSLProtocols

instance ToXML OriginSSLProtocols where
  toXML OriginSSLProtocols' {..} =
    mconcat
      [ "Quantity" @= _ospQuantity,
        "Items" @= toXMLList "SslProtocol" _ospItems
      ]
