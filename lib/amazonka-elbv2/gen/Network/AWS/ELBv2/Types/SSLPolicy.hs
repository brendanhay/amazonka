{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SSLPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SSLPolicy where

import Network.AWS.ELBv2.Types.Cipher
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy used for SSL negotiation.
--
--
--
-- /See:/ 'sslPolicy' smart constructor.
data SSLPolicy = SSLPolicy'
  { _spCiphers :: !(Maybe [Cipher]),
    _spName :: !(Maybe Text),
    _spSSLProtocols :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSLPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spCiphers' - The ciphers.
--
-- * 'spName' - The name of the policy.
--
-- * 'spSSLProtocols' - The protocols.
sslPolicy ::
  SSLPolicy
sslPolicy =
  SSLPolicy'
    { _spCiphers = Nothing,
      _spName = Nothing,
      _spSSLProtocols = Nothing
    }

-- | The ciphers.
spCiphers :: Lens' SSLPolicy [Cipher]
spCiphers = lens _spCiphers (\s a -> s {_spCiphers = a}) . _Default . _Coerce

-- | The name of the policy.
spName :: Lens' SSLPolicy (Maybe Text)
spName = lens _spName (\s a -> s {_spName = a})

-- | The protocols.
spSSLProtocols :: Lens' SSLPolicy [Text]
spSSLProtocols = lens _spSSLProtocols (\s a -> s {_spSSLProtocols = a}) . _Default . _Coerce

instance FromXML SSLPolicy where
  parseXML x =
    SSLPolicy'
      <$> (x .@? "Ciphers" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Name")
      <*> (x .@? "SslProtocols" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable SSLPolicy

instance NFData SSLPolicy
