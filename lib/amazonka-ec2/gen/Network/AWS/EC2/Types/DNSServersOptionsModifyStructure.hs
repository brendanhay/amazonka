{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSServersOptionsModifyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSServersOptionsModifyStructure where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the DNS server to be used.
--
--
--
-- /See:/ 'dnsServersOptionsModifyStructure' smart constructor.
data DNSServersOptionsModifyStructure = DNSServersOptionsModifyStructure'
  { _dsomsEnabled ::
      !(Maybe Bool),
    _dsomsCustomDNSServers ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSServersOptionsModifyStructure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsomsEnabled' - Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
--
-- * 'dsomsCustomDNSServers' - The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
dnsServersOptionsModifyStructure ::
  DNSServersOptionsModifyStructure
dnsServersOptionsModifyStructure =
  DNSServersOptionsModifyStructure'
    { _dsomsEnabled = Nothing,
      _dsomsCustomDNSServers = Nothing
    }

-- | Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
dsomsEnabled :: Lens' DNSServersOptionsModifyStructure (Maybe Bool)
dsomsEnabled = lens _dsomsEnabled (\s a -> s {_dsomsEnabled = a})

-- | The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
dsomsCustomDNSServers :: Lens' DNSServersOptionsModifyStructure [Text]
dsomsCustomDNSServers = lens _dsomsCustomDNSServers (\s a -> s {_dsomsCustomDNSServers = a}) . _Default . _Coerce

instance Hashable DNSServersOptionsModifyStructure

instance NFData DNSServersOptionsModifyStructure

instance ToQuery DNSServersOptionsModifyStructure where
  toQuery DNSServersOptionsModifyStructure' {..} =
    mconcat
      [ "Enabled" =: _dsomsEnabled,
        toQuery
          (toQueryList "CustomDnsServers" <$> _dsomsCustomDNSServers)
      ]
