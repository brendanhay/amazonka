{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSConfigChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSConfigChange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSRecord

-- | A complex type that contains information about changes to the Route 53 DNS records that AWS Cloud Map creates when you register an instance.
--
--
--
-- /See:/ 'dnsConfigChange' smart constructor.
newtype DNSConfigChange = DNSConfigChange'
  { _dccDNSRecords ::
      [DNSRecord]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSConfigChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccDNSRecords' - An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
dnsConfigChange ::
  DNSConfigChange
dnsConfigChange = DNSConfigChange' {_dccDNSRecords = mempty}

-- | An array that contains one @DnsRecord@ object for each Route 53 record that you want AWS Cloud Map to create when you register an instance.
dccDNSRecords :: Lens' DNSConfigChange [DNSRecord]
dccDNSRecords = lens _dccDNSRecords (\s a -> s {_dccDNSRecords = a}) . _Coerce

instance Hashable DNSConfigChange

instance NFData DNSConfigChange

instance ToJSON DNSConfigChange where
  toJSON DNSConfigChange' {..} =
    object (catMaybes [Just ("DnsRecords" .= _dccDNSRecords)])
