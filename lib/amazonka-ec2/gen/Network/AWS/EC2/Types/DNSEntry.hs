{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSEntry where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a DNS entry.
--
--
--
-- /See:/ 'dnsEntry' smart constructor.
data DNSEntry = DNSEntry'
  { _deHostedZoneId :: !(Maybe Text),
    _deDNSName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deHostedZoneId' - The ID of the private hosted zone.
--
-- * 'deDNSName' - The DNS name.
dnsEntry ::
  DNSEntry
dnsEntry =
  DNSEntry' {_deHostedZoneId = Nothing, _deDNSName = Nothing}

-- | The ID of the private hosted zone.
deHostedZoneId :: Lens' DNSEntry (Maybe Text)
deHostedZoneId = lens _deHostedZoneId (\s a -> s {_deHostedZoneId = a})

-- | The DNS name.
deDNSName :: Lens' DNSEntry (Maybe Text)
deDNSName = lens _deDNSName (\s a -> s {_deDNSName = a})

instance FromXML DNSEntry where
  parseXML x =
    DNSEntry' <$> (x .@? "hostedZoneId") <*> (x .@? "dnsName")

instance Hashable DNSEntry

instance NFData DNSEntry
