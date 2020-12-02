{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ConditionalForwarder where

import Network.AWS.DirectoryService.Types.ReplicationScope
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
--
--
-- /See:/ 'conditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { _cfDNSIPAddrs ::
      !(Maybe [Text]),
    _cfRemoteDomainName :: !(Maybe Text),
    _cfReplicationScope :: !(Maybe ReplicationScope)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDNSIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
--
-- * 'cfRemoteDomainName' - The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
--
-- * 'cfReplicationScope' - The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
conditionalForwarder ::
  ConditionalForwarder
conditionalForwarder =
  ConditionalForwarder'
    { _cfDNSIPAddrs = Nothing,
      _cfRemoteDomainName = Nothing,
      _cfReplicationScope = Nothing
    }

-- | The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
cfDNSIPAddrs :: Lens' ConditionalForwarder [Text]
cfDNSIPAddrs = lens _cfDNSIPAddrs (\s a -> s {_cfDNSIPAddrs = a}) . _Default . _Coerce

-- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
cfRemoteDomainName :: Lens' ConditionalForwarder (Maybe Text)
cfRemoteDomainName = lens _cfRemoteDomainName (\s a -> s {_cfRemoteDomainName = a})

-- | The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
cfReplicationScope :: Lens' ConditionalForwarder (Maybe ReplicationScope)
cfReplicationScope = lens _cfReplicationScope (\s a -> s {_cfReplicationScope = a})

instance FromJSON ConditionalForwarder where
  parseJSON =
    withObject
      "ConditionalForwarder"
      ( \x ->
          ConditionalForwarder'
            <$> (x .:? "DnsIpAddrs" .!= mempty)
            <*> (x .:? "RemoteDomainName")
            <*> (x .:? "ReplicationScope")
      )

instance Hashable ConditionalForwarder

instance NFData ConditionalForwarder
