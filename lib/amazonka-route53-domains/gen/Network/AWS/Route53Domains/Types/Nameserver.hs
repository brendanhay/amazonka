{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Nameserver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Nameserver where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Nameserver includes the following elements.
--
--
--
-- /See:/ 'nameserver' smart constructor.
data Nameserver = Nameserver'
  { _nGlueIPs :: !(Maybe [Text]),
    _nName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Nameserver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nGlueIPs' - Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com. Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- * 'nName' - The fully qualified host name of the name server. Constraint: Maximum 255 characters
nameserver ::
  -- | 'nName'
  Text ->
  Nameserver
nameserver pName_ =
  Nameserver' {_nGlueIPs = Nothing, _nName = pName_}

-- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com. Constraints: The list can contain only one IPv4 and one IPv6 address.
nGlueIPs :: Lens' Nameserver [Text]
nGlueIPs = lens _nGlueIPs (\s a -> s {_nGlueIPs = a}) . _Default . _Coerce

-- | The fully qualified host name of the name server. Constraint: Maximum 255 characters
nName :: Lens' Nameserver Text
nName = lens _nName (\s a -> s {_nName = a})

instance FromJSON Nameserver where
  parseJSON =
    withObject
      "Nameserver"
      ( \x ->
          Nameserver' <$> (x .:? "GlueIps" .!= mempty) <*> (x .: "Name")
      )

instance Hashable Nameserver

instance NFData Nameserver

instance ToJSON Nameserver where
  toJSON Nameserver' {..} =
    object
      (catMaybes [("GlueIps" .=) <$> _nGlueIPs, Just ("Name" .= _nName)])
