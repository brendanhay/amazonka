{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information for the 'ConnectDirectory' operation when an AD Connector directory is being created.
--
--
--
-- /See:/ 'directoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { _dcsVPCId ::
      !Text,
    _dcsSubnetIds :: ![Text],
    _dcsCustomerDNSIPs :: ![Text],
    _dcsCustomerUserName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryConnectSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsVPCId' - The identifier of the VPC in which the AD Connector is created.
--
-- * 'dcsSubnetIds' - A list of subnet identifiers in the VPC in which the AD Connector is created.
--
-- * 'dcsCustomerDNSIPs' - A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
--
-- * 'dcsCustomerUserName' - The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:     * Read users and groups     * Create computer objects     * Join computers to the domain
directoryConnectSettings ::
  -- | 'dcsVPCId'
  Text ->
  -- | 'dcsCustomerUserName'
  Text ->
  DirectoryConnectSettings
directoryConnectSettings pVPCId_ pCustomerUserName_ =
  DirectoryConnectSettings'
    { _dcsVPCId = pVPCId_,
      _dcsSubnetIds = mempty,
      _dcsCustomerDNSIPs = mempty,
      _dcsCustomerUserName = pCustomerUserName_
    }

-- | The identifier of the VPC in which the AD Connector is created.
dcsVPCId :: Lens' DirectoryConnectSettings Text
dcsVPCId = lens _dcsVPCId (\s a -> s {_dcsVPCId = a})

-- | A list of subnet identifiers in the VPC in which the AD Connector is created.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\s a -> s {_dcsSubnetIds = a}) . _Coerce

-- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\s a -> s {_dcsCustomerDNSIPs = a}) . _Coerce

-- | The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:     * Read users and groups     * Create computer objects     * Join computers to the domain
dcsCustomerUserName :: Lens' DirectoryConnectSettings Text
dcsCustomerUserName = lens _dcsCustomerUserName (\s a -> s {_dcsCustomerUserName = a})

instance Hashable DirectoryConnectSettings

instance NFData DirectoryConnectSettings

instance ToJSON DirectoryConnectSettings where
  toJSON DirectoryConnectSettings' {..} =
    object
      ( catMaybes
          [ Just ("VpcId" .= _dcsVPCId),
            Just ("SubnetIds" .= _dcsSubnetIds),
            Just ("CustomerDnsIps" .= _dcsCustomerDNSIPs),
            Just ("CustomerUserName" .= _dcsCustomerUserName)
          ]
      )
