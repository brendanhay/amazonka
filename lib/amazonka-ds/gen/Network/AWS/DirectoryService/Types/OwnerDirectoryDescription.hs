{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.OwnerDirectoryDescription where

import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the directory owner account details that have been shared to the directory consumer account.
--
--
--
-- /See:/ 'ownerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { _oddRadiusStatus ::
      !(Maybe RadiusStatus),
    _oddDirectoryId :: !(Maybe Text),
    _oddRadiusSettings ::
      !(Maybe RadiusSettings),
    _oddAccountId :: !(Maybe Text),
    _oddDNSIPAddrs :: !(Maybe [Text]),
    _oddVPCSettings ::
      !( Maybe
           DirectoryVPCSettingsDescription
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'OwnerDirectoryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oddRadiusStatus' - Information about the status of the RADIUS server.
--
-- * 'oddDirectoryId' - Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
--
-- * 'oddRadiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- * 'oddAccountId' - Identifier of the directory owner account.
--
-- * 'oddDNSIPAddrs' - IP address of the directory’s domain controllers.
--
-- * 'oddVPCSettings' - Information about the VPC settings for the directory.
ownerDirectoryDescription ::
  OwnerDirectoryDescription
ownerDirectoryDescription =
  OwnerDirectoryDescription'
    { _oddRadiusStatus = Nothing,
      _oddDirectoryId = Nothing,
      _oddRadiusSettings = Nothing,
      _oddAccountId = Nothing,
      _oddDNSIPAddrs = Nothing,
      _oddVPCSettings = Nothing
    }

-- | Information about the status of the RADIUS server.
oddRadiusStatus :: Lens' OwnerDirectoryDescription (Maybe RadiusStatus)
oddRadiusStatus = lens _oddRadiusStatus (\s a -> s {_oddRadiusStatus = a})

-- | Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
oddDirectoryId :: Lens' OwnerDirectoryDescription (Maybe Text)
oddDirectoryId = lens _oddDirectoryId (\s a -> s {_oddDirectoryId = a})

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
oddRadiusSettings :: Lens' OwnerDirectoryDescription (Maybe RadiusSettings)
oddRadiusSettings = lens _oddRadiusSettings (\s a -> s {_oddRadiusSettings = a})

-- | Identifier of the directory owner account.
oddAccountId :: Lens' OwnerDirectoryDescription (Maybe Text)
oddAccountId = lens _oddAccountId (\s a -> s {_oddAccountId = a})

-- | IP address of the directory’s domain controllers.
oddDNSIPAddrs :: Lens' OwnerDirectoryDescription [Text]
oddDNSIPAddrs = lens _oddDNSIPAddrs (\s a -> s {_oddDNSIPAddrs = a}) . _Default . _Coerce

-- | Information about the VPC settings for the directory.
oddVPCSettings :: Lens' OwnerDirectoryDescription (Maybe DirectoryVPCSettingsDescription)
oddVPCSettings = lens _oddVPCSettings (\s a -> s {_oddVPCSettings = a})

instance FromJSON OwnerDirectoryDescription where
  parseJSON =
    withObject
      "OwnerDirectoryDescription"
      ( \x ->
          OwnerDirectoryDescription'
            <$> (x .:? "RadiusStatus")
            <*> (x .:? "DirectoryId")
            <*> (x .:? "RadiusSettings")
            <*> (x .:? "AccountId")
            <*> (x .:? "DnsIpAddrs" .!= mempty)
            <*> (x .:? "VpcSettings")
      )

instance Hashable OwnerDirectoryDescription

instance NFData OwnerDirectoryDescription
