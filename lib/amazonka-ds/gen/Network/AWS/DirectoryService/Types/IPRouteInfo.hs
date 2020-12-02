{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRouteInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRouteInfo where

import Network.AWS.DirectoryService.Types.IPRouteStatusMsg
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about one or more IP address blocks.
--
--
--
-- /See:/ 'ipRouteInfo' smart constructor.
data IPRouteInfo = IPRouteInfo'
  { _iriDirectoryId :: !(Maybe Text),
    _iriIPRouteStatusReason :: !(Maybe Text),
    _iriAddedDateTime :: !(Maybe POSIX),
    _iriCidrIP :: !(Maybe Text),
    _iriIPRouteStatusMsg :: !(Maybe IPRouteStatusMsg),
    _iriDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRouteInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriDirectoryId' - Identifier (ID) of the directory associated with the IP addresses.
--
-- * 'iriIPRouteStatusReason' - The reason for the IpRouteStatusMsg.
--
-- * 'iriAddedDateTime' - The date and time the address block was added to the directory.
--
-- * 'iriCidrIP' - IP address block in the 'IpRoute' .
--
-- * 'iriIPRouteStatusMsg' - The status of the IP address block.
--
-- * 'iriDescription' - Description of the 'IpRouteInfo' .
ipRouteInfo ::
  IPRouteInfo
ipRouteInfo =
  IPRouteInfo'
    { _iriDirectoryId = Nothing,
      _iriIPRouteStatusReason = Nothing,
      _iriAddedDateTime = Nothing,
      _iriCidrIP = Nothing,
      _iriIPRouteStatusMsg = Nothing,
      _iriDescription = Nothing
    }

-- | Identifier (ID) of the directory associated with the IP addresses.
iriDirectoryId :: Lens' IPRouteInfo (Maybe Text)
iriDirectoryId = lens _iriDirectoryId (\s a -> s {_iriDirectoryId = a})

-- | The reason for the IpRouteStatusMsg.
iriIPRouteStatusReason :: Lens' IPRouteInfo (Maybe Text)
iriIPRouteStatusReason = lens _iriIPRouteStatusReason (\s a -> s {_iriIPRouteStatusReason = a})

-- | The date and time the address block was added to the directory.
iriAddedDateTime :: Lens' IPRouteInfo (Maybe UTCTime)
iriAddedDateTime = lens _iriAddedDateTime (\s a -> s {_iriAddedDateTime = a}) . mapping _Time

-- | IP address block in the 'IpRoute' .
iriCidrIP :: Lens' IPRouteInfo (Maybe Text)
iriCidrIP = lens _iriCidrIP (\s a -> s {_iriCidrIP = a})

-- | The status of the IP address block.
iriIPRouteStatusMsg :: Lens' IPRouteInfo (Maybe IPRouteStatusMsg)
iriIPRouteStatusMsg = lens _iriIPRouteStatusMsg (\s a -> s {_iriIPRouteStatusMsg = a})

-- | Description of the 'IpRouteInfo' .
iriDescription :: Lens' IPRouteInfo (Maybe Text)
iriDescription = lens _iriDescription (\s a -> s {_iriDescription = a})

instance FromJSON IPRouteInfo where
  parseJSON =
    withObject
      "IPRouteInfo"
      ( \x ->
          IPRouteInfo'
            <$> (x .:? "DirectoryId")
            <*> (x .:? "IpRouteStatusReason")
            <*> (x .:? "AddedDateTime")
            <*> (x .:? "CidrIp")
            <*> (x .:? "IpRouteStatusMsg")
            <*> (x .:? "Description")
      )

instance Hashable IPRouteInfo

instance NFData IPRouteInfo
