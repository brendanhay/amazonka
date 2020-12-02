{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ConnectionState

-- | Describes the connection status of a WorkSpace.
--
--
--
-- /See:/ 'workspaceConnectionStatus' smart constructor.
data WorkspaceConnectionStatus = WorkspaceConnectionStatus'
  { _wcsLastKnownUserConnectionTimestamp ::
      !(Maybe POSIX),
    _wcsConnectionStateCheckTimestamp ::
      !(Maybe POSIX),
    _wcsWorkspaceId :: !(Maybe Text),
    _wcsConnectionState ::
      !(Maybe ConnectionState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcsLastKnownUserConnectionTimestamp' - The timestamp of the last known user connection.
--
-- * 'wcsConnectionStateCheckTimestamp' - The timestamp of the connection status check.
--
-- * 'wcsWorkspaceId' - The identifier of the WorkSpace.
--
-- * 'wcsConnectionState' - The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
workspaceConnectionStatus ::
  WorkspaceConnectionStatus
workspaceConnectionStatus =
  WorkspaceConnectionStatus'
    { _wcsLastKnownUserConnectionTimestamp =
        Nothing,
      _wcsConnectionStateCheckTimestamp = Nothing,
      _wcsWorkspaceId = Nothing,
      _wcsConnectionState = Nothing
    }

-- | The timestamp of the last known user connection.
wcsLastKnownUserConnectionTimestamp :: Lens' WorkspaceConnectionStatus (Maybe UTCTime)
wcsLastKnownUserConnectionTimestamp = lens _wcsLastKnownUserConnectionTimestamp (\s a -> s {_wcsLastKnownUserConnectionTimestamp = a}) . mapping _Time

-- | The timestamp of the connection status check.
wcsConnectionStateCheckTimestamp :: Lens' WorkspaceConnectionStatus (Maybe UTCTime)
wcsConnectionStateCheckTimestamp = lens _wcsConnectionStateCheckTimestamp (\s a -> s {_wcsConnectionStateCheckTimestamp = a}) . mapping _Time

-- | The identifier of the WorkSpace.
wcsWorkspaceId :: Lens' WorkspaceConnectionStatus (Maybe Text)
wcsWorkspaceId = lens _wcsWorkspaceId (\s a -> s {_wcsWorkspaceId = a})

-- | The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
wcsConnectionState :: Lens' WorkspaceConnectionStatus (Maybe ConnectionState)
wcsConnectionState = lens _wcsConnectionState (\s a -> s {_wcsConnectionState = a})

instance FromJSON WorkspaceConnectionStatus where
  parseJSON =
    withObject
      "WorkspaceConnectionStatus"
      ( \x ->
          WorkspaceConnectionStatus'
            <$> (x .:? "LastKnownUserConnectionTimestamp")
            <*> (x .:? "ConnectionStateCheckTimestamp")
            <*> (x .:? "WorkspaceId")
            <*> (x .:? "ConnectionState")
      )

instance Hashable WorkspaceConnectionStatus

instance NFData WorkspaceConnectionStatus
