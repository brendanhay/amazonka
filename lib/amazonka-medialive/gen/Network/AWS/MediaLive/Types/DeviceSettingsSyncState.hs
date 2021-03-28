{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DeviceSettingsSyncState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DeviceSettingsSyncState
  ( DeviceSettingsSyncState
    ( DeviceSettingsSyncState'
    , DeviceSettingsSyncStateSynced
    , DeviceSettingsSyncStateSyncing
    , fromDeviceSettingsSyncState
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
newtype DeviceSettingsSyncState = DeviceSettingsSyncState'{fromDeviceSettingsSyncState
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern DeviceSettingsSyncStateSynced :: DeviceSettingsSyncState
pattern DeviceSettingsSyncStateSynced = DeviceSettingsSyncState' "SYNCED"

pattern DeviceSettingsSyncStateSyncing :: DeviceSettingsSyncState
pattern DeviceSettingsSyncStateSyncing = DeviceSettingsSyncState' "SYNCING"

{-# COMPLETE 
  DeviceSettingsSyncStateSynced,

  DeviceSettingsSyncStateSyncing,
  DeviceSettingsSyncState'
  #-}
