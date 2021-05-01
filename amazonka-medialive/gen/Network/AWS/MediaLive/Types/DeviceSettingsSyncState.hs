{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DeviceSettingsSyncState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DeviceSettingsSyncState
  ( DeviceSettingsSyncState
      ( ..,
        DeviceSettingsSyncState_SYNCED,
        DeviceSettingsSyncState_SYNCING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
newtype DeviceSettingsSyncState = DeviceSettingsSyncState'
  { fromDeviceSettingsSyncState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DeviceSettingsSyncState_SYNCED :: DeviceSettingsSyncState
pattern DeviceSettingsSyncState_SYNCED = DeviceSettingsSyncState' "SYNCED"

pattern DeviceSettingsSyncState_SYNCING :: DeviceSettingsSyncState
pattern DeviceSettingsSyncState_SYNCING = DeviceSettingsSyncState' "SYNCING"

{-# COMPLETE
  DeviceSettingsSyncState_SYNCED,
  DeviceSettingsSyncState_SYNCING,
  DeviceSettingsSyncState'
  #-}
