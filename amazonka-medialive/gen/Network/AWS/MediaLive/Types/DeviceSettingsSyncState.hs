{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import qualified Network.AWS.Core as Core

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
newtype DeviceSettingsSyncState = DeviceSettingsSyncState'
  { fromDeviceSettingsSyncState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
