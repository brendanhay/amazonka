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
-- Module      : Amazonka.IoTWireless.Types.DeviceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DeviceState
  ( DeviceState
      ( ..,
        DeviceState_Provisioned,
        DeviceState_RegisteredNotSeen,
        DeviceState_RegisteredReachable,
        DeviceState_RegisteredUnreachable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Device state defines the device status of sidewalk device.
newtype DeviceState = DeviceState'
  { fromDeviceState ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern DeviceState_Provisioned :: DeviceState
pattern DeviceState_Provisioned = DeviceState' "Provisioned"

pattern DeviceState_RegisteredNotSeen :: DeviceState
pattern DeviceState_RegisteredNotSeen = DeviceState' "RegisteredNotSeen"

pattern DeviceState_RegisteredReachable :: DeviceState
pattern DeviceState_RegisteredReachable = DeviceState' "RegisteredReachable"

pattern DeviceState_RegisteredUnreachable :: DeviceState
pattern DeviceState_RegisteredUnreachable = DeviceState' "RegisteredUnreachable"

{-# COMPLETE
  DeviceState_Provisioned,
  DeviceState_RegisteredNotSeen,
  DeviceState_RegisteredReachable,
  DeviceState_RegisteredUnreachable,
  DeviceState'
  #-}
