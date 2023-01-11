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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceFrameInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceFrameInfo
  ( WirelessDeviceFrameInfo
      ( ..,
        WirelessDeviceFrameInfo_DISABLED,
        WirelessDeviceFrameInfo_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | @FrameInfo@ of your wireless device resources for the trace content. Use
-- FrameInfo to debug the communication between your LoRaWAN end devices
-- and the network server.
newtype WirelessDeviceFrameInfo = WirelessDeviceFrameInfo'
  { fromWirelessDeviceFrameInfo ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern WirelessDeviceFrameInfo_DISABLED :: WirelessDeviceFrameInfo
pattern WirelessDeviceFrameInfo_DISABLED = WirelessDeviceFrameInfo' "DISABLED"

pattern WirelessDeviceFrameInfo_ENABLED :: WirelessDeviceFrameInfo
pattern WirelessDeviceFrameInfo_ENABLED = WirelessDeviceFrameInfo' "ENABLED"

{-# COMPLETE
  WirelessDeviceFrameInfo_DISABLED,
  WirelessDeviceFrameInfo_ENABLED,
  WirelessDeviceFrameInfo'
  #-}
