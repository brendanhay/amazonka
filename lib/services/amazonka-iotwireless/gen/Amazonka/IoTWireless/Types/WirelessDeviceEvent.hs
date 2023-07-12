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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceEvent
  ( WirelessDeviceEvent
      ( ..,
        WirelessDeviceEvent_Downlink_Data,
        WirelessDeviceEvent_Join,
        WirelessDeviceEvent_Registration,
        WirelessDeviceEvent_Rejoin,
        WirelessDeviceEvent_Uplink_Data
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The event for a log message, if the log message is tied to a wireless
-- device.
newtype WirelessDeviceEvent = WirelessDeviceEvent'
  { fromWirelessDeviceEvent ::
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

pattern WirelessDeviceEvent_Downlink_Data :: WirelessDeviceEvent
pattern WirelessDeviceEvent_Downlink_Data = WirelessDeviceEvent' "Downlink_Data"

pattern WirelessDeviceEvent_Join :: WirelessDeviceEvent
pattern WirelessDeviceEvent_Join = WirelessDeviceEvent' "Join"

pattern WirelessDeviceEvent_Registration :: WirelessDeviceEvent
pattern WirelessDeviceEvent_Registration = WirelessDeviceEvent' "Registration"

pattern WirelessDeviceEvent_Rejoin :: WirelessDeviceEvent
pattern WirelessDeviceEvent_Rejoin = WirelessDeviceEvent' "Rejoin"

pattern WirelessDeviceEvent_Uplink_Data :: WirelessDeviceEvent
pattern WirelessDeviceEvent_Uplink_Data = WirelessDeviceEvent' "Uplink_Data"

{-# COMPLETE
  WirelessDeviceEvent_Downlink_Data,
  WirelessDeviceEvent_Join,
  WirelessDeviceEvent_Registration,
  WirelessDeviceEvent_Rejoin,
  WirelessDeviceEvent_Uplink_Data,
  WirelessDeviceEvent'
  #-}
