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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceIdType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceIdType
  ( WirelessDeviceIdType
      ( ..,
        WirelessDeviceIdType_DevEui,
        WirelessDeviceIdType_SidewalkManufacturingSn,
        WirelessDeviceIdType_ThingName,
        WirelessDeviceIdType_WirelessDeviceId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WirelessDeviceIdType = WirelessDeviceIdType'
  { fromWirelessDeviceIdType ::
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

pattern WirelessDeviceIdType_DevEui :: WirelessDeviceIdType
pattern WirelessDeviceIdType_DevEui = WirelessDeviceIdType' "DevEui"

pattern WirelessDeviceIdType_SidewalkManufacturingSn :: WirelessDeviceIdType
pattern WirelessDeviceIdType_SidewalkManufacturingSn = WirelessDeviceIdType' "SidewalkManufacturingSn"

pattern WirelessDeviceIdType_ThingName :: WirelessDeviceIdType
pattern WirelessDeviceIdType_ThingName = WirelessDeviceIdType' "ThingName"

pattern WirelessDeviceIdType_WirelessDeviceId :: WirelessDeviceIdType
pattern WirelessDeviceIdType_WirelessDeviceId = WirelessDeviceIdType' "WirelessDeviceId"

{-# COMPLETE
  WirelessDeviceIdType_DevEui,
  WirelessDeviceIdType_SidewalkManufacturingSn,
  WirelessDeviceIdType_ThingName,
  WirelessDeviceIdType_WirelessDeviceId,
  WirelessDeviceIdType'
  #-}
