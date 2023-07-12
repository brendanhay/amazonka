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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceType
  ( WirelessDeviceType
      ( ..,
        WirelessDeviceType_LoRaWAN,
        WirelessDeviceType_Sidewalk
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WirelessDeviceType = WirelessDeviceType'
  { fromWirelessDeviceType ::
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

pattern WirelessDeviceType_LoRaWAN :: WirelessDeviceType
pattern WirelessDeviceType_LoRaWAN = WirelessDeviceType' "LoRaWAN"

pattern WirelessDeviceType_Sidewalk :: WirelessDeviceType
pattern WirelessDeviceType_Sidewalk = WirelessDeviceType' "Sidewalk"

{-# COMPLETE
  WirelessDeviceType_LoRaWAN,
  WirelessDeviceType_Sidewalk,
  WirelessDeviceType'
  #-}
