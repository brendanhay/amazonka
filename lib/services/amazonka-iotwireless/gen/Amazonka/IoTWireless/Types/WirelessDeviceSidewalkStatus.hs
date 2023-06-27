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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceSidewalkStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceSidewalkStatus
  ( WirelessDeviceSidewalkStatus
      ( ..,
        WirelessDeviceSidewalkStatus_ACTIVATED,
        WirelessDeviceSidewalkStatus_PROVISIONED,
        WirelessDeviceSidewalkStatus_REGISTERED,
        WirelessDeviceSidewalkStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WirelessDeviceSidewalkStatus = WirelessDeviceSidewalkStatus'
  { fromWirelessDeviceSidewalkStatus ::
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

pattern WirelessDeviceSidewalkStatus_ACTIVATED :: WirelessDeviceSidewalkStatus
pattern WirelessDeviceSidewalkStatus_ACTIVATED = WirelessDeviceSidewalkStatus' "ACTIVATED"

pattern WirelessDeviceSidewalkStatus_PROVISIONED :: WirelessDeviceSidewalkStatus
pattern WirelessDeviceSidewalkStatus_PROVISIONED = WirelessDeviceSidewalkStatus' "PROVISIONED"

pattern WirelessDeviceSidewalkStatus_REGISTERED :: WirelessDeviceSidewalkStatus
pattern WirelessDeviceSidewalkStatus_REGISTERED = WirelessDeviceSidewalkStatus' "REGISTERED"

pattern WirelessDeviceSidewalkStatus_UNKNOWN :: WirelessDeviceSidewalkStatus
pattern WirelessDeviceSidewalkStatus_UNKNOWN = WirelessDeviceSidewalkStatus' "UNKNOWN"

{-# COMPLETE
  WirelessDeviceSidewalkStatus_ACTIVATED,
  WirelessDeviceSidewalkStatus_PROVISIONED,
  WirelessDeviceSidewalkStatus_REGISTERED,
  WirelessDeviceSidewalkStatus_UNKNOWN,
  WirelessDeviceSidewalkStatus'
  #-}
