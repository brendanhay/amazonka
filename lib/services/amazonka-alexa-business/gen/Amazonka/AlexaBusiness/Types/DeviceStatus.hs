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
-- Module      : Amazonka.AlexaBusiness.Types.DeviceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceStatus
  ( DeviceStatus
      ( ..,
        DeviceStatus_DEREGISTERED,
        DeviceStatus_FAILED,
        DeviceStatus_PENDING,
        DeviceStatus_READY,
        DeviceStatus_WAS_OFFLINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceStatus = DeviceStatus'
  { fromDeviceStatus ::
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

pattern DeviceStatus_DEREGISTERED :: DeviceStatus
pattern DeviceStatus_DEREGISTERED = DeviceStatus' "DEREGISTERED"

pattern DeviceStatus_FAILED :: DeviceStatus
pattern DeviceStatus_FAILED = DeviceStatus' "FAILED"

pattern DeviceStatus_PENDING :: DeviceStatus
pattern DeviceStatus_PENDING = DeviceStatus' "PENDING"

pattern DeviceStatus_READY :: DeviceStatus
pattern DeviceStatus_READY = DeviceStatus' "READY"

pattern DeviceStatus_WAS_OFFLINE :: DeviceStatus
pattern DeviceStatus_WAS_OFFLINE = DeviceStatus' "WAS_OFFLINE"

{-# COMPLETE
  DeviceStatus_DEREGISTERED,
  DeviceStatus_FAILED,
  DeviceStatus_PENDING,
  DeviceStatus_READY,
  DeviceStatus_WAS_OFFLINE,
  DeviceStatus'
  #-}
