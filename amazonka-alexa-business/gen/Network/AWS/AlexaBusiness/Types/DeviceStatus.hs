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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype DeviceStatus = DeviceStatus'
  { fromDeviceStatus ::
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
