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
-- Module      : Amazonka.Braket.Types.DeviceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.DeviceStatus
  ( DeviceStatus
      ( ..,
        DeviceStatus_OFFLINE,
        DeviceStatus_ONLINE,
        DeviceStatus_RETIRED
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

pattern DeviceStatus_OFFLINE :: DeviceStatus
pattern DeviceStatus_OFFLINE = DeviceStatus' "OFFLINE"

pattern DeviceStatus_ONLINE :: DeviceStatus
pattern DeviceStatus_ONLINE = DeviceStatus' "ONLINE"

pattern DeviceStatus_RETIRED :: DeviceStatus
pattern DeviceStatus_RETIRED = DeviceStatus' "RETIRED"

{-# COMPLETE
  DeviceStatus_OFFLINE,
  DeviceStatus_ONLINE,
  DeviceStatus_RETIRED,
  DeviceStatus'
  #-}
