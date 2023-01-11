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
-- Module      : Amazonka.Panorama.Types.DeviceConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceConnectionStatus
  ( DeviceConnectionStatus
      ( ..,
        DeviceConnectionStatus_AWAITING_CREDENTIALS,
        DeviceConnectionStatus_ERROR,
        DeviceConnectionStatus_NOT_AVAILABLE,
        DeviceConnectionStatus_OFFLINE,
        DeviceConnectionStatus_ONLINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceConnectionStatus = DeviceConnectionStatus'
  { fromDeviceConnectionStatus ::
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

pattern DeviceConnectionStatus_AWAITING_CREDENTIALS :: DeviceConnectionStatus
pattern DeviceConnectionStatus_AWAITING_CREDENTIALS = DeviceConnectionStatus' "AWAITING_CREDENTIALS"

pattern DeviceConnectionStatus_ERROR :: DeviceConnectionStatus
pattern DeviceConnectionStatus_ERROR = DeviceConnectionStatus' "ERROR"

pattern DeviceConnectionStatus_NOT_AVAILABLE :: DeviceConnectionStatus
pattern DeviceConnectionStatus_NOT_AVAILABLE = DeviceConnectionStatus' "NOT_AVAILABLE"

pattern DeviceConnectionStatus_OFFLINE :: DeviceConnectionStatus
pattern DeviceConnectionStatus_OFFLINE = DeviceConnectionStatus' "OFFLINE"

pattern DeviceConnectionStatus_ONLINE :: DeviceConnectionStatus
pattern DeviceConnectionStatus_ONLINE = DeviceConnectionStatus' "ONLINE"

{-# COMPLETE
  DeviceConnectionStatus_AWAITING_CREDENTIALS,
  DeviceConnectionStatus_ERROR,
  DeviceConnectionStatus_NOT_AVAILABLE,
  DeviceConnectionStatus_OFFLINE,
  DeviceConnectionStatus_ONLINE,
  DeviceConnectionStatus'
  #-}
