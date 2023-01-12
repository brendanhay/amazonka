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
-- Module      : Amazonka.Panorama.Types.DeviceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceStatus
  ( DeviceStatus
      ( ..,
        DeviceStatus_AWAITING_PROVISIONING,
        DeviceStatus_DELETING,
        DeviceStatus_ERROR,
        DeviceStatus_FAILED,
        DeviceStatus_PENDING,
        DeviceStatus_SUCCEEDED
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

pattern DeviceStatus_AWAITING_PROVISIONING :: DeviceStatus
pattern DeviceStatus_AWAITING_PROVISIONING = DeviceStatus' "AWAITING_PROVISIONING"

pattern DeviceStatus_DELETING :: DeviceStatus
pattern DeviceStatus_DELETING = DeviceStatus' "DELETING"

pattern DeviceStatus_ERROR :: DeviceStatus
pattern DeviceStatus_ERROR = DeviceStatus' "ERROR"

pattern DeviceStatus_FAILED :: DeviceStatus
pattern DeviceStatus_FAILED = DeviceStatus' "FAILED"

pattern DeviceStatus_PENDING :: DeviceStatus
pattern DeviceStatus_PENDING = DeviceStatus' "PENDING"

pattern DeviceStatus_SUCCEEDED :: DeviceStatus
pattern DeviceStatus_SUCCEEDED = DeviceStatus' "SUCCEEDED"

{-# COMPLETE
  DeviceStatus_AWAITING_PROVISIONING,
  DeviceStatus_DELETING,
  DeviceStatus_ERROR,
  DeviceStatus_FAILED,
  DeviceStatus_PENDING,
  DeviceStatus_SUCCEEDED,
  DeviceStatus'
  #-}
