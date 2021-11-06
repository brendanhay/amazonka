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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype DeviceStatus = DeviceStatus'
  { fromDeviceStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
