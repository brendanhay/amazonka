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
-- Module      : Amazonka.MediaLive.Types.DeviceUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DeviceUpdateStatus
  ( DeviceUpdateStatus
      ( ..,
        DeviceUpdateStatus_NOT_UP_TO_DATE,
        DeviceUpdateStatus_UPDATING,
        DeviceUpdateStatus_UP_TO_DATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The status of software on the input device.
newtype DeviceUpdateStatus = DeviceUpdateStatus'
  { fromDeviceUpdateStatus ::
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

pattern DeviceUpdateStatus_NOT_UP_TO_DATE :: DeviceUpdateStatus
pattern DeviceUpdateStatus_NOT_UP_TO_DATE = DeviceUpdateStatus' "NOT_UP_TO_DATE"

pattern DeviceUpdateStatus_UPDATING :: DeviceUpdateStatus
pattern DeviceUpdateStatus_UPDATING = DeviceUpdateStatus' "UPDATING"

pattern DeviceUpdateStatus_UP_TO_DATE :: DeviceUpdateStatus
pattern DeviceUpdateStatus_UP_TO_DATE = DeviceUpdateStatus' "UP_TO_DATE"

{-# COMPLETE
  DeviceUpdateStatus_NOT_UP_TO_DATE,
  DeviceUpdateStatus_UPDATING,
  DeviceUpdateStatus_UP_TO_DATE,
  DeviceUpdateStatus'
  #-}
