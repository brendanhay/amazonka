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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of software on the input device.
newtype DeviceUpdateStatus = DeviceUpdateStatus'
  { fromDeviceUpdateStatus ::
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
