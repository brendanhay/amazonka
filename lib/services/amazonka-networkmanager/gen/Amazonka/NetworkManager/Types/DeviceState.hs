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
-- Module      : Amazonka.NetworkManager.Types.DeviceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.DeviceState
  ( DeviceState
      ( ..,
        DeviceState_AVAILABLE,
        DeviceState_DELETING,
        DeviceState_PENDING,
        DeviceState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceState = DeviceState'
  { fromDeviceState ::
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

pattern DeviceState_AVAILABLE :: DeviceState
pattern DeviceState_AVAILABLE = DeviceState' "AVAILABLE"

pattern DeviceState_DELETING :: DeviceState
pattern DeviceState_DELETING = DeviceState' "DELETING"

pattern DeviceState_PENDING :: DeviceState
pattern DeviceState_PENDING = DeviceState' "PENDING"

pattern DeviceState_UPDATING :: DeviceState
pattern DeviceState_UPDATING = DeviceState' "UPDATING"

{-# COMPLETE
  DeviceState_AVAILABLE,
  DeviceState_DELETING,
  DeviceState_PENDING,
  DeviceState_UPDATING,
  DeviceState'
  #-}
