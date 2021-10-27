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
-- Module      : Network.AWS.NetworkManager.Types.DeviceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkManager.Types.DeviceState
  ( DeviceState
      ( ..,
        DeviceState_AVAILABLE,
        DeviceState_DELETING,
        DeviceState_PENDING,
        DeviceState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DeviceState = DeviceState'
  { fromDeviceState ::
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
