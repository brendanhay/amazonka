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
-- Module      : Amazonka.Lightsail.Types.InstanceHealthState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceHealthState
  ( InstanceHealthState
      ( ..,
        InstanceHealthState_Draining,
        InstanceHealthState_Healthy,
        InstanceHealthState_Initial,
        InstanceHealthState_Unavailable,
        InstanceHealthState_Unhealthy,
        InstanceHealthState_Unused
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InstanceHealthState = InstanceHealthState'
  { fromInstanceHealthState ::
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

pattern InstanceHealthState_Draining :: InstanceHealthState
pattern InstanceHealthState_Draining = InstanceHealthState' "draining"

pattern InstanceHealthState_Healthy :: InstanceHealthState
pattern InstanceHealthState_Healthy = InstanceHealthState' "healthy"

pattern InstanceHealthState_Initial :: InstanceHealthState
pattern InstanceHealthState_Initial = InstanceHealthState' "initial"

pattern InstanceHealthState_Unavailable :: InstanceHealthState
pattern InstanceHealthState_Unavailable = InstanceHealthState' "unavailable"

pattern InstanceHealthState_Unhealthy :: InstanceHealthState
pattern InstanceHealthState_Unhealthy = InstanceHealthState' "unhealthy"

pattern InstanceHealthState_Unused :: InstanceHealthState
pattern InstanceHealthState_Unused = InstanceHealthState' "unused"

{-# COMPLETE
  InstanceHealthState_Draining,
  InstanceHealthState_Healthy,
  InstanceHealthState_Initial,
  InstanceHealthState_Unavailable,
  InstanceHealthState_Unhealthy,
  InstanceHealthState_Unused,
  InstanceHealthState'
  #-}
