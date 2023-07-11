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
-- Module      : Amazonka.EMR.Types.InstanceFleetState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetState
  ( InstanceFleetState
      ( ..,
        InstanceFleetState_BOOTSTRAPPING,
        InstanceFleetState_PROVISIONING,
        InstanceFleetState_RESIZING,
        InstanceFleetState_RUNNING,
        InstanceFleetState_SUSPENDED,
        InstanceFleetState_TERMINATED,
        InstanceFleetState_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceFleetState = InstanceFleetState'
  { fromInstanceFleetState ::
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

pattern InstanceFleetState_BOOTSTRAPPING :: InstanceFleetState
pattern InstanceFleetState_BOOTSTRAPPING = InstanceFleetState' "BOOTSTRAPPING"

pattern InstanceFleetState_PROVISIONING :: InstanceFleetState
pattern InstanceFleetState_PROVISIONING = InstanceFleetState' "PROVISIONING"

pattern InstanceFleetState_RESIZING :: InstanceFleetState
pattern InstanceFleetState_RESIZING = InstanceFleetState' "RESIZING"

pattern InstanceFleetState_RUNNING :: InstanceFleetState
pattern InstanceFleetState_RUNNING = InstanceFleetState' "RUNNING"

pattern InstanceFleetState_SUSPENDED :: InstanceFleetState
pattern InstanceFleetState_SUSPENDED = InstanceFleetState' "SUSPENDED"

pattern InstanceFleetState_TERMINATED :: InstanceFleetState
pattern InstanceFleetState_TERMINATED = InstanceFleetState' "TERMINATED"

pattern InstanceFleetState_TERMINATING :: InstanceFleetState
pattern InstanceFleetState_TERMINATING = InstanceFleetState' "TERMINATING"

{-# COMPLETE
  InstanceFleetState_BOOTSTRAPPING,
  InstanceFleetState_PROVISIONING,
  InstanceFleetState_RESIZING,
  InstanceFleetState_RUNNING,
  InstanceFleetState_SUSPENDED,
  InstanceFleetState_TERMINATED,
  InstanceFleetState_TERMINATING,
  InstanceFleetState'
  #-}
