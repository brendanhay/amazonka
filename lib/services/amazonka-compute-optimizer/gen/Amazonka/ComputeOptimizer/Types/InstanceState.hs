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
-- Module      : Amazonka.ComputeOptimizer.Types.InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.InstanceState
  ( InstanceState
      ( ..,
        InstanceState_Pending,
        InstanceState_Running,
        InstanceState_Shutting_down,
        InstanceState_Stopped,
        InstanceState_Stopping,
        InstanceState_Terminated
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceState = InstanceState'
  { fromInstanceState ::
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

pattern InstanceState_Pending :: InstanceState
pattern InstanceState_Pending = InstanceState' "pending"

pattern InstanceState_Running :: InstanceState
pattern InstanceState_Running = InstanceState' "running"

pattern InstanceState_Shutting_down :: InstanceState
pattern InstanceState_Shutting_down = InstanceState' "shutting-down"

pattern InstanceState_Stopped :: InstanceState
pattern InstanceState_Stopped = InstanceState' "stopped"

pattern InstanceState_Stopping :: InstanceState
pattern InstanceState_Stopping = InstanceState' "stopping"

pattern InstanceState_Terminated :: InstanceState
pattern InstanceState_Terminated = InstanceState' "terminated"

{-# COMPLETE
  InstanceState_Pending,
  InstanceState_Running,
  InstanceState_Shutting_down,
  InstanceState_Stopped,
  InstanceState_Stopping,
  InstanceState_Terminated,
  InstanceState'
  #-}
