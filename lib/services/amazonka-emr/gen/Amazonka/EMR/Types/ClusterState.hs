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
-- Module      : Amazonka.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterState
  ( ClusterState
      ( ..,
        ClusterState_BOOTSTRAPPING,
        ClusterState_RUNNING,
        ClusterState_STARTING,
        ClusterState_TERMINATED,
        ClusterState_TERMINATED_WITH_ERRORS,
        ClusterState_TERMINATING,
        ClusterState_WAITING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClusterState = ClusterState'
  { fromClusterState ::
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

pattern ClusterState_BOOTSTRAPPING :: ClusterState
pattern ClusterState_BOOTSTRAPPING = ClusterState' "BOOTSTRAPPING"

pattern ClusterState_RUNNING :: ClusterState
pattern ClusterState_RUNNING = ClusterState' "RUNNING"

pattern ClusterState_STARTING :: ClusterState
pattern ClusterState_STARTING = ClusterState' "STARTING"

pattern ClusterState_TERMINATED :: ClusterState
pattern ClusterState_TERMINATED = ClusterState' "TERMINATED"

pattern ClusterState_TERMINATED_WITH_ERRORS :: ClusterState
pattern ClusterState_TERMINATED_WITH_ERRORS = ClusterState' "TERMINATED_WITH_ERRORS"

pattern ClusterState_TERMINATING :: ClusterState
pattern ClusterState_TERMINATING = ClusterState' "TERMINATING"

pattern ClusterState_WAITING :: ClusterState
pattern ClusterState_WAITING = ClusterState' "WAITING"

{-# COMPLETE
  ClusterState_BOOTSTRAPPING,
  ClusterState_RUNNING,
  ClusterState_STARTING,
  ClusterState_TERMINATED,
  ClusterState_TERMINATED_WITH_ERRORS,
  ClusterState_TERMINATING,
  ClusterState_WAITING,
  ClusterState'
  #-}
