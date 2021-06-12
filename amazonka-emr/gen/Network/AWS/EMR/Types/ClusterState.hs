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
-- Module      : Network.AWS.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterState
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

import qualified Network.AWS.Core as Core

newtype ClusterState = ClusterState'
  { fromClusterState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
