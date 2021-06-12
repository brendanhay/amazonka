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
-- Module      : Network.AWS.CloudHSMv2.Types.ClusterState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.ClusterState
  ( ClusterState
      ( ..,
        ClusterState_ACTIVE,
        ClusterState_CREATE_IN_PROGRESS,
        ClusterState_DEGRADED,
        ClusterState_DELETED,
        ClusterState_DELETE_IN_PROGRESS,
        ClusterState_INITIALIZED,
        ClusterState_INITIALIZE_IN_PROGRESS,
        ClusterState_UNINITIALIZED,
        ClusterState_UPDATE_IN_PROGRESS
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

pattern ClusterState_ACTIVE :: ClusterState
pattern ClusterState_ACTIVE = ClusterState' "ACTIVE"

pattern ClusterState_CREATE_IN_PROGRESS :: ClusterState
pattern ClusterState_CREATE_IN_PROGRESS = ClusterState' "CREATE_IN_PROGRESS"

pattern ClusterState_DEGRADED :: ClusterState
pattern ClusterState_DEGRADED = ClusterState' "DEGRADED"

pattern ClusterState_DELETED :: ClusterState
pattern ClusterState_DELETED = ClusterState' "DELETED"

pattern ClusterState_DELETE_IN_PROGRESS :: ClusterState
pattern ClusterState_DELETE_IN_PROGRESS = ClusterState' "DELETE_IN_PROGRESS"

pattern ClusterState_INITIALIZED :: ClusterState
pattern ClusterState_INITIALIZED = ClusterState' "INITIALIZED"

pattern ClusterState_INITIALIZE_IN_PROGRESS :: ClusterState
pattern ClusterState_INITIALIZE_IN_PROGRESS = ClusterState' "INITIALIZE_IN_PROGRESS"

pattern ClusterState_UNINITIALIZED :: ClusterState
pattern ClusterState_UNINITIALIZED = ClusterState' "UNINITIALIZED"

pattern ClusterState_UPDATE_IN_PROGRESS :: ClusterState
pattern ClusterState_UPDATE_IN_PROGRESS = ClusterState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  ClusterState_ACTIVE,
  ClusterState_CREATE_IN_PROGRESS,
  ClusterState_DEGRADED,
  ClusterState_DELETED,
  ClusterState_DELETE_IN_PROGRESS,
  ClusterState_INITIALIZED,
  ClusterState_INITIALIZE_IN_PROGRESS,
  ClusterState_UNINITIALIZED,
  ClusterState_UPDATE_IN_PROGRESS,
  ClusterState'
  #-}
