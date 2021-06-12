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
-- Module      : Network.AWS.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterState
  ( ClusterState
      ( ..,
        ClusterState_AwaitingQuorum,
        ClusterState_Cancelled,
        ClusterState_Complete,
        ClusterState_InUse,
        ClusterState_Pending
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

pattern ClusterState_AwaitingQuorum :: ClusterState
pattern ClusterState_AwaitingQuorum = ClusterState' "AwaitingQuorum"

pattern ClusterState_Cancelled :: ClusterState
pattern ClusterState_Cancelled = ClusterState' "Cancelled"

pattern ClusterState_Complete :: ClusterState
pattern ClusterState_Complete = ClusterState' "Complete"

pattern ClusterState_InUse :: ClusterState
pattern ClusterState_InUse = ClusterState' "InUse"

pattern ClusterState_Pending :: ClusterState
pattern ClusterState_Pending = ClusterState' "Pending"

{-# COMPLETE
  ClusterState_AwaitingQuorum,
  ClusterState_Cancelled,
  ClusterState_Complete,
  ClusterState_InUse,
  ClusterState_Pending,
  ClusterState'
  #-}
