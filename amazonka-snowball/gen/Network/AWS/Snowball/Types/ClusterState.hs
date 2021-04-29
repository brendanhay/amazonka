{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ClusterState = ClusterState'
  { fromClusterState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
