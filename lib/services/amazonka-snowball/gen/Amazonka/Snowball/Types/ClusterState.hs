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
-- Module      : Amazonka.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ClusterState
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
