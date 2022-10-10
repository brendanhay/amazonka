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
-- Module      : Amazonka.EC2.Types.SnapshotState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotState
  ( SnapshotState
      ( ..,
        SnapshotState_Completed,
        SnapshotState_Error,
        SnapshotState_Pending,
        SnapshotState_Recoverable,
        SnapshotState_Recovering
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype SnapshotState = SnapshotState'
  { fromSnapshotState ::
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

pattern SnapshotState_Completed :: SnapshotState
pattern SnapshotState_Completed = SnapshotState' "completed"

pattern SnapshotState_Error :: SnapshotState
pattern SnapshotState_Error = SnapshotState' "error"

pattern SnapshotState_Pending :: SnapshotState
pattern SnapshotState_Pending = SnapshotState' "pending"

pattern SnapshotState_Recoverable :: SnapshotState
pattern SnapshotState_Recoverable = SnapshotState' "recoverable"

pattern SnapshotState_Recovering :: SnapshotState
pattern SnapshotState_Recovering = SnapshotState' "recovering"

{-# COMPLETE
  SnapshotState_Completed,
  SnapshotState_Error,
  SnapshotState_Pending,
  SnapshotState_Recoverable,
  SnapshotState_Recovering,
  SnapshotState'
  #-}
