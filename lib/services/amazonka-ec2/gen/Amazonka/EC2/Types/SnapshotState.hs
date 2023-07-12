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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype SnapshotState = SnapshotState'
  { fromSnapshotState ::
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
