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
-- Module      : Amazonka.Lightsail.Types.InstanceSnapshotState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceSnapshotState
  ( InstanceSnapshotState
      ( ..,
        InstanceSnapshotState_Available,
        InstanceSnapshotState_Error,
        InstanceSnapshotState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceSnapshotState = InstanceSnapshotState'
  { fromInstanceSnapshotState ::
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

pattern InstanceSnapshotState_Available :: InstanceSnapshotState
pattern InstanceSnapshotState_Available = InstanceSnapshotState' "available"

pattern InstanceSnapshotState_Error :: InstanceSnapshotState
pattern InstanceSnapshotState_Error = InstanceSnapshotState' "error"

pattern InstanceSnapshotState_Pending :: InstanceSnapshotState
pattern InstanceSnapshotState_Pending = InstanceSnapshotState' "pending"

{-# COMPLETE
  InstanceSnapshotState_Available,
  InstanceSnapshotState_Error,
  InstanceSnapshotState_Pending,
  InstanceSnapshotState'
  #-}
