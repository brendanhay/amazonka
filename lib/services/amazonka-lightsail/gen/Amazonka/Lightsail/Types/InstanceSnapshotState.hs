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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype InstanceSnapshotState = InstanceSnapshotState'
  { fromInstanceSnapshotState ::
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
