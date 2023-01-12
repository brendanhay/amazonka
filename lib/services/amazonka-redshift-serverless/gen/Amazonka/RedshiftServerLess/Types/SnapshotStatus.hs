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
-- Module      : Amazonka.RedshiftServerLess.Types.SnapshotStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.SnapshotStatus
  ( SnapshotStatus
      ( ..,
        SnapshotStatus_AVAILABLE,
        SnapshotStatus_CANCELLED,
        SnapshotStatus_COPYING,
        SnapshotStatus_CREATING,
        SnapshotStatus_DELETED,
        SnapshotStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SnapshotStatus = SnapshotStatus'
  { fromSnapshotStatus ::
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

pattern SnapshotStatus_AVAILABLE :: SnapshotStatus
pattern SnapshotStatus_AVAILABLE = SnapshotStatus' "AVAILABLE"

pattern SnapshotStatus_CANCELLED :: SnapshotStatus
pattern SnapshotStatus_CANCELLED = SnapshotStatus' "CANCELLED"

pattern SnapshotStatus_COPYING :: SnapshotStatus
pattern SnapshotStatus_COPYING = SnapshotStatus' "COPYING"

pattern SnapshotStatus_CREATING :: SnapshotStatus
pattern SnapshotStatus_CREATING = SnapshotStatus' "CREATING"

pattern SnapshotStatus_DELETED :: SnapshotStatus
pattern SnapshotStatus_DELETED = SnapshotStatus' "DELETED"

pattern SnapshotStatus_FAILED :: SnapshotStatus
pattern SnapshotStatus_FAILED = SnapshotStatus' "FAILED"

{-# COMPLETE
  SnapshotStatus_AVAILABLE,
  SnapshotStatus_CANCELLED,
  SnapshotStatus_COPYING,
  SnapshotStatus_CREATING,
  SnapshotStatus_DELETED,
  SnapshotStatus_FAILED,
  SnapshotStatus'
  #-}
