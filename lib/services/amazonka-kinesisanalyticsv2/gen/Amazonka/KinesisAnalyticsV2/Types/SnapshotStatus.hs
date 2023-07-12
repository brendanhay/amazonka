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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.SnapshotStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.SnapshotStatus
  ( SnapshotStatus
      ( ..,
        SnapshotStatus_CREATING,
        SnapshotStatus_DELETING,
        SnapshotStatus_FAILED,
        SnapshotStatus_READY
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

pattern SnapshotStatus_CREATING :: SnapshotStatus
pattern SnapshotStatus_CREATING = SnapshotStatus' "CREATING"

pattern SnapshotStatus_DELETING :: SnapshotStatus
pattern SnapshotStatus_DELETING = SnapshotStatus' "DELETING"

pattern SnapshotStatus_FAILED :: SnapshotStatus
pattern SnapshotStatus_FAILED = SnapshotStatus' "FAILED"

pattern SnapshotStatus_READY :: SnapshotStatus
pattern SnapshotStatus_READY = SnapshotStatus' "READY"

{-# COMPLETE
  SnapshotStatus_CREATING,
  SnapshotStatus_DELETING,
  SnapshotStatus_FAILED,
  SnapshotStatus_READY,
  SnapshotStatus'
  #-}
