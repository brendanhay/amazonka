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
-- Module      : Amazonka.FSx.Types.SnapshotLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SnapshotLifecycle
  ( SnapshotLifecycle
      ( ..,
        SnapshotLifecycle_AVAILABLE,
        SnapshotLifecycle_CREATING,
        SnapshotLifecycle_DELETING,
        SnapshotLifecycle_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SnapshotLifecycle = SnapshotLifecycle'
  { fromSnapshotLifecycle ::
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

pattern SnapshotLifecycle_AVAILABLE :: SnapshotLifecycle
pattern SnapshotLifecycle_AVAILABLE = SnapshotLifecycle' "AVAILABLE"

pattern SnapshotLifecycle_CREATING :: SnapshotLifecycle
pattern SnapshotLifecycle_CREATING = SnapshotLifecycle' "CREATING"

pattern SnapshotLifecycle_DELETING :: SnapshotLifecycle
pattern SnapshotLifecycle_DELETING = SnapshotLifecycle' "DELETING"

pattern SnapshotLifecycle_PENDING :: SnapshotLifecycle
pattern SnapshotLifecycle_PENDING = SnapshotLifecycle' "PENDING"

{-# COMPLETE
  SnapshotLifecycle_AVAILABLE,
  SnapshotLifecycle_CREATING,
  SnapshotLifecycle_DELETING,
  SnapshotLifecycle_PENDING,
  SnapshotLifecycle'
  #-}
