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
-- Module      : Amazonka.DirectoryService.Types.SnapshotStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SnapshotStatus
  ( SnapshotStatus
      ( ..,
        SnapshotStatus_Completed,
        SnapshotStatus_Creating,
        SnapshotStatus_Failed
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

pattern SnapshotStatus_Completed :: SnapshotStatus
pattern SnapshotStatus_Completed = SnapshotStatus' "Completed"

pattern SnapshotStatus_Creating :: SnapshotStatus
pattern SnapshotStatus_Creating = SnapshotStatus' "Creating"

pattern SnapshotStatus_Failed :: SnapshotStatus
pattern SnapshotStatus_Failed = SnapshotStatus' "Failed"

{-# COMPLETE
  SnapshotStatus_Completed,
  SnapshotStatus_Creating,
  SnapshotStatus_Failed,
  SnapshotStatus'
  #-}
