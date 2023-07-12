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
-- Module      : Amazonka.KinesisVideo.Types.SyncStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.SyncStatus
  ( SyncStatus
      ( ..,
        SyncStatus_ACKNOWLEDGED,
        SyncStatus_DELETE_FAILED,
        SyncStatus_DELETING,
        SyncStatus_IN_SYNC,
        SyncStatus_SYNCING,
        SyncStatus_SYNC_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SyncStatus = SyncStatus'
  { fromSyncStatus ::
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

pattern SyncStatus_ACKNOWLEDGED :: SyncStatus
pattern SyncStatus_ACKNOWLEDGED = SyncStatus' "ACKNOWLEDGED"

pattern SyncStatus_DELETE_FAILED :: SyncStatus
pattern SyncStatus_DELETE_FAILED = SyncStatus' "DELETE_FAILED"

pattern SyncStatus_DELETING :: SyncStatus
pattern SyncStatus_DELETING = SyncStatus' "DELETING"

pattern SyncStatus_IN_SYNC :: SyncStatus
pattern SyncStatus_IN_SYNC = SyncStatus' "IN_SYNC"

pattern SyncStatus_SYNCING :: SyncStatus
pattern SyncStatus_SYNCING = SyncStatus' "SYNCING"

pattern SyncStatus_SYNC_FAILED :: SyncStatus
pattern SyncStatus_SYNC_FAILED = SyncStatus' "SYNC_FAILED"

{-# COMPLETE
  SyncStatus_ACKNOWLEDGED,
  SyncStatus_DELETE_FAILED,
  SyncStatus_DELETING,
  SyncStatus_IN_SYNC,
  SyncStatus_SYNCING,
  SyncStatus_SYNC_FAILED,
  SyncStatus'
  #-}
