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
-- Module      : Amazonka.BackupGateway.Types.SyncMetadataStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.SyncMetadataStatus
  ( SyncMetadataStatus
      ( ..,
        SyncMetadataStatus_CREATED,
        SyncMetadataStatus_FAILED,
        SyncMetadataStatus_PARTIALLY_FAILED,
        SyncMetadataStatus_RUNNING,
        SyncMetadataStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SyncMetadataStatus = SyncMetadataStatus'
  { fromSyncMetadataStatus ::
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

pattern SyncMetadataStatus_CREATED :: SyncMetadataStatus
pattern SyncMetadataStatus_CREATED = SyncMetadataStatus' "CREATED"

pattern SyncMetadataStatus_FAILED :: SyncMetadataStatus
pattern SyncMetadataStatus_FAILED = SyncMetadataStatus' "FAILED"

pattern SyncMetadataStatus_PARTIALLY_FAILED :: SyncMetadataStatus
pattern SyncMetadataStatus_PARTIALLY_FAILED = SyncMetadataStatus' "PARTIALLY_FAILED"

pattern SyncMetadataStatus_RUNNING :: SyncMetadataStatus
pattern SyncMetadataStatus_RUNNING = SyncMetadataStatus' "RUNNING"

pattern SyncMetadataStatus_SUCCEEDED :: SyncMetadataStatus
pattern SyncMetadataStatus_SUCCEEDED = SyncMetadataStatus' "SUCCEEDED"

{-# COMPLETE
  SyncMetadataStatus_CREATED,
  SyncMetadataStatus_FAILED,
  SyncMetadataStatus_PARTIALLY_FAILED,
  SyncMetadataStatus_RUNNING,
  SyncMetadataStatus_SUCCEEDED,
  SyncMetadataStatus'
  #-}
