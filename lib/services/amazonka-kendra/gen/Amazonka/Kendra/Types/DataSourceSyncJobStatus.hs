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
-- Module      : Amazonka.Kendra.Types.DataSourceSyncJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceSyncJobStatus
  ( DataSourceSyncJobStatus
      ( ..,
        DataSourceSyncJobStatus_ABORTED,
        DataSourceSyncJobStatus_FAILED,
        DataSourceSyncJobStatus_INCOMPLETE,
        DataSourceSyncJobStatus_STOPPING,
        DataSourceSyncJobStatus_SUCCEEDED,
        DataSourceSyncJobStatus_SYNCING,
        DataSourceSyncJobStatus_SYNCING_INDEXING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceSyncJobStatus = DataSourceSyncJobStatus'
  { fromDataSourceSyncJobStatus ::
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

pattern DataSourceSyncJobStatus_ABORTED :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_ABORTED = DataSourceSyncJobStatus' "ABORTED"

pattern DataSourceSyncJobStatus_FAILED :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_FAILED = DataSourceSyncJobStatus' "FAILED"

pattern DataSourceSyncJobStatus_INCOMPLETE :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_INCOMPLETE = DataSourceSyncJobStatus' "INCOMPLETE"

pattern DataSourceSyncJobStatus_STOPPING :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_STOPPING = DataSourceSyncJobStatus' "STOPPING"

pattern DataSourceSyncJobStatus_SUCCEEDED :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_SUCCEEDED = DataSourceSyncJobStatus' "SUCCEEDED"

pattern DataSourceSyncJobStatus_SYNCING :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_SYNCING = DataSourceSyncJobStatus' "SYNCING"

pattern DataSourceSyncJobStatus_SYNCING_INDEXING :: DataSourceSyncJobStatus
pattern DataSourceSyncJobStatus_SYNCING_INDEXING = DataSourceSyncJobStatus' "SYNCING_INDEXING"

{-# COMPLETE
  DataSourceSyncJobStatus_ABORTED,
  DataSourceSyncJobStatus_FAILED,
  DataSourceSyncJobStatus_INCOMPLETE,
  DataSourceSyncJobStatus_STOPPING,
  DataSourceSyncJobStatus_SUCCEEDED,
  DataSourceSyncJobStatus_SYNCING,
  DataSourceSyncJobStatus_SYNCING_INDEXING,
  DataSourceSyncJobStatus'
  #-}
