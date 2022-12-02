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
-- Module      : Amazonka.CloudWatchLogs.Types.ExportTaskStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.ExportTaskStatusCode
  ( ExportTaskStatusCode
      ( ..,
        ExportTaskStatusCode_CANCELLED,
        ExportTaskStatusCode_COMPLETED,
        ExportTaskStatusCode_FAILED,
        ExportTaskStatusCode_PENDING,
        ExportTaskStatusCode_PENDING_CANCEL,
        ExportTaskStatusCode_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportTaskStatusCode = ExportTaskStatusCode'
  { fromExportTaskStatusCode ::
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

pattern ExportTaskStatusCode_CANCELLED :: ExportTaskStatusCode
pattern ExportTaskStatusCode_CANCELLED = ExportTaskStatusCode' "CANCELLED"

pattern ExportTaskStatusCode_COMPLETED :: ExportTaskStatusCode
pattern ExportTaskStatusCode_COMPLETED = ExportTaskStatusCode' "COMPLETED"

pattern ExportTaskStatusCode_FAILED :: ExportTaskStatusCode
pattern ExportTaskStatusCode_FAILED = ExportTaskStatusCode' "FAILED"

pattern ExportTaskStatusCode_PENDING :: ExportTaskStatusCode
pattern ExportTaskStatusCode_PENDING = ExportTaskStatusCode' "PENDING"

pattern ExportTaskStatusCode_PENDING_CANCEL :: ExportTaskStatusCode
pattern ExportTaskStatusCode_PENDING_CANCEL = ExportTaskStatusCode' "PENDING_CANCEL"

pattern ExportTaskStatusCode_RUNNING :: ExportTaskStatusCode
pattern ExportTaskStatusCode_RUNNING = ExportTaskStatusCode' "RUNNING"

{-# COMPLETE
  ExportTaskStatusCode_CANCELLED,
  ExportTaskStatusCode_COMPLETED,
  ExportTaskStatusCode_FAILED,
  ExportTaskStatusCode_PENDING,
  ExportTaskStatusCode_PENDING_CANCEL,
  ExportTaskStatusCode_RUNNING,
  ExportTaskStatusCode'
  #-}
