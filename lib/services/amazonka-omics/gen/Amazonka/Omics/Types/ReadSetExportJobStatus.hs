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
-- Module      : Amazonka.Omics.Types.ReadSetExportJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetExportJobStatus
  ( ReadSetExportJobStatus
      ( ..,
        ReadSetExportJobStatus_CANCELLED,
        ReadSetExportJobStatus_CANCELLING,
        ReadSetExportJobStatus_COMPLETED,
        ReadSetExportJobStatus_COMPLETED_WITH_FAILURES,
        ReadSetExportJobStatus_FAILED,
        ReadSetExportJobStatus_IN_PROGRESS,
        ReadSetExportJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetExportJobStatus = ReadSetExportJobStatus'
  { fromReadSetExportJobStatus ::
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

pattern ReadSetExportJobStatus_CANCELLED :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_CANCELLED = ReadSetExportJobStatus' "CANCELLED"

pattern ReadSetExportJobStatus_CANCELLING :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_CANCELLING = ReadSetExportJobStatus' "CANCELLING"

pattern ReadSetExportJobStatus_COMPLETED :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_COMPLETED = ReadSetExportJobStatus' "COMPLETED"

pattern ReadSetExportJobStatus_COMPLETED_WITH_FAILURES :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_COMPLETED_WITH_FAILURES = ReadSetExportJobStatus' "COMPLETED_WITH_FAILURES"

pattern ReadSetExportJobStatus_FAILED :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_FAILED = ReadSetExportJobStatus' "FAILED"

pattern ReadSetExportJobStatus_IN_PROGRESS :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_IN_PROGRESS = ReadSetExportJobStatus' "IN_PROGRESS"

pattern ReadSetExportJobStatus_SUBMITTED :: ReadSetExportJobStatus
pattern ReadSetExportJobStatus_SUBMITTED = ReadSetExportJobStatus' "SUBMITTED"

{-# COMPLETE
  ReadSetExportJobStatus_CANCELLED,
  ReadSetExportJobStatus_CANCELLING,
  ReadSetExportJobStatus_COMPLETED,
  ReadSetExportJobStatus_COMPLETED_WITH_FAILURES,
  ReadSetExportJobStatus_FAILED,
  ReadSetExportJobStatus_IN_PROGRESS,
  ReadSetExportJobStatus_SUBMITTED,
  ReadSetExportJobStatus'
  #-}
