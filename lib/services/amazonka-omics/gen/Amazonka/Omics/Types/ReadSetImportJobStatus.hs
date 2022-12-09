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
-- Module      : Amazonka.Omics.Types.ReadSetImportJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetImportJobStatus
  ( ReadSetImportJobStatus
      ( ..,
        ReadSetImportJobStatus_CANCELLED,
        ReadSetImportJobStatus_CANCELLING,
        ReadSetImportJobStatus_COMPLETED,
        ReadSetImportJobStatus_COMPLETED_WITH_FAILURES,
        ReadSetImportJobStatus_FAILED,
        ReadSetImportJobStatus_IN_PROGRESS,
        ReadSetImportJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetImportJobStatus = ReadSetImportJobStatus'
  { fromReadSetImportJobStatus ::
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

pattern ReadSetImportJobStatus_CANCELLED :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_CANCELLED = ReadSetImportJobStatus' "CANCELLED"

pattern ReadSetImportJobStatus_CANCELLING :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_CANCELLING = ReadSetImportJobStatus' "CANCELLING"

pattern ReadSetImportJobStatus_COMPLETED :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_COMPLETED = ReadSetImportJobStatus' "COMPLETED"

pattern ReadSetImportJobStatus_COMPLETED_WITH_FAILURES :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_COMPLETED_WITH_FAILURES = ReadSetImportJobStatus' "COMPLETED_WITH_FAILURES"

pattern ReadSetImportJobStatus_FAILED :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_FAILED = ReadSetImportJobStatus' "FAILED"

pattern ReadSetImportJobStatus_IN_PROGRESS :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_IN_PROGRESS = ReadSetImportJobStatus' "IN_PROGRESS"

pattern ReadSetImportJobStatus_SUBMITTED :: ReadSetImportJobStatus
pattern ReadSetImportJobStatus_SUBMITTED = ReadSetImportJobStatus' "SUBMITTED"

{-# COMPLETE
  ReadSetImportJobStatus_CANCELLED,
  ReadSetImportJobStatus_CANCELLING,
  ReadSetImportJobStatus_COMPLETED,
  ReadSetImportJobStatus_COMPLETED_WITH_FAILURES,
  ReadSetImportJobStatus_FAILED,
  ReadSetImportJobStatus_IN_PROGRESS,
  ReadSetImportJobStatus_SUBMITTED,
  ReadSetImportJobStatus'
  #-}
