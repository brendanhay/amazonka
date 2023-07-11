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
-- Module      : Amazonka.Omics.Types.ReferenceImportJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceImportJobStatus
  ( ReferenceImportJobStatus
      ( ..,
        ReferenceImportJobStatus_CANCELLED,
        ReferenceImportJobStatus_CANCELLING,
        ReferenceImportJobStatus_COMPLETED,
        ReferenceImportJobStatus_COMPLETED_WITH_FAILURES,
        ReferenceImportJobStatus_FAILED,
        ReferenceImportJobStatus_IN_PROGRESS,
        ReferenceImportJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReferenceImportJobStatus = ReferenceImportJobStatus'
  { fromReferenceImportJobStatus ::
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

pattern ReferenceImportJobStatus_CANCELLED :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_CANCELLED = ReferenceImportJobStatus' "CANCELLED"

pattern ReferenceImportJobStatus_CANCELLING :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_CANCELLING = ReferenceImportJobStatus' "CANCELLING"

pattern ReferenceImportJobStatus_COMPLETED :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_COMPLETED = ReferenceImportJobStatus' "COMPLETED"

pattern ReferenceImportJobStatus_COMPLETED_WITH_FAILURES :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_COMPLETED_WITH_FAILURES = ReferenceImportJobStatus' "COMPLETED_WITH_FAILURES"

pattern ReferenceImportJobStatus_FAILED :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_FAILED = ReferenceImportJobStatus' "FAILED"

pattern ReferenceImportJobStatus_IN_PROGRESS :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_IN_PROGRESS = ReferenceImportJobStatus' "IN_PROGRESS"

pattern ReferenceImportJobStatus_SUBMITTED :: ReferenceImportJobStatus
pattern ReferenceImportJobStatus_SUBMITTED = ReferenceImportJobStatus' "SUBMITTED"

{-# COMPLETE
  ReferenceImportJobStatus_CANCELLED,
  ReferenceImportJobStatus_CANCELLING,
  ReferenceImportJobStatus_COMPLETED,
  ReferenceImportJobStatus_COMPLETED_WITH_FAILURES,
  ReferenceImportJobStatus_FAILED,
  ReferenceImportJobStatus_IN_PROGRESS,
  ReferenceImportJobStatus_SUBMITTED,
  ReferenceImportJobStatus'
  #-}
