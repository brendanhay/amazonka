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
-- Module      : Amazonka.HoneyCode.Types.TableDataImportJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.TableDataImportJobStatus
  ( TableDataImportJobStatus
      ( ..,
        TableDataImportJobStatus_COMPLETED,
        TableDataImportJobStatus_FAILED,
        TableDataImportJobStatus_IN_PROGRESS,
        TableDataImportJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TableDataImportJobStatus = TableDataImportJobStatus'
  { fromTableDataImportJobStatus ::
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

pattern TableDataImportJobStatus_COMPLETED :: TableDataImportJobStatus
pattern TableDataImportJobStatus_COMPLETED = TableDataImportJobStatus' "COMPLETED"

pattern TableDataImportJobStatus_FAILED :: TableDataImportJobStatus
pattern TableDataImportJobStatus_FAILED = TableDataImportJobStatus' "FAILED"

pattern TableDataImportJobStatus_IN_PROGRESS :: TableDataImportJobStatus
pattern TableDataImportJobStatus_IN_PROGRESS = TableDataImportJobStatus' "IN_PROGRESS"

pattern TableDataImportJobStatus_SUBMITTED :: TableDataImportJobStatus
pattern TableDataImportJobStatus_SUBMITTED = TableDataImportJobStatus' "SUBMITTED"

{-# COMPLETE
  TableDataImportJobStatus_COMPLETED,
  TableDataImportJobStatus_FAILED,
  TableDataImportJobStatus_IN_PROGRESS,
  TableDataImportJobStatus_SUBMITTED,
  TableDataImportJobStatus'
  #-}
