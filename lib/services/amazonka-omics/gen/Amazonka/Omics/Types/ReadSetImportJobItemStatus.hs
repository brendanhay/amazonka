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
-- Module      : Amazonka.Omics.Types.ReadSetImportJobItemStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetImportJobItemStatus
  ( ReadSetImportJobItemStatus
      ( ..,
        ReadSetImportJobItemStatus_FAILED,
        ReadSetImportJobItemStatus_FINISHED,
        ReadSetImportJobItemStatus_IN_PROGRESS,
        ReadSetImportJobItemStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetImportJobItemStatus = ReadSetImportJobItemStatus'
  { fromReadSetImportJobItemStatus ::
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

pattern ReadSetImportJobItemStatus_FAILED :: ReadSetImportJobItemStatus
pattern ReadSetImportJobItemStatus_FAILED = ReadSetImportJobItemStatus' "FAILED"

pattern ReadSetImportJobItemStatus_FINISHED :: ReadSetImportJobItemStatus
pattern ReadSetImportJobItemStatus_FINISHED = ReadSetImportJobItemStatus' "FINISHED"

pattern ReadSetImportJobItemStatus_IN_PROGRESS :: ReadSetImportJobItemStatus
pattern ReadSetImportJobItemStatus_IN_PROGRESS = ReadSetImportJobItemStatus' "IN_PROGRESS"

pattern ReadSetImportJobItemStatus_NOT_STARTED :: ReadSetImportJobItemStatus
pattern ReadSetImportJobItemStatus_NOT_STARTED = ReadSetImportJobItemStatus' "NOT_STARTED"

{-# COMPLETE
  ReadSetImportJobItemStatus_FAILED,
  ReadSetImportJobItemStatus_FINISHED,
  ReadSetImportJobItemStatus_IN_PROGRESS,
  ReadSetImportJobItemStatus_NOT_STARTED,
  ReadSetImportJobItemStatus'
  #-}
