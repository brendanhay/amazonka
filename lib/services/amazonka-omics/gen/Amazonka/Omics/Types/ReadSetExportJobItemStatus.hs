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
-- Module      : Amazonka.Omics.Types.ReadSetExportJobItemStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetExportJobItemStatus
  ( ReadSetExportJobItemStatus
      ( ..,
        ReadSetExportJobItemStatus_FAILED,
        ReadSetExportJobItemStatus_FINISHED,
        ReadSetExportJobItemStatus_IN_PROGRESS,
        ReadSetExportJobItemStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetExportJobItemStatus = ReadSetExportJobItemStatus'
  { fromReadSetExportJobItemStatus ::
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

pattern ReadSetExportJobItemStatus_FAILED :: ReadSetExportJobItemStatus
pattern ReadSetExportJobItemStatus_FAILED = ReadSetExportJobItemStatus' "FAILED"

pattern ReadSetExportJobItemStatus_FINISHED :: ReadSetExportJobItemStatus
pattern ReadSetExportJobItemStatus_FINISHED = ReadSetExportJobItemStatus' "FINISHED"

pattern ReadSetExportJobItemStatus_IN_PROGRESS :: ReadSetExportJobItemStatus
pattern ReadSetExportJobItemStatus_IN_PROGRESS = ReadSetExportJobItemStatus' "IN_PROGRESS"

pattern ReadSetExportJobItemStatus_NOT_STARTED :: ReadSetExportJobItemStatus
pattern ReadSetExportJobItemStatus_NOT_STARTED = ReadSetExportJobItemStatus' "NOT_STARTED"

{-# COMPLETE
  ReadSetExportJobItemStatus_FAILED,
  ReadSetExportJobItemStatus_FINISHED,
  ReadSetExportJobItemStatus_IN_PROGRESS,
  ReadSetExportJobItemStatus_NOT_STARTED,
  ReadSetExportJobItemStatus'
  #-}
