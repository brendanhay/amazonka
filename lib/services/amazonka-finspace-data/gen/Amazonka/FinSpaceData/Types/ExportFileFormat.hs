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
-- Module      : Amazonka.FinSpaceData.Types.ExportFileFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ExportFileFormat
  ( ExportFileFormat
      ( ..,
        ExportFileFormat_DELIMITED_TEXT,
        ExportFileFormat_PARQUET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data View Export File Format
newtype ExportFileFormat = ExportFileFormat'
  { fromExportFileFormat ::
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

pattern ExportFileFormat_DELIMITED_TEXT :: ExportFileFormat
pattern ExportFileFormat_DELIMITED_TEXT = ExportFileFormat' "DELIMITED_TEXT"

pattern ExportFileFormat_PARQUET :: ExportFileFormat
pattern ExportFileFormat_PARQUET = ExportFileFormat' "PARQUET"

{-# COMPLETE
  ExportFileFormat_DELIMITED_TEXT,
  ExportFileFormat_PARQUET,
  ExportFileFormat'
  #-}
