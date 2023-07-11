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
-- Module      : Amazonka.QuickSight.Types.FileFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FileFormat
  ( FileFormat
      ( ..,
        FileFormat_CLF,
        FileFormat_CSV,
        FileFormat_ELF,
        FileFormat_JSON,
        FileFormat_TSV,
        FileFormat_XLSX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FileFormat = FileFormat'
  { fromFileFormat ::
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

pattern FileFormat_CLF :: FileFormat
pattern FileFormat_CLF = FileFormat' "CLF"

pattern FileFormat_CSV :: FileFormat
pattern FileFormat_CSV = FileFormat' "CSV"

pattern FileFormat_ELF :: FileFormat
pattern FileFormat_ELF = FileFormat' "ELF"

pattern FileFormat_JSON :: FileFormat
pattern FileFormat_JSON = FileFormat' "JSON"

pattern FileFormat_TSV :: FileFormat
pattern FileFormat_TSV = FileFormat' "TSV"

pattern FileFormat_XLSX :: FileFormat
pattern FileFormat_XLSX = FileFormat' "XLSX"

{-# COMPLETE
  FileFormat_CLF,
  FileFormat_CSV,
  FileFormat_ELF,
  FileFormat_JSON,
  FileFormat_TSV,
  FileFormat_XLSX,
  FileFormat'
  #-}
