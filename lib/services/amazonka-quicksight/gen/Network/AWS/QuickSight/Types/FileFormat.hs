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
-- Module      : Network.AWS.QuickSight.Types.FileFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.FileFormat
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FileFormat = FileFormat'
  { fromFileFormat ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
