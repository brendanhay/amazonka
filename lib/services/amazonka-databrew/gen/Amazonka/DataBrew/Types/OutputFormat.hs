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
-- Module      : Amazonka.DataBrew.Types.OutputFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.OutputFormat
  ( OutputFormat
      ( ..,
        OutputFormat_AVRO,
        OutputFormat_CSV,
        OutputFormat_GLUEPARQUET,
        OutputFormat_JSON,
        OutputFormat_ORC,
        OutputFormat_PARQUET,
        OutputFormat_TABLEAUHYPER,
        OutputFormat_XML
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OutputFormat = OutputFormat'
  { fromOutputFormat ::
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

pattern OutputFormat_AVRO :: OutputFormat
pattern OutputFormat_AVRO = OutputFormat' "AVRO"

pattern OutputFormat_CSV :: OutputFormat
pattern OutputFormat_CSV = OutputFormat' "CSV"

pattern OutputFormat_GLUEPARQUET :: OutputFormat
pattern OutputFormat_GLUEPARQUET = OutputFormat' "GLUEPARQUET"

pattern OutputFormat_JSON :: OutputFormat
pattern OutputFormat_JSON = OutputFormat' "JSON"

pattern OutputFormat_ORC :: OutputFormat
pattern OutputFormat_ORC = OutputFormat' "ORC"

pattern OutputFormat_PARQUET :: OutputFormat
pattern OutputFormat_PARQUET = OutputFormat' "PARQUET"

pattern OutputFormat_TABLEAUHYPER :: OutputFormat
pattern OutputFormat_TABLEAUHYPER = OutputFormat' "TABLEAUHYPER"

pattern OutputFormat_XML :: OutputFormat
pattern OutputFormat_XML = OutputFormat' "XML"

{-# COMPLETE
  OutputFormat_AVRO,
  OutputFormat_CSV,
  OutputFormat_GLUEPARQUET,
  OutputFormat_JSON,
  OutputFormat_ORC,
  OutputFormat_PARQUET,
  OutputFormat_TABLEAUHYPER,
  OutputFormat_XML,
  OutputFormat'
  #-}
