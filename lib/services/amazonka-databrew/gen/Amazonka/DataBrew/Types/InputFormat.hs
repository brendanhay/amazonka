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
-- Module      : Amazonka.DataBrew.Types.InputFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.InputFormat
  ( InputFormat
      ( ..,
        InputFormat_CSV,
        InputFormat_EXCEL,
        InputFormat_JSON,
        InputFormat_ORC,
        InputFormat_PARQUET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InputFormat = InputFormat'
  { fromInputFormat ::
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

pattern InputFormat_CSV :: InputFormat
pattern InputFormat_CSV = InputFormat' "CSV"

pattern InputFormat_EXCEL :: InputFormat
pattern InputFormat_EXCEL = InputFormat' "EXCEL"

pattern InputFormat_JSON :: InputFormat
pattern InputFormat_JSON = InputFormat' "JSON"

pattern InputFormat_ORC :: InputFormat
pattern InputFormat_ORC = InputFormat' "ORC"

pattern InputFormat_PARQUET :: InputFormat
pattern InputFormat_PARQUET = InputFormat' "PARQUET"

{-# COMPLETE
  InputFormat_CSV,
  InputFormat_EXCEL,
  InputFormat_JSON,
  InputFormat_ORC,
  InputFormat_PARQUET,
  InputFormat'
  #-}
