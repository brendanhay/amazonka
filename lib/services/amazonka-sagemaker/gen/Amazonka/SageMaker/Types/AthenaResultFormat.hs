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
-- Module      : Amazonka.SageMaker.Types.AthenaResultFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AthenaResultFormat
  ( AthenaResultFormat
      ( ..,
        AthenaResultFormat_AVRO,
        AthenaResultFormat_JSON,
        AthenaResultFormat_ORC,
        AthenaResultFormat_PARQUET,
        AthenaResultFormat_TEXTFILE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data storage format for Athena query results.
newtype AthenaResultFormat = AthenaResultFormat'
  { fromAthenaResultFormat ::
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

pattern AthenaResultFormat_AVRO :: AthenaResultFormat
pattern AthenaResultFormat_AVRO = AthenaResultFormat' "AVRO"

pattern AthenaResultFormat_JSON :: AthenaResultFormat
pattern AthenaResultFormat_JSON = AthenaResultFormat' "JSON"

pattern AthenaResultFormat_ORC :: AthenaResultFormat
pattern AthenaResultFormat_ORC = AthenaResultFormat' "ORC"

pattern AthenaResultFormat_PARQUET :: AthenaResultFormat
pattern AthenaResultFormat_PARQUET = AthenaResultFormat' "PARQUET"

pattern AthenaResultFormat_TEXTFILE :: AthenaResultFormat
pattern AthenaResultFormat_TEXTFILE = AthenaResultFormat' "TEXTFILE"

{-# COMPLETE
  AthenaResultFormat_AVRO,
  AthenaResultFormat_JSON,
  AthenaResultFormat_ORC,
  AthenaResultFormat_PARQUET,
  AthenaResultFormat_TEXTFILE,
  AthenaResultFormat'
  #-}
