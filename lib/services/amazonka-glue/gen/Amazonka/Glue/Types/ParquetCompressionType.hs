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
-- Module      : Amazonka.Glue.Types.ParquetCompressionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ParquetCompressionType
  ( ParquetCompressionType
      ( ..,
        ParquetCompressionType_Gzip,
        ParquetCompressionType_Lzo,
        ParquetCompressionType_None,
        ParquetCompressionType_Snappy,
        ParquetCompressionType_Uncompressed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParquetCompressionType = ParquetCompressionType'
  { fromParquetCompressionType ::
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

pattern ParquetCompressionType_Gzip :: ParquetCompressionType
pattern ParquetCompressionType_Gzip = ParquetCompressionType' "gzip"

pattern ParquetCompressionType_Lzo :: ParquetCompressionType
pattern ParquetCompressionType_Lzo = ParquetCompressionType' "lzo"

pattern ParquetCompressionType_None :: ParquetCompressionType
pattern ParquetCompressionType_None = ParquetCompressionType' "none"

pattern ParquetCompressionType_Snappy :: ParquetCompressionType
pattern ParquetCompressionType_Snappy = ParquetCompressionType' "snappy"

pattern ParquetCompressionType_Uncompressed :: ParquetCompressionType
pattern ParquetCompressionType_Uncompressed = ParquetCompressionType' "uncompressed"

{-# COMPLETE
  ParquetCompressionType_Gzip,
  ParquetCompressionType_Lzo,
  ParquetCompressionType_None,
  ParquetCompressionType_Snappy,
  ParquetCompressionType_Uncompressed,
  ParquetCompressionType'
  #-}
