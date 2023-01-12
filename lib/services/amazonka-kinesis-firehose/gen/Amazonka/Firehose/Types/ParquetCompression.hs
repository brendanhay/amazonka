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
-- Module      : Amazonka.Firehose.Types.ParquetCompression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ParquetCompression
  ( ParquetCompression
      ( ..,
        ParquetCompression_GZIP,
        ParquetCompression_SNAPPY,
        ParquetCompression_UNCOMPRESSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParquetCompression = ParquetCompression'
  { fromParquetCompression ::
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

pattern ParquetCompression_GZIP :: ParquetCompression
pattern ParquetCompression_GZIP = ParquetCompression' "GZIP"

pattern ParquetCompression_SNAPPY :: ParquetCompression
pattern ParquetCompression_SNAPPY = ParquetCompression' "SNAPPY"

pattern ParquetCompression_UNCOMPRESSED :: ParquetCompression
pattern ParquetCompression_UNCOMPRESSED = ParquetCompression' "UNCOMPRESSED"

{-# COMPLETE
  ParquetCompression_GZIP,
  ParquetCompression_SNAPPY,
  ParquetCompression_UNCOMPRESSED,
  ParquetCompression'
  #-}
