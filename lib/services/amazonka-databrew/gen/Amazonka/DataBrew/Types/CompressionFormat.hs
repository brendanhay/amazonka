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
-- Module      : Amazonka.DataBrew.Types.CompressionFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.CompressionFormat
  ( CompressionFormat
      ( ..,
        CompressionFormat_BROTLI,
        CompressionFormat_BZIP2,
        CompressionFormat_DEFLATE,
        CompressionFormat_GZIP,
        CompressionFormat_LZ4,
        CompressionFormat_LZO,
        CompressionFormat_SNAPPY,
        CompressionFormat_ZLIB,
        CompressionFormat_ZSTD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CompressionFormat = CompressionFormat'
  { fromCompressionFormat ::
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

pattern CompressionFormat_BROTLI :: CompressionFormat
pattern CompressionFormat_BROTLI = CompressionFormat' "BROTLI"

pattern CompressionFormat_BZIP2 :: CompressionFormat
pattern CompressionFormat_BZIP2 = CompressionFormat' "BZIP2"

pattern CompressionFormat_DEFLATE :: CompressionFormat
pattern CompressionFormat_DEFLATE = CompressionFormat' "DEFLATE"

pattern CompressionFormat_GZIP :: CompressionFormat
pattern CompressionFormat_GZIP = CompressionFormat' "GZIP"

pattern CompressionFormat_LZ4 :: CompressionFormat
pattern CompressionFormat_LZ4 = CompressionFormat' "LZ4"

pattern CompressionFormat_LZO :: CompressionFormat
pattern CompressionFormat_LZO = CompressionFormat' "LZO"

pattern CompressionFormat_SNAPPY :: CompressionFormat
pattern CompressionFormat_SNAPPY = CompressionFormat' "SNAPPY"

pattern CompressionFormat_ZLIB :: CompressionFormat
pattern CompressionFormat_ZLIB = CompressionFormat' "ZLIB"

pattern CompressionFormat_ZSTD :: CompressionFormat
pattern CompressionFormat_ZSTD = CompressionFormat' "ZSTD"

{-# COMPLETE
  CompressionFormat_BROTLI,
  CompressionFormat_BZIP2,
  CompressionFormat_DEFLATE,
  CompressionFormat_GZIP,
  CompressionFormat_LZ4,
  CompressionFormat_LZO,
  CompressionFormat_SNAPPY,
  CompressionFormat_ZLIB,
  CompressionFormat_ZSTD,
  CompressionFormat'
  #-}
