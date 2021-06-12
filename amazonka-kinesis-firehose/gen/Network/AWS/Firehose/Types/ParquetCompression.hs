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
-- Module      : Network.AWS.Firehose.Types.ParquetCompression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetCompression
  ( ParquetCompression
      ( ..,
        ParquetCompression_GZIP,
        ParquetCompression_SNAPPY,
        ParquetCompression_UNCOMPRESSED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParquetCompression = ParquetCompression'
  { fromParquetCompression ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
