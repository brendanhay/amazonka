{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ParquetCompression = ParquetCompression'
  { fromParquetCompression ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
