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
-- Module      : Amazonka.Firehose.Types.CompressionFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.CompressionFormat
  ( CompressionFormat
      ( ..,
        CompressionFormat_GZIP,
        CompressionFormat_HADOOP_SNAPPY,
        CompressionFormat_Snappy,
        CompressionFormat_UNCOMPRESSED,
        CompressionFormat_ZIP
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

pattern CompressionFormat_GZIP :: CompressionFormat
pattern CompressionFormat_GZIP = CompressionFormat' "GZIP"

pattern CompressionFormat_HADOOP_SNAPPY :: CompressionFormat
pattern CompressionFormat_HADOOP_SNAPPY = CompressionFormat' "HADOOP_SNAPPY"

pattern CompressionFormat_Snappy :: CompressionFormat
pattern CompressionFormat_Snappy = CompressionFormat' "Snappy"

pattern CompressionFormat_UNCOMPRESSED :: CompressionFormat
pattern CompressionFormat_UNCOMPRESSED = CompressionFormat' "UNCOMPRESSED"

pattern CompressionFormat_ZIP :: CompressionFormat
pattern CompressionFormat_ZIP = CompressionFormat' "ZIP"

{-# COMPLETE
  CompressionFormat_GZIP,
  CompressionFormat_HADOOP_SNAPPY,
  CompressionFormat_Snappy,
  CompressionFormat_UNCOMPRESSED,
  CompressionFormat_ZIP,
  CompressionFormat'
  #-}
