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
-- Module      : Network.AWS.Firehose.Types.CompressionFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CompressionFormat
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

import qualified Network.AWS.Core as Core

newtype CompressionFormat = CompressionFormat'
  { fromCompressionFormat ::
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
