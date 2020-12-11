-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CompressionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CompressionFormat
  ( CompressionFormat
      ( CompressionFormat',
        Gzip,
        HadoopSnappy,
        Snappy,
        Uncompressed,
        Zip
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CompressionFormat = CompressionFormat' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Gzip :: CompressionFormat
pattern Gzip = CompressionFormat' "GZIP"

pattern HadoopSnappy :: CompressionFormat
pattern HadoopSnappy = CompressionFormat' "HADOOP_SNAPPY"

pattern Snappy :: CompressionFormat
pattern Snappy = CompressionFormat' "Snappy"

pattern Uncompressed :: CompressionFormat
pattern Uncompressed = CompressionFormat' "UNCOMPRESSED"

pattern Zip :: CompressionFormat
pattern Zip = CompressionFormat' "ZIP"

{-# COMPLETE
  Gzip,
  HadoopSnappy,
  Snappy,
  Uncompressed,
  Zip,
  CompressionFormat'
  #-}
