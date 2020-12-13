{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Uncompressed,
        Gzip,
        Zip,
        Snappy,
        HadoopSnappy
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

pattern Uncompressed :: CompressionFormat
pattern Uncompressed = CompressionFormat' "UNCOMPRESSED"

pattern Gzip :: CompressionFormat
pattern Gzip = CompressionFormat' "GZIP"

pattern Zip :: CompressionFormat
pattern Zip = CompressionFormat' "ZIP"

pattern Snappy :: CompressionFormat
pattern Snappy = CompressionFormat' "Snappy"

pattern HadoopSnappy :: CompressionFormat
pattern HadoopSnappy = CompressionFormat' "HADOOP_SNAPPY"

{-# COMPLETE
  Uncompressed,
  Gzip,
  Zip,
  Snappy,
  HadoopSnappy,
  CompressionFormat'
  #-}
