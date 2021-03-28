{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CompressionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.CompressionFormat
  ( CompressionFormat
    ( CompressionFormat'
    , CompressionFormatUncompressed
    , CompressionFormatGzip
    , CompressionFormatZip
    , CompressionFormatSnappy
    , CompressionFormatHadoopSnappy
    , fromCompressionFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CompressionFormat = CompressionFormat'{fromCompressionFormat
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern CompressionFormatUncompressed :: CompressionFormat
pattern CompressionFormatUncompressed = CompressionFormat' "UNCOMPRESSED"

pattern CompressionFormatGzip :: CompressionFormat
pattern CompressionFormatGzip = CompressionFormat' "GZIP"

pattern CompressionFormatZip :: CompressionFormat
pattern CompressionFormatZip = CompressionFormat' "ZIP"

pattern CompressionFormatSnappy :: CompressionFormat
pattern CompressionFormatSnappy = CompressionFormat' "Snappy"

pattern CompressionFormatHadoopSnappy :: CompressionFormat
pattern CompressionFormatHadoopSnappy = CompressionFormat' "HADOOP_SNAPPY"

{-# COMPLETE 
  CompressionFormatUncompressed,

  CompressionFormatGzip,

  CompressionFormatZip,

  CompressionFormatSnappy,

  CompressionFormatHadoopSnappy,
  CompressionFormat'
  #-}
