{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.CompressionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.CompressionFormat
  ( CompressionFormat
    ( CompressionFormat'
    , CompressionFormatZip
    , CompressionFormatGzip
    , CompressionFormatParquet
    , fromCompressionFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The compression format that AWS uses for the report.
newtype CompressionFormat = CompressionFormat'{fromCompressionFormat
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern CompressionFormatZip :: CompressionFormat
pattern CompressionFormatZip = CompressionFormat' "ZIP"

pattern CompressionFormatGzip :: CompressionFormat
pattern CompressionFormatGzip = CompressionFormat' "GZIP"

pattern CompressionFormatParquet :: CompressionFormat
pattern CompressionFormatParquet = CompressionFormat' "Parquet"

{-# COMPLETE 
  CompressionFormatZip,

  CompressionFormatGzip,

  CompressionFormatParquet,
  CompressionFormat'
  #-}
