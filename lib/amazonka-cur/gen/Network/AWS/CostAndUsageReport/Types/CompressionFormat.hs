{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.CompressionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.CompressionFormat
  ( CompressionFormat
      ( CompressionFormat',
        CFGzip,
        CFParquet,
        CFZip
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The compression format that AWS uses for the report.
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

pattern CFGzip :: CompressionFormat
pattern CFGzip = CompressionFormat' "GZIP"

pattern CFParquet :: CompressionFormat
pattern CFParquet = CompressionFormat' "Parquet"

pattern CFZip :: CompressionFormat
pattern CFZip = CompressionFormat' "ZIP"

{-# COMPLETE
  CFGzip,
  CFParquet,
  CFZip,
  CompressionFormat'
  #-}
