{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InputSerialization
  ( InputSerialization (..),

    -- * Smart constructor
    mkInputSerialization,

    -- * Lenses
    isJSON,
    isCSV,
    isParquet,
    isCompressionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CSVInput
import Network.AWS.S3.Types.CompressionType
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.ParquetInput

-- | Describes the serialization format of the object.
--
-- /See:/ 'mkInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { json ::
      Lude.Maybe JSONInput,
    csv :: Lude.Maybe CSVInput,
    parquet :: Lude.Maybe ParquetInput,
    compressionType :: Lude.Maybe CompressionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- * 'compressionType' - Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
-- * 'csv' - Describes the serialization of a CSV-encoded object.
-- * 'json' - Specifies JSON as object's input serialization format.
-- * 'parquet' - Specifies Parquet as object's input serialization format.
mkInputSerialization ::
  InputSerialization
mkInputSerialization =
  InputSerialization'
    { json = Lude.Nothing,
      csv = Lude.Nothing,
      parquet = Lude.Nothing,
      compressionType = Lude.Nothing
    }

-- | Specifies JSON as object's input serialization format.
--
-- /Note:/ Consider using 'json' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isJSON :: Lens.Lens' InputSerialization (Lude.Maybe JSONInput)
isJSON = Lens.lens (json :: InputSerialization -> Lude.Maybe JSONInput) (\s a -> s {json = a} :: InputSerialization)
{-# DEPRECATED isJSON "Use generic-lens or generic-optics with 'json' instead." #-}

-- | Describes the serialization of a CSV-encoded object.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCSV :: Lens.Lens' InputSerialization (Lude.Maybe CSVInput)
isCSV = Lens.lens (csv :: InputSerialization -> Lude.Maybe CSVInput) (\s a -> s {csv = a} :: InputSerialization)
{-# DEPRECATED isCSV "Use generic-lens or generic-optics with 'csv' instead." #-}

-- | Specifies Parquet as object's input serialization format.
--
-- /Note:/ Consider using 'parquet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isParquet :: Lens.Lens' InputSerialization (Lude.Maybe ParquetInput)
isParquet = Lens.lens (parquet :: InputSerialization -> Lude.Maybe ParquetInput) (\s a -> s {parquet = a} :: InputSerialization)
{-# DEPRECATED isParquet "Use generic-lens or generic-optics with 'parquet' instead." #-}

-- | Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCompressionType :: Lens.Lens' InputSerialization (Lude.Maybe CompressionType)
isCompressionType = Lens.lens (compressionType :: InputSerialization -> Lude.Maybe CompressionType) (\s a -> s {compressionType = a} :: InputSerialization)
{-# DEPRECATED isCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

instance Lude.ToXML InputSerialization where
  toXML InputSerialization' {..} =
    Lude.mconcat
      [ "JSON" Lude.@= json,
        "CSV" Lude.@= csv,
        "Parquet" Lude.@= parquet,
        "CompressionType" Lude.@= compressionType
      ]
