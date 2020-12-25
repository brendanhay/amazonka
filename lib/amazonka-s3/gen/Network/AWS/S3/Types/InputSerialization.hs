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
    isCSV,
    isCompressionType,
    isJSON,
    isParquet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.CSVInput as Types
import qualified Network.AWS.S3.Types.CompressionType as Types
import qualified Network.AWS.S3.Types.JSONInput as Types
import qualified Network.AWS.S3.Types.ParquetInput as Types

-- | Describes the serialization format of the object.
--
-- /See:/ 'mkInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { -- | Describes the serialization of a CSV-encoded object.
    csv :: Core.Maybe Types.CSVInput,
    -- | Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
    compressionType :: Core.Maybe Types.CompressionType,
    -- | Specifies JSON as object's input serialization format.
    json :: Core.Maybe Types.JSONInput,
    -- | Specifies Parquet as object's input serialization format.
    parquet :: Core.Maybe Types.ParquetInput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSerialization' value with any optional fields omitted.
mkInputSerialization ::
  InputSerialization
mkInputSerialization =
  InputSerialization'
    { csv = Core.Nothing,
      compressionType = Core.Nothing,
      json = Core.Nothing,
      parquet = Core.Nothing
    }

-- | Describes the serialization of a CSV-encoded object.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCSV :: Lens.Lens' InputSerialization (Core.Maybe Types.CSVInput)
isCSV = Lens.field @"csv"
{-# DEPRECATED isCSV "Use generic-lens or generic-optics with 'csv' instead." #-}

-- | Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCompressionType :: Lens.Lens' InputSerialization (Core.Maybe Types.CompressionType)
isCompressionType = Lens.field @"compressionType"
{-# DEPRECATED isCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | Specifies JSON as object's input serialization format.
--
-- /Note:/ Consider using 'json' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isJSON :: Lens.Lens' InputSerialization (Core.Maybe Types.JSONInput)
isJSON = Lens.field @"json"
{-# DEPRECATED isJSON "Use generic-lens or generic-optics with 'json' instead." #-}

-- | Specifies Parquet as object's input serialization format.
--
-- /Note:/ Consider using 'parquet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isParquet :: Lens.Lens' InputSerialization (Core.Maybe Types.ParquetInput)
isParquet = Lens.field @"parquet"
{-# DEPRECATED isParquet "Use generic-lens or generic-optics with 'parquet' instead." #-}

instance Core.ToXML InputSerialization where
  toXML InputSerialization {..} =
    Core.toXMLNode "CSV" Core.<$> csv
      Core.<> Core.toXMLNode "CompressionType" Core.<$> compressionType
      Core.<> Core.toXMLNode "JSON" Core.<$> json
      Core.<> Core.toXMLNode "Parquet" Core.<$> parquet
