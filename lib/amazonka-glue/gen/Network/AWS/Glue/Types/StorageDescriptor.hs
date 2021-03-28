{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.StorageDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.StorageDescriptor
  ( StorageDescriptor (..)
  -- * Smart constructor
  , mkStorageDescriptor
  -- * Lenses
  , sdBucketColumns
  , sdColumns
  , sdCompressed
  , sdInputFormat
  , sdLocation
  , sdNumberOfBuckets
  , sdOutputFormat
  , sdParameters
  , sdSchemaReference
  , sdSerdeInfo
  , sdSkewedInfo
  , sdSortColumns
  , sdStoredAsSubDirectories
  ) where

import qualified Network.AWS.Glue.Types.Column as Types
import qualified Network.AWS.Glue.Types.InputFormat as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.LocationString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.Order as Types
import qualified Network.AWS.Glue.Types.OutputFormat as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.SchemaReference as Types
import qualified Network.AWS.Glue.Types.SerDeInfo as Types
import qualified Network.AWS.Glue.Types.SkewedInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the physical storage of table data.
--
-- /See:/ 'mkStorageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { bucketColumns :: Core.Maybe [Types.NameString]
    -- ^ A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
  , columns :: Core.Maybe [Types.Column]
    -- ^ A list of the @Columns@ in the table.
  , compressed :: Core.Maybe Core.Bool
    -- ^ @True@ if the data in the table is compressed, or @False@ if not.
  , inputFormat :: Core.Maybe Types.InputFormat
    -- ^ The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
  , location :: Core.Maybe Types.LocationString
    -- ^ The physical location of the table. By default, this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
  , numberOfBuckets :: Core.Maybe Core.Int
    -- ^ Must be specified if the table contains any dimension columns.
  , outputFormat :: Core.Maybe Types.OutputFormat
    -- ^ The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
  , parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
    -- ^ The user-supplied properties in key-value form.
  , schemaReference :: Core.Maybe Types.SchemaReference
    -- ^ An object that references a schema stored in the AWS Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the schema, and instead use a schema reference.
  , serdeInfo :: Core.Maybe Types.SerDeInfo
    -- ^ The serialization/deserialization (SerDe) information.
  , skewedInfo :: Core.Maybe Types.SkewedInfo
    -- ^ The information about values that appear frequently in a column (skewed values).
  , sortColumns :: Core.Maybe [Types.Order]
    -- ^ A list specifying the sort order of each bucket in the table.
  , storedAsSubDirectories :: Core.Maybe Core.Bool
    -- ^ @True@ if the table data is stored in subdirectories, or @False@ if not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageDescriptor' value with any optional fields omitted.
mkStorageDescriptor
    :: StorageDescriptor
mkStorageDescriptor
  = StorageDescriptor'{bucketColumns = Core.Nothing,
                       columns = Core.Nothing, compressed = Core.Nothing,
                       inputFormat = Core.Nothing, location = Core.Nothing,
                       numberOfBuckets = Core.Nothing, outputFormat = Core.Nothing,
                       parameters = Core.Nothing, schemaReference = Core.Nothing,
                       serdeInfo = Core.Nothing, skewedInfo = Core.Nothing,
                       sortColumns = Core.Nothing, storedAsSubDirectories = Core.Nothing}

-- | A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
--
-- /Note:/ Consider using 'bucketColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucketColumns :: Lens.Lens' StorageDescriptor (Core.Maybe [Types.NameString])
sdBucketColumns = Lens.field @"bucketColumns"
{-# INLINEABLE sdBucketColumns #-}
{-# DEPRECATED bucketColumns "Use generic-lens or generic-optics with 'bucketColumns' instead"  #-}

-- | A list of the @Columns@ in the table.
--
-- /Note:/ Consider using 'columns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdColumns :: Lens.Lens' StorageDescriptor (Core.Maybe [Types.Column])
sdColumns = Lens.field @"columns"
{-# INLINEABLE sdColumns #-}
{-# DEPRECATED columns "Use generic-lens or generic-optics with 'columns' instead"  #-}

-- | @True@ if the data in the table is compressed, or @False@ if not.
--
-- /Note:/ Consider using 'compressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCompressed :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Bool)
sdCompressed = Lens.field @"compressed"
{-# INLINEABLE sdCompressed #-}
{-# DEPRECATED compressed "Use generic-lens or generic-optics with 'compressed' instead"  #-}

-- | The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
--
-- /Note:/ Consider using 'inputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInputFormat :: Lens.Lens' StorageDescriptor (Core.Maybe Types.InputFormat)
sdInputFormat = Lens.field @"inputFormat"
{-# INLINEABLE sdInputFormat #-}
{-# DEPRECATED inputFormat "Use generic-lens or generic-optics with 'inputFormat' instead"  #-}

-- | The physical location of the table. By default, this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLocation :: Lens.Lens' StorageDescriptor (Core.Maybe Types.LocationString)
sdLocation = Lens.field @"location"
{-# INLINEABLE sdLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | Must be specified if the table contains any dimension columns.
--
-- /Note:/ Consider using 'numberOfBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNumberOfBuckets :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Int)
sdNumberOfBuckets = Lens.field @"numberOfBuckets"
{-# INLINEABLE sdNumberOfBuckets #-}
{-# DEPRECATED numberOfBuckets "Use generic-lens or generic-optics with 'numberOfBuckets' instead"  #-}

-- | The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOutputFormat :: Lens.Lens' StorageDescriptor (Core.Maybe Types.OutputFormat)
sdOutputFormat = Lens.field @"outputFormat"
{-# INLINEABLE sdOutputFormat #-}
{-# DEPRECATED outputFormat "Use generic-lens or generic-optics with 'outputFormat' instead"  #-}

-- | The user-supplied properties in key-value form.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdParameters :: Lens.Lens' StorageDescriptor (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
sdParameters = Lens.field @"parameters"
{-# INLINEABLE sdParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | An object that references a schema stored in the AWS Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the schema, and instead use a schema reference.
--
-- /Note:/ Consider using 'schemaReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSchemaReference :: Lens.Lens' StorageDescriptor (Core.Maybe Types.SchemaReference)
sdSchemaReference = Lens.field @"schemaReference"
{-# INLINEABLE sdSchemaReference #-}
{-# DEPRECATED schemaReference "Use generic-lens or generic-optics with 'schemaReference' instead"  #-}

-- | The serialization/deserialization (SerDe) information.
--
-- /Note:/ Consider using 'serdeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSerdeInfo :: Lens.Lens' StorageDescriptor (Core.Maybe Types.SerDeInfo)
sdSerdeInfo = Lens.field @"serdeInfo"
{-# INLINEABLE sdSerdeInfo #-}
{-# DEPRECATED serdeInfo "Use generic-lens or generic-optics with 'serdeInfo' instead"  #-}

-- | The information about values that appear frequently in a column (skewed values).
--
-- /Note:/ Consider using 'skewedInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSkewedInfo :: Lens.Lens' StorageDescriptor (Core.Maybe Types.SkewedInfo)
sdSkewedInfo = Lens.field @"skewedInfo"
{-# INLINEABLE sdSkewedInfo #-}
{-# DEPRECATED skewedInfo "Use generic-lens or generic-optics with 'skewedInfo' instead"  #-}

-- | A list specifying the sort order of each bucket in the table.
--
-- /Note:/ Consider using 'sortColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSortColumns :: Lens.Lens' StorageDescriptor (Core.Maybe [Types.Order])
sdSortColumns = Lens.field @"sortColumns"
{-# INLINEABLE sdSortColumns #-}
{-# DEPRECATED sortColumns "Use generic-lens or generic-optics with 'sortColumns' instead"  #-}

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
--
-- /Note:/ Consider using 'storedAsSubDirectories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStoredAsSubDirectories :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Bool)
sdStoredAsSubDirectories = Lens.field @"storedAsSubDirectories"
{-# INLINEABLE sdStoredAsSubDirectories #-}
{-# DEPRECATED storedAsSubDirectories "Use generic-lens or generic-optics with 'storedAsSubDirectories' instead"  #-}

instance Core.FromJSON StorageDescriptor where
        toJSON StorageDescriptor{..}
          = Core.object
              (Core.catMaybes
                 [("BucketColumns" Core..=) Core.<$> bucketColumns,
                  ("Columns" Core..=) Core.<$> columns,
                  ("Compressed" Core..=) Core.<$> compressed,
                  ("InputFormat" Core..=) Core.<$> inputFormat,
                  ("Location" Core..=) Core.<$> location,
                  ("NumberOfBuckets" Core..=) Core.<$> numberOfBuckets,
                  ("OutputFormat" Core..=) Core.<$> outputFormat,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("SchemaReference" Core..=) Core.<$> schemaReference,
                  ("SerdeInfo" Core..=) Core.<$> serdeInfo,
                  ("SkewedInfo" Core..=) Core.<$> skewedInfo,
                  ("SortColumns" Core..=) Core.<$> sortColumns,
                  ("StoredAsSubDirectories" Core..=) Core.<$>
                    storedAsSubDirectories])

instance Core.FromJSON StorageDescriptor where
        parseJSON
          = Core.withObject "StorageDescriptor" Core.$
              \ x ->
                StorageDescriptor' Core.<$>
                  (x Core..:? "BucketColumns") Core.<*> x Core..:? "Columns" Core.<*>
                    x Core..:? "Compressed"
                    Core.<*> x Core..:? "InputFormat"
                    Core.<*> x Core..:? "Location"
                    Core.<*> x Core..:? "NumberOfBuckets"
                    Core.<*> x Core..:? "OutputFormat"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "SchemaReference"
                    Core.<*> x Core..:? "SerdeInfo"
                    Core.<*> x Core..:? "SkewedInfo"
                    Core.<*> x Core..:? "SortColumns"
                    Core.<*> x Core..:? "StoredAsSubDirectories"
