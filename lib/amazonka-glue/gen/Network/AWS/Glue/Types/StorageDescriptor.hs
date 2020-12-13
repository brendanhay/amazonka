{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.StorageDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StorageDescriptor
  ( StorageDescriptor (..),

    -- * Smart constructor
    mkStorageDescriptor,

    -- * Lenses
    sdSortColumns,
    sdCompressed,
    sdLocation,
    sdBucketColumns,
    sdSerdeInfo,
    sdOutputFormat,
    sdNumberOfBuckets,
    sdSchemaReference,
    sdStoredAsSubDirectories,
    sdParameters,
    sdInputFormat,
    sdSkewedInfo,
    sdColumns,
  )
where

import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.Order
import Network.AWS.Glue.Types.SchemaReference
import Network.AWS.Glue.Types.SerDeInfo
import Network.AWS.Glue.Types.SkewedInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the physical storage of table data.
--
-- /See:/ 'mkStorageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { -- | A list specifying the sort order of each bucket in the table.
    sortColumns :: Lude.Maybe [Order],
    -- | @True@ if the data in the table is compressed, or @False@ if not.
    compressed :: Lude.Maybe Lude.Bool,
    -- | The physical location of the table. By default, this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
    location :: Lude.Maybe Lude.Text,
    -- | A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
    bucketColumns :: Lude.Maybe [Lude.Text],
    -- | The serialization/deserialization (SerDe) information.
    serdeInfo :: Lude.Maybe SerDeInfo,
    -- | The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
    outputFormat :: Lude.Maybe Lude.Text,
    -- | Must be specified if the table contains any dimension columns.
    numberOfBuckets :: Lude.Maybe Lude.Int,
    -- | An object that references a schema stored in the AWS Glue Schema Registry.
    --
    -- When creating a table, you can pass an empty list of columns for the schema, and instead use a schema reference.
    schemaReference :: Lude.Maybe SchemaReference,
    -- | @True@ if the table data is stored in subdirectories, or @False@ if not.
    storedAsSubDirectories :: Lude.Maybe Lude.Bool,
    -- | The user-supplied properties in key-value form.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
    inputFormat :: Lude.Maybe Lude.Text,
    -- | The information about values that appear frequently in a column (skewed values).
    skewedInfo :: Lude.Maybe SkewedInfo,
    -- | A list of the @Columns@ in the table.
    columns :: Lude.Maybe [Column]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageDescriptor' with the minimum fields required to make a request.
--
-- * 'sortColumns' - A list specifying the sort order of each bucket in the table.
-- * 'compressed' - @True@ if the data in the table is compressed, or @False@ if not.
-- * 'location' - The physical location of the table. By default, this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
-- * 'bucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
-- * 'serdeInfo' - The serialization/deserialization (SerDe) information.
-- * 'outputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
-- * 'numberOfBuckets' - Must be specified if the table contains any dimension columns.
-- * 'schemaReference' - An object that references a schema stored in the AWS Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the schema, and instead use a schema reference.
-- * 'storedAsSubDirectories' - @True@ if the table data is stored in subdirectories, or @False@ if not.
-- * 'parameters' - The user-supplied properties in key-value form.
-- * 'inputFormat' - The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
-- * 'skewedInfo' - The information about values that appear frequently in a column (skewed values).
-- * 'columns' - A list of the @Columns@ in the table.
mkStorageDescriptor ::
  StorageDescriptor
mkStorageDescriptor =
  StorageDescriptor'
    { sortColumns = Lude.Nothing,
      compressed = Lude.Nothing,
      location = Lude.Nothing,
      bucketColumns = Lude.Nothing,
      serdeInfo = Lude.Nothing,
      outputFormat = Lude.Nothing,
      numberOfBuckets = Lude.Nothing,
      schemaReference = Lude.Nothing,
      storedAsSubDirectories = Lude.Nothing,
      parameters = Lude.Nothing,
      inputFormat = Lude.Nothing,
      skewedInfo = Lude.Nothing,
      columns = Lude.Nothing
    }

-- | A list specifying the sort order of each bucket in the table.
--
-- /Note:/ Consider using 'sortColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSortColumns :: Lens.Lens' StorageDescriptor (Lude.Maybe [Order])
sdSortColumns = Lens.lens (sortColumns :: StorageDescriptor -> Lude.Maybe [Order]) (\s a -> s {sortColumns = a} :: StorageDescriptor)
{-# DEPRECATED sdSortColumns "Use generic-lens or generic-optics with 'sortColumns' instead." #-}

-- | @True@ if the data in the table is compressed, or @False@ if not.
--
-- /Note:/ Consider using 'compressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCompressed :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Bool)
sdCompressed = Lens.lens (compressed :: StorageDescriptor -> Lude.Maybe Lude.Bool) (\s a -> s {compressed = a} :: StorageDescriptor)
{-# DEPRECATED sdCompressed "Use generic-lens or generic-optics with 'compressed' instead." #-}

-- | The physical location of the table. By default, this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLocation :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Text)
sdLocation = Lens.lens (location :: StorageDescriptor -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: StorageDescriptor)
{-# DEPRECATED sdLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
--
-- /Note:/ Consider using 'bucketColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucketColumns :: Lens.Lens' StorageDescriptor (Lude.Maybe [Lude.Text])
sdBucketColumns = Lens.lens (bucketColumns :: StorageDescriptor -> Lude.Maybe [Lude.Text]) (\s a -> s {bucketColumns = a} :: StorageDescriptor)
{-# DEPRECATED sdBucketColumns "Use generic-lens or generic-optics with 'bucketColumns' instead." #-}

-- | The serialization/deserialization (SerDe) information.
--
-- /Note:/ Consider using 'serdeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSerdeInfo :: Lens.Lens' StorageDescriptor (Lude.Maybe SerDeInfo)
sdSerdeInfo = Lens.lens (serdeInfo :: StorageDescriptor -> Lude.Maybe SerDeInfo) (\s a -> s {serdeInfo = a} :: StorageDescriptor)
{-# DEPRECATED sdSerdeInfo "Use generic-lens or generic-optics with 'serdeInfo' instead." #-}

-- | The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOutputFormat :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Text)
sdOutputFormat = Lens.lens (outputFormat :: StorageDescriptor -> Lude.Maybe Lude.Text) (\s a -> s {outputFormat = a} :: StorageDescriptor)
{-# DEPRECATED sdOutputFormat "Use generic-lens or generic-optics with 'outputFormat' instead." #-}

-- | Must be specified if the table contains any dimension columns.
--
-- /Note:/ Consider using 'numberOfBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNumberOfBuckets :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Int)
sdNumberOfBuckets = Lens.lens (numberOfBuckets :: StorageDescriptor -> Lude.Maybe Lude.Int) (\s a -> s {numberOfBuckets = a} :: StorageDescriptor)
{-# DEPRECATED sdNumberOfBuckets "Use generic-lens or generic-optics with 'numberOfBuckets' instead." #-}

-- | An object that references a schema stored in the AWS Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the schema, and instead use a schema reference.
--
-- /Note:/ Consider using 'schemaReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSchemaReference :: Lens.Lens' StorageDescriptor (Lude.Maybe SchemaReference)
sdSchemaReference = Lens.lens (schemaReference :: StorageDescriptor -> Lude.Maybe SchemaReference) (\s a -> s {schemaReference = a} :: StorageDescriptor)
{-# DEPRECATED sdSchemaReference "Use generic-lens or generic-optics with 'schemaReference' instead." #-}

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
--
-- /Note:/ Consider using 'storedAsSubDirectories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStoredAsSubDirectories :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Bool)
sdStoredAsSubDirectories = Lens.lens (storedAsSubDirectories :: StorageDescriptor -> Lude.Maybe Lude.Bool) (\s a -> s {storedAsSubDirectories = a} :: StorageDescriptor)
{-# DEPRECATED sdStoredAsSubDirectories "Use generic-lens or generic-optics with 'storedAsSubDirectories' instead." #-}

-- | The user-supplied properties in key-value form.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdParameters :: Lens.Lens' StorageDescriptor (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sdParameters = Lens.lens (parameters :: StorageDescriptor -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: StorageDescriptor)
{-# DEPRECATED sdParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
--
-- /Note:/ Consider using 'inputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInputFormat :: Lens.Lens' StorageDescriptor (Lude.Maybe Lude.Text)
sdInputFormat = Lens.lens (inputFormat :: StorageDescriptor -> Lude.Maybe Lude.Text) (\s a -> s {inputFormat = a} :: StorageDescriptor)
{-# DEPRECATED sdInputFormat "Use generic-lens or generic-optics with 'inputFormat' instead." #-}

-- | The information about values that appear frequently in a column (skewed values).
--
-- /Note:/ Consider using 'skewedInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSkewedInfo :: Lens.Lens' StorageDescriptor (Lude.Maybe SkewedInfo)
sdSkewedInfo = Lens.lens (skewedInfo :: StorageDescriptor -> Lude.Maybe SkewedInfo) (\s a -> s {skewedInfo = a} :: StorageDescriptor)
{-# DEPRECATED sdSkewedInfo "Use generic-lens or generic-optics with 'skewedInfo' instead." #-}

-- | A list of the @Columns@ in the table.
--
-- /Note:/ Consider using 'columns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdColumns :: Lens.Lens' StorageDescriptor (Lude.Maybe [Column])
sdColumns = Lens.lens (columns :: StorageDescriptor -> Lude.Maybe [Column]) (\s a -> s {columns = a} :: StorageDescriptor)
{-# DEPRECATED sdColumns "Use generic-lens or generic-optics with 'columns' instead." #-}

instance Lude.FromJSON StorageDescriptor where
  parseJSON =
    Lude.withObject
      "StorageDescriptor"
      ( \x ->
          StorageDescriptor'
            Lude.<$> (x Lude..:? "SortColumns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Compressed")
            Lude.<*> (x Lude..:? "Location")
            Lude.<*> (x Lude..:? "BucketColumns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SerdeInfo")
            Lude.<*> (x Lude..:? "OutputFormat")
            Lude.<*> (x Lude..:? "NumberOfBuckets")
            Lude.<*> (x Lude..:? "SchemaReference")
            Lude.<*> (x Lude..:? "StoredAsSubDirectories")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InputFormat")
            Lude.<*> (x Lude..:? "SkewedInfo")
            Lude.<*> (x Lude..:? "Columns" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON StorageDescriptor where
  toJSON StorageDescriptor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SortColumns" Lude..=) Lude.<$> sortColumns,
            ("Compressed" Lude..=) Lude.<$> compressed,
            ("Location" Lude..=) Lude.<$> location,
            ("BucketColumns" Lude..=) Lude.<$> bucketColumns,
            ("SerdeInfo" Lude..=) Lude.<$> serdeInfo,
            ("OutputFormat" Lude..=) Lude.<$> outputFormat,
            ("NumberOfBuckets" Lude..=) Lude.<$> numberOfBuckets,
            ("SchemaReference" Lude..=) Lude.<$> schemaReference,
            ("StoredAsSubDirectories" Lude..=) Lude.<$> storedAsSubDirectories,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("InputFormat" Lude..=) Lude.<$> inputFormat,
            ("SkewedInfo" Lude..=) Lude.<$> skewedInfo,
            ("Columns" Lude..=) Lude.<$> columns
          ]
      )
