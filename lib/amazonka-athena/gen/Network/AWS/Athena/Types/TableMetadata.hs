{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.TableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.TableMetadata
  ( TableMetadata (..),

    -- * Smart constructor
    mkTableMetadata,

    -- * Lenses
    tmTableType,
    tmName,
    tmParameters,
    tmColumns,
    tmLastAccessTime,
    tmPartitionKeys,
    tmCreateTime,
  )
where

import Network.AWS.Athena.Types.Column
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata for a table.
--
-- /See:/ 'mkTableMetadata' smart constructor.
data TableMetadata = TableMetadata'
  { -- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
    tableType :: Lude.Maybe Lude.Text,
    -- | The name of the table.
    name :: Lude.Text,
    -- | A set of custom key/value pairs for table properties.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of the columns in the table.
    columns :: Lude.Maybe [Column],
    -- | The last time the table was accessed.
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    -- | A list of the partition keys in the table.
    partitionKeys :: Lude.Maybe [Column],
    -- | The time that the table was created.
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableMetadata' with the minimum fields required to make a request.
--
-- * 'tableType' - The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
-- * 'name' - The name of the table.
-- * 'parameters' - A set of custom key/value pairs for table properties.
-- * 'columns' - A list of the columns in the table.
-- * 'lastAccessTime' - The last time the table was accessed.
-- * 'partitionKeys' - A list of the partition keys in the table.
-- * 'createTime' - The time that the table was created.
mkTableMetadata ::
  -- | 'name'
  Lude.Text ->
  TableMetadata
mkTableMetadata pName_ =
  TableMetadata'
    { tableType = Lude.Nothing,
      name = pName_,
      parameters = Lude.Nothing,
      columns = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      partitionKeys = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTableType :: Lens.Lens' TableMetadata (Lude.Maybe Lude.Text)
tmTableType = Lens.lens (tableType :: TableMetadata -> Lude.Maybe Lude.Text) (\s a -> s {tableType = a} :: TableMetadata)
{-# DEPRECATED tmTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmName :: Lens.Lens' TableMetadata Lude.Text
tmName = Lens.lens (name :: TableMetadata -> Lude.Text) (\s a -> s {name = a} :: TableMetadata)
{-# DEPRECATED tmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A set of custom key/value pairs for table properties.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmParameters :: Lens.Lens' TableMetadata (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tmParameters = Lens.lens (parameters :: TableMetadata -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: TableMetadata)
{-# DEPRECATED tmParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of the columns in the table.
--
-- /Note:/ Consider using 'columns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmColumns :: Lens.Lens' TableMetadata (Lude.Maybe [Column])
tmColumns = Lens.lens (columns :: TableMetadata -> Lude.Maybe [Column]) (\s a -> s {columns = a} :: TableMetadata)
{-# DEPRECATED tmColumns "Use generic-lens or generic-optics with 'columns' instead." #-}

-- | The last time the table was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmLastAccessTime :: Lens.Lens' TableMetadata (Lude.Maybe Lude.Timestamp)
tmLastAccessTime = Lens.lens (lastAccessTime :: TableMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: TableMetadata)
{-# DEPRECATED tmLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | A list of the partition keys in the table.
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmPartitionKeys :: Lens.Lens' TableMetadata (Lude.Maybe [Column])
tmPartitionKeys = Lens.lens (partitionKeys :: TableMetadata -> Lude.Maybe [Column]) (\s a -> s {partitionKeys = a} :: TableMetadata)
{-# DEPRECATED tmPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

-- | The time that the table was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCreateTime :: Lens.Lens' TableMetadata (Lude.Maybe Lude.Timestamp)
tmCreateTime = Lens.lens (createTime :: TableMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: TableMetadata)
{-# DEPRECATED tmCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON TableMetadata where
  parseJSON =
    Lude.withObject
      "TableMetadata"
      ( \x ->
          TableMetadata'
            Lude.<$> (x Lude..:? "TableType")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Columns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastAccessTime")
            Lude.<*> (x Lude..:? "PartitionKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreateTime")
      )
