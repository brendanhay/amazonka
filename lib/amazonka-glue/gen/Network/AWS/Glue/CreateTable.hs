{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table definition in the Data Catalog.
module Network.AWS.Glue.CreateTable
  ( -- * Creating a request
    CreateTable (..),
    mkCreateTable,

    -- ** Request lenses
    cPartitionIndexes,
    cCatalogId,
    cDatabaseName,
    cTableInput,

    -- * Destructuring the response
    CreateTableResponse (..),
    mkCreateTableResponse,

    -- ** Response lenses
    ctfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | A list of partition indexes, @PartitionIndex@ structures, to create in the table.
    partitionIndexes :: Lude.Maybe [PartitionIndex],
    -- | The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text,
    -- | The @TableInput@ object that defines the metadata table to create in the catalog.
    tableInput :: TableInput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTable' with the minimum fields required to make a request.
--
-- * 'partitionIndexes' - A list of partition indexes, @PartitionIndex@ structures, to create in the table.
-- * 'catalogId' - The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
-- * 'tableInput' - The @TableInput@ object that defines the metadata table to create in the catalog.
mkCreateTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableInput'
  TableInput ->
  CreateTable
mkCreateTable pDatabaseName_ pTableInput_ =
  CreateTable'
    { partitionIndexes = Lude.Nothing,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableInput = pTableInput_
    }

-- | A list of partition indexes, @PartitionIndex@ structures, to create in the table.
--
-- /Note:/ Consider using 'partitionIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPartitionIndexes :: Lens.Lens' CreateTable (Lude.Maybe [PartitionIndex])
cPartitionIndexes = Lens.lens (partitionIndexes :: CreateTable -> Lude.Maybe [PartitionIndex]) (\s a -> s {partitionIndexes = a} :: CreateTable)
{-# DEPRECATED cPartitionIndexes "Use generic-lens or generic-optics with 'partitionIndexes' instead." #-}

-- | The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCatalogId :: Lens.Lens' CreateTable (Lude.Maybe Lude.Text)
cCatalogId = Lens.lens (catalogId :: CreateTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreateTable)
{-# DEPRECATED cCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDatabaseName :: Lens.Lens' CreateTable Lude.Text
cDatabaseName = Lens.lens (databaseName :: CreateTable -> Lude.Text) (\s a -> s {databaseName = a} :: CreateTable)
{-# DEPRECATED cDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The @TableInput@ object that defines the metadata table to create in the catalog.
--
-- /Note:/ Consider using 'tableInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTableInput :: Lens.Lens' CreateTable TableInput
cTableInput = Lens.lens (tableInput :: CreateTable -> TableInput) (\s a -> s {tableInput = a} :: CreateTable)
{-# DEPRECATED cTableInput "Use generic-lens or generic-optics with 'tableInput' instead." #-}

instance Lude.AWSRequest CreateTable where
  type Rs CreateTable = CreateTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateTableResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.CreateTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PartitionIndexes" Lude..=) Lude.<$> partitionIndexes,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableInput" Lude..= tableInput)
          ]
      )

instance Lude.ToPath CreateTable where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTableResponse' smart constructor.
newtype CreateTableResponse = CreateTableResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTableResponse
mkCreateTableResponse pResponseStatus_ =
  CreateTableResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrsResponseStatus :: Lens.Lens' CreateTableResponse Lude.Int
ctfrsResponseStatus = Lens.lens (responseStatus :: CreateTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTableResponse)
{-# DEPRECATED ctfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
