{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes multiple tables at once.
module Network.AWS.Glue.BatchDeleteTable
  ( -- * Creating a request
    BatchDeleteTable (..),
    mkBatchDeleteTable,

    -- ** Request lenses
    bdtTablesToDelete,
    bdtCatalogId,
    bdtDatabaseName,

    -- * Destructuring the response
    BatchDeleteTableResponse (..),
    mkBatchDeleteTableResponse,

    -- ** Response lenses
    bdtrsErrors,
    bdtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteTable' smart constructor.
data BatchDeleteTable = BatchDeleteTable'
  { -- | A list of the table to delete.
    tablesToDelete :: [Lude.Text],
    -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database in which the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteTable' with the minimum fields required to make a request.
--
-- * 'tablesToDelete' - A list of the table to delete.
-- * 'catalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
mkBatchDeleteTable ::
  -- | 'databaseName'
  Lude.Text ->
  BatchDeleteTable
mkBatchDeleteTable pDatabaseName_ =
  BatchDeleteTable'
    { tablesToDelete = Lude.mempty,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_
    }

-- | A list of the table to delete.
--
-- /Note:/ Consider using 'tablesToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtTablesToDelete :: Lens.Lens' BatchDeleteTable [Lude.Text]
bdtTablesToDelete = Lens.lens (tablesToDelete :: BatchDeleteTable -> [Lude.Text]) (\s a -> s {tablesToDelete = a} :: BatchDeleteTable)
{-# DEPRECATED bdtTablesToDelete "Use generic-lens or generic-optics with 'tablesToDelete' instead." #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtCatalogId :: Lens.Lens' BatchDeleteTable (Lude.Maybe Lude.Text)
bdtCatalogId = Lens.lens (catalogId :: BatchDeleteTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchDeleteTable)
{-# DEPRECATED bdtCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtDatabaseName :: Lens.Lens' BatchDeleteTable Lude.Text
bdtDatabaseName = Lens.lens (databaseName :: BatchDeleteTable -> Lude.Text) (\s a -> s {databaseName = a} :: BatchDeleteTable)
{-# DEPRECATED bdtDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.AWSRequest BatchDeleteTable where
  type Rs BatchDeleteTable = BatchDeleteTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteTableResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchDeleteTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteTable where
  toJSON BatchDeleteTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TablesToDelete" Lude..= tablesToDelete),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )

instance Lude.ToPath BatchDeleteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteTableResponse' smart constructor.
data BatchDeleteTableResponse = BatchDeleteTableResponse'
  { -- | A list of errors encountered in attempting to delete the specified tables.
    errors :: Lude.Maybe [TableError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteTableResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of errors encountered in attempting to delete the specified tables.
-- * 'responseStatus' - The response status code.
mkBatchDeleteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteTableResponse
mkBatchDeleteTableResponse pResponseStatus_ =
  BatchDeleteTableResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of errors encountered in attempting to delete the specified tables.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtrsErrors :: Lens.Lens' BatchDeleteTableResponse (Lude.Maybe [TableError])
bdtrsErrors = Lens.lens (errors :: BatchDeleteTableResponse -> Lude.Maybe [TableError]) (\s a -> s {errors = a} :: BatchDeleteTableResponse)
{-# DEPRECATED bdtrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtrsResponseStatus :: Lens.Lens' BatchDeleteTableResponse Lude.Int
bdtrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteTableResponse)
{-# DEPRECATED bdtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
