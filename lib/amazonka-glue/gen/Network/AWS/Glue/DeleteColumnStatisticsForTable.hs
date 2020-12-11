{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @DeleteTable@ .
module Network.AWS.Glue.DeleteColumnStatisticsForTable
  ( -- * Creating a request
    DeleteColumnStatisticsForTable (..),
    mkDeleteColumnStatisticsForTable,

    -- ** Request lenses
    dcsftCatalogId,
    dcsftDatabaseName,
    dcsftTableName,
    dcsftColumnName,

    -- * Destructuring the response
    DeleteColumnStatisticsForTableResponse (..),
    mkDeleteColumnStatisticsForTableResponse,

    -- ** Response lenses
    dcsftrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteColumnStatisticsForTable' smart constructor.
data DeleteColumnStatisticsForTable = DeleteColumnStatisticsForTable'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    columnName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteColumnStatisticsForTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'columnName' - The name of the column.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'tableName' - The name of the partitions' table.
mkDeleteColumnStatisticsForTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'columnName'
  Lude.Text ->
  DeleteColumnStatisticsForTable
mkDeleteColumnStatisticsForTable
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForTable'
      { catalogId = Lude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        columnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftCatalogId :: Lens.Lens' DeleteColumnStatisticsForTable (Lude.Maybe Lude.Text)
dcsftCatalogId = Lens.lens (catalogId :: DeleteColumnStatisticsForTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteColumnStatisticsForTable)
{-# DEPRECATED dcsftCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftDatabaseName :: Lens.Lens' DeleteColumnStatisticsForTable Lude.Text
dcsftDatabaseName = Lens.lens (databaseName :: DeleteColumnStatisticsForTable -> Lude.Text) (\s a -> s {databaseName = a} :: DeleteColumnStatisticsForTable)
{-# DEPRECATED dcsftDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftTableName :: Lens.Lens' DeleteColumnStatisticsForTable Lude.Text
dcsftTableName = Lens.lens (tableName :: DeleteColumnStatisticsForTable -> Lude.Text) (\s a -> s {tableName = a} :: DeleteColumnStatisticsForTable)
{-# DEPRECATED dcsftTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftColumnName :: Lens.Lens' DeleteColumnStatisticsForTable Lude.Text
dcsftColumnName = Lens.lens (columnName :: DeleteColumnStatisticsForTable -> Lude.Text) (\s a -> s {columnName = a} :: DeleteColumnStatisticsForTable)
{-# DEPRECATED dcsftColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

instance Lude.AWSRequest DeleteColumnStatisticsForTable where
  type
    Rs DeleteColumnStatisticsForTable =
      DeleteColumnStatisticsForTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForTableResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteColumnStatisticsForTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteColumnStatisticsForTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteColumnStatisticsForTable where
  toJSON DeleteColumnStatisticsForTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("ColumnName" Lude..= columnName)
          ]
      )

instance Lude.ToPath DeleteColumnStatisticsForTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteColumnStatisticsForTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteColumnStatisticsForTableResponse' smart constructor.
newtype DeleteColumnStatisticsForTableResponse = DeleteColumnStatisticsForTableResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteColumnStatisticsForTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteColumnStatisticsForTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteColumnStatisticsForTableResponse
mkDeleteColumnStatisticsForTableResponse pResponseStatus_ =
  DeleteColumnStatisticsForTableResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftrsResponseStatus :: Lens.Lens' DeleteColumnStatisticsForTableResponse Lude.Int
dcsftrsResponseStatus = Lens.lens (responseStatus :: DeleteColumnStatisticsForTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteColumnStatisticsForTableResponse)
{-# DEPRECATED dcsftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
