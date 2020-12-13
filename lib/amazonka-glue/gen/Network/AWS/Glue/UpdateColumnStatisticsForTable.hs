{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @UpdateTable@ .
module Network.AWS.Glue.UpdateColumnStatisticsForTable
  ( -- * Creating a request
    UpdateColumnStatisticsForTable (..),
    mkUpdateColumnStatisticsForTable,

    -- ** Request lenses
    ucsftCatalogId,
    ucsftDatabaseName,
    ucsftTableName,
    ucsftColumnStatisticsList,

    -- * Destructuring the response
    UpdateColumnStatisticsForTableResponse (..),
    mkUpdateColumnStatisticsForTableResponse,

    -- ** Response lenses
    ucsftrsErrors,
    ucsftrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateColumnStatisticsForTable' smart constructor.
data UpdateColumnStatisticsForTable = UpdateColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Lude.Text,
    -- | The name of the partitions' table.
    tableName :: Lude.Text,
    -- | A list of the column statistics.
    columnStatisticsList :: [ColumnStatistics]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateColumnStatisticsForTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'tableName' - The name of the partitions' table.
-- * 'columnStatisticsList' - A list of the column statistics.
mkUpdateColumnStatisticsForTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  UpdateColumnStatisticsForTable
mkUpdateColumnStatisticsForTable pDatabaseName_ pTableName_ =
  UpdateColumnStatisticsForTable'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      columnStatisticsList = Lude.mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftCatalogId :: Lens.Lens' UpdateColumnStatisticsForTable (Lude.Maybe Lude.Text)
ucsftCatalogId = Lens.lens (catalogId :: UpdateColumnStatisticsForTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateColumnStatisticsForTable)
{-# DEPRECATED ucsftCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftDatabaseName :: Lens.Lens' UpdateColumnStatisticsForTable Lude.Text
ucsftDatabaseName = Lens.lens (databaseName :: UpdateColumnStatisticsForTable -> Lude.Text) (\s a -> s {databaseName = a} :: UpdateColumnStatisticsForTable)
{-# DEPRECATED ucsftDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftTableName :: Lens.Lens' UpdateColumnStatisticsForTable Lude.Text
ucsftTableName = Lens.lens (tableName :: UpdateColumnStatisticsForTable -> Lude.Text) (\s a -> s {tableName = a} :: UpdateColumnStatisticsForTable)
{-# DEPRECATED ucsftTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of the column statistics.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftColumnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForTable [ColumnStatistics]
ucsftColumnStatisticsList = Lens.lens (columnStatisticsList :: UpdateColumnStatisticsForTable -> [ColumnStatistics]) (\s a -> s {columnStatisticsList = a} :: UpdateColumnStatisticsForTable)
{-# DEPRECATED ucsftColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

instance Lude.AWSRequest UpdateColumnStatisticsForTable where
  type
    Rs UpdateColumnStatisticsForTable =
      UpdateColumnStatisticsForTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForTableResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateColumnStatisticsForTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateColumnStatisticsForTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateColumnStatisticsForTable where
  toJSON UpdateColumnStatisticsForTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("ColumnStatisticsList" Lude..= columnStatisticsList)
          ]
      )

instance Lude.ToPath UpdateColumnStatisticsForTable where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateColumnStatisticsForTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateColumnStatisticsForTableResponse' smart constructor.
data UpdateColumnStatisticsForTableResponse = UpdateColumnStatisticsForTableResponse'
  { -- | List of ColumnStatisticsErrors.
    errors :: Lude.Maybe [ColumnStatisticsError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateColumnStatisticsForTableResponse' with the minimum fields required to make a request.
--
-- * 'errors' - List of ColumnStatisticsErrors.
-- * 'responseStatus' - The response status code.
mkUpdateColumnStatisticsForTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateColumnStatisticsForTableResponse
mkUpdateColumnStatisticsForTableResponse pResponseStatus_ =
  UpdateColumnStatisticsForTableResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of ColumnStatisticsErrors.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftrsErrors :: Lens.Lens' UpdateColumnStatisticsForTableResponse (Lude.Maybe [ColumnStatisticsError])
ucsftrsErrors = Lens.lens (errors :: UpdateColumnStatisticsForTableResponse -> Lude.Maybe [ColumnStatisticsError]) (\s a -> s {errors = a} :: UpdateColumnStatisticsForTableResponse)
{-# DEPRECATED ucsftrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftrsResponseStatus :: Lens.Lens' UpdateColumnStatisticsForTableResponse Lude.Int
ucsftrsResponseStatus = Lens.lens (responseStatus :: UpdateColumnStatisticsForTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateColumnStatisticsForTableResponse)
{-# DEPRECATED ucsftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
