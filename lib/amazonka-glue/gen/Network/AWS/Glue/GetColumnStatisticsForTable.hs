{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @GetTable@ .
module Network.AWS.Glue.GetColumnStatisticsForTable
  ( -- * Creating a request
    GetColumnStatisticsForTable (..),
    mkGetColumnStatisticsForTable,

    -- ** Request lenses
    gcsftCatalogId,
    gcsftDatabaseName,
    gcsftColumnNames,
    gcsftTableName,

    -- * Destructuring the response
    GetColumnStatisticsForTableResponse (..),
    mkGetColumnStatisticsForTableResponse,

    -- ** Response lenses
    gcsftrsErrors,
    gcsftrsColumnStatisticsList,
    gcsftrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetColumnStatisticsForTable' smart constructor.
data GetColumnStatisticsForTable = GetColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Lude.Text,
    -- | A list of the column names.
    columnNames :: [Lude.Text],
    -- | The name of the partitions' table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetColumnStatisticsForTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'columnNames' - A list of the column names.
-- * 'tableName' - The name of the partitions' table.
mkGetColumnStatisticsForTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetColumnStatisticsForTable
mkGetColumnStatisticsForTable pDatabaseName_ pTableName_ =
  GetColumnStatisticsForTable'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      columnNames = Lude.mempty,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftCatalogId :: Lens.Lens' GetColumnStatisticsForTable (Lude.Maybe Lude.Text)
gcsftCatalogId = Lens.lens (catalogId :: GetColumnStatisticsForTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetColumnStatisticsForTable)
{-# DEPRECATED gcsftCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftDatabaseName :: Lens.Lens' GetColumnStatisticsForTable Lude.Text
gcsftDatabaseName = Lens.lens (databaseName :: GetColumnStatisticsForTable -> Lude.Text) (\s a -> s {databaseName = a} :: GetColumnStatisticsForTable)
{-# DEPRECATED gcsftDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of the column names.
--
-- /Note:/ Consider using 'columnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftColumnNames :: Lens.Lens' GetColumnStatisticsForTable [Lude.Text]
gcsftColumnNames = Lens.lens (columnNames :: GetColumnStatisticsForTable -> [Lude.Text]) (\s a -> s {columnNames = a} :: GetColumnStatisticsForTable)
{-# DEPRECATED gcsftColumnNames "Use generic-lens or generic-optics with 'columnNames' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftTableName :: Lens.Lens' GetColumnStatisticsForTable Lude.Text
gcsftTableName = Lens.lens (tableName :: GetColumnStatisticsForTable -> Lude.Text) (\s a -> s {tableName = a} :: GetColumnStatisticsForTable)
{-# DEPRECATED gcsftTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest GetColumnStatisticsForTable where
  type
    Rs GetColumnStatisticsForTable =
      GetColumnStatisticsForTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForTableResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ColumnStatisticsList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetColumnStatisticsForTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetColumnStatisticsForTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetColumnStatisticsForTable where
  toJSON GetColumnStatisticsForTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("ColumnNames" Lude..= columnNames),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetColumnStatisticsForTable where
  toPath = Lude.const "/"

instance Lude.ToQuery GetColumnStatisticsForTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetColumnStatisticsForTableResponse' smart constructor.
data GetColumnStatisticsForTableResponse = GetColumnStatisticsForTableResponse'
  { -- | List of ColumnStatistics that failed to be retrieved.
    errors :: Lude.Maybe [ColumnError],
    -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Lude.Maybe [ColumnStatistics],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetColumnStatisticsForTableResponse' with the minimum fields required to make a request.
--
-- * 'errors' - List of ColumnStatistics that failed to be retrieved.
-- * 'columnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
-- * 'responseStatus' - The response status code.
mkGetColumnStatisticsForTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetColumnStatisticsForTableResponse
mkGetColumnStatisticsForTableResponse pResponseStatus_ =
  GetColumnStatisticsForTableResponse'
    { errors = Lude.Nothing,
      columnStatisticsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrsErrors :: Lens.Lens' GetColumnStatisticsForTableResponse (Lude.Maybe [ColumnError])
gcsftrsErrors = Lens.lens (errors :: GetColumnStatisticsForTableResponse -> Lude.Maybe [ColumnError]) (\s a -> s {errors = a} :: GetColumnStatisticsForTableResponse)
{-# DEPRECATED gcsftrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrsColumnStatisticsList :: Lens.Lens' GetColumnStatisticsForTableResponse (Lude.Maybe [ColumnStatistics])
gcsftrsColumnStatisticsList = Lens.lens (columnStatisticsList :: GetColumnStatisticsForTableResponse -> Lude.Maybe [ColumnStatistics]) (\s a -> s {columnStatisticsList = a} :: GetColumnStatisticsForTableResponse)
{-# DEPRECATED gcsftrsColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrsResponseStatus :: Lens.Lens' GetColumnStatisticsForTableResponse Lude.Int
gcsftrsResponseStatus = Lens.lens (responseStatus :: GetColumnStatisticsForTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetColumnStatisticsForTableResponse)
{-# DEPRECATED gcsftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
