{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definitions of some or all of the tables in a given @Database@ .
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTables
  ( -- * Creating a request
    GetTables (..),
    mkGetTables,

    -- ** Request lenses
    gtCatalogId,
    gtNextToken,
    gtExpression,
    gtDatabaseName,
    gtMaxResults,

    -- * Destructuring the response
    GetTablesResponse (..),
    mkGetTablesResponse,

    -- ** Response lenses
    gtsrsTableList,
    gtsrsNextToken,
    gtsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTables' smart constructor.
data GetTables = GetTables'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | A continuation token, included if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A regular expression pattern. If present, only those tables whose names match the pattern are returned.
    expression :: Lude.Maybe Lude.Text,
    -- | The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text,
    -- | The maximum number of tables to return in a single response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTables' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'nextToken' - A continuation token, included if this is a continuation call.
-- * 'expression' - A regular expression pattern. If present, only those tables whose names match the pattern are returned.
-- * 'databaseName' - The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
-- * 'maxResults' - The maximum number of tables to return in a single response.
mkGetTables ::
  -- | 'databaseName'
  Lude.Text ->
  GetTables
mkGetTables pDatabaseName_ =
  GetTables'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      expression = Lude.Nothing,
      databaseName = pDatabaseName_,
      maxResults = Lude.Nothing
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtCatalogId :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtCatalogId = Lens.lens (catalogId :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTables)
{-# DEPRECATED gtCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtNextToken :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtNextToken = Lens.lens (nextToken :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTables)
{-# DEPRECATED gtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A regular expression pattern. If present, only those tables whose names match the pattern are returned.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtExpression :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtExpression = Lens.lens (expression :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: GetTables)
{-# DEPRECATED gtExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDatabaseName :: Lens.Lens' GetTables Lude.Text
gtDatabaseName = Lens.lens (databaseName :: GetTables -> Lude.Text) (\s a -> s {databaseName = a} :: GetTables)
{-# DEPRECATED gtDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The maximum number of tables to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtMaxResults :: Lens.Lens' GetTables (Lude.Maybe Lude.Natural)
gtMaxResults = Lens.lens (maxResults :: GetTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTables)
{-# DEPRECATED gtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetTables where
  page rq rs
    | Page.stop (rs Lens.^. gtsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtsrsTableList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtNextToken Lens..~ rs Lens.^. gtsrsNextToken

instance Lude.AWSRequest GetTables where
  type Rs GetTables = GetTablesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTablesResponse'
            Lude.<$> (x Lude..?> "TableList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTables where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetTables" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTables where
  toJSON GetTables' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Expression" Lude..=) Lude.<$> expression,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetTables where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTables where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { -- | A list of the requested @Table@ objects.
    tableList :: Lude.Maybe [Table],
    -- | A continuation token, present if the current list segment is not the last.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTablesResponse' with the minimum fields required to make a request.
--
-- * 'tableList' - A list of the requested @Table@ objects.
-- * 'nextToken' - A continuation token, present if the current list segment is not the last.
-- * 'responseStatus' - The response status code.
mkGetTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTablesResponse
mkGetTablesResponse pResponseStatus_ =
  GetTablesResponse'
    { tableList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the requested @Table@ objects.
--
-- /Note:/ Consider using 'tableList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsTableList :: Lens.Lens' GetTablesResponse (Lude.Maybe [Table])
gtsrsTableList = Lens.lens (tableList :: GetTablesResponse -> Lude.Maybe [Table]) (\s a -> s {tableList = a} :: GetTablesResponse)
{-# DEPRECATED gtsrsTableList "Use generic-lens or generic-optics with 'tableList' instead." #-}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsNextToken :: Lens.Lens' GetTablesResponse (Lude.Maybe Lude.Text)
gtsrsNextToken = Lens.lens (nextToken :: GetTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTablesResponse)
{-# DEPRECATED gtsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResponseStatus :: Lens.Lens' GetTablesResponse Lude.Int
gtsrsResponseStatus = Lens.lens (responseStatus :: GetTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTablesResponse)
{-# DEPRECATED gtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
