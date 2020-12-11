{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gtsCatalogId,
    gtsNextToken,
    gtsExpression,
    gtsMaxResults,
    gtsDatabaseName,

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
  { catalogId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    expression :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    databaseName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTables' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
-- * 'expression' - A regular expression pattern. If present, only those tables whose names match the pattern are returned.
-- * 'maxResults' - The maximum number of tables to return in a single response.
-- * 'nextToken' - A continuation token, included if this is a continuation call.
mkGetTables ::
  -- | 'databaseName'
  Lude.Text ->
  GetTables
mkGetTables pDatabaseName_ =
  GetTables'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      expression = Lude.Nothing,
      maxResults = Lude.Nothing,
      databaseName = pDatabaseName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsCatalogId :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtsCatalogId = Lens.lens (catalogId :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTables)
{-# DEPRECATED gtsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsNextToken :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtsNextToken = Lens.lens (nextToken :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTables)
{-# DEPRECATED gtsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A regular expression pattern. If present, only those tables whose names match the pattern are returned.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsExpression :: Lens.Lens' GetTables (Lude.Maybe Lude.Text)
gtsExpression = Lens.lens (expression :: GetTables -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: GetTables)
{-# DEPRECATED gtsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The maximum number of tables to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsMaxResults :: Lens.Lens' GetTables (Lude.Maybe Lude.Natural)
gtsMaxResults = Lens.lens (maxResults :: GetTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTables)
{-# DEPRECATED gtsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsDatabaseName :: Lens.Lens' GetTables Lude.Text
gtsDatabaseName = Lens.lens (databaseName :: GetTables -> Lude.Text) (\s a -> s {databaseName = a} :: GetTables)
{-# DEPRECATED gtsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Page.AWSPager GetTables where
  page rq rs
    | Page.stop (rs Lens.^. gtsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtsrsTableList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtsNextToken Lens..~ rs Lens.^. gtsrsNextToken

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
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )

instance Lude.ToPath GetTables where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTables where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { tableList ::
      Lude.Maybe [Table],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTablesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, present if the current list segment is not the last.
-- * 'responseStatus' - The response status code.
-- * 'tableList' - A list of the requested @Table@ objects.
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
