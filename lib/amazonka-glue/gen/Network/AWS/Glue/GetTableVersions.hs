{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTableVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of strings that identify available versions of a specified table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTableVersions
  ( -- * Creating a request
    GetTableVersions (..),
    mkGetTableVersions,

    -- ** Request lenses
    gtvsCatalogId,
    gtvsNextToken,
    gtvsMaxResults,
    gtvsDatabaseName,
    gtvsTableName,

    -- * Destructuring the response
    GetTableVersionsResponse (..),
    mkGetTableVersionsResponse,

    -- ** Response lenses
    gtvsrsTableVersions,
    gtvsrsNextToken,
    gtvsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { catalogId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    databaseName :: Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTableVersions' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'maxResults' - The maximum number of table versions to return in one response.
-- * 'nextToken' - A continuation token, if this is not the first call.
-- * 'tableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
mkGetTableVersions ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetTableVersions
mkGetTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsCatalogId :: Lens.Lens' GetTableVersions (Lude.Maybe Lude.Text)
gtvsCatalogId = Lens.lens (catalogId :: GetTableVersions -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTableVersions)
{-# DEPRECATED gtvsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is not the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsNextToken :: Lens.Lens' GetTableVersions (Lude.Maybe Lude.Text)
gtvsNextToken = Lens.lens (nextToken :: GetTableVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTableVersions)
{-# DEPRECATED gtvsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of table versions to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsMaxResults :: Lens.Lens' GetTableVersions (Lude.Maybe Lude.Natural)
gtvsMaxResults = Lens.lens (maxResults :: GetTableVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTableVersions)
{-# DEPRECATED gtvsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsDatabaseName :: Lens.Lens' GetTableVersions Lude.Text
gtvsDatabaseName = Lens.lens (databaseName :: GetTableVersions -> Lude.Text) (\s a -> s {databaseName = a} :: GetTableVersions)
{-# DEPRECATED gtvsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsTableName :: Lens.Lens' GetTableVersions Lude.Text
gtvsTableName = Lens.lens (tableName :: GetTableVersions -> Lude.Text) (\s a -> s {tableName = a} :: GetTableVersions)
{-# DEPRECATED gtvsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Page.AWSPager GetTableVersions where
  page rq rs
    | Page.stop (rs Lens.^. gtvsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtvsrsTableVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtvsNextToken Lens..~ rs Lens.^. gtvsrsNextToken

instance Lude.AWSRequest GetTableVersions where
  type Rs GetTableVersions = GetTableVersionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTableVersionsResponse'
            Lude.<$> (x Lude..?> "TableVersions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTableVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetTableVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTableVersions where
  toJSON GetTableVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetTableVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTableVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { tableVersions ::
      Lude.Maybe [TableVersion],
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

-- | Creates a value of 'GetTableVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the list of available versions does not include the last one.
-- * 'responseStatus' - The response status code.
-- * 'tableVersions' - A list of strings identifying available versions of the specified table.
mkGetTableVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTableVersionsResponse
mkGetTableVersionsResponse pResponseStatus_ =
  GetTableVersionsResponse'
    { tableVersions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of strings identifying available versions of the specified table.
--
-- /Note:/ Consider using 'tableVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsrsTableVersions :: Lens.Lens' GetTableVersionsResponse (Lude.Maybe [TableVersion])
gtvsrsTableVersions = Lens.lens (tableVersions :: GetTableVersionsResponse -> Lude.Maybe [TableVersion]) (\s a -> s {tableVersions = a} :: GetTableVersionsResponse)
{-# DEPRECATED gtvsrsTableVersions "Use generic-lens or generic-optics with 'tableVersions' instead." #-}

-- | A continuation token, if the list of available versions does not include the last one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsrsNextToken :: Lens.Lens' GetTableVersionsResponse (Lude.Maybe Lude.Text)
gtvsrsNextToken = Lens.lens (nextToken :: GetTableVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTableVersionsResponse)
{-# DEPRECATED gtvsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsrsResponseStatus :: Lens.Lens' GetTableVersionsResponse Lude.Int
gtvsrsResponseStatus = Lens.lens (responseStatus :: GetTableVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTableVersionsResponse)
{-# DEPRECATED gtvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
