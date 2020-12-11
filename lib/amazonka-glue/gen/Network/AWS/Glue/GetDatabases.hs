{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDatabases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all databases defined in a given Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDatabases
  ( -- * Creating a request
    GetDatabases (..),
    mkGetDatabases,

    -- ** Request lenses
    gdResourceShareType,
    gdCatalogId,
    gdNextToken,
    gdMaxResults,

    -- * Destructuring the response
    GetDatabasesResponse (..),
    mkGetDatabasesResponse,

    -- ** Response lenses
    gdsrsNextToken,
    gdsrsResponseStatus,
    gdsrsDatabaseList,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDatabases' smart constructor.
data GetDatabases = GetDatabases'
  { resourceShareType ::
      Lude.Maybe ResourceShareType,
    catalogId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatabases' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog from which to retrieve @Databases@ . If none is provided, the AWS account ID is used by default.
-- * 'maxResults' - The maximum number of databases to return in one response.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'resourceShareType' - Allows you to specify that you want to list the databases shared with your account. The allowable values are @FOREIGN@ or @ALL@ .
--
--
--     * If set to @FOREIGN@ , will list the databases shared with your account.
--
--
--     * If set to @ALL@ , will list the databases shared with your account, as well as the databases in yor local account.
mkGetDatabases ::
  GetDatabases
mkGetDatabases =
  GetDatabases'
    { resourceShareType = Lude.Nothing,
      catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Allows you to specify that you want to list the databases shared with your account. The allowable values are @FOREIGN@ or @ALL@ .
--
--
--     * If set to @FOREIGN@ , will list the databases shared with your account.
--
--
--     * If set to @ALL@ , will list the databases shared with your account, as well as the databases in yor local account.
--
--
--
-- /Note:/ Consider using 'resourceShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdResourceShareType :: Lens.Lens' GetDatabases (Lude.Maybe ResourceShareType)
gdResourceShareType = Lens.lens (resourceShareType :: GetDatabases -> Lude.Maybe ResourceShareType) (\s a -> s {resourceShareType = a} :: GetDatabases)
{-# DEPRECATED gdResourceShareType "Use generic-lens or generic-optics with 'resourceShareType' instead." #-}

-- | The ID of the Data Catalog from which to retrieve @Databases@ . If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCatalogId :: Lens.Lens' GetDatabases (Lude.Maybe Lude.Text)
gdCatalogId = Lens.lens (catalogId :: GetDatabases -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetDatabases)
{-# DEPRECATED gdCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdNextToken :: Lens.Lens' GetDatabases (Lude.Maybe Lude.Text)
gdNextToken = Lens.lens (nextToken :: GetDatabases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDatabases)
{-# DEPRECATED gdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of databases to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdMaxResults :: Lens.Lens' GetDatabases (Lude.Maybe Lude.Natural)
gdMaxResults = Lens.lens (maxResults :: GetDatabases -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetDatabases)
{-# DEPRECATED gdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetDatabases where
  page rq rs
    | Page.stop (rs Lens.^. gdsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdsrsDatabaseList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdNextToken Lens..~ rs Lens.^. gdsrsNextToken

instance Lude.AWSRequest GetDatabases where
  type Rs GetDatabases = GetDatabasesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDatabasesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "DatabaseList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetDatabases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetDatabases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDatabases where
  toJSON GetDatabases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceShareType" Lude..=) Lude.<$> resourceShareType,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetDatabases where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDatabases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDatabasesResponse' smart constructor.
data GetDatabasesResponse = GetDatabasesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    databaseList :: [Database]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatabasesResponse' with the minimum fields required to make a request.
--
-- * 'databaseList' - A list of @Database@ objects from the specified catalog.
-- * 'nextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
-- * 'responseStatus' - The response status code.
mkGetDatabasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDatabasesResponse
mkGetDatabasesResponse pResponseStatus_ =
  GetDatabasesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      databaseList = Lude.mempty
    }

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsNextToken :: Lens.Lens' GetDatabasesResponse (Lude.Maybe Lude.Text)
gdsrsNextToken = Lens.lens (nextToken :: GetDatabasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDatabasesResponse)
{-# DEPRECATED gdsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDatabasesResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDatabasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDatabasesResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of @Database@ objects from the specified catalog.
--
-- /Note:/ Consider using 'databaseList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDatabaseList :: Lens.Lens' GetDatabasesResponse [Database]
gdsrsDatabaseList = Lens.lens (databaseList :: GetDatabasesResponse -> [Database]) (\s a -> s {databaseList = a} :: GetDatabasesResponse)
{-# DEPRECATED gdsrsDatabaseList "Use generic-lens or generic-optics with 'databaseList' instead." #-}
