{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connection definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetConnections
  ( -- * Creating a request
    GetConnections (..),
    mkGetConnections,

    -- ** Request lenses
    gcsCatalogId,
    gcsNextToken,
    gcsHidePassword,
    gcsFilter,
    gcsMaxResults,

    -- * Destructuring the response
    GetConnectionsResponse (..),
    mkGetConnectionsResponse,

    -- ** Response lenses
    ggrsNextToken,
    ggrsConnectionList,
    ggrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnections' smart constructor.
data GetConnections = GetConnections'
  { catalogId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    hidePassword :: Lude.Maybe Lude.Bool,
    filter :: Lude.Maybe GetConnectionsFilter,
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

-- | Creates a value of 'GetConnections' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
-- * 'filter' - A filter that controls which connections are returned.
-- * 'hidePassword' - Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
-- * 'maxResults' - The maximum number of connections to return in one response.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkGetConnections ::
  GetConnections
mkGetConnections =
  GetConnections'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      hidePassword = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsCatalogId :: Lens.Lens' GetConnections (Lude.Maybe Lude.Text)
gcsCatalogId = Lens.lens (catalogId :: GetConnections -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetConnections)
{-# DEPRECATED gcsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsNextToken :: Lens.Lens' GetConnections (Lude.Maybe Lude.Text)
gcsNextToken = Lens.lens (nextToken :: GetConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnections)
{-# DEPRECATED gcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
--
-- /Note:/ Consider using 'hidePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsHidePassword :: Lens.Lens' GetConnections (Lude.Maybe Lude.Bool)
gcsHidePassword = Lens.lens (hidePassword :: GetConnections -> Lude.Maybe Lude.Bool) (\s a -> s {hidePassword = a} :: GetConnections)
{-# DEPRECATED gcsHidePassword "Use generic-lens or generic-optics with 'hidePassword' instead." #-}

-- | A filter that controls which connections are returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsFilter :: Lens.Lens' GetConnections (Lude.Maybe GetConnectionsFilter)
gcsFilter = Lens.lens (filter :: GetConnections -> Lude.Maybe GetConnectionsFilter) (\s a -> s {filter = a} :: GetConnections)
{-# DEPRECATED gcsFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of connections to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsMaxResults :: Lens.Lens' GetConnections (Lude.Maybe Lude.Natural)
gcsMaxResults = Lens.lens (maxResults :: GetConnections -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetConnections)
{-# DEPRECATED gcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetConnections where
  page rq rs
    | Page.stop (rs Lens.^. ggrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ggrsConnectionList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcsNextToken Lens..~ rs Lens.^. ggrsNextToken

instance Lude.AWSRequest GetConnections where
  type Rs GetConnections = GetConnectionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ConnectionList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetConnections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConnections where
  toJSON GetConnections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("HidePassword" Lude..=) Lude.<$> hidePassword,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConnections where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    connectionList :: Lude.Maybe [Connection],
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

-- | Creates a value of 'GetConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'connectionList' - A list of requested connection definitions.
-- * 'nextToken' - A continuation token, if the list of connections returned does not include the last of the filtered connections.
-- * 'responseStatus' - The response status code.
mkGetConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectionsResponse
mkGetConnectionsResponse pResponseStatus_ =
  GetConnectionsResponse'
    { nextToken = Lude.Nothing,
      connectionList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the list of connections returned does not include the last of the filtered connections.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsNextToken :: Lens.Lens' GetConnectionsResponse (Lude.Maybe Lude.Text)
ggrsNextToken = Lens.lens (nextToken :: GetConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnectionsResponse)
{-# DEPRECATED ggrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of requested connection definitions.
--
-- /Note:/ Consider using 'connectionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsConnectionList :: Lens.Lens' GetConnectionsResponse (Lude.Maybe [Connection])
ggrsConnectionList = Lens.lens (connectionList :: GetConnectionsResponse -> Lude.Maybe [Connection]) (\s a -> s {connectionList = a} :: GetConnectionsResponse)
{-# DEPRECATED ggrsConnectionList "Use generic-lens or generic-optics with 'connectionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetConnectionsResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectionsResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
