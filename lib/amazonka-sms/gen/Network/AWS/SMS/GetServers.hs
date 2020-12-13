{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the servers in your server catalog.
--
-- Before you can describe your servers, you must import them using 'ImportServerCatalog' .
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetServers
  ( -- * Creating a request
    GetServers (..),
    mkGetServers,

    -- ** Request lenses
    gsVmServerAddressList,
    gsNextToken,
    gsMaxResults,

    -- * Destructuring the response
    GetServersResponse (..),
    mkGetServersResponse,

    -- ** Response lenses
    gsrsServerCatalogStatus,
    gsrsLastModifiedOn,
    gsrsNextToken,
    gsrsServerList,
    gsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetServers' smart constructor.
data GetServers = GetServers'
  { -- | The server addresses.
    vmServerAddressList :: Lude.Maybe [VMServerAddress],
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServers' with the minimum fields required to make a request.
--
-- * 'vmServerAddressList' - The server addresses.
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkGetServers ::
  GetServers
mkGetServers =
  GetServers'
    { vmServerAddressList = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The server addresses.
--
-- /Note:/ Consider using 'vmServerAddressList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsVmServerAddressList :: Lens.Lens' GetServers (Lude.Maybe [VMServerAddress])
gsVmServerAddressList = Lens.lens (vmServerAddressList :: GetServers -> Lude.Maybe [VMServerAddress]) (\s a -> s {vmServerAddressList = a} :: GetServers)
{-# DEPRECATED gsVmServerAddressList "Use generic-lens or generic-optics with 'vmServerAddressList' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsNextToken :: Lens.Lens' GetServers (Lude.Maybe Lude.Text)
gsNextToken = Lens.lens (nextToken :: GetServers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetServers)
{-# DEPRECATED gsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMaxResults :: Lens.Lens' GetServers (Lude.Maybe Lude.Int)
gsMaxResults = Lens.lens (maxResults :: GetServers -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetServers)
{-# DEPRECATED gsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetServers where
  page rq rs
    | Page.stop (rs Lens.^. gsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gsrsServerList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gsNextToken Lens..~ rs Lens.^. gsrsNextToken

instance Lude.AWSRequest GetServers where
  type Rs GetServers = GetServersResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetServersResponse'
            Lude.<$> (x Lude..?> "serverCatalogStatus")
            Lude.<*> (x Lude..?> "lastModifiedOn")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "serverList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetServers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetServers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetServers where
  toJSON GetServers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("vmServerAddressList" Lude..=) Lude.<$> vmServerAddressList,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetServers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetServers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetServersResponse' smart constructor.
data GetServersResponse = GetServersResponse'
  { -- | The status of the server catalog.
    serverCatalogStatus :: Lude.Maybe ServerCatalogStatus,
    -- | The time when the server was last modified.
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the servers.
    serverList :: Lude.Maybe [Server],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServersResponse' with the minimum fields required to make a request.
--
-- * 'serverCatalogStatus' - The status of the server catalog.
-- * 'lastModifiedOn' - The time when the server was last modified.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
-- * 'serverList' - Information about the servers.
-- * 'responseStatus' - The response status code.
mkGetServersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetServersResponse
mkGetServersResponse pResponseStatus_ =
  GetServersResponse'
    { serverCatalogStatus = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      nextToken = Lude.Nothing,
      serverList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the server catalog.
--
-- /Note:/ Consider using 'serverCatalogStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsServerCatalogStatus :: Lens.Lens' GetServersResponse (Lude.Maybe ServerCatalogStatus)
gsrsServerCatalogStatus = Lens.lens (serverCatalogStatus :: GetServersResponse -> Lude.Maybe ServerCatalogStatus) (\s a -> s {serverCatalogStatus = a} :: GetServersResponse)
{-# DEPRECATED gsrsServerCatalogStatus "Use generic-lens or generic-optics with 'serverCatalogStatus' instead." #-}

-- | The time when the server was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLastModifiedOn :: Lens.Lens' GetServersResponse (Lude.Maybe Lude.Timestamp)
gsrsLastModifiedOn = Lens.lens (lastModifiedOn :: GetServersResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: GetServersResponse)
{-# DEPRECATED gsrsLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsNextToken :: Lens.Lens' GetServersResponse (Lude.Maybe Lude.Text)
gsrsNextToken = Lens.lens (nextToken :: GetServersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetServersResponse)
{-# DEPRECATED gsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the servers.
--
-- /Note:/ Consider using 'serverList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsServerList :: Lens.Lens' GetServersResponse (Lude.Maybe [Server])
gsrsServerList = Lens.lens (serverList :: GetServersResponse -> Lude.Maybe [Server]) (\s a -> s {serverList = a} :: GetServersResponse)
{-# DEPRECATED gsrsServerList "Use generic-lens or generic-optics with 'serverList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetServersResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetServersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServersResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
