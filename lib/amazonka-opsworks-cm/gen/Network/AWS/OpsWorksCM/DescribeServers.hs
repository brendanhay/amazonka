{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all configuration management servers that are identified with your account. Only the stored results from Amazon DynamoDB are returned. AWS OpsWorks CM does not query other services.
--
-- This operation is synchronous.
-- A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeServers
  ( -- * Creating a request
    DescribeServers (..),
    mkDescribeServers,

    -- ** Request lenses
    dssServerName,
    dssNextToken,
    dssMaxResults,

    -- * Destructuring the response
    DescribeServersResponse (..),
    mkDescribeServersResponse,

    -- ** Response lenses
    dssrsServers,
    dssrsNextToken,
    dssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeServers' smart constructor.
data DescribeServers = DescribeServers'
  { serverName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeServers' with the minimum fields required to make a request.
--
-- * 'maxResults' - This is not currently implemented for @DescribeServers@ requests.
-- * 'nextToken' - This is not currently implemented for @DescribeServers@ requests.
-- * 'serverName' - Describes the server with the specified ServerName.
mkDescribeServers ::
  DescribeServers
mkDescribeServers =
  DescribeServers'
    { serverName = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Describes the server with the specified ServerName.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServerName :: Lens.Lens' DescribeServers (Lude.Maybe Lude.Text)
dssServerName = Lens.lens (serverName :: DescribeServers -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: DescribeServers)
{-# DEPRECATED dssServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | This is not currently implemented for @DescribeServers@ requests.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeServers (Lude.Maybe Lude.Text)
dssNextToken = Lens.lens (nextToken :: DescribeServers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeServers)
{-# DEPRECATED dssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | This is not currently implemented for @DescribeServers@ requests.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxResults :: Lens.Lens' DescribeServers (Lude.Maybe Lude.Natural)
dssMaxResults = Lens.lens (maxResults :: DescribeServers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeServers)
{-# DEPRECATED dssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeServers where
  page rq rs
    | Page.stop (rs Lens.^. dssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsServers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dssNextToken Lens..~ rs Lens.^. dssrsNextToken

instance Lude.AWSRequest DescribeServers where
  type Rs DescribeServers = DescribeServersResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServersResponse'
            Lude.<$> (x Lude..?> "Servers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.DescribeServers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServers where
  toJSON DescribeServers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeServers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeServersResponse' smart constructor.
data DescribeServersResponse = DescribeServersResponse'
  { servers ::
      Lude.Maybe [Server],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - This is not currently implemented for @DescribeServers@ requests.
-- * 'responseStatus' - The response status code.
-- * 'servers' - Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If @DescribeServersResponse$Servers$EngineAttributes@ includes CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server to Chef Automate 2. To be eligible for upgrade, a server running Chef Automate 1 must have had at least one successful maintenance run after November 1, 2019.
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
mkDescribeServersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServersResponse
mkDescribeServersResponse pResponseStatus_ =
  DescribeServersResponse'
    { servers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If @DescribeServersResponse$Servers$EngineAttributes@ includes CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server to Chef Automate 2. To be eligible for upgrade, a server running Chef Automate 1 must have had at least one successful maintenance run after November 1, 2019.
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
--
-- /Note:/ Consider using 'servers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsServers :: Lens.Lens' DescribeServersResponse (Lude.Maybe [Server])
dssrsServers = Lens.lens (servers :: DescribeServersResponse -> Lude.Maybe [Server]) (\s a -> s {servers = a} :: DescribeServersResponse)
{-# DEPRECATED dssrsServers "Use generic-lens or generic-optics with 'servers' instead." #-}

-- | This is not currently implemented for @DescribeServers@ requests.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsNextToken :: Lens.Lens' DescribeServersResponse (Lude.Maybe Lude.Text)
dssrsNextToken = Lens.lens (nextToken :: DescribeServersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeServersResponse)
{-# DEPRECATED dssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeServersResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeServersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServersResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
