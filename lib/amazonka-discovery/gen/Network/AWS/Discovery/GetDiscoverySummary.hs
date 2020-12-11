{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.GetDiscoverySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a short summary of discovered assets.
--
-- This API operation takes no request parameters and is called as is at the command prompt as shown in the example.
module Network.AWS.Discovery.GetDiscoverySummary
  ( -- * Creating a request
    GetDiscoverySummary (..),
    mkGetDiscoverySummary,

    -- * Destructuring the response
    GetDiscoverySummaryResponse (..),
    mkGetDiscoverySummaryResponse,

    -- ** Response lenses
    gdsrsServers,
    gdsrsServersMappedtoTags,
    gdsrsServersMappedToApplications,
    gdsrsConnectorSummary,
    gdsrsAgentSummary,
    gdsrsApplications,
    gdsrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDiscoverySummary' smart constructor.
data GetDiscoverySummary = GetDiscoverySummary'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiscoverySummary' with the minimum fields required to make a request.
mkGetDiscoverySummary ::
  GetDiscoverySummary
mkGetDiscoverySummary = GetDiscoverySummary'

instance Lude.AWSRequest GetDiscoverySummary where
  type Rs GetDiscoverySummary = GetDiscoverySummaryResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDiscoverySummaryResponse'
            Lude.<$> (x Lude..?> "servers")
            Lude.<*> (x Lude..?> "serversMappedtoTags")
            Lude.<*> (x Lude..?> "serversMappedToApplications")
            Lude.<*> (x Lude..?> "connectorSummary")
            Lude.<*> (x Lude..?> "agentSummary")
            Lude.<*> (x Lude..?> "applications")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDiscoverySummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.GetDiscoverySummary" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDiscoverySummary where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetDiscoverySummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDiscoverySummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDiscoverySummaryResponse' smart constructor.
data GetDiscoverySummaryResponse = GetDiscoverySummaryResponse'
  { servers ::
      Lude.Maybe Lude.Integer,
    serversMappedtoTags ::
      Lude.Maybe Lude.Integer,
    serversMappedToApplications ::
      Lude.Maybe Lude.Integer,
    connectorSummary ::
      Lude.Maybe CustomerConnectorInfo,
    agentSummary ::
      Lude.Maybe CustomerAgentInfo,
    applications ::
      Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'GetDiscoverySummaryResponse' with the minimum fields required to make a request.
--
-- * 'agentSummary' - Details about discovered agents, including agent status and health.
-- * 'applications' - The number of applications discovered.
-- * 'connectorSummary' - Details about discovered connectors, including connector status and health.
-- * 'responseStatus' - The response status code.
-- * 'servers' - The number of servers discovered.
-- * 'serversMappedToApplications' - The number of servers mapped to applications.
-- * 'serversMappedtoTags' - The number of servers mapped to tags.
mkGetDiscoverySummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDiscoverySummaryResponse
mkGetDiscoverySummaryResponse pResponseStatus_ =
  GetDiscoverySummaryResponse'
    { servers = Lude.Nothing,
      serversMappedtoTags = Lude.Nothing,
      serversMappedToApplications = Lude.Nothing,
      connectorSummary = Lude.Nothing,
      agentSummary = Lude.Nothing,
      applications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of servers discovered.
--
-- /Note:/ Consider using 'servers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsServers :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe Lude.Integer)
gdsrsServers = Lens.lens (servers :: GetDiscoverySummaryResponse -> Lude.Maybe Lude.Integer) (\s a -> s {servers = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsServers "Use generic-lens or generic-optics with 'servers' instead." #-}

-- | The number of servers mapped to tags.
--
-- /Note:/ Consider using 'serversMappedtoTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsServersMappedtoTags :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe Lude.Integer)
gdsrsServersMappedtoTags = Lens.lens (serversMappedtoTags :: GetDiscoverySummaryResponse -> Lude.Maybe Lude.Integer) (\s a -> s {serversMappedtoTags = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsServersMappedtoTags "Use generic-lens or generic-optics with 'serversMappedtoTags' instead." #-}

-- | The number of servers mapped to applications.
--
-- /Note:/ Consider using 'serversMappedToApplications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsServersMappedToApplications :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe Lude.Integer)
gdsrsServersMappedToApplications = Lens.lens (serversMappedToApplications :: GetDiscoverySummaryResponse -> Lude.Maybe Lude.Integer) (\s a -> s {serversMappedToApplications = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsServersMappedToApplications "Use generic-lens or generic-optics with 'serversMappedToApplications' instead." #-}

-- | Details about discovered connectors, including connector status and health.
--
-- /Note:/ Consider using 'connectorSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsConnectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe CustomerConnectorInfo)
gdsrsConnectorSummary = Lens.lens (connectorSummary :: GetDiscoverySummaryResponse -> Lude.Maybe CustomerConnectorInfo) (\s a -> s {connectorSummary = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsConnectorSummary "Use generic-lens or generic-optics with 'connectorSummary' instead." #-}

-- | Details about discovered agents, including agent status and health.
--
-- /Note:/ Consider using 'agentSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsAgentSummary :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe CustomerAgentInfo)
gdsrsAgentSummary = Lens.lens (agentSummary :: GetDiscoverySummaryResponse -> Lude.Maybe CustomerAgentInfo) (\s a -> s {agentSummary = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsAgentSummary "Use generic-lens or generic-optics with 'agentSummary' instead." #-}

-- | The number of applications discovered.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsApplications :: Lens.Lens' GetDiscoverySummaryResponse (Lude.Maybe Lude.Integer)
gdsrsApplications = Lens.lens (applications :: GetDiscoverySummaryResponse -> Lude.Maybe Lude.Integer) (\s a -> s {applications = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDiscoverySummaryResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDiscoverySummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDiscoverySummaryResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
