{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetConnectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connectors registered with the AWS SMS.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetConnectors
  ( -- * Creating a request
    GetConnectors (..),
    mkGetConnectors,

    -- ** Request lenses
    gcNextToken,
    gcMaxResults,

    -- * Destructuring the response
    GetConnectorsResponse (..),
    mkGetConnectorsResponse,

    -- ** Response lenses
    gcrsConnectorList,
    gcrsNextToken,
    gcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetConnectors' smart constructor.
data GetConnectors = GetConnectors'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectors' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkGetConnectors ::
  GetConnectors
mkGetConnectors =
  GetConnectors'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcNextToken :: Lens.Lens' GetConnectors (Lude.Maybe Lude.Text)
gcNextToken = Lens.lens (nextToken :: GetConnectors -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnectors)
{-# DEPRECATED gcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcMaxResults :: Lens.Lens' GetConnectors (Lude.Maybe Lude.Int)
gcMaxResults = Lens.lens (maxResults :: GetConnectors -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetConnectors)
{-# DEPRECATED gcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetConnectors where
  page rq rs
    | Page.stop (rs Lens.^. gcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcrsConnectorList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcNextToken Lens..~ rs Lens.^. gcrsNextToken

instance Lude.AWSRequest GetConnectors where
  type Rs GetConnectors = GetConnectorsResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectorsResponse'
            Lude.<$> (x Lude..?> "connectorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetConnectors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConnectors where
  toJSON GetConnectors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetConnectors where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConnectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectorsResponse' smart constructor.
data GetConnectorsResponse = GetConnectorsResponse'
  { -- | Information about the registered connectors.
    connectorList :: Lude.Maybe [Connector],
    -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectorsResponse' with the minimum fields required to make a request.
--
-- * 'connectorList' - Information about the registered connectors.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetConnectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectorsResponse
mkGetConnectorsResponse pResponseStatus_ =
  GetConnectorsResponse'
    { connectorList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the registered connectors.
--
-- /Note:/ Consider using 'connectorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsConnectorList :: Lens.Lens' GetConnectorsResponse (Lude.Maybe [Connector])
gcrsConnectorList = Lens.lens (connectorList :: GetConnectorsResponse -> Lude.Maybe [Connector]) (\s a -> s {connectorList = a} :: GetConnectorsResponse)
{-# DEPRECATED gcrsConnectorList "Use generic-lens or generic-optics with 'connectorList' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsNextToken :: Lens.Lens' GetConnectorsResponse (Lude.Maybe Lude.Text)
gcrsNextToken = Lens.lens (nextToken :: GetConnectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnectorsResponse)
{-# DEPRECATED gcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetConnectorsResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetConnectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectorsResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
