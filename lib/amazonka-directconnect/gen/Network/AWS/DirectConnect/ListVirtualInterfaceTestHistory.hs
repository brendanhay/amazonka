{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual interface failover test history.
module Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
  ( -- * Creating a request
    ListVirtualInterfaceTestHistory (..),
    mkListVirtualInterfaceTestHistory,

    -- ** Request lenses
    lvithBgpPeers,
    lvithStatus,
    lvithTestId,
    lvithNextToken,
    lvithMaxResults,
    lvithVirtualInterfaceId,

    -- * Destructuring the response
    ListVirtualInterfaceTestHistoryResponse (..),
    mkListVirtualInterfaceTestHistoryResponse,

    -- ** Response lenses
    lvithrsNextToken,
    lvithrsVirtualInterfaceTestHistory,
    lvithrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListVirtualInterfaceTestHistory' smart constructor.
data ListVirtualInterfaceTestHistory = ListVirtualInterfaceTestHistory'
  { -- | The BGP peers that were placed in the DOWN state during the virtual interface failover test.
    bgpPeers :: Lude.Maybe [Lude.Text],
    -- | The status of the virtual interface failover test.
    status :: Lude.Maybe Lude.Text,
    -- | The ID of the virtual interface failover test.
    testId :: Lude.Maybe Lude.Text,
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The ID of the virtual interface that was tested.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVirtualInterfaceTestHistory' with the minimum fields required to make a request.
--
-- * 'bgpPeers' - The BGP peers that were placed in the DOWN state during the virtual interface failover test.
-- * 'status' - The status of the virtual interface failover test.
-- * 'testId' - The ID of the virtual interface failover test.
-- * 'nextToken' - The token for the next page of results.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
-- * 'virtualInterfaceId' - The ID of the virtual interface that was tested.
mkListVirtualInterfaceTestHistory ::
  ListVirtualInterfaceTestHistory
mkListVirtualInterfaceTestHistory =
  ListVirtualInterfaceTestHistory'
    { bgpPeers = Lude.Nothing,
      status = Lude.Nothing,
      testId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | The BGP peers that were placed in the DOWN state during the virtual interface failover test.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithBgpPeers :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe [Lude.Text])
lvithBgpPeers = Lens.lens (bgpPeers :: ListVirtualInterfaceTestHistory -> Lude.Maybe [Lude.Text]) (\s a -> s {bgpPeers = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The status of the virtual interface failover test.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithStatus :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
lvithStatus = Lens.lens (status :: ListVirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the virtual interface failover test.
--
-- /Note:/ Consider using 'testId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithTestId :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
lvithTestId = Lens.lens (testId :: ListVirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {testId = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithTestId "Use generic-lens or generic-optics with 'testId' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithNextToken :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
lvithNextToken = Lens.lens (nextToken :: ListVirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithMaxResults :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe Lude.Int)
lvithMaxResults = Lens.lens (maxResults :: ListVirtualInterfaceTestHistory -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the virtual interface that was tested.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithVirtualInterfaceId :: Lens.Lens' ListVirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
lvithVirtualInterfaceId = Lens.lens (virtualInterfaceId :: ListVirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: ListVirtualInterfaceTestHistory)
{-# DEPRECATED lvithVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest ListVirtualInterfaceTestHistory where
  type
    Rs ListVirtualInterfaceTestHistory =
      ListVirtualInterfaceTestHistoryResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVirtualInterfaceTestHistoryResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "virtualInterfaceTestHistory" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVirtualInterfaceTestHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.ListVirtualInterfaceTestHistory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVirtualInterfaceTestHistory where
  toJSON ListVirtualInterfaceTestHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bgpPeers" Lude..=) Lude.<$> bgpPeers,
            ("status" Lude..=) Lude.<$> status,
            ("testId" Lude..=) Lude.<$> testId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("virtualInterfaceId" Lude..=) Lude.<$> virtualInterfaceId
          ]
      )

instance Lude.ToPath ListVirtualInterfaceTestHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVirtualInterfaceTestHistory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListVirtualInterfaceTestHistoryResponse' smart constructor.
data ListVirtualInterfaceTestHistoryResponse = ListVirtualInterfaceTestHistoryResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the tested virtual interface.
    virtualInterfaceTestHistory :: Lude.Maybe [VirtualInterfaceTestHistory],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVirtualInterfaceTestHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'virtualInterfaceTestHistory' - The ID of the tested virtual interface.
-- * 'responseStatus' - The response status code.
mkListVirtualInterfaceTestHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVirtualInterfaceTestHistoryResponse
mkListVirtualInterfaceTestHistoryResponse pResponseStatus_ =
  ListVirtualInterfaceTestHistoryResponse'
    { nextToken =
        Lude.Nothing,
      virtualInterfaceTestHistory = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrsNextToken :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Lude.Maybe Lude.Text)
lvithrsNextToken = Lens.lens (nextToken :: ListVirtualInterfaceTestHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVirtualInterfaceTestHistoryResponse)
{-# DEPRECATED lvithrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the tested virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceTestHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrsVirtualInterfaceTestHistory :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Lude.Maybe [VirtualInterfaceTestHistory])
lvithrsVirtualInterfaceTestHistory = Lens.lens (virtualInterfaceTestHistory :: ListVirtualInterfaceTestHistoryResponse -> Lude.Maybe [VirtualInterfaceTestHistory]) (\s a -> s {virtualInterfaceTestHistory = a} :: ListVirtualInterfaceTestHistoryResponse)
{-# DEPRECATED lvithrsVirtualInterfaceTestHistory "Use generic-lens or generic-optics with 'virtualInterfaceTestHistory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrsResponseStatus :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse Lude.Int
lvithrsResponseStatus = Lens.lens (responseStatus :: ListVirtualInterfaceTestHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVirtualInterfaceTestHistoryResponse)
{-# DEPRECATED lvithrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
