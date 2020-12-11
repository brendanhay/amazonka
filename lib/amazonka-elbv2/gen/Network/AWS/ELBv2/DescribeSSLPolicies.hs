{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeSSLPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies or all policies used for SSL negotiation.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeSSLPolicies
  ( -- * Creating a request
    DescribeSSLPolicies (..),
    mkDescribeSSLPolicies,

    -- ** Request lenses
    dspNames,
    dspMarker,
    dspPageSize,

    -- * Destructuring the response
    DescribeSSLPoliciesResponse (..),
    mkDescribeSSLPoliciesResponse,

    -- ** Response lenses
    dsprsSSLPolicies,
    dsprsNextMarker,
    dsprsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSSLPolicies' smart constructor.
data DescribeSSLPolicies = DescribeSSLPolicies'
  { names ::
      Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSSLPolicies' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'names' - The names of the policies.
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeSSLPolicies ::
  DescribeSSLPolicies
mkDescribeSSLPolicies =
  DescribeSSLPolicies'
    { names = Lude.Nothing,
      marker = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The names of the policies.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspNames :: Lens.Lens' DescribeSSLPolicies (Lude.Maybe [Lude.Text])
dspNames = Lens.lens (names :: DescribeSSLPolicies -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeSSLPolicies)
{-# DEPRECATED dspNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspMarker :: Lens.Lens' DescribeSSLPolicies (Lude.Maybe Lude.Text)
dspMarker = Lens.lens (marker :: DescribeSSLPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSSLPolicies)
{-# DEPRECATED dspMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspPageSize :: Lens.Lens' DescribeSSLPolicies (Lude.Maybe Lude.Natural)
dspPageSize = Lens.lens (pageSize :: DescribeSSLPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeSSLPolicies)
{-# DEPRECATED dspPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeSSLPolicies where
  page rq rs
    | Page.stop (rs Lens.^. dsprsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsprsSSLPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dspMarker Lens..~ rs Lens.^. dsprsNextMarker

instance Lude.AWSRequest DescribeSSLPolicies where
  type Rs DescribeSSLPolicies = DescribeSSLPoliciesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeSSLPoliciesResult"
      ( \s h x ->
          DescribeSSLPoliciesResponse'
            Lude.<$> ( x Lude..@? "SslPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSSLPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSSLPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSSLPolicies where
  toQuery DescribeSSLPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSSLPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Names"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> names),
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeSSLPoliciesResponse' smart constructor.
data DescribeSSLPoliciesResponse = DescribeSSLPoliciesResponse'
  { sslPolicies ::
      Lude.Maybe [SSLPolicy],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeSSLPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'responseStatus' - The response status code.
-- * 'sslPolicies' - Information about the security policies.
mkDescribeSSLPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSSLPoliciesResponse
mkDescribeSSLPoliciesResponse pResponseStatus_ =
  DescribeSSLPoliciesResponse'
    { sslPolicies = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the security policies.
--
-- /Note:/ Consider using 'sslPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsSSLPolicies :: Lens.Lens' DescribeSSLPoliciesResponse (Lude.Maybe [SSLPolicy])
dsprsSSLPolicies = Lens.lens (sslPolicies :: DescribeSSLPoliciesResponse -> Lude.Maybe [SSLPolicy]) (\s a -> s {sslPolicies = a} :: DescribeSSLPoliciesResponse)
{-# DEPRECATED dsprsSSLPolicies "Use generic-lens or generic-optics with 'sslPolicies' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsNextMarker :: Lens.Lens' DescribeSSLPoliciesResponse (Lude.Maybe Lude.Text)
dsprsNextMarker = Lens.lens (nextMarker :: DescribeSSLPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeSSLPoliciesResponse)
{-# DEPRECATED dsprsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DescribeSSLPoliciesResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DescribeSSLPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSSLPoliciesResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
