{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAttachedPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified thing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAttachedPolicies
  ( -- * Creating a request
    ListAttachedPolicies (..),
    mkListAttachedPolicies,

    -- ** Request lenses
    lapMarker,
    lapRecursive,
    lapPageSize,
    lapTarget,

    -- * Destructuring the response
    ListAttachedPoliciesResponse (..),
    mkListAttachedPoliciesResponse,

    -- ** Response lenses
    laprsNextMarker,
    laprsPolicies,
    laprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttachedPolicies' smart constructor.
data ListAttachedPolicies = ListAttachedPolicies'
  { -- | The token to retrieve the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | When true, recursively list attached policies.
    recursive :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to be returned per request.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The group or principal for which the policies will be listed. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedPolicies' with the minimum fields required to make a request.
--
-- * 'marker' - The token to retrieve the next set of results.
-- * 'recursive' - When true, recursively list attached policies.
-- * 'pageSize' - The maximum number of results to be returned per request.
-- * 'target' - The group or principal for which the policies will be listed. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
mkListAttachedPolicies ::
  -- | 'target'
  Lude.Text ->
  ListAttachedPolicies
mkListAttachedPolicies pTarget_ =
  ListAttachedPolicies'
    { marker = Lude.Nothing,
      recursive = Lude.Nothing,
      pageSize = Lude.Nothing,
      target = pTarget_
    }

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapMarker :: Lens.Lens' ListAttachedPolicies (Lude.Maybe Lude.Text)
lapMarker = Lens.lens (marker :: ListAttachedPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedPolicies)
{-# DEPRECATED lapMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | When true, recursively list attached policies.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapRecursive :: Lens.Lens' ListAttachedPolicies (Lude.Maybe Lude.Bool)
lapRecursive = Lens.lens (recursive :: ListAttachedPolicies -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: ListAttachedPolicies)
{-# DEPRECATED lapRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapPageSize :: Lens.Lens' ListAttachedPolicies (Lude.Maybe Lude.Natural)
lapPageSize = Lens.lens (pageSize :: ListAttachedPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListAttachedPolicies)
{-# DEPRECATED lapPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The group or principal for which the policies will be listed. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapTarget :: Lens.Lens' ListAttachedPolicies Lude.Text
lapTarget = Lens.lens (target :: ListAttachedPolicies -> Lude.Text) (\s a -> s {target = a} :: ListAttachedPolicies)
{-# DEPRECATED lapTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Page.AWSPager ListAttachedPolicies where
  page rq rs
    | Page.stop (rs Lens.^. laprsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. laprsPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lapMarker Lens..~ rs Lens.^. laprsNextMarker

instance Lude.AWSRequest ListAttachedPolicies where
  type Rs ListAttachedPolicies = ListAttachedPoliciesResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAttachedPoliciesResponse'
            Lude.<$> (x Lude..?> "nextMarker")
            Lude.<*> (x Lude..?> "policies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttachedPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListAttachedPolicies where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ListAttachedPolicies where
  toPath ListAttachedPolicies' {..} =
    Lude.mconcat ["/attached-policies/", Lude.toBS target]

instance Lude.ToQuery ListAttachedPolicies where
  toQuery ListAttachedPolicies' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "recursive" Lude.=: recursive,
        "pageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkListAttachedPoliciesResponse' smart constructor.
data ListAttachedPoliciesResponse = ListAttachedPoliciesResponse'
  { -- | The token to retrieve the next set of results, or ``null`` if there are no more results.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The policies.
    policies :: Lude.Maybe [Policy],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - The token to retrieve the next set of results, or ``null`` if there are no more results.
-- * 'policies' - The policies.
-- * 'responseStatus' - The response status code.
mkListAttachedPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttachedPoliciesResponse
mkListAttachedPoliciesResponse pResponseStatus_ =
  ListAttachedPoliciesResponse'
    { nextMarker = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next set of results, or ``null`` if there are no more results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprsNextMarker :: Lens.Lens' ListAttachedPoliciesResponse (Lude.Maybe Lude.Text)
laprsNextMarker = Lens.lens (nextMarker :: ListAttachedPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListAttachedPoliciesResponse)
{-# DEPRECATED laprsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprsPolicies :: Lens.Lens' ListAttachedPoliciesResponse (Lude.Maybe [Policy])
laprsPolicies = Lens.lens (policies :: ListAttachedPoliciesResponse -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: ListAttachedPoliciesResponse)
{-# DEPRECATED laprsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laprsResponseStatus :: Lens.Lens' ListAttachedPoliciesResponse Lude.Int
laprsResponseStatus = Lens.lens (responseStatus :: ListAttachedPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttachedPoliciesResponse)
{-# DEPRECATED laprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
