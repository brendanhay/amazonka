{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTargetsForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List targets for the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForPolicy
  ( -- * Creating a request
    ListTargetsForPolicy (..),
    mkListTargetsForPolicy,

    -- ** Request lenses
    ltfpMarker,
    ltfpPageSize,
    ltfpPolicyName,

    -- * Destructuring the response
    ListTargetsForPolicyResponse (..),
    mkListTargetsForPolicyResponse,

    -- ** Response lenses
    ltfprsTargets,
    ltfprsNextMarker,
    ltfprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { marker ::
      Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsForPolicy' with the minimum fields required to make a request.
--
-- * 'marker' - A marker used to get the next set of results.
-- * 'pageSize' - The maximum number of results to return at one time.
-- * 'policyName' - The policy name.
mkListTargetsForPolicy ::
  -- | 'policyName'
  Lude.Text ->
  ListTargetsForPolicy
mkListTargetsForPolicy pPolicyName_ =
  ListTargetsForPolicy'
    { marker = Lude.Nothing,
      pageSize = Lude.Nothing,
      policyName = pPolicyName_
    }

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMarker :: Lens.Lens' ListTargetsForPolicy (Lude.Maybe Lude.Text)
ltfpMarker = Lens.lens (marker :: ListTargetsForPolicy -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPageSize :: Lens.Lens' ListTargetsForPolicy (Lude.Maybe Lude.Natural)
ltfpPageSize = Lens.lens (pageSize :: ListTargetsForPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPolicyName :: Lens.Lens' ListTargetsForPolicy Lude.Text
ltfpPolicyName = Lens.lens (policyName :: ListTargetsForPolicy -> Lude.Text) (\s a -> s {policyName = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Page.AWSPager ListTargetsForPolicy where
  page rq rs
    | Page.stop (rs Lens.^. ltfprsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfprsTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfpMarker Lens..~ rs Lens.^. ltfprsNextMarker

instance Lude.AWSRequest ListTargetsForPolicy where
  type Rs ListTargetsForPolicy = ListTargetsForPolicyResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Lude.<$> (x Lude..?> "targets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTargetsForPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListTargetsForPolicy where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ListTargetsForPolicy where
  toPath ListTargetsForPolicy' {..} =
    Lude.mconcat ["/policy-targets/", Lude.toBS policyName]

instance Lude.ToQuery ListTargetsForPolicy where
  toQuery ListTargetsForPolicy' {..} =
    Lude.mconcat
      ["marker" Lude.=: marker, "pageSize" Lude.=: pageSize]

-- | /See:/ 'mkListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { targets ::
      Lude.Maybe [Lude.Text],
    nextMarker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTargetsForPolicyResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - A marker used to get the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'targets' - The policy targets.
mkListTargetsForPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTargetsForPolicyResponse
mkListTargetsForPolicyResponse pResponseStatus_ =
  ListTargetsForPolicyResponse'
    { targets = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsTargets :: Lens.Lens' ListTargetsForPolicyResponse (Lude.Maybe [Lude.Text])
ltfprsTargets = Lens.lens (targets :: ListTargetsForPolicyResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {targets = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsNextMarker :: Lens.Lens' ListTargetsForPolicyResponse (Lude.Maybe Lude.Text)
ltfprsNextMarker = Lens.lens (nextMarker :: ListTargetsForPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsResponseStatus :: Lens.Lens' ListTargetsForPolicyResponse Lude.Int
ltfprsResponseStatus = Lens.lens (responseStatus :: ListTargetsForPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
