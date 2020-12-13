{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCachePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of cache policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListCachePolicies
  ( -- * Creating a request
    ListCachePolicies (..),
    mkListCachePolicies,

    -- ** Request lenses
    lcpMarker,
    lcpMaxItems,
    lcpType,

    -- * Destructuring the response
    ListCachePoliciesResponse (..),
    mkListCachePoliciesResponse,

    -- ** Response lenses
    lcprsCachePolicyList,
    lcprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCachePolicies' smart constructor.
data ListCachePolicies = ListCachePolicies'
  { -- | Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of cache policies that you want in the response.
    maxItems :: Lude.Maybe Lude.Text,
    -- | A filter to return only the specified kinds of cache policies. Valid values are:
    --
    --
    --     * @managed@ – Returns only the managed policies created by AWS.
    --
    --
    --     * @custom@ – Returns only the custom policies created in your AWS account.
    type' :: Lude.Maybe CachePolicyType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCachePolicies' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of cache policies that you want in the response.
-- * 'type'' - A filter to return only the specified kinds of cache policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
mkListCachePolicies ::
  ListCachePolicies
mkListCachePolicies =
  ListCachePolicies'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMarker :: Lens.Lens' ListCachePolicies (Lude.Maybe Lude.Text)
lcpMarker = Lens.lens (marker :: ListCachePolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCachePolicies)
{-# DEPRECATED lcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of cache policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMaxItems :: Lens.Lens' ListCachePolicies (Lude.Maybe Lude.Text)
lcpMaxItems = Lens.lens (maxItems :: ListCachePolicies -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListCachePolicies)
{-# DEPRECATED lcpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A filter to return only the specified kinds of cache policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpType :: Lens.Lens' ListCachePolicies (Lude.Maybe CachePolicyType)
lcpType = Lens.lens (type' :: ListCachePolicies -> Lude.Maybe CachePolicyType) (\s a -> s {type' = a} :: ListCachePolicies)
{-# DEPRECATED lcpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest ListCachePolicies where
  type Rs ListCachePolicies = ListCachePoliciesResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListCachePoliciesResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCachePolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCachePolicies where
  toPath = Lude.const "/2020-05-31/cache-policy"

instance Lude.ToQuery ListCachePolicies where
  toQuery ListCachePolicies' {..} =
    Lude.mconcat
      [ "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "Type" Lude.=: type'
      ]

-- | /See:/ 'mkListCachePoliciesResponse' smart constructor.
data ListCachePoliciesResponse = ListCachePoliciesResponse'
  { -- | A list of cache policies.
    cachePolicyList :: Lude.Maybe CachePolicyList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCachePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'cachePolicyList' - A list of cache policies.
-- * 'responseStatus' - The response status code.
mkListCachePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCachePoliciesResponse
mkListCachePoliciesResponse pResponseStatus_ =
  ListCachePoliciesResponse'
    { cachePolicyList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of cache policies.
--
-- /Note:/ Consider using 'cachePolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprsCachePolicyList :: Lens.Lens' ListCachePoliciesResponse (Lude.Maybe CachePolicyList)
lcprsCachePolicyList = Lens.lens (cachePolicyList :: ListCachePoliciesResponse -> Lude.Maybe CachePolicyList) (\s a -> s {cachePolicyList = a} :: ListCachePoliciesResponse)
{-# DEPRECATED lcprsCachePolicyList "Use generic-lens or generic-optics with 'cachePolicyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprsResponseStatus :: Lens.Lens' ListCachePoliciesResponse Lude.Int
lcprsResponseStatus = Lens.lens (responseStatus :: ListCachePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCachePoliciesResponse)
{-# DEPRECATED lcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
