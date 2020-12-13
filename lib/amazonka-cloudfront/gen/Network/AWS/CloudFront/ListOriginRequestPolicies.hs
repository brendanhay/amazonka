{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListOriginRequestPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of origin request policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListOriginRequestPolicies
  ( -- * Creating a request
    ListOriginRequestPolicies (..),
    mkListOriginRequestPolicies,

    -- ** Request lenses
    lorpMarker,
    lorpMaxItems,
    lorpType,

    -- * Destructuring the response
    ListOriginRequestPoliciesResponse (..),
    mkListOriginRequestPoliciesResponse,

    -- ** Response lenses
    lorprsOriginRequestPolicyList,
    lorprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOriginRequestPolicies' smart constructor.
data ListOriginRequestPolicies = ListOriginRequestPolicies'
  { -- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of origin request policies that you want in the response.
    maxItems :: Lude.Maybe Lude.Text,
    -- | A filter to return only the specified kinds of origin request policies. Valid values are:
    --
    --
    --     * @managed@ – Returns only the managed policies created by AWS.
    --
    --
    --     * @custom@ – Returns only the custom policies created in your AWS account.
    type' :: Lude.Maybe OriginRequestPolicyType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOriginRequestPolicies' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of origin request policies that you want in the response.
-- * 'type'' - A filter to return only the specified kinds of origin request policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
mkListOriginRequestPolicies ::
  ListOriginRequestPolicies
mkListOriginRequestPolicies =
  ListOriginRequestPolicies'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMarker :: Lens.Lens' ListOriginRequestPolicies (Lude.Maybe Lude.Text)
lorpMarker = Lens.lens (marker :: ListOriginRequestPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListOriginRequestPolicies)
{-# DEPRECATED lorpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of origin request policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMaxItems :: Lens.Lens' ListOriginRequestPolicies (Lude.Maybe Lude.Text)
lorpMaxItems = Lens.lens (maxItems :: ListOriginRequestPolicies -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListOriginRequestPolicies)
{-# DEPRECATED lorpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A filter to return only the specified kinds of origin request policies. Valid values are:
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
lorpType :: Lens.Lens' ListOriginRequestPolicies (Lude.Maybe OriginRequestPolicyType)
lorpType = Lens.lens (type' :: ListOriginRequestPolicies -> Lude.Maybe OriginRequestPolicyType) (\s a -> s {type' = a} :: ListOriginRequestPolicies)
{-# DEPRECATED lorpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest ListOriginRequestPolicies where
  type
    Rs ListOriginRequestPolicies =
      ListOriginRequestPoliciesResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListOriginRequestPoliciesResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOriginRequestPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListOriginRequestPolicies where
  toPath = Lude.const "/2020-05-31/origin-request-policy"

instance Lude.ToQuery ListOriginRequestPolicies where
  toQuery ListOriginRequestPolicies' {..} =
    Lude.mconcat
      [ "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "Type" Lude.=: type'
      ]

-- | /See:/ 'mkListOriginRequestPoliciesResponse' smart constructor.
data ListOriginRequestPoliciesResponse = ListOriginRequestPoliciesResponse'
  { -- | A list of origin request policies.
    originRequestPolicyList :: Lude.Maybe OriginRequestPolicyList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOriginRequestPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'originRequestPolicyList' - A list of origin request policies.
-- * 'responseStatus' - The response status code.
mkListOriginRequestPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOriginRequestPoliciesResponse
mkListOriginRequestPoliciesResponse pResponseStatus_ =
  ListOriginRequestPoliciesResponse'
    { originRequestPolicyList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of origin request policies.
--
-- /Note:/ Consider using 'originRequestPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprsOriginRequestPolicyList :: Lens.Lens' ListOriginRequestPoliciesResponse (Lude.Maybe OriginRequestPolicyList)
lorprsOriginRequestPolicyList = Lens.lens (originRequestPolicyList :: ListOriginRequestPoliciesResponse -> Lude.Maybe OriginRequestPolicyList) (\s a -> s {originRequestPolicyList = a} :: ListOriginRequestPoliciesResponse)
{-# DEPRECATED lorprsOriginRequestPolicyList "Use generic-lens or generic-optics with 'originRequestPolicyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprsResponseStatus :: Lens.Lens' ListOriginRequestPoliciesResponse Lude.Int
lorprsResponseStatus = Lens.lens (responseStatus :: ListOriginRequestPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOriginRequestPoliciesResponse)
{-# DEPRECATED lorprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
