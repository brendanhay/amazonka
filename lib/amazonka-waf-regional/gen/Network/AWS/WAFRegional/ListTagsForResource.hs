{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the tags associated with the specified AWS resource. Tags are key:value pairs that you can use to categorize and manage your resources, for purposes like billing. For example, you might set the tag key to "customer" and the value to the customer name or ID. You can specify one or more tags to add to each AWS resource, up to 50 tags for a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can't manage or view tags through the AWS WAF Classic console. You can tag the AWS resources that you manage through AWS WAF Classic: web ACLs, rule groups, and rules.
module Network.AWS.WAFRegional.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResourceARN,
    ltfrNextMarker,
    ltfrLimit,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsTagInfoForResource,
    ltfrrsNextMarker,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- |
    resourceARN :: Lude.Text,
    -- |
    nextMarker :: Lude.Maybe Lude.Text,
    -- |
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' -
-- * 'nextMarker' -
-- * 'limit' -
mkListTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { resourceARN = pResourceARN_,
      nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceARN = Lens.lens (resourceARN :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- |
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextMarker :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrNextMarker = Lens.lens (nextMarker :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListTagsForResource)
{-# DEPRECATED ltfrNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- |
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrLimit :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Natural)
ltfrLimit = Lens.lens (limit :: ListTagsForResource -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTagsForResource)
{-# DEPRECATED ltfrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "TagInfoForResource")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListTagsForResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- |
    tagInfoForResource :: Lude.Maybe TagInfoForResource,
    -- |
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'tagInfoForResource' -
-- * 'nextMarker' -
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { tagInfoForResource = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- |
--
-- /Note:/ Consider using 'tagInfoForResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTagInfoForResource :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe TagInfoForResource)
ltfrrsTagInfoForResource = Lens.lens (tagInfoForResource :: ListTagsForResourceResponse -> Lude.Maybe TagInfoForResource) (\s a -> s {tagInfoForResource = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTagInfoForResource "Use generic-lens or generic-optics with 'tagInfoForResource' instead." #-}

-- |
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsNextMarker :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsNextMarker = Lens.lens (nextMarker :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
