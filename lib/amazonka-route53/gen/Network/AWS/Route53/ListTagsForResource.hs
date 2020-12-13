{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for one health check or hosted zone.
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    lResourceId,
    lResourceType,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsResourceTagSet,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type containing information about a request for a list of the tags that are associated with an individual resource.
--
-- /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The ID of the resource for which you want to retrieve tags.
    resourceId :: Lude.Text,
    -- | The type of the resource.
    --
    --
    --     * The resource type for health checks is @healthcheck@ .
    --
    --
    --     * The resource type for hosted zones is @hostedzone@ .
    resourceType :: TagResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource for which you want to retrieve tags.
-- * 'resourceType' - The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
mkListTagsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  TagResourceType ->
  ListTagsForResource
mkListTagsForResource pResourceId_ pResourceType_ =
  ListTagsForResource'
    { resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The ID of the resource for which you want to retrieve tags.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceId :: Lens.Lens' ListTagsForResource Lude.Text
lResourceId = Lens.lens (resourceId :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ListTagsForResource)
{-# DEPRECATED lResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceType :: Lens.Lens' ListTagsForResource TagResourceType
lResourceType = Lens.lens (resourceType :: ListTagsForResource -> TagResourceType) (\s a -> s {resourceType = a} :: ListTagsForResource)
{-# DEPRECATED lResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..@ "ResourceTagSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResource where
  toPath ListTagsForResource' {..} =
    Lude.mconcat
      [ "/2013-04-01/tags/",
        Lude.toBS resourceType,
        "/",
        Lude.toBS resourceId
      ]

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains information about the health checks or hosted zones for which you want to list tags.
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A @ResourceTagSet@ containing tags associated with the specified resource.
    resourceTagSet :: ResourceTagSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'resourceTagSet' - A @ResourceTagSet@ containing tags associated with the specified resource.
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'resourceTagSet'
  ResourceTagSet ->
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResourceTagSet_ pResponseStatus_ =
  ListTagsForResourceResponse'
    { resourceTagSet = pResourceTagSet_,
      responseStatus = pResponseStatus_
    }

-- | A @ResourceTagSet@ containing tags associated with the specified resource.
--
-- /Note:/ Consider using 'resourceTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResourceTagSet :: Lens.Lens' ListTagsForResourceResponse ResourceTagSet
ltfrrsResourceTagSet = Lens.lens (resourceTagSet :: ListTagsForResourceResponse -> ResourceTagSet) (\s a -> s {resourceTagSet = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResourceTagSet "Use generic-lens or generic-optics with 'resourceTagSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
