{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for up to 10 health checks or hosted zones.
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ListTagsForResources
  ( -- * Creating a request
    ListTagsForResources (..),
    mkListTagsForResources,

    -- ** Request lenses
    lResourceType,
    lResourceIds,

    -- * Destructuring the response
    ListTagsForResourcesResponse (..),
    mkListTagsForResourcesResponse,

    -- ** Response lenses
    lrsResponseStatus,
    lrsResourceTagSets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the health checks or hosted zones for which you want to list tags.
--
-- /See:/ 'mkListTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
  { resourceType ::
      TagResourceType,
    resourceIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResources' with the minimum fields required to make a request.
--
-- * 'resourceIds' - A complex type that contains the ResourceId element for each resource for which you want to get a list of tags.
-- * 'resourceType' - The type of the resources.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
mkListTagsForResources ::
  -- | 'resourceType'
  TagResourceType ->
  -- | 'resourceIds'
  Lude.NonEmpty Lude.Text ->
  ListTagsForResources
mkListTagsForResources pResourceType_ pResourceIds_ =
  ListTagsForResources'
    { resourceType = pResourceType_,
      resourceIds = pResourceIds_
    }

-- | The type of the resources.
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
lResourceType :: Lens.Lens' ListTagsForResources TagResourceType
lResourceType = Lens.lens (resourceType :: ListTagsForResources -> TagResourceType) (\s a -> s {resourceType = a} :: ListTagsForResources)
{-# DEPRECATED lResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A complex type that contains the ResourceId element for each resource for which you want to get a list of tags.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceIds :: Lens.Lens' ListTagsForResources (Lude.NonEmpty Lude.Text)
lResourceIds = Lens.lens (resourceIds :: ListTagsForResources -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceIds = a} :: ListTagsForResources)
{-# DEPRECATED lResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

instance Lude.AWSRequest ListTagsForResources where
  type Rs ListTagsForResources = ListTagsForResourcesResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTagsForResourcesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "ResourceTagSets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "ResourceTagSet"
                     )
      )

instance Lude.ToElement ListTagsForResources where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ListTagsForResourcesRequest"

instance Lude.ToHeaders ListTagsForResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResources where
  toPath ListTagsForResources' {..} =
    Lude.mconcat ["/2013-04-01/tags/", Lude.toBS resourceType]

instance Lude.ToQuery ListTagsForResources where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML ListTagsForResources where
  toXML ListTagsForResources' {..} =
    Lude.mconcat
      ["ResourceIds" Lude.@= Lude.toXMLList "ResourceId" resourceIds]

-- | A complex type containing tags for the specified resources.
--
-- /See:/ 'mkListTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
  { responseStatus ::
      Lude.Int,
    resourceTagSets ::
      [ResourceTagSet]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourcesResponse' with the minimum fields required to make a request.
--
-- * 'resourceTagSets' - A list of @ResourceTagSet@ s containing tags associated with the specified resources.
-- * 'responseStatus' - The response status code.
mkListTagsForResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourcesResponse
mkListTagsForResourcesResponse pResponseStatus_ =
  ListTagsForResourcesResponse'
    { responseStatus = pResponseStatus_,
      resourceTagSets = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTagsForResourcesResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourcesResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of @ResourceTagSet@ s containing tags associated with the specified resources.
--
-- /Note:/ Consider using 'resourceTagSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResourceTagSets :: Lens.Lens' ListTagsForResourcesResponse [ResourceTagSet]
lrsResourceTagSets = Lens.lens (resourceTagSets :: ListTagsForResourcesResponse -> [ResourceTagSet]) (\s a -> s {resourceTagSets = a} :: ListTagsForResourcesResponse)
{-# DEPRECATED lrsResourceTagSets "Use generic-lens or generic-optics with 'resourceTagSets' instead." #-}
