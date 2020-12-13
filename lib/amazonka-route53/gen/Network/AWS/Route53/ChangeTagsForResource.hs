{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, edits, or deletes tags for a health check or a hosted zone.
--
-- For information about using tags for cost allocation, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.Route53.ChangeTagsForResource
  ( -- * Creating a request
    ChangeTagsForResource (..),
    mkChangeTagsForResource,

    -- ** Request lenses
    ctfrResourceId,
    ctfrResourceType,
    ctfrRemoveTagKeys,
    ctfrAddTags,

    -- * Destructuring the response
    ChangeTagsForResourceResponse (..),
    mkChangeTagsForResourceResponse,

    -- ** Response lenses
    ctfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the tags that you want to add, edit, or delete.
--
-- /See:/ 'mkChangeTagsForResource' smart constructor.
data ChangeTagsForResource = ChangeTagsForResource'
  { -- | The ID of the resource for which you want to add, change, or delete tags.
    resourceId :: Lude.Text,
    -- | The type of the resource.
    --
    --
    --     * The resource type for health checks is @healthcheck@ .
    --
    --
    --     * The resource type for hosted zones is @hostedzone@ .
    resourceType :: TagResourceType,
    -- | A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
    removeTagKeys :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for.
    --
    -- You can add a maximum of 10 tags to a health check or a hosted zone.
    addTags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource for which you want to add, change, or delete tags.
-- * 'resourceType' - The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
-- * 'removeTagKeys' - A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
-- * 'addTags' - A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for.
--
-- You can add a maximum of 10 tags to a health check or a hosted zone.
mkChangeTagsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  TagResourceType ->
  ChangeTagsForResource
mkChangeTagsForResource pResourceId_ pResourceType_ =
  ChangeTagsForResource'
    { resourceId = pResourceId_,
      resourceType = pResourceType_,
      removeTagKeys = Lude.Nothing,
      addTags = Lude.Nothing
    }

-- | The ID of the resource for which you want to add, change, or delete tags.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrResourceId :: Lens.Lens' ChangeTagsForResource Lude.Text
ctfrResourceId = Lens.lens (resourceId :: ChangeTagsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ChangeTagsForResource)
{-# DEPRECATED ctfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

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
ctfrResourceType :: Lens.Lens' ChangeTagsForResource TagResourceType
ctfrResourceType = Lens.lens (resourceType :: ChangeTagsForResource -> TagResourceType) (\s a -> s {resourceType = a} :: ChangeTagsForResource)
{-# DEPRECATED ctfrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
--
-- /Note:/ Consider using 'removeTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrRemoveTagKeys :: Lens.Lens' ChangeTagsForResource (Lude.Maybe (Lude.NonEmpty Lude.Text))
ctfrRemoveTagKeys = Lens.lens (removeTagKeys :: ChangeTagsForResource -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {removeTagKeys = a} :: ChangeTagsForResource)
{-# DEPRECATED ctfrRemoveTagKeys "Use generic-lens or generic-optics with 'removeTagKeys' instead." #-}

-- | A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for.
--
-- You can add a maximum of 10 tags to a health check or a hosted zone.
--
-- /Note:/ Consider using 'addTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrAddTags :: Lens.Lens' ChangeTagsForResource (Lude.Maybe (Lude.NonEmpty Tag))
ctfrAddTags = Lens.lens (addTags :: ChangeTagsForResource -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {addTags = a} :: ChangeTagsForResource)
{-# DEPRECATED ctfrAddTags "Use generic-lens or generic-optics with 'addTags' instead." #-}

instance Lude.AWSRequest ChangeTagsForResource where
  type Rs ChangeTagsForResource = ChangeTagsForResourceResponse
  request = Req.postXML route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          ChangeTagsForResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement ChangeTagsForResource where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance Lude.ToHeaders ChangeTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ChangeTagsForResource where
  toPath ChangeTagsForResource' {..} =
    Lude.mconcat
      [ "/2013-04-01/tags/",
        Lude.toBS resourceType,
        "/",
        Lude.toBS resourceId
      ]

instance Lude.ToQuery ChangeTagsForResource where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML ChangeTagsForResource where
  toXML ChangeTagsForResource' {..} =
    Lude.mconcat
      [ "RemoveTagKeys"
          Lude.@= Lude.toXML (Lude.toXMLList "Key" Lude.<$> removeTagKeys),
        "AddTags"
          Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> addTags)
      ]

-- | Empty response for the request.
--
-- /See:/ 'mkChangeTagsForResourceResponse' smart constructor.
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkChangeTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ChangeTagsForResourceResponse
mkChangeTagsForResourceResponse pResponseStatus_ =
  ChangeTagsForResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctfrrsResponseStatus :: Lens.Lens' ChangeTagsForResourceResponse Lude.Int
ctfrrsResponseStatus = Lens.lens (responseStatus :: ChangeTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ChangeTagsForResourceResponse)
{-# DEPRECATED ctfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
