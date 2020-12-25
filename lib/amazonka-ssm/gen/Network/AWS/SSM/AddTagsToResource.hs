{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified resource. Tags are metadata that you can assign to your documents, managed instances, maintenance windows, Parameter Store parameters, and patch baselines. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. For example, you could define a set of tags for your account's managed instances that helps you track each instance's owner and stack level. For example: Key=Owner and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production, Pre-Production, or Test.
--
-- Each resource can have a maximum of 50 tags.
-- We recommend that you devise a set of tag keys that meets your needs for each resource type. Using a consistent set of tag keys makes it easier for you to manage your resources. You can search and filter the resources based on the tags you add. Tags don't have any semantic meaning to and are interpreted strictly as a string of characters.
-- For more information about using tags with EC2 instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> in the /Amazon EC2 User Guide/ .
module Network.AWS.SSM.AddTagsToResource
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceType,
    attrResourceId,
    attrTags,

    -- * Destructuring the response
    AddTagsToResourceResponse (..),
    mkAddTagsToResourceResponse,

    -- ** Response lenses
    attrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | Specifies the type of resource you are tagging.
    resourceType :: Types.ResourceTypeForTagging,
    -- | The resource ID you want to tag.
    --
    -- Use the ID of the resource. Here are some examples:
    -- ManagedInstance: mi-012345abcde
    -- MaintenanceWindow: mw-012345abcde
    -- PatchBaseline: pb-012345abcde
    -- For the Document and Parameter values, use the name of the resource.
    resourceId :: Types.ResourceId,
    -- | One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource ::
  -- | 'resourceType'
  Types.ResourceTypeForTagging ->
  -- | 'resourceId'
  Types.ResourceId ->
  AddTagsToResource
mkAddTagsToResource resourceType resourceId =
  AddTagsToResource' {resourceType, resourceId, tags = Core.mempty}

-- | Specifies the type of resource you are tagging.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceType :: Lens.Lens' AddTagsToResource Types.ResourceTypeForTagging
attrResourceType = Lens.field @"resourceType"
{-# DEPRECATED attrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
-- ManagedInstance: mi-012345abcde
-- MaintenanceWindow: mw-012345abcde
-- PatchBaseline: pb-012345abcde
-- For the Document and Parameter values, use the name of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceId :: Lens.Lens' AddTagsToResource Types.ResourceId
attrResourceId = Lens.field @"resourceId"
{-# DEPRECATED attrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTags = Lens.field @"tags"
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON AddTagsToResource where
  toJSON AddTagsToResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.AddTagsToResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResourceResponse' value with any optional fields omitted.
mkAddTagsToResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddTagsToResourceResponse
mkAddTagsToResourceResponse responseStatus =
  AddTagsToResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
attrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED attrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
