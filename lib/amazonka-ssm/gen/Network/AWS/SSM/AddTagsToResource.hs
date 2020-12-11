{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    attrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceType ::
      ResourceTypeForTagging,
    resourceId :: Lude.Text,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
-- ManagedInstance: mi-012345abcde
-- MaintenanceWindow: mw-012345abcde
-- PatchBaseline: pb-012345abcde
-- For the Document and Parameter values, use the name of the resource.
-- * 'resourceType' - Specifies the type of resource you are tagging.
-- * 'tags' - One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
--
-- /Important:/ Do not enter personally identifiable information in this field.
mkAddTagsToResource ::
  -- | 'resourceType'
  ResourceTypeForTagging ->
  -- | 'resourceId'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceType_ pResourceId_ =
  AddTagsToResource'
    { resourceType = pResourceType_,
      resourceId = pResourceId_,
      tags = Lude.mempty
    }

-- | Specifies the type of resource you are tagging.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceType :: Lens.Lens' AddTagsToResource ResourceTypeForTagging
attrResourceType = Lens.lens (resourceType :: AddTagsToResource -> ResourceTypeForTagging) (\s a -> s {resourceType = a} :: AddTagsToResource)
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
attrResourceId :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceId = Lens.lens (resourceId :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceId = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTagsToResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.AddTagsToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddTagsToResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsToResourceResponse
mkAddTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Lude.Int
attrrsResponseStatus = Lens.lens (responseStatus :: AddTagsToResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
