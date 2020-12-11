{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a trail, up to a limit of 50. Overwrites an existing tag's value when a new value is specified for an existing tag key. Tag key names must be unique for a trail; you cannot have two keys with the same name but different values. If you specify a key without a value, the tag will be created with the specified key and a value of null. You can tag a trail that applies to all AWS Regions only from the Region in which the trail was created (also known as its home region).
module Network.AWS.CloudTrail.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atTagsList,
    atResourceId,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Specifies the tags to add to a trail.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { tagsList :: Lude.Maybe [Tag],
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - Specifies the ARN of the trail to which one or more tags will be added. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 'tagsList' - Contains a list of CloudTrail tags, up to a limit of 50
mkAddTags ::
  -- | 'resourceId'
  Lude.Text ->
  AddTags
mkAddTags pResourceId_ =
  AddTags' {tagsList = Lude.Nothing, resourceId = pResourceId_}

-- | Contains a list of CloudTrail tags, up to a limit of 50
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTagsList :: Lens.Lens' AddTags (Lude.Maybe [Tag])
atTagsList = Lens.lens (tagsList :: AddTags -> Lude.Maybe [Tag]) (\s a -> s {tagsList = a} :: AddTags)
{-# DEPRECATED atTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

-- | Specifies the ARN of the trail to which one or more tags will be added. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Lude.Text
atResourceId = Lens.lens (resourceId :: AddTags -> Lude.Text) (\s a -> s {resourceId = a} :: AddTags)
{-# DEPRECATED atResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.AddTags" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagsList" Lude..=) Lude.<$> tagsList,
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
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

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsResponse
mkAddTagsResponse pResponseStatus_ =
  AddTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResponseStatus :: Lens.Lens' AddTagsResponse Lude.Int
atrsResponseStatus = Lens.lens (responseStatus :: AddTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsResponse)
{-# DEPRECATED atrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
