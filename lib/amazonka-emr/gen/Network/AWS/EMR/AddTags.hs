{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
module Network.AWS.EMR.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atResourceId,
    atTags,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input identifies a cluster and a list of tags to attach.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags' {resourceId :: Lude.Text, tags :: [Tag]}
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
-- * 'resourceId' - The Amazon EMR resource identifier to which tags will be added. This value must be a cluster identifier.
-- * 'tags' - A list of tags to associate with a cluster and propagate to EC2 instances. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
mkAddTags ::
  -- | 'resourceId'
  Lude.Text ->
  AddTags
mkAddTags pResourceId_ =
  AddTags' {resourceId = pResourceId_, tags = Lude.mempty}

-- | The Amazon EMR resource identifier to which tags will be added. This value must be a cluster identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Lude.Text
atResourceId = Lens.lens (resourceId :: AddTags -> Lude.Text) (\s a -> s {resourceId = a} :: AddTags)
{-# DEPRECATED atResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tags to associate with a cluster and propagate to EC2 instances. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Tag]
atTags = Lens.lens (tags :: AddTags -> [Tag]) (\s a -> s {tags = a} :: AddTags)
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON emrService
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
              Lude.=# ("ElasticMapReduce.AddTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | This output indicates the result of adding tags to a resource.
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
