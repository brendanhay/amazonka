{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified Elastic Load Balancing resource. You can tag your Application Load Balancers, Network Load Balancers, Gateway Load Balancers, target groups, listeners, and rules.
--
-- Each tag consists of a key and an optional value. If a resource already has a tag with the same key, @AddTags@ updates its value.
module Network.AWS.ELBv2.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atResourceARNs,
    atTags,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { resourceARNs :: [Lude.Text],
    tags :: Lude.NonEmpty Tag
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
-- * 'resourceARNs' - The Amazon Resource Name (ARN) of the resource.
-- * 'tags' - The tags.
mkAddTags ::
  -- | 'tags'
  Lude.NonEmpty Tag ->
  AddTags
mkAddTags pTags_ =
  AddTags' {resourceARNs = Lude.mempty, tags = pTags_}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceARNs :: Lens.Lens' AddTags [Lude.Text]
atResourceARNs = Lens.lens (resourceARNs :: AddTags -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: AddTags)
{-# DEPRECATED atResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags (Lude.NonEmpty Tag)
atTags = Lens.lens (tags :: AddTags -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: AddTags)
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "AddTagsResult"
      ( \s h x ->
          AddTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery AddTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddTags" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ResourceArns" Lude.=: Lude.toQueryList "member" resourceARNs,
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | /See:/ 'mkAddTagsResponse' smart constructor.
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
