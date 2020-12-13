{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a resource group with the specified ARN. Existing tags on a resource group are not changed if they are not specified in the request parameters.
--
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
module Network.AWS.ResourceGroups.Tag
  ( -- * Creating a request
    Tag (..),
    mkTag,

    -- ** Request lenses
    tARN,
    tTags,

    -- * Destructuring the response
    TagResponse (..),
    mkTagResponse,

    -- ** Response lenses
    trsARN,
    trsTags,
    trsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The ARN of the resource group to which to add tags.
    arn :: Lude.Text,
    -- | The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource group to which to add tags.
-- * 'tags' - The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
mkTag ::
  -- | 'arn'
  Lude.Text ->
  Tag
mkTag pARN_ = Tag' {arn = pARN_, tags = Lude.mempty}

-- | The ARN of the resource group to which to add tags.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tARN :: Lens.Lens' Tag Lude.Text
tARN = Lens.lens (arn :: Tag -> Lude.Text) (\s a -> s {arn = a} :: Tag)
{-# DEPRECATED tARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTags :: Lens.Lens' Tag (Lude.HashMap Lude.Text (Lude.Text))
tTags = Lens.lens (tags :: Tag -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: Tag)
{-# DEPRECATED tTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest Tag where
  type Rs Tag = TagResponse
  request = Req.putJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TagResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Tag where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Tags" Lude..= tags)])

instance Lude.ToPath Tag where
  toPath Tag' {..} =
    Lude.mconcat ["/resources/", Lude.toBS arn, "/tags"]

instance Lude.ToQuery Tag where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResponse' smart constructor.
data TagResponse = TagResponse'
  { -- | The ARN of the tagged resource.
    arn :: Lude.Maybe Lude.Text,
    -- | The tags that have been added to the specified resource group.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the tagged resource.
-- * 'tags' - The tags that have been added to the specified resource group.
-- * 'responseStatus' - The response status code.
mkTagResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagResponse
mkTagResponse pResponseStatus_ =
  TagResponse'
    { arn = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the tagged resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsARN :: Lens.Lens' TagResponse (Lude.Maybe Lude.Text)
trsARN = Lens.lens (arn :: TagResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TagResponse)
{-# DEPRECATED trsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags that have been added to the specified resource group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTags :: Lens.Lens' TagResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
trsTags = Lens.lens (tags :: TagResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: TagResponse)
{-# DEPRECATED trsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsResponseStatus :: Lens.Lens' TagResponse Lude.Int
trsResponseStatus = Lens.lens (responseStatus :: TagResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagResponse)
{-# DEPRECATED trsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
