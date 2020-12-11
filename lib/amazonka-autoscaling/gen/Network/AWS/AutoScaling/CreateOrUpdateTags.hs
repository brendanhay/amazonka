{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates tags for the specified Auto Scaling group.
--
-- When you specify a tag with a key that already exists, the operation overwrites the previous tag definition, and you do not get an error message.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.CreateOrUpdateTags
  ( -- * Creating a request
    CreateOrUpdateTags (..),
    mkCreateOrUpdateTags,

    -- ** Request lenses
    coutTags,

    -- * Destructuring the response
    CreateOrUpdateTagsResponse (..),
    mkCreateOrUpdateTagsResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOrUpdateTags' smart constructor.
newtype CreateOrUpdateTags = CreateOrUpdateTags' {tags :: [Tag]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrUpdateTags' with the minimum fields required to make a request.
--
-- * 'tags' - One or more tags.
mkCreateOrUpdateTags ::
  CreateOrUpdateTags
mkCreateOrUpdateTags = CreateOrUpdateTags' {tags = Lude.mempty}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coutTags :: Lens.Lens' CreateOrUpdateTags [Tag]
coutTags = Lens.lens (tags :: CreateOrUpdateTags -> [Tag]) (\s a -> s {tags = a} :: CreateOrUpdateTags)
{-# DEPRECATED coutTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateOrUpdateTags where
  type Rs CreateOrUpdateTags = CreateOrUpdateTagsResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull CreateOrUpdateTagsResponse'

instance Lude.ToHeaders CreateOrUpdateTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateOrUpdateTags where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOrUpdateTags where
  toQuery CreateOrUpdateTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateOrUpdateTags" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | /See:/ 'mkCreateOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrUpdateTagsResponse' with the minimum fields required to make a request.
mkCreateOrUpdateTagsResponse ::
  CreateOrUpdateTagsResponse
mkCreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
