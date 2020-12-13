{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags.
module Network.AWS.AutoScaling.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtTags,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTags' smart constructor.
newtype DeleteTags = DeleteTags'
  { -- | One or more tags.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'tags' - One or more tags.
mkDeleteTags ::
  DeleteTags
mkDeleteTags = DeleteTags' {tags = Lude.mempty}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTags :: Lens.Lens' DeleteTags [Tag]
dtTags = Lens.lens (tags :: DeleteTags -> [Tag]) (\s a -> s {tags = a} :: DeleteTags)
{-# DEPRECATED dtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeleteTagsResponse'

instance Lude.ToHeaders DeleteTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTags" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
mkDeleteTagsResponse ::
  DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
