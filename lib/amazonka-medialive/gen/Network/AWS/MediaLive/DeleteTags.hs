{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags for a resource
module Network.AWS.MediaLive.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtTagKeys,
    dtResourceARN,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteTagsRequest
--
-- /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | An array of tag keys to delete
    tagKeys :: [Lude.Text],
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'tagKeys' - An array of tag keys to delete
-- * 'resourceARN' -
mkDeleteTags ::
  -- | 'resourceARN'
  Lude.Text ->
  DeleteTags
mkDeleteTags pResourceARN_ =
  DeleteTags' {tagKeys = Lude.mempty, resourceARN = pResourceARN_}

-- | An array of tag keys to delete
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DeleteTags [Lude.Text]
dtTagKeys = Lens.lens (tagKeys :: DeleteTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: DeleteTags)
{-# DEPRECATED dtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceARN :: Lens.Lens' DeleteTags Lude.Text
dtResourceARN = Lens.lens (resourceARN :: DeleteTags -> Lude.Text) (\s a -> s {resourceARN = a} :: DeleteTags)
{-# DEPRECATED dtResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.delete mediaLiveService
  response = Res.receiveNull DeleteTagsResponse'

instance Lude.ToHeaders DeleteTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteTags where
  toPath DeleteTags' {..} =
    Lude.mconcat ["/prod/tags/", Lude.toBS resourceARN]

instance Lude.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Lude.mconcat
      ["tagKeys" Lude.=: Lude.toQueryList "member" tagKeys]

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
mkDeleteTagsResponse ::
  DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
