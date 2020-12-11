{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.
module Network.AWS.Discovery.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtTags,
    dtConfigurationIds,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { tags :: Lude.Maybe [Tag],
    configurationIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'configurationIds' - A list of configuration items with tags that you want to delete.
-- * 'tags' - Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
mkDeleteTags ::
  DeleteTags
mkDeleteTags =
  DeleteTags' {tags = Lude.Nothing, configurationIds = Lude.mempty}

-- | Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTags :: Lens.Lens' DeleteTags (Lude.Maybe [Tag])
dtTags = Lens.lens (tags :: DeleteTags -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DeleteTags)
{-# DEPRECATED dtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of configuration items with tags that you want to delete.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtConfigurationIds :: Lens.Lens' DeleteTags [Lude.Text]
dtConfigurationIds = Lens.lens (configurationIds :: DeleteTags -> [Lude.Text]) (\s a -> s {configurationIds = a} :: DeleteTags)
{-# DEPRECATED dtConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPoseidonService_V2015_11_01.DeleteTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("configurationIds" Lude..= configurationIds)
          ]
      )

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
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

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTagsResponse
mkDeleteTagsResponse pResponseStatus_ =
  DeleteTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTagsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
