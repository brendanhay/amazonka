{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.
module Network.AWS.Discovery.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctConfigurationIds,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,

    -- ** Response lenses
    ctrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | A list of configuration items that you want to tag.
    configurationIds :: [Lude.Text],
    -- | Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example:
    --
    -- @{"key": "serverType", "value": "webServer"}@
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'configurationIds' - A list of configuration items that you want to tag.
-- * 'tags' - Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
mkCreateTags ::
  CreateTags
mkCreateTags =
  CreateTags' {configurationIds = Lude.mempty, tags = Lude.mempty}

-- | A list of configuration items that you want to tag.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationIds :: Lens.Lens' CreateTags [Lude.Text]
ctConfigurationIds = Lens.lens (configurationIds :: CreateTags -> [Lude.Text]) (\s a -> s {configurationIds = a} :: CreateTags)
{-# DEPRECATED ctConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

-- | Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Tag]
ctTags = Lens.lens (tags :: CreateTags -> [Tag]) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPoseidonService_V2015_11_01.CreateTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("configurationIds" Lude..= configurationIds),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath CreateTags where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTagsResponse
mkCreateTagsResponse pResponseStatus_ =
  CreateTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTagsResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTagsResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
