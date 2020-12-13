{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified resource. This operation is supported in storage gateways of all types.
module Network.AWS.StorageGateway.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrTagKeys,
    rtfrResourceARN,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrsResourceARN,
    rtfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | RemoveTagsFromResourceInput
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
    tagKeys :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
mkRemoveTagsFromResource ::
  -- | 'resourceARN'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    { tagKeys = Lude.mempty,
      resourceARN = pResourceARN_
    }

-- | The keys of the tags you want to remove from the specified resource. A tag is composed of a key-value pair.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeys = Lens.lens (tagKeys :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource you want to remove the tags from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceARN :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceARN = Lens.lens (resourceARN :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceARN = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Lude.<$> (x Lude..?> "ResourceARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.RemoveTagsFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TagKeys" Lude..= tagKeys),
            Lude.Just ("ResourceARN" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery = Lude.const Lude.mempty

-- | RemoveTagsFromResourceOutput
--
-- /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource that the tags were removed from.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource that the tags were removed from.
-- * 'responseStatus' - The response status code.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse pResponseStatus_ =
  RemoveTagsFromResourceResponse'
    { resourceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource that the tags were removed from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsResourceARN :: Lens.Lens' RemoveTagsFromResourceResponse (Lude.Maybe Lude.Text)
rtfrrsResourceARN = Lens.lens (resourceARN :: RemoveTagsFromResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Lude.Int
rtfrrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
