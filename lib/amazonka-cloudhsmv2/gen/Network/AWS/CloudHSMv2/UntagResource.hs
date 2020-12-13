{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tag or tags from the specified AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urResourceId,
    urTagKeyList,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The cluster identifier (ID) for the cluster whose tags you are removing. To find the cluster ID, use 'DescribeClusters' .
    resourceId :: Lude.Text,
    -- | A list of one or more tag keys for the tags that you are removing. Specify only the tag keys, not the tag values.
    tagKeyList :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The cluster identifier (ID) for the cluster whose tags you are removing. To find the cluster ID, use 'DescribeClusters' .
-- * 'tagKeyList' - A list of one or more tag keys for the tags that you are removing. Specify only the tag keys, not the tag values.
mkUntagResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'tagKeyList'
  Lude.NonEmpty Lude.Text ->
  UntagResource
mkUntagResource pResourceId_ pTagKeyList_ =
  UntagResource'
    { resourceId = pResourceId_,
      tagKeyList = pTagKeyList_
    }

-- | The cluster identifier (ID) for the cluster whose tags you are removing. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UntagResource Lude.Text
urResourceId = Lens.lens (resourceId :: UntagResource -> Lude.Text) (\s a -> s {resourceId = a} :: UntagResource)
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of one or more tag keys for the tags that you are removing. Specify only the tag keys, not the tag values.
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeyList :: Lens.Lens' UntagResource (Lude.NonEmpty Lude.Text)
urTagKeyList = Lens.lens (tagKeyList :: UntagResource -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeyList = a} :: UntagResource)
{-# DEPRECATED urTagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.UntagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("TagKeyList" Lude..= tagKeyList)
          ]
      )

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResourceResponse
mkUntagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResourceResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UntagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResourceResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
