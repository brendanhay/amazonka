{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tag keys and their values from the specified Amazon Lightsail resource.
--
-- The @untag resource@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @resource name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urResourceName,
    urTagKeys,
    urResourceARN,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrsOperations,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The name of the resource from which you are removing a tag.
    resourceName :: Lude.Text,
    -- | The tag keys to delete from the specified resource.
    tagKeys :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the resource from which you want to remove a tag.
    resourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the resource from which you are removing a tag.
-- * 'tagKeys' - The tag keys to delete from the specified resource.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource from which you want to remove a tag.
mkUntagResource ::
  -- | 'resourceName'
  Lude.Text ->
  UntagResource
mkUntagResource pResourceName_ =
  UntagResource'
    { resourceName = pResourceName_,
      tagKeys = Lude.mempty,
      resourceARN = Lude.Nothing
    }

-- | The name of the resource from which you are removing a tag.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceName :: Lens.Lens' UntagResource Lude.Text
urResourceName = Lens.lens (resourceName :: UntagResource -> Lude.Text) (\s a -> s {resourceName = a} :: UntagResource)
{-# DEPRECATED urResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The tag keys to delete from the specified resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource from which you want to remove a tag.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource (Lude.Maybe Lude.Text)
urResourceARN = Lens.lens (resourceARN :: UntagResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: UntagResource)
{-# DEPRECATED urResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UntagResourceResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UntagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceName" Lude..= resourceName),
            Lude.Just ("tagKeys" Lude..= tagKeys),
            ("resourceArn" Lude..=) Lude.<$> resourceARN
          ]
      )

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUntagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResourceResponse
mkUntagResourceResponse pResponseStatus_ =
  UntagResourceResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsOperations :: Lens.Lens' UntagResourceResponse (Lude.Maybe [Operation])
urrsOperations = Lens.lens (operations :: UntagResourceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: UntagResourceResponse)
{-# DEPRECATED urrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResourceResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UntagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResourceResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
