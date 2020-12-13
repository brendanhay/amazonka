{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove tags from a CloudFront resource.
module Network.AWS.CloudFront.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urTagKeys,
    urResource,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to remove tags from a CloudFront resource.
--
-- /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | A complex type that contains zero or more @Tag@ key elements.
    tagKeys :: TagKeys,
    -- | An ARN of a CloudFront resource.
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - A complex type that contains zero or more @Tag@ key elements.
-- * 'resource' - An ARN of a CloudFront resource.
mkUntagResource ::
  -- | 'tagKeys'
  TagKeys ->
  -- | 'resource'
  Lude.Text ->
  UntagResource
mkUntagResource pTagKeys_ pResource_ =
  UntagResource' {tagKeys = pTagKeys_, resource = pResource_}

-- | A complex type that contains zero or more @Tag@ key elements.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource TagKeys
urTagKeys = Lens.lens (tagKeys :: UntagResource -> TagKeys) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | An ARN of a CloudFront resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResource :: Lens.Lens' UntagResource Lude.Text
urResource = Lens.lens (resource :: UntagResource -> Lude.Text) (\s a -> s {resource = a} :: UntagResource)
{-# DEPRECATED urResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postXML cloudFrontService
  response = Res.receiveNull UntagResourceResponse'

instance Lude.ToElement UntagResource where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}TagKeys"
      Lude.. tagKeys

instance Lude.ToHeaders UntagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/2020-05-31/tagging"

instance Lude.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Lude.mconcat ["Resource" Lude.=: resource, "Operation=Untag"]

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
