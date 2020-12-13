{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add tags to a CloudFront resource.
module Network.AWS.CloudFront.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResource,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to add tags to a CloudFront resource.
--
-- /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Lude.Text,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'resource' - An ARN of a CloudFront resource.
-- * 'tags' - A complex type that contains zero or more @Tag@ elements.
mkTagResource ::
  -- | 'resource'
  Lude.Text ->
  -- | 'tags'
  Tags ->
  TagResource
mkTagResource pResource_ pTags_ =
  TagResource' {resource = pResource_, tags = pTags_}

-- | An ARN of a CloudFront resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResource :: Lens.Lens' TagResource Lude.Text
trResource = Lens.lens (resource :: TagResource -> Lude.Text) (\s a -> s {resource = a} :: TagResource)
{-# DEPRECATED trResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource Tags
trTags = Lens.lens (tags :: TagResource -> Tags) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postXML cloudFrontService
  response = Res.receiveNull TagResourceResponse'

instance Lude.ToElement TagResource where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}Tags"
      Lude.. tags

instance Lude.ToHeaders TagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TagResource where
  toPath = Lude.const "/2020-05-31/tagging"

instance Lude.ToQuery TagResource where
  toQuery TagResource' {..} =
    Lude.mconcat ["Resource" Lude.=: resource, "Operation=Tag"]

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
mkTagResourceResponse ::
  TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
