{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies tags to an existing AWS X-Ray group or sampling rule.
module Network.AWS.XRay.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResourceARN,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,

    -- ** Response lenses
    trrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
    resourceARN :: Lude.Text,
    -- | A map that contains one or more tag keys and tag values to attach to an X-Ray group or sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
    --
    -- The following restrictions apply to tags:
    --
    --     * Maximum number of user-applied tags per resource: 50
    --
    --
    --     * Maximum tag key length: 128 Unicode characters
    --
    --
    --     * Maximum tag value length: 256 Unicode characters
    --
    --
    --     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
    --
    --
    --     * Tag keys and values are case sensitive.
    --
    --
    --     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use. You cannot edit or delete system tags.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
-- * 'tags' - A map that contains one or more tag keys and tag values to attach to an X-Ray group or sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use. You cannot edit or delete system tags.
mkTagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  TagResource
mkTagResource pResourceARN_ =
  TagResource' {resourceARN = pResourceARN_, tags = Lude.mempty}

-- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARN :: Lens.Lens' TagResource Lude.Text
trResourceARN = Lens.lens (resourceARN :: TagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: TagResource)
{-# DEPRECATED trResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray group or sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use. You cannot edit or delete system tags.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Tag]
trTags = Lens.lens (tags :: TagResource -> [Tag]) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagResource where
  toPath = Lude.const "/TagResource"

instance Lude.ToQuery TagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagResourceResponse
mkTagResourceResponse pResponseStatus_ =
  TagResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsResponseStatus :: Lens.Lens' TagResourceResponse Lude.Int
trrsResponseStatus = Lens.lens (responseStatus :: TagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagResourceResponse)
{-# DEPRECATED trrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
