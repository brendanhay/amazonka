{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply cost-allocation tags to a specified stack or layer in AWS OpsWorks Stacks. For more information about how tagging works, see <https://docs.aws.amazon.com/opsworks/latest/userguide/tagging.html Tags> in the AWS OpsWorks User Guide.
module Network.AWS.OpsWorks.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResourceARN,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The stack or layer's Amazon Resource Number (ARN).
    resourceARN :: Lude.Text,
    -- | A map that contains tag keys and tag values that are attached to a stack or layer.
    --
    --
    --     * The key cannot be empty.
    --
    --
    --     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    --
    --
    --     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    --
    --
    --     * Leading and trailing white spaces are trimmed from both the key and value.
    --
    --
    --     * A maximum of 40 tags is allowed for any resource.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The stack or layer's Amazon Resource Number (ARN).
-- * 'tags' - A map that contains tag keys and tag values that are attached to a stack or layer.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 40 tags is allowed for any resource.
mkTagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  TagResource
mkTagResource pResourceARN_ =
  TagResource' {resourceARN = pResourceARN_, tags = Lude.mempty}

-- | The stack or layer's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARN :: Lens.Lens' TagResource Lude.Text
trResourceARN = Lens.lens (resourceARN :: TagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: TagResource)
{-# DEPRECATED trResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | A map that contains tag keys and tag values that are attached to a stack or layer.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 40 tags is allowed for any resource.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource (Lude.HashMap Lude.Text (Lude.Text))
trTags = Lens.lens (tags :: TagResource -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull TagResourceResponse'

instance Lude.ToHeaders TagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.TagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery TagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
mkTagResourceResponse ::
  TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
