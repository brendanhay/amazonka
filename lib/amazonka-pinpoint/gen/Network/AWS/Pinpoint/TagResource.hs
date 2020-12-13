{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags (keys and values) to an application, campaign, message template, or segment.
module Network.AWS.Pinpoint.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trTagsModel,
    trResourceARN,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { tagsModel :: TagsModel,
    -- | The Amazon Resource Name (ARN) of the resource.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'tagsModel' -
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource.
mkTagResource ::
  -- | 'tagsModel'
  TagsModel ->
  -- | 'resourceARN'
  Lude.Text ->
  TagResource
mkTagResource pTagsModel_ pResourceARN_ =
  TagResource'
    { tagsModel = pTagsModel_,
      resourceARN = pResourceARN_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagsModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTagsModel :: Lens.Lens' TagResource TagsModel
trTagsModel = Lens.lens (tagsModel :: TagResource -> TagsModel) (\s a -> s {tagsModel = a} :: TagResource)
{-# DEPRECATED trTagsModel "Use generic-lens or generic-optics with 'tagsModel' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARN :: Lens.Lens' TagResource Lude.Text
trResourceARN = Lens.lens (resourceARN :: TagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: TagResource)
{-# DEPRECATED trResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON pinpointService
  response = Res.receiveNull TagResourceResponse'

instance Lude.ToHeaders TagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TagsModel" Lude..= tagsModel)])

instance Lude.ToPath TagResource where
  toPath TagResource' {..} =
    Lude.mconcat ["/v1/tags/", Lude.toBS resourceARN]

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
