{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.SetTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets tags (key and value pairs) to the assessment template that is specified by the ARN of the assessment template.
module Network.AWS.Inspector.SetTagsForResource
  ( -- * Creating a request
    SetTagsForResource (..),
    mkSetTagsForResource,

    -- ** Request lenses
    stfrResourceARN,
    stfrTags,

    -- * Destructuring the response
    SetTagsForResourceResponse (..),
    mkSetTagsForResourceResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
  { -- | The ARN of the assessment template that you want to set tags to.
    resourceARN :: Lude.Text,
    -- | A collection of key and value pairs that you want to set to the assessment template.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the assessment template that you want to set tags to.
-- * 'tags' - A collection of key and value pairs that you want to set to the assessment template.
mkSetTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  SetTagsForResource
mkSetTagsForResource pResourceARN_ =
  SetTagsForResource'
    { resourceARN = pResourceARN_,
      tags = Lude.Nothing
    }

-- | The ARN of the assessment template that you want to set tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrResourceARN :: Lens.Lens' SetTagsForResource Lude.Text
stfrResourceARN = Lens.lens (resourceARN :: SetTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: SetTagsForResource)
{-# DEPRECATED stfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | A collection of key and value pairs that you want to set to the assessment template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrTags :: Lens.Lens' SetTagsForResource (Lude.Maybe [Tag])
stfrTags = Lens.lens (tags :: SetTagsForResource -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SetTagsForResource)
{-# DEPRECATED stfrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest SetTagsForResource where
  type Rs SetTagsForResource = SetTagsForResourceResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull SetTagsForResourceResponse'

instance Lude.ToHeaders SetTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.SetTagsForResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetTagsForResource where
  toJSON SetTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceArn" Lude..= resourceARN),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath SetTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTagsForResourceResponse' with the minimum fields required to make a request.
mkSetTagsForResourceResponse ::
  SetTagsForResourceResponse
mkSetTagsForResourceResponse = SetTagsForResourceResponse'
