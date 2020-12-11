{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an EFS resource. You can remove tags from EFS file systems and access points using this API operation.
--
-- This operation requires permissions for the @elasticfilesystem:UntagResource@ action.
module Network.AWS.EFS.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urResourceId,
    urTagKeys,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { resourceId :: Lude.Text,
    tagKeys :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - Specifies the EFS resource that you want to remove tags from.
-- * 'tagKeys' - The keys of the key:value tag pairs that you want to remove from the specified EFS resource.
mkUntagResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'tagKeys'
  Lude.NonEmpty Lude.Text ->
  UntagResource
mkUntagResource pResourceId_ pTagKeys_ =
  UntagResource' {resourceId = pResourceId_, tagKeys = pTagKeys_}

-- | Specifies the EFS resource that you want to remove tags from.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UntagResource Lude.Text
urResourceId = Lens.lens (resourceId :: UntagResource -> Lude.Text) (\s a -> s {resourceId = a} :: UntagResource)
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The keys of the key:value tag pairs that you want to remove from the specified EFS resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource (Lude.NonEmpty Lude.Text)
urTagKeys = Lens.lens (tagKeys :: UntagResource -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.delete efsService
  response = Res.receiveNull UntagResourceResponse'

instance Lude.ToHeaders UntagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagResource where
  toPath UntagResource' {..} =
    Lude.mconcat ["/2015-02-01/resource-tags/", Lude.toBS resourceId]

instance Lude.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Lude.mconcat
      ["tagKeys" Lude.=: Lude.toQueryList "member" tagKeys]

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
