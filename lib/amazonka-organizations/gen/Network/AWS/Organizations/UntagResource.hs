{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes any tags with the specified keys from the specified resource.
--
-- You can attach tags to the following resources in AWS Organizations.
--
--     * AWS account
--
--
--     * Organization root
--
--
--     * Organizational unit (OU)
--
--
--     * Policy (any type)
--
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.UntagResource
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The ID of the resource to remove a tag from.
    --
    -- You can specify any of the following taggable resources.
    --
    --     * AWS account – specify the account ID number.
    --
    --
    --     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
    --
    --
    --     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
    --
    --
    --     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
    resourceId :: Lude.Text,
    -- | The list of keys for tags to remove from the specified resource.
    tagKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
--
--
-- * 'tagKeys' - The list of keys for tags to remove from the specified resource.
mkUntagResource ::
  -- | 'resourceId'
  Lude.Text ->
  UntagResource
mkUntagResource pResourceId_ =
  UntagResource' {resourceId = pResourceId_, tagKeys = Lude.mempty}

-- | The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UntagResource Lude.Text
urResourceId = Lens.lens (resourceId :: UntagResource -> Lude.Text) (\s a -> s {resourceId = a} :: UntagResource)
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The list of keys for tags to remove from the specified resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull UntagResourceResponse'

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.UntagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
