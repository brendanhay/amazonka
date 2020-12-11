{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Resource' resource.
module Network.AWS.APIGateway.UpdateResource
  ( -- * Creating a request
    UpdateResource (..),
    mkUpdateResource,

    -- ** Request lenses
    urPatchOperations,
    urRestAPIId,
    urResourceId,

    -- * Destructuring the response
    Resource (..),
    mkResource,

    -- ** Response lenses
    rPathPart,
    rPath,
    rId,
    rResourceMethods,
    rParentId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to change information about a 'Resource' resource.
--
-- /See:/ 'mkUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResource' with the minimum fields required to make a request.
--
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'resourceId' - [Required] The identifier of the 'Resource' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateResource ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  UpdateResource
mkUpdateResource pRestAPIId_ pResourceId_ =
  UpdateResource'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urPatchOperations :: Lens.Lens' UpdateResource (Lude.Maybe [PatchOperation])
urPatchOperations = Lens.lens (patchOperations :: UpdateResource -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateResource)
{-# DEPRECATED urPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRestAPIId :: Lens.Lens' UpdateResource Lude.Text
urRestAPIId = Lens.lens (restAPIId :: UpdateResource -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateResource)
{-# DEPRECATED urRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UpdateResource Lude.Text
urResourceId = Lens.lens (resourceId :: UpdateResource -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateResource)
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest UpdateResource where
  type Rs UpdateResource = Resource
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateResource where
  toPath UpdateResource' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId
      ]

instance Lude.ToQuery UpdateResource where
  toQuery = Lude.const Lude.mempty
