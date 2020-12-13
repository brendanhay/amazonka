{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateVPCLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'VpcLink' of a specified identifier.
module Network.AWS.APIGateway.UpdateVPCLink
  ( -- * Creating a request
    UpdateVPCLink (..),
    mkUpdateVPCLink,

    -- ** Request lenses
    uvlVpcLinkId,
    uvlPatchOperations,

    -- * Destructuring the response
    VPCLink (..),
    mkVPCLink,

    -- ** Response lenses
    vlStatus,
    vlTargetARNs,
    vlName,
    vlStatusMessage,
    vlId,
    vlDescription,
    vlTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates an existing 'VpcLink' of a specified identifier.
--
-- /See:/ 'mkUpdateVPCLink' smart constructor.
data UpdateVPCLink = UpdateVPCLink'
  { -- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
    vpcLinkId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVPCLink' with the minimum fields required to make a request.
--
-- * 'vpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateVPCLink ::
  -- | 'vpcLinkId'
  Lude.Text ->
  UpdateVPCLink
mkUpdateVPCLink pVpcLinkId_ =
  UpdateVPCLink'
    { vpcLinkId = pVpcLinkId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvlVpcLinkId :: Lens.Lens' UpdateVPCLink Lude.Text
uvlVpcLinkId = Lens.lens (vpcLinkId :: UpdateVPCLink -> Lude.Text) (\s a -> s {vpcLinkId = a} :: UpdateVPCLink)
{-# DEPRECATED uvlVpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvlPatchOperations :: Lens.Lens' UpdateVPCLink (Lude.Maybe [PatchOperation])
uvlPatchOperations = Lens.lens (patchOperations :: UpdateVPCLink -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateVPCLink)
{-# DEPRECATED uvlPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateVPCLink where
  type Rs UpdateVPCLink = VPCLink
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateVPCLink where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateVPCLink where
  toJSON UpdateVPCLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateVPCLink where
  toPath UpdateVPCLink' {..} =
    Lude.mconcat ["/vpclinks/", Lude.toBS vpcLinkId]

instance Lude.ToQuery UpdateVPCLink where
  toQuery = Lude.const Lude.mempty
