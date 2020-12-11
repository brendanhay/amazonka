{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteVPCLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'VpcLink' of a specified identifier.
module Network.AWS.APIGateway.DeleteVPCLink
  ( -- * Creating a request
    DeleteVPCLink (..),
    mkDeleteVPCLink,

    -- ** Request lenses
    dvlVpcLinkId,

    -- * Destructuring the response
    DeleteVPCLinkResponse (..),
    mkDeleteVPCLinkResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes an existing 'VpcLink' of a specified identifier.
--
-- /See:/ 'mkDeleteVPCLink' smart constructor.
newtype DeleteVPCLink = DeleteVPCLink' {vpcLinkId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCLink' with the minimum fields required to make a request.
--
-- * 'vpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
mkDeleteVPCLink ::
  -- | 'vpcLinkId'
  Lude.Text ->
  DeleteVPCLink
mkDeleteVPCLink pVpcLinkId_ =
  DeleteVPCLink' {vpcLinkId = pVpcLinkId_}

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlVpcLinkId :: Lens.Lens' DeleteVPCLink Lude.Text
dvlVpcLinkId = Lens.lens (vpcLinkId :: DeleteVPCLink -> Lude.Text) (\s a -> s {vpcLinkId = a} :: DeleteVPCLink)
{-# DEPRECATED dvlVpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead." #-}

instance Lude.AWSRequest DeleteVPCLink where
  type Rs DeleteVPCLink = DeleteVPCLinkResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteVPCLinkResponse'

instance Lude.ToHeaders DeleteVPCLink where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteVPCLink where
  toPath DeleteVPCLink' {..} =
    Lude.mconcat ["/vpclinks/", Lude.toBS vpcLinkId]

instance Lude.ToQuery DeleteVPCLink where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVPCLinkResponse' smart constructor.
data DeleteVPCLinkResponse = DeleteVPCLinkResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCLinkResponse' with the minimum fields required to make a request.
mkDeleteVPCLinkResponse ::
  DeleteVPCLinkResponse
mkDeleteVPCLinkResponse = DeleteVPCLinkResponse'
