{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetVPCLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified VPC link under the caller's account in a region.
module Network.AWS.APIGateway.GetVPCLink
  ( -- * Creating a request
    GetVPCLink (..),
    mkGetVPCLink,

    -- ** Request lenses
    gvlVpcLinkId,

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

-- | Gets a specified VPC link under the caller's account in a region.
--
-- /See:/ 'mkGetVPCLink' smart constructor.
newtype GetVPCLink = GetVPCLink' {vpcLinkId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVPCLink' with the minimum fields required to make a request.
--
-- * 'vpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
mkGetVPCLink ::
  -- | 'vpcLinkId'
  Lude.Text ->
  GetVPCLink
mkGetVPCLink pVpcLinkId_ = GetVPCLink' {vpcLinkId = pVpcLinkId_}

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlVpcLinkId :: Lens.Lens' GetVPCLink Lude.Text
gvlVpcLinkId = Lens.lens (vpcLinkId :: GetVPCLink -> Lude.Text) (\s a -> s {vpcLinkId = a} :: GetVPCLink)
{-# DEPRECATED gvlVpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead." #-}

instance Lude.AWSRequest GetVPCLink where
  type Rs GetVPCLink = VPCLink
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetVPCLink where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetVPCLink where
  toPath GetVPCLink' {..} =
    Lude.mconcat ["/vpclinks/", Lude.toBS vpcLinkId]

instance Lude.ToQuery GetVPCLink where
  toQuery = Lude.const Lude.mempty
