-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGatewayAttachment
  ( InternetGatewayAttachment (..),

    -- * Smart constructor
    mkInternetGatewayAttachment,

    -- * Lenses
    igaState,
    igaVPCId,
  )
where

import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the attachment of a VPC to an internet gateway or an egress-only internet gateway.
--
-- /See:/ 'mkInternetGatewayAttachment' smart constructor.
data InternetGatewayAttachment = InternetGatewayAttachment'
  { state ::
      AttachmentStatus,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InternetGatewayAttachment' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
-- * 'vpcId' - The ID of the VPC.
mkInternetGatewayAttachment ::
  -- | 'state'
  AttachmentStatus ->
  -- | 'vpcId'
  Lude.Text ->
  InternetGatewayAttachment
mkInternetGatewayAttachment pState_ pVPCId_ =
  InternetGatewayAttachment' {state = pState_, vpcId = pVPCId_}

-- | The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igaState :: Lens.Lens' InternetGatewayAttachment AttachmentStatus
igaState = Lens.lens (state :: InternetGatewayAttachment -> AttachmentStatus) (\s a -> s {state = a} :: InternetGatewayAttachment)
{-# DEPRECATED igaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igaVPCId :: Lens.Lens' InternetGatewayAttachment Lude.Text
igaVPCId = Lens.lens (vpcId :: InternetGatewayAttachment -> Lude.Text) (\s a -> s {vpcId = a} :: InternetGatewayAttachment)
{-# DEPRECATED igaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromXML InternetGatewayAttachment where
  parseXML x =
    InternetGatewayAttachment'
      Lude.<$> (x Lude..@ "state") Lude.<*> (x Lude..@ "vpcId")
