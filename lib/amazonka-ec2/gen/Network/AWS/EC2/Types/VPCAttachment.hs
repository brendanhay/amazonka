-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCAttachment
  ( VPCAttachment (..),

    -- * Smart constructor
    mkVPCAttachment,

    -- * Lenses
    vaState,
    vaVPCId,
  )
where

import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an attachment between a virtual private gateway and a VPC.
--
-- /See:/ 'mkVPCAttachment' smart constructor.
data VPCAttachment = VPCAttachment'
  { state ::
      Lude.Maybe AttachmentStatus,
    vpcId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCAttachment' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the attachment.
-- * 'vpcId' - The ID of the VPC.
mkVPCAttachment ::
  VPCAttachment
mkVPCAttachment =
  VPCAttachment' {state = Lude.Nothing, vpcId = Lude.Nothing}

-- | The current state of the attachment.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaState :: Lens.Lens' VPCAttachment (Lude.Maybe AttachmentStatus)
vaState = Lens.lens (state :: VPCAttachment -> Lude.Maybe AttachmentStatus) (\s a -> s {state = a} :: VPCAttachment)
{-# DEPRECATED vaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaVPCId :: Lens.Lens' VPCAttachment (Lude.Maybe Lude.Text)
vaVPCId = Lens.lens (vpcId :: VPCAttachment -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCAttachment)
{-# DEPRECATED vaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromXML VPCAttachment where
  parseXML x =
    VPCAttachment'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "vpcId")
