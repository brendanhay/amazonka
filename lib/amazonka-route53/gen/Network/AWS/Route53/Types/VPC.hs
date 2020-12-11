-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.VPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.VPC
  ( VPC (..),

    -- * Smart constructor
    mkVPC,

    -- * Lenses
    vpcVPCRegion,
    vpcVPCId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.VPCRegion

-- | (Private hosted zones only) A complex type that contains information about an Amazon VPC.
--
-- /See:/ 'mkVPC' smart constructor.
data VPC = VPC'
  { vpcRegion :: Lude.Maybe VPCRegion,
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

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- * 'vpcId' - Undocumented field.
-- * 'vpcRegion' - (Private hosted zones only) The region that an Amazon VPC was created in.
mkVPC ::
  VPC
mkVPC = VPC' {vpcRegion = Lude.Nothing, vpcId = Lude.Nothing}

-- | (Private hosted zones only) The region that an Amazon VPC was created in.
--
-- /Note:/ Consider using 'vpcRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCRegion :: Lens.Lens' VPC (Lude.Maybe VPCRegion)
vpcVPCRegion = Lens.lens (vpcRegion :: VPC -> Lude.Maybe VPCRegion) (\s a -> s {vpcRegion = a} :: VPC)
{-# DEPRECATED vpcVPCRegion "Use generic-lens or generic-optics with 'vpcRegion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCId :: Lens.Lens' VPC (Lude.Maybe Lude.Text)
vpcVPCId = Lens.lens (vpcId :: VPC -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPC)
{-# DEPRECATED vpcVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromXML VPC where
  parseXML x =
    VPC'
      Lude.<$> (x Lude..@? "VPCRegion") Lude.<*> (x Lude..@? "VPCId")

instance Lude.ToXML VPC where
  toXML VPC' {..} =
    Lude.mconcat
      ["VPCRegion" Lude.@= vpcRegion, "VPCId" Lude.@= vpcId]
