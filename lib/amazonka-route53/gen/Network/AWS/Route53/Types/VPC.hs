{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    vpcVPCId,
    vpcVPCRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.VPCId as Types
import qualified Network.AWS.Route53.Types.VPCRegion as Types

-- | (Private hosted zones only) A complex type that contains information about an Amazon VPC.
--
-- /See:/ 'mkVPC' smart constructor.
data VPC = VPC'
  { vPCId :: Core.Maybe Types.VPCId,
    -- | (Private hosted zones only) The region that an Amazon VPC was created in.
    vPCRegion :: Core.Maybe Types.VPCRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VPC' value with any optional fields omitted.
mkVPC ::
  VPC
mkVPC = VPC' {vPCId = Core.Nothing, vPCRegion = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCId :: Lens.Lens' VPC (Core.Maybe Types.VPCId)
vpcVPCId = Lens.field @"vPCId"
{-# DEPRECATED vpcVPCId "Use generic-lens or generic-optics with 'vPCId' instead." #-}

-- | (Private hosted zones only) The region that an Amazon VPC was created in.
--
-- /Note:/ Consider using 'vPCRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCRegion :: Lens.Lens' VPC (Core.Maybe Types.VPCRegion)
vpcVPCRegion = Lens.field @"vPCRegion"
{-# DEPRECATED vpcVPCRegion "Use generic-lens or generic-optics with 'vPCRegion' instead." #-}

instance Core.ToXML VPC where
  toXML VPC {..} =
    Core.toXMLNode "VPCId" Core.<$> vPCId
      Core.<> Core.toXMLNode "VPCRegion" Core.<$> vPCRegion

instance Core.FromXML VPC where
  parseXML x =
    VPC'
      Core.<$> (x Core..@? "VPCId") Core.<*> (x Core..@? "VPCRegion")
