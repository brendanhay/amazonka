{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcClassicLink
  ( VpcClassicLink (..)
  -- * Smart constructor
  , mkVpcClassicLink
  -- * Lenses
  , vclClassicLinkEnabled
  , vclTags
  , vclVpcId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether a VPC is enabled for ClassicLink.
--
-- /See:/ 'mkVpcClassicLink' smart constructor.
data VpcClassicLink = VpcClassicLink'
  { classicLinkEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the VPC is enabled for ClassicLink.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the VPC.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcClassicLink' value with any optional fields omitted.
mkVpcClassicLink
    :: VpcClassicLink
mkVpcClassicLink
  = VpcClassicLink'{classicLinkEnabled = Core.Nothing,
                    tags = Core.Nothing, vpcId = Core.Nothing}

-- | Indicates whether the VPC is enabled for ClassicLink.
--
-- /Note:/ Consider using 'classicLinkEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclClassicLinkEnabled :: Lens.Lens' VpcClassicLink (Core.Maybe Core.Bool)
vclClassicLinkEnabled = Lens.field @"classicLinkEnabled"
{-# INLINEABLE vclClassicLinkEnabled #-}
{-# DEPRECATED classicLinkEnabled "Use generic-lens or generic-optics with 'classicLinkEnabled' instead"  #-}

-- | Any tags assigned to the VPC.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclTags :: Lens.Lens' VpcClassicLink (Core.Maybe [Types.Tag])
vclTags = Lens.field @"tags"
{-# INLINEABLE vclTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vclVpcId :: Lens.Lens' VpcClassicLink (Core.Maybe Core.Text)
vclVpcId = Lens.field @"vpcId"
{-# INLINEABLE vclVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML VpcClassicLink where
        parseXML x
          = VpcClassicLink' Core.<$>
              (x Core..@? "classicLinkEnabled") Core.<*>
                x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcId"
