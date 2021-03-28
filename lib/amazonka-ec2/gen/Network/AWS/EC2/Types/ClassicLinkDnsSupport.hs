{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLinkDnsSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClassicLinkDnsSupport
  ( ClassicLinkDnsSupport (..)
  -- * Smart constructor
  , mkClassicLinkDnsSupport
  -- * Lenses
  , cldsClassicLinkDnsSupported
  , cldsVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the ClassicLink DNS support status of a VPC.
--
-- /See:/ 'mkClassicLinkDnsSupport' smart constructor.
data ClassicLinkDnsSupport = ClassicLinkDnsSupport'
  { classicLinkDnsSupported :: Core.Maybe Core.Bool
    -- ^ Indicates whether ClassicLink DNS support is enabled for the VPC.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassicLinkDnsSupport' value with any optional fields omitted.
mkClassicLinkDnsSupport
    :: ClassicLinkDnsSupport
mkClassicLinkDnsSupport
  = ClassicLinkDnsSupport'{classicLinkDnsSupported = Core.Nothing,
                           vpcId = Core.Nothing}

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
--
-- /Note:/ Consider using 'classicLinkDnsSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldsClassicLinkDnsSupported :: Lens.Lens' ClassicLinkDnsSupport (Core.Maybe Core.Bool)
cldsClassicLinkDnsSupported = Lens.field @"classicLinkDnsSupported"
{-# INLINEABLE cldsClassicLinkDnsSupported #-}
{-# DEPRECATED classicLinkDnsSupported "Use generic-lens or generic-optics with 'classicLinkDnsSupported' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldsVpcId :: Lens.Lens' ClassicLinkDnsSupport (Core.Maybe Core.Text)
cldsVpcId = Lens.field @"vpcId"
{-# INLINEABLE cldsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML ClassicLinkDnsSupport where
        parseXML x
          = ClassicLinkDnsSupport' Core.<$>
              (x Core..@? "classicLinkDnsSupported") Core.<*> x Core..@? "vpcId"
