{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DhcpOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DhcpOptions
  ( DhcpOptions (..)
  -- * Smart constructor
  , mkDhcpOptions
  -- * Lenses
  , doDhcpConfigurations
  , doDhcpOptionsId
  , doOwnerId
  , doTags
  ) where

import qualified Network.AWS.EC2.Types.DhcpConfiguration as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a set of DHCP options.
--
-- /See:/ 'mkDhcpOptions' smart constructor.
data DhcpOptions = DhcpOptions'
  { dhcpConfigurations :: Core.Maybe [Types.DhcpConfiguration]
    -- ^ One or more DHCP options in the set.
  , dhcpOptionsId :: Core.Maybe Core.Text
    -- ^ The ID of the set of DHCP options.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the DHCP options set.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the DHCP options set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DhcpOptions' value with any optional fields omitted.
mkDhcpOptions
    :: DhcpOptions
mkDhcpOptions
  = DhcpOptions'{dhcpConfigurations = Core.Nothing,
                 dhcpOptionsId = Core.Nothing, ownerId = Core.Nothing,
                 tags = Core.Nothing}

-- | One or more DHCP options in the set.
--
-- /Note:/ Consider using 'dhcpConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDhcpConfigurations :: Lens.Lens' DhcpOptions (Core.Maybe [Types.DhcpConfiguration])
doDhcpConfigurations = Lens.field @"dhcpConfigurations"
{-# INLINEABLE doDhcpConfigurations #-}
{-# DEPRECATED dhcpConfigurations "Use generic-lens or generic-optics with 'dhcpConfigurations' instead"  #-}

-- | The ID of the set of DHCP options.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDhcpOptionsId :: Lens.Lens' DhcpOptions (Core.Maybe Core.Text)
doDhcpOptionsId = Lens.field @"dhcpOptionsId"
{-# INLINEABLE doDhcpOptionsId #-}
{-# DEPRECATED dhcpOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead"  #-}

-- | The ID of the AWS account that owns the DHCP options set.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doOwnerId :: Lens.Lens' DhcpOptions (Core.Maybe Core.Text)
doOwnerId = Lens.field @"ownerId"
{-# INLINEABLE doOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Any tags assigned to the DHCP options set.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doTags :: Lens.Lens' DhcpOptions (Core.Maybe [Types.Tag])
doTags = Lens.field @"tags"
{-# INLINEABLE doTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML DhcpOptions where
        parseXML x
          = DhcpOptions' Core.<$>
              (x Core..@? "dhcpConfigurationSet" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "dhcpOptionsId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
