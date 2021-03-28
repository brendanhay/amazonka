{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DhcpConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DhcpConfiguration
  ( DhcpConfiguration (..)
  -- * Smart constructor
  , mkDhcpConfiguration
  -- * Lenses
  , dcKey
  , dcValues
  ) where

import qualified Network.AWS.EC2.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a DHCP configuration option.
--
-- /See:/ 'mkDhcpConfiguration' smart constructor.
data DhcpConfiguration = DhcpConfiguration'
  { key :: Core.Maybe Core.Text
    -- ^ The name of a DHCP option.
  , values :: Core.Maybe [Types.AttributeValue]
    -- ^ One or more values for the DHCP option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DhcpConfiguration' value with any optional fields omitted.
mkDhcpConfiguration
    :: DhcpConfiguration
mkDhcpConfiguration
  = DhcpConfiguration'{key = Core.Nothing, values = Core.Nothing}

-- | The name of a DHCP option.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcKey :: Lens.Lens' DhcpConfiguration (Core.Maybe Core.Text)
dcKey = Lens.field @"key"
{-# INLINEABLE dcKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | One or more values for the DHCP option.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcValues :: Lens.Lens' DhcpConfiguration (Core.Maybe [Types.AttributeValue])
dcValues = Lens.field @"values"
{-# INLINEABLE dcValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromXML DhcpConfiguration where
        parseXML x
          = DhcpConfiguration' Core.<$>
              (x Core..@? "key") Core.<*>
                x Core..@? "valueSet" Core..<@> Core.parseXMLList "item"
