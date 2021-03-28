{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.AvailabilityZone
  ( AvailabilityZone (..)
  -- * Smart constructor
  , mkAvailabilityZone
  -- * Lenses
  , azName
  , azSupportedPlatforms
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SupportedPlatform as Types

-- | Describes an availability zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the availability zone.
  , supportedPlatforms :: Core.Maybe [Types.SupportedPlatform]
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone
    :: AvailabilityZone
mkAvailabilityZone
  = AvailabilityZone'{name = Core.Nothing,
                      supportedPlatforms = Core.Nothing}

-- | The name of the availability zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
azName = Lens.field @"name"
{-# INLINEABLE azName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'supportedPlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azSupportedPlatforms :: Lens.Lens' AvailabilityZone (Core.Maybe [Types.SupportedPlatform])
azSupportedPlatforms = Lens.field @"supportedPlatforms"
{-# INLINEABLE azSupportedPlatforms #-}
{-# DEPRECATED supportedPlatforms "Use generic-lens or generic-optics with 'supportedPlatforms' instead"  #-}

instance Core.FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' Core.<$>
              (x Core..@? "Name") Core.<*>
                x Core..@? "SupportedPlatforms" Core..<@>
                  Core.parseXMLList "SupportedPlatform"
