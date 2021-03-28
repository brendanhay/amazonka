{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
  ( AvailabilityOptionsStatus (..)
  -- * Smart constructor
  , mkAvailabilityOptionsStatus
  -- * Lenses
  , aosOptions
  , aosStatus
  ) where

import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status and configuration of the domain's availability options.
--
-- /See:/ 'mkAvailabilityOptionsStatus' smart constructor.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'
  { options :: Core.Bool
    -- ^ The availability options configured for the domain.
  , status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AvailabilityOptionsStatus' value with any optional fields omitted.
mkAvailabilityOptionsStatus
    :: Core.Bool -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> AvailabilityOptionsStatus
mkAvailabilityOptionsStatus options status
  = AvailabilityOptionsStatus'{options, status}

-- | The availability options configured for the domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosOptions :: Lens.Lens' AvailabilityOptionsStatus Core.Bool
aosOptions = Lens.field @"options"
{-# INLINEABLE aosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosStatus :: Lens.Lens' AvailabilityOptionsStatus Types.OptionStatus
aosStatus = Lens.field @"status"
{-# INLINEABLE aosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML AvailabilityOptionsStatus where
        parseXML x
          = AvailabilityOptionsStatus' Core.<$>
              (x Core..@ "Options") Core.<*> x Core..@ "Status"
