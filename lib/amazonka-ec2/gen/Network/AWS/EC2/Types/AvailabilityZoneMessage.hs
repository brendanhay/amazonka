{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AvailabilityZoneMessage
  ( AvailabilityZoneMessage (..)
  -- * Smart constructor
  , mkAvailabilityZoneMessage
  -- * Lenses
  , azmMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a message about an Availability Zone, Local Zone, or Wavelength Zone.
--
-- /See:/ 'mkAvailabilityZoneMessage' smart constructor.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage'
  { message :: Core.Maybe Core.Text
    -- ^ The message about the Availability Zone, Local Zone, or Wavelength Zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZoneMessage' value with any optional fields omitted.
mkAvailabilityZoneMessage
    :: AvailabilityZoneMessage
mkAvailabilityZoneMessage
  = AvailabilityZoneMessage'{message = Core.Nothing}

-- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azmMessage :: Lens.Lens' AvailabilityZoneMessage (Core.Maybe Core.Text)
azmMessage = Lens.field @"message"
{-# INLINEABLE azmMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML AvailabilityZoneMessage where
        parseXML x
          = AvailabilityZoneMessage' Core.<$> (x Core..@? "message")
