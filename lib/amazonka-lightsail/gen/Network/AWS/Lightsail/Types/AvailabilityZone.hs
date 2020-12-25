{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azState,
    azZoneName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Availability Zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The state of the Availability Zone.
    state :: Core.Maybe Types.NonEmptyString,
    -- | The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
    zoneName :: Core.Maybe Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone' {state = Core.Nothing, zoneName = Core.Nothing}

-- | The state of the Availability Zone.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azState :: Lens.Lens' AvailabilityZone (Core.Maybe Types.NonEmptyString)
azState = Lens.field @"state"
{-# DEPRECATED azState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.NonEmptyString)
azZoneName = Lens.field @"zoneName"
{-# DEPRECATED azZoneName "Use generic-lens or generic-optics with 'zoneName' instead." #-}

instance Core.FromJSON AvailabilityZone where
  parseJSON =
    Core.withObject "AvailabilityZone" Core.$
      \x ->
        AvailabilityZone'
          Core.<$> (x Core..:? "state") Core.<*> (x Core..:? "zoneName")
