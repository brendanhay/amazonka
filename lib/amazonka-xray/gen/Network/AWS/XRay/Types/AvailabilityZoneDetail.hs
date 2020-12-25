{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AvailabilityZoneDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AvailabilityZoneDetail
  ( AvailabilityZoneDetail (..),

    -- * Smart constructor
    mkAvailabilityZoneDetail,

    -- * Lenses
    azdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.String as Types

-- | A list of Availability Zones corresponding to the segments in a trace.
--
-- /See:/ 'mkAvailabilityZoneDetail' smart constructor.
newtype AvailabilityZoneDetail = AvailabilityZoneDetail'
  { -- | The name of a corresponding Availability Zone.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZoneDetail' value with any optional fields omitted.
mkAvailabilityZoneDetail ::
  AvailabilityZoneDetail
mkAvailabilityZoneDetail =
  AvailabilityZoneDetail' {name = Core.Nothing}

-- | The name of a corresponding Availability Zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azdName :: Lens.Lens' AvailabilityZoneDetail (Core.Maybe Types.String)
azdName = Lens.field @"name"
{-# DEPRECATED azdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON AvailabilityZoneDetail where
  parseJSON =
    Core.withObject "AvailabilityZoneDetail" Core.$
      \x -> AvailabilityZoneDetail' Core.<$> (x Core..:? "Name")
