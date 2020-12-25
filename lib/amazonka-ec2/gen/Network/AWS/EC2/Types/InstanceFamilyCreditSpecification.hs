{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
  ( InstanceFamilyCreditSpecification (..),

    -- * Smart constructor
    mkInstanceFamilyCreditSpecification,

    -- * Lenses
    ifcsCpuCredits,
    ifcsInstanceFamily,
  )
where

import qualified Network.AWS.EC2.Types.CpuCredits as Types
import qualified Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the default credit option for CPU usage of a burstable performance instance family.
--
-- /See:/ 'mkInstanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { -- | The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
    cpuCredits :: Core.Maybe Types.CpuCredits,
    -- | The instance family.
    instanceFamily :: Core.Maybe Types.UnlimitedSupportedInstanceFamily
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFamilyCreditSpecification' value with any optional fields omitted.
mkInstanceFamilyCreditSpecification ::
  InstanceFamilyCreditSpecification
mkInstanceFamilyCreditSpecification =
  InstanceFamilyCreditSpecification'
    { cpuCredits = Core.Nothing,
      instanceFamily = Core.Nothing
    }

-- | The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsCpuCredits :: Lens.Lens' InstanceFamilyCreditSpecification (Core.Maybe Types.CpuCredits)
ifcsCpuCredits = Lens.field @"cpuCredits"
{-# DEPRECATED ifcsCpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsInstanceFamily :: Lens.Lens' InstanceFamilyCreditSpecification (Core.Maybe Types.UnlimitedSupportedInstanceFamily)
ifcsInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED ifcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

instance Core.FromXML InstanceFamilyCreditSpecification where
  parseXML x =
    InstanceFamilyCreditSpecification'
      Core.<$> (x Core..@? "cpuCredits") Core.<*> (x Core..@? "instanceFamily")
