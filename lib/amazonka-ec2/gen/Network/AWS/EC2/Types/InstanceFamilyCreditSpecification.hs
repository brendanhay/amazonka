{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
  ( InstanceFamilyCreditSpecification (..)
  -- * Smart constructor
  , mkInstanceFamilyCreditSpecification
  -- * Lenses
  , ifcsCpuCredits
  , ifcsInstanceFamily
  ) where

import qualified Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the default credit option for CPU usage of a burstable performance instance family.
--
-- /See:/ 'mkInstanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { cpuCredits :: Core.Maybe Core.Text
    -- ^ The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
  , instanceFamily :: Core.Maybe Types.UnlimitedSupportedInstanceFamily
    -- ^ The instance family.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFamilyCreditSpecification' value with any optional fields omitted.
mkInstanceFamilyCreditSpecification
    :: InstanceFamilyCreditSpecification
mkInstanceFamilyCreditSpecification
  = InstanceFamilyCreditSpecification'{cpuCredits = Core.Nothing,
                                       instanceFamily = Core.Nothing}

-- | The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsCpuCredits :: Lens.Lens' InstanceFamilyCreditSpecification (Core.Maybe Core.Text)
ifcsCpuCredits = Lens.field @"cpuCredits"
{-# INLINEABLE ifcsCpuCredits #-}
{-# DEPRECATED cpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead"  #-}

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsInstanceFamily :: Lens.Lens' InstanceFamilyCreditSpecification (Core.Maybe Types.UnlimitedSupportedInstanceFamily)
ifcsInstanceFamily = Lens.field @"instanceFamily"
{-# INLINEABLE ifcsInstanceFamily #-}
{-# DEPRECATED instanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead"  #-}

instance Core.FromXML InstanceFamilyCreditSpecification where
        parseXML x
          = InstanceFamilyCreditSpecification' Core.<$>
              (x Core..@? "cpuCredits") Core.<*> x Core..@? "instanceFamily"
