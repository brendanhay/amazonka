{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceCreditSpecification
  ( InstanceCreditSpecification (..)
  -- * Smart constructor
  , mkInstanceCreditSpecification
  -- * Lenses
  , icsCpuCredits
  , icsInstanceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the credit option for CPU usage of a burstable performance instance. 
--
-- /See:/ 'mkInstanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { cpuCredits :: Core.Maybe Core.Text
    -- ^ The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCreditSpecification' value with any optional fields omitted.
mkInstanceCreditSpecification
    :: InstanceCreditSpecification
mkInstanceCreditSpecification
  = InstanceCreditSpecification'{cpuCredits = Core.Nothing,
                                 instanceId = Core.Nothing}

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsCpuCredits :: Lens.Lens' InstanceCreditSpecification (Core.Maybe Core.Text)
icsCpuCredits = Lens.field @"cpuCredits"
{-# INLINEABLE icsCpuCredits #-}
{-# DEPRECATED cpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsInstanceId :: Lens.Lens' InstanceCreditSpecification (Core.Maybe Core.Text)
icsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE icsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.FromXML InstanceCreditSpecification where
        parseXML x
          = InstanceCreditSpecification' Core.<$>
              (x Core..@? "cpuCredits") Core.<*> x Core..@? "instanceId"
