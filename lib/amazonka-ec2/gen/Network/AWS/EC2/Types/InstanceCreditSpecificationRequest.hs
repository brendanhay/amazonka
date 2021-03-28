{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
  ( InstanceCreditSpecificationRequest (..)
  -- * Smart constructor
  , mkInstanceCreditSpecificationRequest
  -- * Lenses
  , icsrCpuCredits
  , icsrInstanceId
  ) where

import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
-- /See:/ 'mkInstanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { cpuCredits :: Core.Maybe Core.Text
    -- ^ The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCreditSpecificationRequest' value with any optional fields omitted.
mkInstanceCreditSpecificationRequest
    :: InstanceCreditSpecificationRequest
mkInstanceCreditSpecificationRequest
  = InstanceCreditSpecificationRequest'{cpuCredits = Core.Nothing,
                                        instanceId = Core.Nothing}

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrCpuCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Core.Text)
icsrCpuCredits = Lens.field @"cpuCredits"
{-# INLINEABLE icsrCpuCredits #-}
{-# DEPRECATED cpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrInstanceId :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Types.InstanceId)
icsrInstanceId = Lens.field @"instanceId"
{-# INLINEABLE icsrInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery InstanceCreditSpecificationRequest where
        toQuery InstanceCreditSpecificationRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CpuCredits") cpuCredits
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceId") instanceId
