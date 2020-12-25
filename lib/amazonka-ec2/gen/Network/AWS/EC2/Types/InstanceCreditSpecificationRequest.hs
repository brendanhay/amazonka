{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
  ( InstanceCreditSpecificationRequest (..),

    -- * Smart constructor
    mkInstanceCreditSpecificationRequest,

    -- * Lenses
    icsrCpuCredits,
    icsrInstanceId,
  )
where

import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
-- /See:/ 'mkInstanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { -- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
    cpuCredits :: Core.Maybe Types.String,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCreditSpecificationRequest' value with any optional fields omitted.
mkInstanceCreditSpecificationRequest ::
  InstanceCreditSpecificationRequest
mkInstanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    { cpuCredits = Core.Nothing,
      instanceId = Core.Nothing
    }

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrCpuCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Types.String)
icsrCpuCredits = Lens.field @"cpuCredits"
{-# DEPRECATED icsrCpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrInstanceId :: Lens.Lens' InstanceCreditSpecificationRequest (Core.Maybe Types.InstanceId)
icsrInstanceId = Lens.field @"instanceId"
{-# DEPRECATED icsrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}
