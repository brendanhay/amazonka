{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipAddressUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipAddressUsage
  ( CoipAddressUsage (..),

    -- * Smart constructor
    mkCoipAddressUsage,

    -- * Lenses
    cauAllocationId,
    cauAwsAccountId,
    cauAwsService,
    cauCoIp,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes address usage for a customer-owned address pool.
--
-- /See:/ 'mkCoipAddressUsage' smart constructor.
data CoipAddressUsage = CoipAddressUsage'
  { -- | The allocation ID of the address.
    allocationId :: Core.Maybe Types.String,
    -- | The AWS account ID.
    awsAccountId :: Core.Maybe Types.String,
    -- | The AWS service.
    awsService :: Core.Maybe Types.String,
    -- | The customer-owned IP address.
    coIp :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoipAddressUsage' value with any optional fields omitted.
mkCoipAddressUsage ::
  CoipAddressUsage
mkCoipAddressUsage =
  CoipAddressUsage'
    { allocationId = Core.Nothing,
      awsAccountId = Core.Nothing,
      awsService = Core.Nothing,
      coIp = Core.Nothing
    }

-- | The allocation ID of the address.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAllocationId :: Lens.Lens' CoipAddressUsage (Core.Maybe Types.String)
cauAllocationId = Lens.field @"allocationId"
{-# DEPRECATED cauAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAwsAccountId :: Lens.Lens' CoipAddressUsage (Core.Maybe Types.String)
cauAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED cauAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The AWS service.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAwsService :: Lens.Lens' CoipAddressUsage (Core.Maybe Types.String)
cauAwsService = Lens.field @"awsService"
{-# DEPRECATED cauAwsService "Use generic-lens or generic-optics with 'awsService' instead." #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'coIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauCoIp :: Lens.Lens' CoipAddressUsage (Core.Maybe Types.String)
cauCoIp = Lens.field @"coIp"
{-# DEPRECATED cauCoIp "Use generic-lens or generic-optics with 'coIp' instead." #-}

instance Core.FromXML CoipAddressUsage where
  parseXML x =
    CoipAddressUsage'
      Core.<$> (x Core..@? "allocationId")
      Core.<*> (x Core..@? "awsAccountId")
      Core.<*> (x Core..@? "awsService")
      Core.<*> (x Core..@? "coIp")
