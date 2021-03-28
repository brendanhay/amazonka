{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceUsage
  ( InstanceUsage (..)
  -- * Smart constructor
  , mkInstanceUsage
  -- * Lenses
  , iuAccountId
  , iuUsedInstanceCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Capacity Reservation usage.
--
-- /See:/ 'mkInstanceUsage' smart constructor.
data InstanceUsage = InstanceUsage'
  { accountId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that is making use of the Capacity Reservation.
  , usedInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances the AWS account currently has in the Capacity Reservation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceUsage' value with any optional fields omitted.
mkInstanceUsage
    :: InstanceUsage
mkInstanceUsage
  = InstanceUsage'{accountId = Core.Nothing,
                   usedInstanceCount = Core.Nothing}

-- | The ID of the AWS account that is making use of the Capacity Reservation.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuAccountId :: Lens.Lens' InstanceUsage (Core.Maybe Core.Text)
iuAccountId = Lens.field @"accountId"
{-# INLINEABLE iuAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The number of instances the AWS account currently has in the Capacity Reservation.
--
-- /Note:/ Consider using 'usedInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuUsedInstanceCount :: Lens.Lens' InstanceUsage (Core.Maybe Core.Int)
iuUsedInstanceCount = Lens.field @"usedInstanceCount"
{-# INLINEABLE iuUsedInstanceCount #-}
{-# DEPRECATED usedInstanceCount "Use generic-lens or generic-optics with 'usedInstanceCount' instead"  #-}

instance Core.FromXML InstanceUsage where
        parseXML x
          = InstanceUsage' Core.<$>
              (x Core..@? "accountId") Core.<*> x Core..@? "usedInstanceCount"
