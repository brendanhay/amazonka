{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceStatusSummary
  ( InstanceStatusSummary (..)
  -- * Smart constructor
  , mkInstanceStatusSummary
  -- * Lenses
  , issDetails
  , issStatus
  ) where

import qualified Network.AWS.EC2.Types.InstanceStatusDetails as Types
import qualified Network.AWS.EC2.Types.SummaryStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of an instance.
--
-- /See:/ 'mkInstanceStatusSummary' smart constructor.
data InstanceStatusSummary = InstanceStatusSummary'
  { details :: Core.Maybe [Types.InstanceStatusDetails]
    -- ^ The system instance health or application instance health.
  , status :: Types.SummaryStatus
    -- ^ The status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceStatusSummary' value with any optional fields omitted.
mkInstanceStatusSummary
    :: Types.SummaryStatus -- ^ 'status'
    -> InstanceStatusSummary
mkInstanceStatusSummary status
  = InstanceStatusSummary'{details = Core.Nothing, status}

-- | The system instance health or application instance health.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issDetails :: Lens.Lens' InstanceStatusSummary (Core.Maybe [Types.InstanceStatusDetails])
issDetails = Lens.field @"details"
{-# INLINEABLE issDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issStatus :: Lens.Lens' InstanceStatusSummary Types.SummaryStatus
issStatus = Lens.field @"status"
{-# INLINEABLE issStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML InstanceStatusSummary where
        parseXML x
          = InstanceStatusSummary' Core.<$>
              (x Core..@? "details" Core..<@> Core.parseXMLList "item") Core.<*>
                x Core..@ "status"
