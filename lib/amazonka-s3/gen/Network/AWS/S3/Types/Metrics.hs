{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Metrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Metrics
  ( Metrics (..)
  -- * Smart constructor
  , mkMetrics
  -- * Lenses
  , mStatus
  , mEventThreshold
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.MetricsStatus as Types
import qualified Network.AWS.S3.Types.ReplicationTimeValue as Types

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- /See:/ 'mkMetrics' smart constructor.
data Metrics = Metrics'
  { status :: Types.MetricsStatus
    -- ^ Specifies whether the replication metrics are enabled. 
  , eventThreshold :: Core.Maybe Types.ReplicationTimeValue
    -- ^ A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Metrics' value with any optional fields omitted.
mkMetrics
    :: Types.MetricsStatus -- ^ 'status'
    -> Metrics
mkMetrics status = Metrics'{status, eventThreshold = Core.Nothing}

-- | Specifies whether the replication metrics are enabled. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mStatus :: Lens.Lens' Metrics Types.MetricsStatus
mStatus = Lens.field @"status"
{-# INLINEABLE mStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event. 
--
-- /Note:/ Consider using 'eventThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEventThreshold :: Lens.Lens' Metrics (Core.Maybe Types.ReplicationTimeValue)
mEventThreshold = Lens.field @"eventThreshold"
{-# INLINEABLE mEventThreshold #-}
{-# DEPRECATED eventThreshold "Use generic-lens or generic-optics with 'eventThreshold' instead"  #-}

instance Core.ToXML Metrics where
        toXML Metrics{..}
          = Core.toXMLElement "Status" status Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "EventThreshold")
                eventThreshold

instance Core.FromXML Metrics where
        parseXML x
          = Metrics' Core.<$>
              (x Core..@ "Status") Core.<*> x Core..@? "EventThreshold"
