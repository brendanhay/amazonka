{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ServiceInfo
  ( ServiceInfo (..)
  -- * Smart constructor
  , mkServiceInfo
  -- * Lenses
  , sAccountId
  , sDurationHistogram
  , sEdges
  , sEndTime
  , sName
  , sNames
  , sReferenceId
  , sResponseTimeHistogram
  , sRoot
  , sStartTime
  , sState
  , sSummaryStatistics
  , sType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Edge as Types
import qualified Network.AWS.XRay.Types.HistogramEntry as Types
import qualified Network.AWS.XRay.Types.ServiceStatistics as Types

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { accountId :: Core.Maybe Core.Text
    -- ^ Identifier of the AWS account in which the service runs.
  , durationHistogram :: Core.Maybe [Types.HistogramEntry]
    -- ^ A histogram that maps the spread of service durations.
  , edges :: Core.Maybe [Types.Edge]
    -- ^ Connections to downstream services.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time of the last segment that the service generated.
  , name :: Core.Maybe Core.Text
    -- ^ The canonical name of the service.
  , names :: Core.Maybe [Core.Text]
    -- ^ A list of names for the service, including the canonical name.
  , referenceId :: Core.Maybe Core.Int
    -- ^ Identifier for the service. Unique within the service map.
  , responseTimeHistogram :: Core.Maybe [Types.HistogramEntry]
    -- ^ A histogram that maps the spread of service response times.
  , root :: Core.Maybe Core.Bool
    -- ^ Indicates that the service was the first service to process a request.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time of the first segment that the service generated.
  , state :: Core.Maybe Core.Text
    -- ^ The service's state.
  , summaryStatistics :: Core.Maybe Types.ServiceStatistics
    -- ^ Aggregated statistics for the service.
  , type' :: Core.Maybe Core.Text
    -- ^ The type of service.
--
--
--     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for an application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.
--
--
--     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * @client@ - Represents the clients that sent requests to a root service.
--
--
--     * @remote@ - A downstream service of indeterminate type.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServiceInfo' value with any optional fields omitted.
mkServiceInfo
    :: ServiceInfo
mkServiceInfo
  = ServiceInfo'{accountId = Core.Nothing,
                 durationHistogram = Core.Nothing, edges = Core.Nothing,
                 endTime = Core.Nothing, name = Core.Nothing, names = Core.Nothing,
                 referenceId = Core.Nothing, responseTimeHistogram = Core.Nothing,
                 root = Core.Nothing, startTime = Core.Nothing,
                 state = Core.Nothing, summaryStatistics = Core.Nothing,
                 type' = Core.Nothing}

-- | Identifier of the AWS account in which the service runs.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountId :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
sAccountId = Lens.field @"accountId"
{-# INLINEABLE sAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | A histogram that maps the spread of service durations.
--
-- /Note:/ Consider using 'durationHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDurationHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [Types.HistogramEntry])
sDurationHistogram = Lens.field @"durationHistogram"
{-# INLINEABLE sDurationHistogram #-}
{-# DEPRECATED durationHistogram "Use generic-lens or generic-optics with 'durationHistogram' instead"  #-}

-- | Connections to downstream services.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEdges :: Lens.Lens' ServiceInfo (Core.Maybe [Types.Edge])
sEdges = Lens.field @"edges"
{-# INLINEABLE sEdges #-}
{-# DEPRECATED edges "Use generic-lens or generic-optics with 'edges' instead"  #-}

-- | The end time of the last segment that the service generated.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.NominalDiffTime)
sEndTime = Lens.field @"endTime"
{-# INLINEABLE sEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The canonical name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
sName = Lens.field @"name"
{-# INLINEABLE sName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of names for the service, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNames :: Lens.Lens' ServiceInfo (Core.Maybe [Core.Text])
sNames = Lens.field @"names"
{-# INLINEABLE sNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | Identifier for the service. Unique within the service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReferenceId :: Lens.Lens' ServiceInfo (Core.Maybe Core.Int)
sReferenceId = Lens.field @"referenceId"
{-# INLINEABLE sReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

-- | A histogram that maps the spread of service response times.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResponseTimeHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [Types.HistogramEntry])
sResponseTimeHistogram = Lens.field @"responseTimeHistogram"
{-# INLINEABLE sResponseTimeHistogram #-}
{-# DEPRECATED responseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead"  #-}

-- | Indicates that the service was the first service to process a request.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoot :: Lens.Lens' ServiceInfo (Core.Maybe Core.Bool)
sRoot = Lens.field @"root"
{-# INLINEABLE sRoot #-}
{-# DEPRECATED root "Use generic-lens or generic-optics with 'root' instead"  #-}

-- | The start time of the first segment that the service generated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.NominalDiffTime)
sStartTime = Lens.field @"startTime"
{-# INLINEABLE sStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The service's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
sState = Lens.field @"state"
{-# INLINEABLE sState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Aggregated statistics for the service.
--
-- /Note:/ Consider using 'summaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSummaryStatistics :: Lens.Lens' ServiceInfo (Core.Maybe Types.ServiceStatistics)
sSummaryStatistics = Lens.field @"summaryStatistics"
{-# INLINEABLE sSummaryStatistics #-}
{-# DEPRECATED summaryStatistics "Use generic-lens or generic-optics with 'summaryStatistics' instead"  #-}

-- | The type of service.
--
--
--     * AWS Resource - The type of an AWS resource. For example, @AWS::EC2::Instance@ for an application running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the application used.
--
--
--     * AWS Service - The type of an AWS service. For example, @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * @client@ - Represents the clients that sent requests to a root service.
--
--
--     * @remote@ - A downstream service of indeterminate type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
sType = Lens.field @"type'"
{-# INLINEABLE sType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ServiceInfo where
        parseJSON
          = Core.withObject "ServiceInfo" Core.$
              \ x ->
                ServiceInfo' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "DurationHistogram"
                    Core.<*> x Core..:? "Edges"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Names"
                    Core.<*> x Core..:? "ReferenceId"
                    Core.<*> x Core..:? "ResponseTimeHistogram"
                    Core.<*> x Core..:? "Root"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "SummaryStatistics"
                    Core.<*> x Core..:? "Type"
