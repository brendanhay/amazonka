{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceInfo
  ( ServiceInfo (..),

    -- * Smart constructor
    mkServiceInfo,

    -- * Lenses
    sAccountId,
    sDurationHistogram,
    sEdges,
    sEndTime,
    sName,
    sNames,
    sReferenceId,
    sResponseTimeHistogram,
    sRoot,
    sStartTime,
    sState,
    sSummaryStatistics,
    sType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Edge as Types
import qualified Network.AWS.XRay.Types.HistogramEntry as Types
import qualified Network.AWS.XRay.Types.ServiceStatistics as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | Identifier of the AWS account in which the service runs.
    accountId :: Core.Maybe Types.String,
    -- | A histogram that maps the spread of service durations.
    durationHistogram :: Core.Maybe [Types.HistogramEntry],
    -- | Connections to downstream services.
    edges :: Core.Maybe [Types.Edge],
    -- | The end time of the last segment that the service generated.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The canonical name of the service.
    name :: Core.Maybe Types.String,
    -- | A list of names for the service, including the canonical name.
    names :: Core.Maybe [Types.String],
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Core.Maybe Core.Int,
    -- | A histogram that maps the spread of service response times.
    responseTimeHistogram :: Core.Maybe [Types.HistogramEntry],
    -- | Indicates that the service was the first service to process a request.
    root :: Core.Maybe Core.Bool,
    -- | The start time of the first segment that the service generated.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The service's state.
    state :: Core.Maybe Types.String,
    -- | Aggregated statistics for the service.
    summaryStatistics :: Core.Maybe Types.ServiceStatistics,
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
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceInfo' value with any optional fields omitted.
mkServiceInfo ::
  ServiceInfo
mkServiceInfo =
  ServiceInfo'
    { accountId = Core.Nothing,
      durationHistogram = Core.Nothing,
      edges = Core.Nothing,
      endTime = Core.Nothing,
      name = Core.Nothing,
      names = Core.Nothing,
      referenceId = Core.Nothing,
      responseTimeHistogram = Core.Nothing,
      root = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      summaryStatistics = Core.Nothing,
      type' = Core.Nothing
    }

-- | Identifier of the AWS account in which the service runs.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountId :: Lens.Lens' ServiceInfo (Core.Maybe Types.String)
sAccountId = Lens.field @"accountId"
{-# DEPRECATED sAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A histogram that maps the spread of service durations.
--
-- /Note:/ Consider using 'durationHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDurationHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [Types.HistogramEntry])
sDurationHistogram = Lens.field @"durationHistogram"
{-# DEPRECATED sDurationHistogram "Use generic-lens or generic-optics with 'durationHistogram' instead." #-}

-- | Connections to downstream services.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEdges :: Lens.Lens' ServiceInfo (Core.Maybe [Types.Edge])
sEdges = Lens.field @"edges"
{-# DEPRECATED sEdges "Use generic-lens or generic-optics with 'edges' instead." #-}

-- | The end time of the last segment that the service generated.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.NominalDiffTime)
sEndTime = Lens.field @"endTime"
{-# DEPRECATED sEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The canonical name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' ServiceInfo (Core.Maybe Types.String)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of names for the service, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNames :: Lens.Lens' ServiceInfo (Core.Maybe [Types.String])
sNames = Lens.field @"names"
{-# DEPRECATED sNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | Identifier for the service. Unique within the service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReferenceId :: Lens.Lens' ServiceInfo (Core.Maybe Core.Int)
sReferenceId = Lens.field @"referenceId"
{-# DEPRECATED sReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | A histogram that maps the spread of service response times.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResponseTimeHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [Types.HistogramEntry])
sResponseTimeHistogram = Lens.field @"responseTimeHistogram"
{-# DEPRECATED sResponseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead." #-}

-- | Indicates that the service was the first service to process a request.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoot :: Lens.Lens' ServiceInfo (Core.Maybe Core.Bool)
sRoot = Lens.field @"root"
{-# DEPRECATED sRoot "Use generic-lens or generic-optics with 'root' instead." #-}

-- | The start time of the first segment that the service generated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.NominalDiffTime)
sStartTime = Lens.field @"startTime"
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The service's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' ServiceInfo (Core.Maybe Types.String)
sState = Lens.field @"state"
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Aggregated statistics for the service.
--
-- /Note:/ Consider using 'summaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSummaryStatistics :: Lens.Lens' ServiceInfo (Core.Maybe Types.ServiceStatistics)
sSummaryStatistics = Lens.field @"summaryStatistics"
{-# DEPRECATED sSummaryStatistics "Use generic-lens or generic-optics with 'summaryStatistics' instead." #-}

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
sType :: Lens.Lens' ServiceInfo (Core.Maybe Types.String)
sType = Lens.field @"type'"
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject "ServiceInfo" Core.$
      \x ->
        ServiceInfo'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "DurationHistogram")
          Core.<*> (x Core..:? "Edges")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "ReferenceId")
          Core.<*> (x Core..:? "ResponseTimeHistogram")
          Core.<*> (x Core..:? "Root")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "SummaryStatistics")
          Core.<*> (x Core..:? "Type")
