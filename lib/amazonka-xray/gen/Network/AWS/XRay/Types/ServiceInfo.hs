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
    sState,
    sStartTime,
    sRoot,
    sResponseTimeHistogram,
    sDurationHistogram,
    sReferenceId,
    sAccountId,
    sNames,
    sName,
    sEndTime,
    sType,
    sEdges,
    sSummaryStatistics,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { state :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    root :: Lude.Maybe Lude.Bool,
    responseTimeHistogram :: Lude.Maybe [HistogramEntry],
    durationHistogram :: Lude.Maybe [HistogramEntry],
    referenceId :: Lude.Maybe Lude.Int,
    accountId :: Lude.Maybe Lude.Text,
    names :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    type' :: Lude.Maybe Lude.Text,
    edges :: Lude.Maybe [Edge],
    summaryStatistics :: Lude.Maybe ServiceStatistics
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- * 'accountId' - Identifier of the AWS account in which the service runs.
-- * 'durationHistogram' - A histogram that maps the spread of service durations.
-- * 'edges' - Connections to downstream services.
-- * 'endTime' - The end time of the last segment that the service generated.
-- * 'name' - The canonical name of the service.
-- * 'names' - A list of names for the service, including the canonical name.
-- * 'referenceId' - Identifier for the service. Unique within the service map.
-- * 'responseTimeHistogram' - A histogram that maps the spread of service response times.
-- * 'root' - Indicates that the service was the first service to process a request.
-- * 'startTime' - The start time of the first segment that the service generated.
-- * 'state' - The service's state.
-- * 'summaryStatistics' - Aggregated statistics for the service.
-- * 'type'' - The type of service.
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
mkServiceInfo ::
  ServiceInfo
mkServiceInfo =
  ServiceInfo'
    { state = Lude.Nothing,
      startTime = Lude.Nothing,
      root = Lude.Nothing,
      responseTimeHistogram = Lude.Nothing,
      durationHistogram = Lude.Nothing,
      referenceId = Lude.Nothing,
      accountId = Lude.Nothing,
      names = Lude.Nothing,
      name = Lude.Nothing,
      endTime = Lude.Nothing,
      type' = Lude.Nothing,
      edges = Lude.Nothing,
      summaryStatistics = Lude.Nothing
    }

-- | The service's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
sState = Lens.lens (state :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ServiceInfo)
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The start time of the first segment that the service generated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Timestamp)
sStartTime = Lens.lens (startTime :: ServiceInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ServiceInfo)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Indicates that the service was the first service to process a request.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoot :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Bool)
sRoot = Lens.lens (root :: ServiceInfo -> Lude.Maybe Lude.Bool) (\s a -> s {root = a} :: ServiceInfo)
{-# DEPRECATED sRoot "Use generic-lens or generic-optics with 'root' instead." #-}

-- | A histogram that maps the spread of service response times.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResponseTimeHistogram :: Lens.Lens' ServiceInfo (Lude.Maybe [HistogramEntry])
sResponseTimeHistogram = Lens.lens (responseTimeHistogram :: ServiceInfo -> Lude.Maybe [HistogramEntry]) (\s a -> s {responseTimeHistogram = a} :: ServiceInfo)
{-# DEPRECATED sResponseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead." #-}

-- | A histogram that maps the spread of service durations.
--
-- /Note:/ Consider using 'durationHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDurationHistogram :: Lens.Lens' ServiceInfo (Lude.Maybe [HistogramEntry])
sDurationHistogram = Lens.lens (durationHistogram :: ServiceInfo -> Lude.Maybe [HistogramEntry]) (\s a -> s {durationHistogram = a} :: ServiceInfo)
{-# DEPRECATED sDurationHistogram "Use generic-lens or generic-optics with 'durationHistogram' instead." #-}

-- | Identifier for the service. Unique within the service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReferenceId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Int)
sReferenceId = Lens.lens (referenceId :: ServiceInfo -> Lude.Maybe Lude.Int) (\s a -> s {referenceId = a} :: ServiceInfo)
{-# DEPRECATED sReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | Identifier of the AWS account in which the service runs.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
sAccountId = Lens.lens (accountId :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ServiceInfo)
{-# DEPRECATED sAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A list of names for the service, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNames :: Lens.Lens' ServiceInfo (Lude.Maybe [Lude.Text])
sNames = Lens.lens (names :: ServiceInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ServiceInfo)
{-# DEPRECATED sNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The canonical name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServiceInfo)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The end time of the last segment that the service generated.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Timestamp)
sEndTime = Lens.lens (endTime :: ServiceInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: ServiceInfo)
{-# DEPRECATED sEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

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
sType :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
sType = Lens.lens (type' :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ServiceInfo)
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Connections to downstream services.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEdges :: Lens.Lens' ServiceInfo (Lude.Maybe [Edge])
sEdges = Lens.lens (edges :: ServiceInfo -> Lude.Maybe [Edge]) (\s a -> s {edges = a} :: ServiceInfo)
{-# DEPRECATED sEdges "Use generic-lens or generic-optics with 'edges' instead." #-}

-- | Aggregated statistics for the service.
--
-- /Note:/ Consider using 'summaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSummaryStatistics :: Lens.Lens' ServiceInfo (Lude.Maybe ServiceStatistics)
sSummaryStatistics = Lens.lens (summaryStatistics :: ServiceInfo -> Lude.Maybe ServiceStatistics) (\s a -> s {summaryStatistics = a} :: ServiceInfo)
{-# DEPRECATED sSummaryStatistics "Use generic-lens or generic-optics with 'summaryStatistics' instead." #-}

instance Lude.FromJSON ServiceInfo where
  parseJSON =
    Lude.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Root")
            Lude.<*> (x Lude..:? "ResponseTimeHistogram" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DurationHistogram" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReferenceId")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Edges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SummaryStatistics")
      )
