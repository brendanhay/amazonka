{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | Information about an application that processed requests, users that
-- made requests, or downstream services, resources, and applications that
-- an application used.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | A list of names for the service, including the canonical name.
    names :: Core.Maybe [Core.Text],
    -- | Identifier of the AWS account in which the service runs.
    accountId :: Core.Maybe Core.Text,
    -- | Aggregated statistics for the service.
    summaryStatistics :: Core.Maybe ServiceStatistics,
    -- | A histogram that maps the spread of service response times.
    responseTimeHistogram :: Core.Maybe [HistogramEntry],
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Core.Maybe Core.Int,
    -- | A histogram that maps the spread of service durations.
    durationHistogram :: Core.Maybe [HistogramEntry],
    -- | Connections to downstream services.
    edges :: Core.Maybe [Edge],
    -- | The start time of the first segment that the service generated.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end time of the last segment that the service generated.
    endTime :: Core.Maybe Core.POSIX,
    -- | The service\'s state.
    state :: Core.Maybe Core.Text,
    -- | The canonical name of the service.
    name :: Core.Maybe Core.Text,
    -- | Indicates that the service was the first service to process a request.
    root :: Core.Maybe Core.Bool,
    -- | The type of service.
    --
    -- -   AWS Resource - The type of an AWS resource. For example,
    --     @AWS::EC2::Instance@ for an application running on Amazon EC2 or
    --     @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the
    --     application used.
    --
    -- -   AWS Service - The type of an AWS service. For example,
    --     @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn\'t
    --     target a specific table.
    --
    -- -   @client@ - Represents the clients that sent requests to a root
    --     service.
    --
    -- -   @remote@ - A downstream service of indeterminate type.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'serviceInfo_names' - A list of names for the service, including the canonical name.
--
-- 'accountId', 'serviceInfo_accountId' - Identifier of the AWS account in which the service runs.
--
-- 'summaryStatistics', 'serviceInfo_summaryStatistics' - Aggregated statistics for the service.
--
-- 'responseTimeHistogram', 'serviceInfo_responseTimeHistogram' - A histogram that maps the spread of service response times.
--
-- 'referenceId', 'serviceInfo_referenceId' - Identifier for the service. Unique within the service map.
--
-- 'durationHistogram', 'serviceInfo_durationHistogram' - A histogram that maps the spread of service durations.
--
-- 'edges', 'serviceInfo_edges' - Connections to downstream services.
--
-- 'startTime', 'serviceInfo_startTime' - The start time of the first segment that the service generated.
--
-- 'endTime', 'serviceInfo_endTime' - The end time of the last segment that the service generated.
--
-- 'state', 'serviceInfo_state' - The service\'s state.
--
-- 'name', 'serviceInfo_name' - The canonical name of the service.
--
-- 'root', 'serviceInfo_root' - Indicates that the service was the first service to process a request.
--
-- 'type'', 'serviceInfo_type' - The type of service.
--
-- -   AWS Resource - The type of an AWS resource. For example,
--     @AWS::EC2::Instance@ for an application running on Amazon EC2 or
--     @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the
--     application used.
--
-- -   AWS Service - The type of an AWS service. For example,
--     @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn\'t
--     target a specific table.
--
-- -   @client@ - Represents the clients that sent requests to a root
--     service.
--
-- -   @remote@ - A downstream service of indeterminate type.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { names = Core.Nothing,
      accountId = Core.Nothing,
      summaryStatistics = Core.Nothing,
      responseTimeHistogram = Core.Nothing,
      referenceId = Core.Nothing,
      durationHistogram = Core.Nothing,
      edges = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      root = Core.Nothing,
      type' = Core.Nothing
    }

-- | A list of names for the service, including the canonical name.
serviceInfo_names :: Lens.Lens' ServiceInfo (Core.Maybe [Core.Text])
serviceInfo_names = Lens.lens (\ServiceInfo' {names} -> names) (\s@ServiceInfo' {} a -> s {names = a} :: ServiceInfo) Core.. Lens.mapping Lens._Coerce

-- | Identifier of the AWS account in which the service runs.
serviceInfo_accountId :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
serviceInfo_accountId = Lens.lens (\ServiceInfo' {accountId} -> accountId) (\s@ServiceInfo' {} a -> s {accountId = a} :: ServiceInfo)

-- | Aggregated statistics for the service.
serviceInfo_summaryStatistics :: Lens.Lens' ServiceInfo (Core.Maybe ServiceStatistics)
serviceInfo_summaryStatistics = Lens.lens (\ServiceInfo' {summaryStatistics} -> summaryStatistics) (\s@ServiceInfo' {} a -> s {summaryStatistics = a} :: ServiceInfo)

-- | A histogram that maps the spread of service response times.
serviceInfo_responseTimeHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [HistogramEntry])
serviceInfo_responseTimeHistogram = Lens.lens (\ServiceInfo' {responseTimeHistogram} -> responseTimeHistogram) (\s@ServiceInfo' {} a -> s {responseTimeHistogram = a} :: ServiceInfo) Core.. Lens.mapping Lens._Coerce

-- | Identifier for the service. Unique within the service map.
serviceInfo_referenceId :: Lens.Lens' ServiceInfo (Core.Maybe Core.Int)
serviceInfo_referenceId = Lens.lens (\ServiceInfo' {referenceId} -> referenceId) (\s@ServiceInfo' {} a -> s {referenceId = a} :: ServiceInfo)

-- | A histogram that maps the spread of service durations.
serviceInfo_durationHistogram :: Lens.Lens' ServiceInfo (Core.Maybe [HistogramEntry])
serviceInfo_durationHistogram = Lens.lens (\ServiceInfo' {durationHistogram} -> durationHistogram) (\s@ServiceInfo' {} a -> s {durationHistogram = a} :: ServiceInfo) Core.. Lens.mapping Lens._Coerce

-- | Connections to downstream services.
serviceInfo_edges :: Lens.Lens' ServiceInfo (Core.Maybe [Edge])
serviceInfo_edges = Lens.lens (\ServiceInfo' {edges} -> edges) (\s@ServiceInfo' {} a -> s {edges = a} :: ServiceInfo) Core.. Lens.mapping Lens._Coerce

-- | The start time of the first segment that the service generated.
serviceInfo_startTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.UTCTime)
serviceInfo_startTime = Lens.lens (\ServiceInfo' {startTime} -> startTime) (\s@ServiceInfo' {} a -> s {startTime = a} :: ServiceInfo) Core.. Lens.mapping Core._Time

-- | The end time of the last segment that the service generated.
serviceInfo_endTime :: Lens.Lens' ServiceInfo (Core.Maybe Core.UTCTime)
serviceInfo_endTime = Lens.lens (\ServiceInfo' {endTime} -> endTime) (\s@ServiceInfo' {} a -> s {endTime = a} :: ServiceInfo) Core.. Lens.mapping Core._Time

-- | The service\'s state.
serviceInfo_state :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
serviceInfo_state = Lens.lens (\ServiceInfo' {state} -> state) (\s@ServiceInfo' {} a -> s {state = a} :: ServiceInfo)

-- | The canonical name of the service.
serviceInfo_name :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
serviceInfo_name = Lens.lens (\ServiceInfo' {name} -> name) (\s@ServiceInfo' {} a -> s {name = a} :: ServiceInfo)

-- | Indicates that the service was the first service to process a request.
serviceInfo_root :: Lens.Lens' ServiceInfo (Core.Maybe Core.Bool)
serviceInfo_root = Lens.lens (\ServiceInfo' {root} -> root) (\s@ServiceInfo' {} a -> s {root = a} :: ServiceInfo)

-- | The type of service.
--
-- -   AWS Resource - The type of an AWS resource. For example,
--     @AWS::EC2::Instance@ for an application running on Amazon EC2 or
--     @AWS::DynamoDB::Table@ for an Amazon DynamoDB table that the
--     application used.
--
-- -   AWS Service - The type of an AWS service. For example,
--     @AWS::DynamoDB@ for downstream calls to Amazon DynamoDB that didn\'t
--     target a specific table.
--
-- -   @client@ - Represents the clients that sent requests to a root
--     service.
--
-- -   @remote@ - A downstream service of indeterminate type.
serviceInfo_type :: Lens.Lens' ServiceInfo (Core.Maybe Core.Text)
serviceInfo_type = Lens.lens (\ServiceInfo' {type'} -> type') (\s@ServiceInfo' {} a -> s {type' = a} :: ServiceInfo)

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "SummaryStatistics")
            Core.<*> ( x Core..:? "ResponseTimeHistogram"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ReferenceId")
            Core.<*> (x Core..:? "DurationHistogram" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Edges" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Root")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ServiceInfo

instance Core.NFData ServiceInfo
