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
-- Module      : Amazonka.XRay.Types.ServiceInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ServiceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.Edge
import Amazonka.XRay.Types.HistogramEntry
import Amazonka.XRay.Types.ServiceStatistics

-- | Information about an application that processed requests, users that
-- made requests, or downstream services, resources, and applications that
-- an application used.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | Connections to downstream services.
    edges :: Prelude.Maybe [Edge],
    -- | The canonical name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of service.
    --
    -- -   Amazon Web Services Resource - The type of an Amazon Web Services
    --     resource. For example, @AWS::EC2::Instance@ for an application
    --     running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon
    --     DynamoDB table that the application used.
    --
    -- -   Amazon Web Services Service - The type of an Amazon Web Services
    --     service. For example, @AWS::DynamoDB@ for downstream calls to Amazon
    --     DynamoDB that didn\'t target a specific table.
    --
    -- -   @client@ - Represents the clients that sent requests to a root
    --     service.
    --
    -- -   @remote@ - A downstream service of indeterminate type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | Aggregated statistics for the service.
    summaryStatistics :: Prelude.Maybe ServiceStatistics,
    -- | The service\'s state.
    state :: Prelude.Maybe Prelude.Text,
    -- | The end time of the last segment that the service generated.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | A list of names for the service, including the canonical name.
    names :: Prelude.Maybe [Prelude.Text],
    -- | A histogram that maps the spread of service response times.
    responseTimeHistogram :: Prelude.Maybe [HistogramEntry],
    -- | Identifier of the Amazon Web Services account in which the service runs.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Indicates that the service was the first service to process a request.
    root :: Prelude.Maybe Prelude.Bool,
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Prelude.Maybe Prelude.Int,
    -- | A histogram that maps the spread of service durations.
    durationHistogram :: Prelude.Maybe [HistogramEntry],
    -- | The start time of the first segment that the service generated.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edges', 'serviceInfo_edges' - Connections to downstream services.
--
-- 'name', 'serviceInfo_name' - The canonical name of the service.
--
-- 'type'', 'serviceInfo_type' - The type of service.
--
-- -   Amazon Web Services Resource - The type of an Amazon Web Services
--     resource. For example, @AWS::EC2::Instance@ for an application
--     running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon
--     DynamoDB table that the application used.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, @AWS::DynamoDB@ for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   @client@ - Represents the clients that sent requests to a root
--     service.
--
-- -   @remote@ - A downstream service of indeterminate type.
--
-- 'summaryStatistics', 'serviceInfo_summaryStatistics' - Aggregated statistics for the service.
--
-- 'state', 'serviceInfo_state' - The service\'s state.
--
-- 'endTime', 'serviceInfo_endTime' - The end time of the last segment that the service generated.
--
-- 'names', 'serviceInfo_names' - A list of names for the service, including the canonical name.
--
-- 'responseTimeHistogram', 'serviceInfo_responseTimeHistogram' - A histogram that maps the spread of service response times.
--
-- 'accountId', 'serviceInfo_accountId' - Identifier of the Amazon Web Services account in which the service runs.
--
-- 'root', 'serviceInfo_root' - Indicates that the service was the first service to process a request.
--
-- 'referenceId', 'serviceInfo_referenceId' - Identifier for the service. Unique within the service map.
--
-- 'durationHistogram', 'serviceInfo_durationHistogram' - A histogram that maps the spread of service durations.
--
-- 'startTime', 'serviceInfo_startTime' - The start time of the first segment that the service generated.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { edges = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      summaryStatistics = Prelude.Nothing,
      state = Prelude.Nothing,
      endTime = Prelude.Nothing,
      names = Prelude.Nothing,
      responseTimeHistogram = Prelude.Nothing,
      accountId = Prelude.Nothing,
      root = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      durationHistogram = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Connections to downstream services.
serviceInfo_edges :: Lens.Lens' ServiceInfo (Prelude.Maybe [Edge])
serviceInfo_edges = Lens.lens (\ServiceInfo' {edges} -> edges) (\s@ServiceInfo' {} a -> s {edges = a} :: ServiceInfo) Prelude.. Lens.mapping Lens.coerced

-- | The canonical name of the service.
serviceInfo_name :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_name = Lens.lens (\ServiceInfo' {name} -> name) (\s@ServiceInfo' {} a -> s {name = a} :: ServiceInfo)

-- | The type of service.
--
-- -   Amazon Web Services Resource - The type of an Amazon Web Services
--     resource. For example, @AWS::EC2::Instance@ for an application
--     running on Amazon EC2 or @AWS::DynamoDB::Table@ for an Amazon
--     DynamoDB table that the application used.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, @AWS::DynamoDB@ for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   @client@ - Represents the clients that sent requests to a root
--     service.
--
-- -   @remote@ - A downstream service of indeterminate type.
serviceInfo_type :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_type = Lens.lens (\ServiceInfo' {type'} -> type') (\s@ServiceInfo' {} a -> s {type' = a} :: ServiceInfo)

-- | Aggregated statistics for the service.
serviceInfo_summaryStatistics :: Lens.Lens' ServiceInfo (Prelude.Maybe ServiceStatistics)
serviceInfo_summaryStatistics = Lens.lens (\ServiceInfo' {summaryStatistics} -> summaryStatistics) (\s@ServiceInfo' {} a -> s {summaryStatistics = a} :: ServiceInfo)

-- | The service\'s state.
serviceInfo_state :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_state = Lens.lens (\ServiceInfo' {state} -> state) (\s@ServiceInfo' {} a -> s {state = a} :: ServiceInfo)

-- | The end time of the last segment that the service generated.
serviceInfo_endTime :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.UTCTime)
serviceInfo_endTime = Lens.lens (\ServiceInfo' {endTime} -> endTime) (\s@ServiceInfo' {} a -> s {endTime = a} :: ServiceInfo) Prelude.. Lens.mapping Core._Time

-- | A list of names for the service, including the canonical name.
serviceInfo_names :: Lens.Lens' ServiceInfo (Prelude.Maybe [Prelude.Text])
serviceInfo_names = Lens.lens (\ServiceInfo' {names} -> names) (\s@ServiceInfo' {} a -> s {names = a} :: ServiceInfo) Prelude.. Lens.mapping Lens.coerced

-- | A histogram that maps the spread of service response times.
serviceInfo_responseTimeHistogram :: Lens.Lens' ServiceInfo (Prelude.Maybe [HistogramEntry])
serviceInfo_responseTimeHistogram = Lens.lens (\ServiceInfo' {responseTimeHistogram} -> responseTimeHistogram) (\s@ServiceInfo' {} a -> s {responseTimeHistogram = a} :: ServiceInfo) Prelude.. Lens.mapping Lens.coerced

-- | Identifier of the Amazon Web Services account in which the service runs.
serviceInfo_accountId :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_accountId = Lens.lens (\ServiceInfo' {accountId} -> accountId) (\s@ServiceInfo' {} a -> s {accountId = a} :: ServiceInfo)

-- | Indicates that the service was the first service to process a request.
serviceInfo_root :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Bool)
serviceInfo_root = Lens.lens (\ServiceInfo' {root} -> root) (\s@ServiceInfo' {} a -> s {root = a} :: ServiceInfo)

-- | Identifier for the service. Unique within the service map.
serviceInfo_referenceId :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Int)
serviceInfo_referenceId = Lens.lens (\ServiceInfo' {referenceId} -> referenceId) (\s@ServiceInfo' {} a -> s {referenceId = a} :: ServiceInfo)

-- | A histogram that maps the spread of service durations.
serviceInfo_durationHistogram :: Lens.Lens' ServiceInfo (Prelude.Maybe [HistogramEntry])
serviceInfo_durationHistogram = Lens.lens (\ServiceInfo' {durationHistogram} -> durationHistogram) (\s@ServiceInfo' {} a -> s {durationHistogram = a} :: ServiceInfo) Prelude.. Lens.mapping Lens.coerced

-- | The start time of the first segment that the service generated.
serviceInfo_startTime :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.UTCTime)
serviceInfo_startTime = Lens.lens (\ServiceInfo' {startTime} -> startTime) (\s@ServiceInfo' {} a -> s {startTime = a} :: ServiceInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Prelude.<$> (x Core..:? "Edges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "SummaryStatistics")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "ResponseTimeHistogram"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Root")
            Prelude.<*> (x Core..:? "ReferenceId")
            Prelude.<*> ( x Core..:? "DurationHistogram"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StartTime")
      )

instance Prelude.Hashable ServiceInfo where
  hashWithSalt _salt ServiceInfo' {..} =
    _salt `Prelude.hashWithSalt` edges
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` summaryStatistics
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` responseTimeHistogram
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` root
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` durationHistogram
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ServiceInfo where
  rnf ServiceInfo' {..} =
    Prelude.rnf edges
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf summaryStatistics
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf responseTimeHistogram
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf root
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf durationHistogram
      `Prelude.seq` Prelude.rnf startTime
