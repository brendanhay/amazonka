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
-- Module      : Amazonka.DataSync.Types.ResourceMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.ResourceMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.Capacity
import Amazonka.DataSync.Types.DiscoveryResourceType
import Amazonka.DataSync.Types.P95Metrics
import qualified Amazonka.Prelude as Prelude

-- | Information, including performance data and capacity usage, provided by
-- DataSync Discovery about a resource in your on-premises storage system.
--
-- /See:/ 'newResourceMetrics' smart constructor.
data ResourceMetrics = ResourceMetrics'
  { -- | The storage capacity of the on-premises storage system resource.
    capacity :: Prelude.Maybe Capacity,
    -- | The types of performance data that DataSync Discovery collects about the
    -- on-premises storage system resource.
    p95Metrics :: Prelude.Maybe P95Metrics,
    -- | The universally unique identifier (UUID) of the on-premises storage
    -- system resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of on-premises storage system resource.
    resourceType :: Prelude.Maybe DiscoveryResourceType,
    -- | The time when DataSync Discovery collected this information from the
    -- resource.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'resourceMetrics_capacity' - The storage capacity of the on-premises storage system resource.
--
-- 'p95Metrics', 'resourceMetrics_p95Metrics' - The types of performance data that DataSync Discovery collects about the
-- on-premises storage system resource.
--
-- 'resourceId', 'resourceMetrics_resourceId' - The universally unique identifier (UUID) of the on-premises storage
-- system resource.
--
-- 'resourceType', 'resourceMetrics_resourceType' - The type of on-premises storage system resource.
--
-- 'timestamp', 'resourceMetrics_timestamp' - The time when DataSync Discovery collected this information from the
-- resource.
newResourceMetrics ::
  ResourceMetrics
newResourceMetrics =
  ResourceMetrics'
    { capacity = Prelude.Nothing,
      p95Metrics = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The storage capacity of the on-premises storage system resource.
resourceMetrics_capacity :: Lens.Lens' ResourceMetrics (Prelude.Maybe Capacity)
resourceMetrics_capacity = Lens.lens (\ResourceMetrics' {capacity} -> capacity) (\s@ResourceMetrics' {} a -> s {capacity = a} :: ResourceMetrics)

-- | The types of performance data that DataSync Discovery collects about the
-- on-premises storage system resource.
resourceMetrics_p95Metrics :: Lens.Lens' ResourceMetrics (Prelude.Maybe P95Metrics)
resourceMetrics_p95Metrics = Lens.lens (\ResourceMetrics' {p95Metrics} -> p95Metrics) (\s@ResourceMetrics' {} a -> s {p95Metrics = a} :: ResourceMetrics)

-- | The universally unique identifier (UUID) of the on-premises storage
-- system resource.
resourceMetrics_resourceId :: Lens.Lens' ResourceMetrics (Prelude.Maybe Prelude.Text)
resourceMetrics_resourceId = Lens.lens (\ResourceMetrics' {resourceId} -> resourceId) (\s@ResourceMetrics' {} a -> s {resourceId = a} :: ResourceMetrics)

-- | The type of on-premises storage system resource.
resourceMetrics_resourceType :: Lens.Lens' ResourceMetrics (Prelude.Maybe DiscoveryResourceType)
resourceMetrics_resourceType = Lens.lens (\ResourceMetrics' {resourceType} -> resourceType) (\s@ResourceMetrics' {} a -> s {resourceType = a} :: ResourceMetrics)

-- | The time when DataSync Discovery collected this information from the
-- resource.
resourceMetrics_timestamp :: Lens.Lens' ResourceMetrics (Prelude.Maybe Prelude.UTCTime)
resourceMetrics_timestamp = Lens.lens (\ResourceMetrics' {timestamp} -> timestamp) (\s@ResourceMetrics' {} a -> s {timestamp = a} :: ResourceMetrics) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ResourceMetrics where
  parseJSON =
    Data.withObject
      "ResourceMetrics"
      ( \x ->
          ResourceMetrics'
            Prelude.<$> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "P95Metrics")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable ResourceMetrics where
  hashWithSalt _salt ResourceMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` p95Metrics
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData ResourceMetrics where
  rnf ResourceMetrics' {..} =
    Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf p95Metrics
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf timestamp
