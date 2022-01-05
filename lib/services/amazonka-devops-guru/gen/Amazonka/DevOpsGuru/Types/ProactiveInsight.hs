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
-- Module      : Amazonka.DevOpsGuru.Types.ProactiveInsight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ProactiveInsight where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a proactive insight. This object is returned by
-- @ListInsights@.
--
-- /See:/ 'newProactiveInsight' smart constructor.
data ProactiveInsight = ProactiveInsight'
  { -- | The status of the proactive insight.
    status :: Prelude.Maybe InsightStatus,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the proactive insight.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The ID of the AWS System Manager OpsItem created for this insight. You
    -- must enable the creation of OpstItems insights before they are created
    -- for each insight.
    ssmOpsItemId :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of the proactive insight.
    name :: Prelude.Maybe Prelude.Text,
    predictionTimeRange :: Prelude.Maybe PredictionTimeRange,
    -- | The ID of the proactive insight.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProactiveInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'proactiveInsight_status' - The status of the proactive insight.
--
-- 'resourceCollection', 'proactiveInsight_resourceCollection' - Undocumented member.
--
-- 'severity', 'proactiveInsight_severity' - The severity of the proactive insight.
--
-- 'ssmOpsItemId', 'proactiveInsight_ssmOpsItemId' - The ID of the AWS System Manager OpsItem created for this insight. You
-- must enable the creation of OpstItems insights before they are created
-- for each insight.
--
-- 'insightTimeRange', 'proactiveInsight_insightTimeRange' - Undocumented member.
--
-- 'name', 'proactiveInsight_name' - The name of the proactive insight.
--
-- 'predictionTimeRange', 'proactiveInsight_predictionTimeRange' - Undocumented member.
--
-- 'id', 'proactiveInsight_id' - The ID of the proactive insight.
newProactiveInsight ::
  ProactiveInsight
newProactiveInsight =
  ProactiveInsight'
    { status = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      ssmOpsItemId = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The status of the proactive insight.
proactiveInsight_status :: Lens.Lens' ProactiveInsight (Prelude.Maybe InsightStatus)
proactiveInsight_status = Lens.lens (\ProactiveInsight' {status} -> status) (\s@ProactiveInsight' {} a -> s {status = a} :: ProactiveInsight)

-- | Undocumented member.
proactiveInsight_resourceCollection :: Lens.Lens' ProactiveInsight (Prelude.Maybe ResourceCollection)
proactiveInsight_resourceCollection = Lens.lens (\ProactiveInsight' {resourceCollection} -> resourceCollection) (\s@ProactiveInsight' {} a -> s {resourceCollection = a} :: ProactiveInsight)

-- | The severity of the proactive insight.
proactiveInsight_severity :: Lens.Lens' ProactiveInsight (Prelude.Maybe InsightSeverity)
proactiveInsight_severity = Lens.lens (\ProactiveInsight' {severity} -> severity) (\s@ProactiveInsight' {} a -> s {severity = a} :: ProactiveInsight)

-- | The ID of the AWS System Manager OpsItem created for this insight. You
-- must enable the creation of OpstItems insights before they are created
-- for each insight.
proactiveInsight_ssmOpsItemId :: Lens.Lens' ProactiveInsight (Prelude.Maybe Prelude.Text)
proactiveInsight_ssmOpsItemId = Lens.lens (\ProactiveInsight' {ssmOpsItemId} -> ssmOpsItemId) (\s@ProactiveInsight' {} a -> s {ssmOpsItemId = a} :: ProactiveInsight)

-- | Undocumented member.
proactiveInsight_insightTimeRange :: Lens.Lens' ProactiveInsight (Prelude.Maybe InsightTimeRange)
proactiveInsight_insightTimeRange = Lens.lens (\ProactiveInsight' {insightTimeRange} -> insightTimeRange) (\s@ProactiveInsight' {} a -> s {insightTimeRange = a} :: ProactiveInsight)

-- | The name of the proactive insight.
proactiveInsight_name :: Lens.Lens' ProactiveInsight (Prelude.Maybe Prelude.Text)
proactiveInsight_name = Lens.lens (\ProactiveInsight' {name} -> name) (\s@ProactiveInsight' {} a -> s {name = a} :: ProactiveInsight)

-- | Undocumented member.
proactiveInsight_predictionTimeRange :: Lens.Lens' ProactiveInsight (Prelude.Maybe PredictionTimeRange)
proactiveInsight_predictionTimeRange = Lens.lens (\ProactiveInsight' {predictionTimeRange} -> predictionTimeRange) (\s@ProactiveInsight' {} a -> s {predictionTimeRange = a} :: ProactiveInsight)

-- | The ID of the proactive insight.
proactiveInsight_id :: Lens.Lens' ProactiveInsight (Prelude.Maybe Prelude.Text)
proactiveInsight_id = Lens.lens (\ProactiveInsight' {id} -> id) (\s@ProactiveInsight' {} a -> s {id = a} :: ProactiveInsight)

instance Core.FromJSON ProactiveInsight where
  parseJSON =
    Core.withObject
      "ProactiveInsight"
      ( \x ->
          ProactiveInsight'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "SsmOpsItemId")
            Prelude.<*> (x Core..:? "InsightTimeRange")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "PredictionTimeRange")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ProactiveInsight where
  hashWithSalt _salt ProactiveInsight' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` ssmOpsItemId
      `Prelude.hashWithSalt` insightTimeRange
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` predictionTimeRange
      `Prelude.hashWithSalt` id

instance Prelude.NFData ProactiveInsight where
  rnf ProactiveInsight' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf ssmOpsItemId
      `Prelude.seq` Prelude.rnf insightTimeRange
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf predictionTimeRange
      `Prelude.seq` Prelude.rnf id
