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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveInsight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveInsight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Prelude as Prelude

-- | Information about a reactive insight. This object is returned by
-- @ListInsights@.
--
-- /See:/ 'newReactiveInsight' smart constructor.
data ReactiveInsight = ReactiveInsight'
  { -- | The severity of the insight. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The name of a reactive insight.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The status of a reactive insight.
    status :: Prelude.Maybe InsightStatus,
    -- | The ID of a reactive insight.
    id :: Prelude.Maybe Prelude.Text,
    -- | Describes the reactive insight.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services System Manager OpsItem created for
    -- this insight. You must enable the creation of OpstItems insights before
    -- they are created for each insight.
    ssmOpsItemId :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactiveInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severity', 'reactiveInsight_severity' - The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'name', 'reactiveInsight_name' - The name of a reactive insight.
--
-- 'resourceCollection', 'reactiveInsight_resourceCollection' - Undocumented member.
--
-- 'status', 'reactiveInsight_status' - The status of a reactive insight.
--
-- 'id', 'reactiveInsight_id' - The ID of a reactive insight.
--
-- 'description', 'reactiveInsight_description' - Describes the reactive insight.
--
-- 'ssmOpsItemId', 'reactiveInsight_ssmOpsItemId' - The ID of the Amazon Web Services System Manager OpsItem created for
-- this insight. You must enable the creation of OpstItems insights before
-- they are created for each insight.
--
-- 'insightTimeRange', 'reactiveInsight_insightTimeRange' - Undocumented member.
newReactiveInsight ::
  ReactiveInsight
newReactiveInsight =
  ReactiveInsight'
    { severity = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      ssmOpsItemId = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing
    }

-- | The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveInsight_severity :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightSeverity)
reactiveInsight_severity = Lens.lens (\ReactiveInsight' {severity} -> severity) (\s@ReactiveInsight' {} a -> s {severity = a} :: ReactiveInsight)

-- | The name of a reactive insight.
reactiveInsight_name :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_name = Lens.lens (\ReactiveInsight' {name} -> name) (\s@ReactiveInsight' {} a -> s {name = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_resourceCollection :: Lens.Lens' ReactiveInsight (Prelude.Maybe ResourceCollection)
reactiveInsight_resourceCollection = Lens.lens (\ReactiveInsight' {resourceCollection} -> resourceCollection) (\s@ReactiveInsight' {} a -> s {resourceCollection = a} :: ReactiveInsight)

-- | The status of a reactive insight.
reactiveInsight_status :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightStatus)
reactiveInsight_status = Lens.lens (\ReactiveInsight' {status} -> status) (\s@ReactiveInsight' {} a -> s {status = a} :: ReactiveInsight)

-- | The ID of a reactive insight.
reactiveInsight_id :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_id = Lens.lens (\ReactiveInsight' {id} -> id) (\s@ReactiveInsight' {} a -> s {id = a} :: ReactiveInsight)

-- | Describes the reactive insight.
reactiveInsight_description :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_description = Lens.lens (\ReactiveInsight' {description} -> description) (\s@ReactiveInsight' {} a -> s {description = a} :: ReactiveInsight)

-- | The ID of the Amazon Web Services System Manager OpsItem created for
-- this insight. You must enable the creation of OpstItems insights before
-- they are created for each insight.
reactiveInsight_ssmOpsItemId :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_ssmOpsItemId = Lens.lens (\ReactiveInsight' {ssmOpsItemId} -> ssmOpsItemId) (\s@ReactiveInsight' {} a -> s {ssmOpsItemId = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_insightTimeRange :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightTimeRange)
reactiveInsight_insightTimeRange = Lens.lens (\ReactiveInsight' {insightTimeRange} -> insightTimeRange) (\s@ReactiveInsight' {} a -> s {insightTimeRange = a} :: ReactiveInsight)

instance Core.FromJSON ReactiveInsight where
  parseJSON =
    Core.withObject
      "ReactiveInsight"
      ( \x ->
          ReactiveInsight'
            Prelude.<$> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SsmOpsItemId")
            Prelude.<*> (x Core..:? "InsightTimeRange")
      )

instance Prelude.Hashable ReactiveInsight where
  hashWithSalt _salt ReactiveInsight' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ssmOpsItemId
      `Prelude.hashWithSalt` insightTimeRange

instance Prelude.NFData ReactiveInsight where
  rnf ReactiveInsight' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ssmOpsItemId
      `Prelude.seq` Prelude.rnf insightTimeRange
