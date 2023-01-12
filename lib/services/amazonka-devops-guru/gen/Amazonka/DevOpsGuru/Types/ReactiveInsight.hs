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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveInsight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Describes the reactive insight.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of a reactive insight.
    id :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of a reactive insight.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the insight. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The ID of the Amazon Web Services System Manager OpsItem created for
    -- this insight. You must enable the creation of OpstItems insights before
    -- they are created for each insight.
    ssmOpsItemId :: Prelude.Maybe Prelude.Text,
    -- | The status of a reactive insight.
    status :: Prelude.Maybe InsightStatus
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
-- 'description', 'reactiveInsight_description' - Describes the reactive insight.
--
-- 'id', 'reactiveInsight_id' - The ID of a reactive insight.
--
-- 'insightTimeRange', 'reactiveInsight_insightTimeRange' - Undocumented member.
--
-- 'name', 'reactiveInsight_name' - The name of a reactive insight.
--
-- 'resourceCollection', 'reactiveInsight_resourceCollection' - Undocumented member.
--
-- 'severity', 'reactiveInsight_severity' - The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'ssmOpsItemId', 'reactiveInsight_ssmOpsItemId' - The ID of the Amazon Web Services System Manager OpsItem created for
-- this insight. You must enable the creation of OpstItems insights before
-- they are created for each insight.
--
-- 'status', 'reactiveInsight_status' - The status of a reactive insight.
newReactiveInsight ::
  ReactiveInsight
newReactiveInsight =
  ReactiveInsight'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      ssmOpsItemId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Describes the reactive insight.
reactiveInsight_description :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_description = Lens.lens (\ReactiveInsight' {description} -> description) (\s@ReactiveInsight' {} a -> s {description = a} :: ReactiveInsight)

-- | The ID of a reactive insight.
reactiveInsight_id :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_id = Lens.lens (\ReactiveInsight' {id} -> id) (\s@ReactiveInsight' {} a -> s {id = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_insightTimeRange :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightTimeRange)
reactiveInsight_insightTimeRange = Lens.lens (\ReactiveInsight' {insightTimeRange} -> insightTimeRange) (\s@ReactiveInsight' {} a -> s {insightTimeRange = a} :: ReactiveInsight)

-- | The name of a reactive insight.
reactiveInsight_name :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_name = Lens.lens (\ReactiveInsight' {name} -> name) (\s@ReactiveInsight' {} a -> s {name = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_resourceCollection :: Lens.Lens' ReactiveInsight (Prelude.Maybe ResourceCollection)
reactiveInsight_resourceCollection = Lens.lens (\ReactiveInsight' {resourceCollection} -> resourceCollection) (\s@ReactiveInsight' {} a -> s {resourceCollection = a} :: ReactiveInsight)

-- | The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveInsight_severity :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightSeverity)
reactiveInsight_severity = Lens.lens (\ReactiveInsight' {severity} -> severity) (\s@ReactiveInsight' {} a -> s {severity = a} :: ReactiveInsight)

-- | The ID of the Amazon Web Services System Manager OpsItem created for
-- this insight. You must enable the creation of OpstItems insights before
-- they are created for each insight.
reactiveInsight_ssmOpsItemId :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_ssmOpsItemId = Lens.lens (\ReactiveInsight' {ssmOpsItemId} -> ssmOpsItemId) (\s@ReactiveInsight' {} a -> s {ssmOpsItemId = a} :: ReactiveInsight)

-- | The status of a reactive insight.
reactiveInsight_status :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightStatus)
reactiveInsight_status = Lens.lens (\ReactiveInsight' {status} -> status) (\s@ReactiveInsight' {} a -> s {status = a} :: ReactiveInsight)

instance Data.FromJSON ReactiveInsight where
  parseJSON =
    Data.withObject
      "ReactiveInsight"
      ( \x ->
          ReactiveInsight'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InsightTimeRange")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "SsmOpsItemId")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ReactiveInsight where
  hashWithSalt _salt ReactiveInsight' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` insightTimeRange
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` ssmOpsItemId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReactiveInsight where
  rnf ReactiveInsight' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf insightTimeRange
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf ssmOpsItemId
      `Prelude.seq` Prelude.rnf status
