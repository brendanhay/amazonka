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
-- Module      : Network.AWS.DevOpsGuru.Types.ReactiveInsight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.ReactiveInsight where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.InsightSeverity
import Network.AWS.DevOpsGuru.Types.InsightStatus
import Network.AWS.DevOpsGuru.Types.InsightTimeRange
import Network.AWS.DevOpsGuru.Types.ResourceCollection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a reactive insight. This object is returned by
-- @ListInsights@.
--
-- /See:/ 'newReactiveInsight' smart constructor.
data ReactiveInsight = ReactiveInsight'
  { -- | The status of a reactive insight.
    status :: Prelude.Maybe InsightStatus,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of a reactive insight.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The ID of the AWS System Manager OpsItem created for this insight. You
    -- must enable the creation of OpstItems insights before they are created
    -- for each insight.
    ssmOpsItemId :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of a reactive insight.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of a reactive insight.
    id :: Prelude.Maybe Prelude.Text
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
-- 'status', 'reactiveInsight_status' - The status of a reactive insight.
--
-- 'resourceCollection', 'reactiveInsight_resourceCollection' - Undocumented member.
--
-- 'severity', 'reactiveInsight_severity' - The severity of a reactive insight.
--
-- 'ssmOpsItemId', 'reactiveInsight_ssmOpsItemId' - The ID of the AWS System Manager OpsItem created for this insight. You
-- must enable the creation of OpstItems insights before they are created
-- for each insight.
--
-- 'insightTimeRange', 'reactiveInsight_insightTimeRange' - Undocumented member.
--
-- 'name', 'reactiveInsight_name' - The name of a reactive insight.
--
-- 'id', 'reactiveInsight_id' - The ID of a reactive insight.
newReactiveInsight ::
  ReactiveInsight
newReactiveInsight =
  ReactiveInsight'
    { status = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      ssmOpsItemId = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The status of a reactive insight.
reactiveInsight_status :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightStatus)
reactiveInsight_status = Lens.lens (\ReactiveInsight' {status} -> status) (\s@ReactiveInsight' {} a -> s {status = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_resourceCollection :: Lens.Lens' ReactiveInsight (Prelude.Maybe ResourceCollection)
reactiveInsight_resourceCollection = Lens.lens (\ReactiveInsight' {resourceCollection} -> resourceCollection) (\s@ReactiveInsight' {} a -> s {resourceCollection = a} :: ReactiveInsight)

-- | The severity of a reactive insight.
reactiveInsight_severity :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightSeverity)
reactiveInsight_severity = Lens.lens (\ReactiveInsight' {severity} -> severity) (\s@ReactiveInsight' {} a -> s {severity = a} :: ReactiveInsight)

-- | The ID of the AWS System Manager OpsItem created for this insight. You
-- must enable the creation of OpstItems insights before they are created
-- for each insight.
reactiveInsight_ssmOpsItemId :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_ssmOpsItemId = Lens.lens (\ReactiveInsight' {ssmOpsItemId} -> ssmOpsItemId) (\s@ReactiveInsight' {} a -> s {ssmOpsItemId = a} :: ReactiveInsight)

-- | Undocumented member.
reactiveInsight_insightTimeRange :: Lens.Lens' ReactiveInsight (Prelude.Maybe InsightTimeRange)
reactiveInsight_insightTimeRange = Lens.lens (\ReactiveInsight' {insightTimeRange} -> insightTimeRange) (\s@ReactiveInsight' {} a -> s {insightTimeRange = a} :: ReactiveInsight)

-- | The name of a reactive insight.
reactiveInsight_name :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_name = Lens.lens (\ReactiveInsight' {name} -> name) (\s@ReactiveInsight' {} a -> s {name = a} :: ReactiveInsight)

-- | The ID of a reactive insight.
reactiveInsight_id :: Lens.Lens' ReactiveInsight (Prelude.Maybe Prelude.Text)
reactiveInsight_id = Lens.lens (\ReactiveInsight' {id} -> id) (\s@ReactiveInsight' {} a -> s {id = a} :: ReactiveInsight)

instance Core.FromJSON ReactiveInsight where
  parseJSON =
    Core.withObject
      "ReactiveInsight"
      ( \x ->
          ReactiveInsight'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "SsmOpsItemId")
            Prelude.<*> (x Core..:? "InsightTimeRange")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ReactiveInsight

instance Prelude.NFData ReactiveInsight
