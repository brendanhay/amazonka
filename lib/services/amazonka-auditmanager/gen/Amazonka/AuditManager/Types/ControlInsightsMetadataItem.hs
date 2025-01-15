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
-- Module      : Amazonka.AuditManager.Types.ControlInsightsMetadataItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlInsightsMetadataItem where

import Amazonka.AuditManager.Types.EvidenceInsights
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the latest analytics data for a specific control.
--
-- This data reflects the total counts for the specified control across all
-- active assessments. Control insights are grouped by control domain, and
-- ranked by the highest total count of non-compliant evidence.
--
-- /See:/ 'newControlInsightsMetadataItem' smart constructor.
data ControlInsightsMetadataItem = ControlInsightsMetadataItem'
  { -- | A breakdown of the compliance check status for the evidence that’s
    -- associated with the control.
    evidenceInsights :: Prelude.Maybe EvidenceInsights,
    -- | The unique identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time when the control insights were last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The name of the control.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlInsightsMetadataItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidenceInsights', 'controlInsightsMetadataItem_evidenceInsights' - A breakdown of the compliance check status for the evidence that’s
-- associated with the control.
--
-- 'id', 'controlInsightsMetadataItem_id' - The unique identifier for the control.
--
-- 'lastUpdated', 'controlInsightsMetadataItem_lastUpdated' - The time when the control insights were last updated.
--
-- 'name', 'controlInsightsMetadataItem_name' - The name of the control.
newControlInsightsMetadataItem ::
  ControlInsightsMetadataItem
newControlInsightsMetadataItem =
  ControlInsightsMetadataItem'
    { evidenceInsights =
        Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A breakdown of the compliance check status for the evidence that’s
-- associated with the control.
controlInsightsMetadataItem_evidenceInsights :: Lens.Lens' ControlInsightsMetadataItem (Prelude.Maybe EvidenceInsights)
controlInsightsMetadataItem_evidenceInsights = Lens.lens (\ControlInsightsMetadataItem' {evidenceInsights} -> evidenceInsights) (\s@ControlInsightsMetadataItem' {} a -> s {evidenceInsights = a} :: ControlInsightsMetadataItem)

-- | The unique identifier for the control.
controlInsightsMetadataItem_id :: Lens.Lens' ControlInsightsMetadataItem (Prelude.Maybe Prelude.Text)
controlInsightsMetadataItem_id = Lens.lens (\ControlInsightsMetadataItem' {id} -> id) (\s@ControlInsightsMetadataItem' {} a -> s {id = a} :: ControlInsightsMetadataItem)

-- | The time when the control insights were last updated.
controlInsightsMetadataItem_lastUpdated :: Lens.Lens' ControlInsightsMetadataItem (Prelude.Maybe Prelude.UTCTime)
controlInsightsMetadataItem_lastUpdated = Lens.lens (\ControlInsightsMetadataItem' {lastUpdated} -> lastUpdated) (\s@ControlInsightsMetadataItem' {} a -> s {lastUpdated = a} :: ControlInsightsMetadataItem) Prelude.. Lens.mapping Data._Time

-- | The name of the control.
controlInsightsMetadataItem_name :: Lens.Lens' ControlInsightsMetadataItem (Prelude.Maybe Prelude.Text)
controlInsightsMetadataItem_name = Lens.lens (\ControlInsightsMetadataItem' {name} -> name) (\s@ControlInsightsMetadataItem' {} a -> s {name = a} :: ControlInsightsMetadataItem)

instance Data.FromJSON ControlInsightsMetadataItem where
  parseJSON =
    Data.withObject
      "ControlInsightsMetadataItem"
      ( \x ->
          ControlInsightsMetadataItem'
            Prelude.<$> (x Data..:? "evidenceInsights")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ControlInsightsMetadataItem where
  hashWithSalt _salt ControlInsightsMetadataItem' {..} =
    _salt
      `Prelude.hashWithSalt` evidenceInsights
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` name

instance Prelude.NFData ControlInsightsMetadataItem where
  rnf ControlInsightsMetadataItem' {..} =
    Prelude.rnf evidenceInsights `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf lastUpdated `Prelude.seq`
          Prelude.rnf name
