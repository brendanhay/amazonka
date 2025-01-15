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
-- Module      : Amazonka.AuditManager.Types.ControlInsightsMetadataByAssessmentItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlInsightsMetadataByAssessmentItem where

import Amazonka.AuditManager.Types.EvidenceInsights
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the latest analytics data for a specific control in a
-- specific active assessment.
--
-- Control insights are grouped by control domain, and ranked by the
-- highest total count of non-compliant evidence.
--
-- /See:/ 'newControlInsightsMetadataByAssessmentItem' smart constructor.
data ControlInsightsMetadataByAssessmentItem = ControlInsightsMetadataByAssessmentItem'
  { -- | The name of the control set that the assessment control belongs to.
    controlSetName :: Prelude.Maybe Prelude.Text,
    -- | A breakdown of the compliance check status for the evidence that’s
    -- associated with the assessment control.
    evidenceInsights :: Prelude.Maybe EvidenceInsights,
    -- | The unique identifier for the assessment control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time when the assessment control insights were last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The name of the assessment control.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlInsightsMetadataByAssessmentItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlSetName', 'controlInsightsMetadataByAssessmentItem_controlSetName' - The name of the control set that the assessment control belongs to.
--
-- 'evidenceInsights', 'controlInsightsMetadataByAssessmentItem_evidenceInsights' - A breakdown of the compliance check status for the evidence that’s
-- associated with the assessment control.
--
-- 'id', 'controlInsightsMetadataByAssessmentItem_id' - The unique identifier for the assessment control.
--
-- 'lastUpdated', 'controlInsightsMetadataByAssessmentItem_lastUpdated' - The time when the assessment control insights were last updated.
--
-- 'name', 'controlInsightsMetadataByAssessmentItem_name' - The name of the assessment control.
newControlInsightsMetadataByAssessmentItem ::
  ControlInsightsMetadataByAssessmentItem
newControlInsightsMetadataByAssessmentItem =
  ControlInsightsMetadataByAssessmentItem'
    { controlSetName =
        Prelude.Nothing,
      evidenceInsights = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The name of the control set that the assessment control belongs to.
controlInsightsMetadataByAssessmentItem_controlSetName :: Lens.Lens' ControlInsightsMetadataByAssessmentItem (Prelude.Maybe Prelude.Text)
controlInsightsMetadataByAssessmentItem_controlSetName = Lens.lens (\ControlInsightsMetadataByAssessmentItem' {controlSetName} -> controlSetName) (\s@ControlInsightsMetadataByAssessmentItem' {} a -> s {controlSetName = a} :: ControlInsightsMetadataByAssessmentItem)

-- | A breakdown of the compliance check status for the evidence that’s
-- associated with the assessment control.
controlInsightsMetadataByAssessmentItem_evidenceInsights :: Lens.Lens' ControlInsightsMetadataByAssessmentItem (Prelude.Maybe EvidenceInsights)
controlInsightsMetadataByAssessmentItem_evidenceInsights = Lens.lens (\ControlInsightsMetadataByAssessmentItem' {evidenceInsights} -> evidenceInsights) (\s@ControlInsightsMetadataByAssessmentItem' {} a -> s {evidenceInsights = a} :: ControlInsightsMetadataByAssessmentItem)

-- | The unique identifier for the assessment control.
controlInsightsMetadataByAssessmentItem_id :: Lens.Lens' ControlInsightsMetadataByAssessmentItem (Prelude.Maybe Prelude.Text)
controlInsightsMetadataByAssessmentItem_id = Lens.lens (\ControlInsightsMetadataByAssessmentItem' {id} -> id) (\s@ControlInsightsMetadataByAssessmentItem' {} a -> s {id = a} :: ControlInsightsMetadataByAssessmentItem)

-- | The time when the assessment control insights were last updated.
controlInsightsMetadataByAssessmentItem_lastUpdated :: Lens.Lens' ControlInsightsMetadataByAssessmentItem (Prelude.Maybe Prelude.UTCTime)
controlInsightsMetadataByAssessmentItem_lastUpdated = Lens.lens (\ControlInsightsMetadataByAssessmentItem' {lastUpdated} -> lastUpdated) (\s@ControlInsightsMetadataByAssessmentItem' {} a -> s {lastUpdated = a} :: ControlInsightsMetadataByAssessmentItem) Prelude.. Lens.mapping Data._Time

-- | The name of the assessment control.
controlInsightsMetadataByAssessmentItem_name :: Lens.Lens' ControlInsightsMetadataByAssessmentItem (Prelude.Maybe Prelude.Text)
controlInsightsMetadataByAssessmentItem_name = Lens.lens (\ControlInsightsMetadataByAssessmentItem' {name} -> name) (\s@ControlInsightsMetadataByAssessmentItem' {} a -> s {name = a} :: ControlInsightsMetadataByAssessmentItem)

instance
  Data.FromJSON
    ControlInsightsMetadataByAssessmentItem
  where
  parseJSON =
    Data.withObject
      "ControlInsightsMetadataByAssessmentItem"
      ( \x ->
          ControlInsightsMetadataByAssessmentItem'
            Prelude.<$> (x Data..:? "controlSetName")
            Prelude.<*> (x Data..:? "evidenceInsights")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    ControlInsightsMetadataByAssessmentItem
  where
  hashWithSalt
    _salt
    ControlInsightsMetadataByAssessmentItem' {..} =
      _salt
        `Prelude.hashWithSalt` controlSetName
        `Prelude.hashWithSalt` evidenceInsights
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastUpdated
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    ControlInsightsMetadataByAssessmentItem
  where
  rnf ControlInsightsMetadataByAssessmentItem' {..} =
    Prelude.rnf controlSetName `Prelude.seq`
      Prelude.rnf evidenceInsights `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastUpdated `Prelude.seq`
            Prelude.rnf name
