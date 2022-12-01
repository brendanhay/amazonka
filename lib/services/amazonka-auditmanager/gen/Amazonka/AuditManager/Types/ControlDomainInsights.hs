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
-- Module      : Amazonka.AuditManager.Types.ControlDomainInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlDomainInsights where

import Amazonka.AuditManager.Types.EvidenceInsights
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of the latest analytics data for a specific control domain.
--
-- Control domain insights are grouped by control domain, and ranked by the
-- highest total count of non-compliant evidence.
--
-- /See:/ 'newControlDomainInsights' smart constructor.
data ControlDomainInsights = ControlDomainInsights'
  { -- | A breakdown of the compliance check status for the evidence that’s
    -- associated with the control domain.
    evidenceInsights :: Prelude.Maybe EvidenceInsights,
    -- | The name of the control domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total number of controls in the control domain.
    totalControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the control domain.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time when the control domain insights were last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The number of controls in the control domain that collected
    -- non-compliant evidence on the @lastUpdated@ date.
    controlsCountByNoncompliantEvidence :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlDomainInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidenceInsights', 'controlDomainInsights_evidenceInsights' - A breakdown of the compliance check status for the evidence that’s
-- associated with the control domain.
--
-- 'name', 'controlDomainInsights_name' - The name of the control domain.
--
-- 'totalControlsCount', 'controlDomainInsights_totalControlsCount' - The total number of controls in the control domain.
--
-- 'id', 'controlDomainInsights_id' - The unique identifier for the control domain.
--
-- 'lastUpdated', 'controlDomainInsights_lastUpdated' - The time when the control domain insights were last updated.
--
-- 'controlsCountByNoncompliantEvidence', 'controlDomainInsights_controlsCountByNoncompliantEvidence' - The number of controls in the control domain that collected
-- non-compliant evidence on the @lastUpdated@ date.
newControlDomainInsights ::
  ControlDomainInsights
newControlDomainInsights =
  ControlDomainInsights'
    { evidenceInsights =
        Prelude.Nothing,
      name = Prelude.Nothing,
      totalControlsCount = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      controlsCountByNoncompliantEvidence =
        Prelude.Nothing
    }

-- | A breakdown of the compliance check status for the evidence that’s
-- associated with the control domain.
controlDomainInsights_evidenceInsights :: Lens.Lens' ControlDomainInsights (Prelude.Maybe EvidenceInsights)
controlDomainInsights_evidenceInsights = Lens.lens (\ControlDomainInsights' {evidenceInsights} -> evidenceInsights) (\s@ControlDomainInsights' {} a -> s {evidenceInsights = a} :: ControlDomainInsights)

-- | The name of the control domain.
controlDomainInsights_name :: Lens.Lens' ControlDomainInsights (Prelude.Maybe Prelude.Text)
controlDomainInsights_name = Lens.lens (\ControlDomainInsights' {name} -> name) (\s@ControlDomainInsights' {} a -> s {name = a} :: ControlDomainInsights)

-- | The total number of controls in the control domain.
controlDomainInsights_totalControlsCount :: Lens.Lens' ControlDomainInsights (Prelude.Maybe Prelude.Int)
controlDomainInsights_totalControlsCount = Lens.lens (\ControlDomainInsights' {totalControlsCount} -> totalControlsCount) (\s@ControlDomainInsights' {} a -> s {totalControlsCount = a} :: ControlDomainInsights)

-- | The unique identifier for the control domain.
controlDomainInsights_id :: Lens.Lens' ControlDomainInsights (Prelude.Maybe Prelude.Text)
controlDomainInsights_id = Lens.lens (\ControlDomainInsights' {id} -> id) (\s@ControlDomainInsights' {} a -> s {id = a} :: ControlDomainInsights)

-- | The time when the control domain insights were last updated.
controlDomainInsights_lastUpdated :: Lens.Lens' ControlDomainInsights (Prelude.Maybe Prelude.UTCTime)
controlDomainInsights_lastUpdated = Lens.lens (\ControlDomainInsights' {lastUpdated} -> lastUpdated) (\s@ControlDomainInsights' {} a -> s {lastUpdated = a} :: ControlDomainInsights) Prelude.. Lens.mapping Core._Time

-- | The number of controls in the control domain that collected
-- non-compliant evidence on the @lastUpdated@ date.
controlDomainInsights_controlsCountByNoncompliantEvidence :: Lens.Lens' ControlDomainInsights (Prelude.Maybe Prelude.Int)
controlDomainInsights_controlsCountByNoncompliantEvidence = Lens.lens (\ControlDomainInsights' {controlsCountByNoncompliantEvidence} -> controlsCountByNoncompliantEvidence) (\s@ControlDomainInsights' {} a -> s {controlsCountByNoncompliantEvidence = a} :: ControlDomainInsights)

instance Core.FromJSON ControlDomainInsights where
  parseJSON =
    Core.withObject
      "ControlDomainInsights"
      ( \x ->
          ControlDomainInsights'
            Prelude.<$> (x Core..:? "evidenceInsights")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "totalControlsCount")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "controlsCountByNoncompliantEvidence")
      )

instance Prelude.Hashable ControlDomainInsights where
  hashWithSalt _salt ControlDomainInsights' {..} =
    _salt `Prelude.hashWithSalt` evidenceInsights
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` totalControlsCount
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` controlsCountByNoncompliantEvidence

instance Prelude.NFData ControlDomainInsights where
  rnf ControlDomainInsights' {..} =
    Prelude.rnf evidenceInsights
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf totalControlsCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf controlsCountByNoncompliantEvidence
