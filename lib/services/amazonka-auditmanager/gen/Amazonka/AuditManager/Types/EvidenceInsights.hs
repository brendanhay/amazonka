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
-- Module      : Amazonka.AuditManager.Types.EvidenceInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.EvidenceInsights where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A breakdown of the latest compliance check status for the evidence in
-- your Audit Manager assessments.
--
-- /See:/ 'newEvidenceInsights' smart constructor.
data EvidenceInsights = EvidenceInsights'
  { -- | The number of compliance check evidence that Audit Manager classified as
    -- compliant. This includes evidence that was collected from Security Hub
    -- with a /Pass/ ruling, or collected from Config with a /Compliant/
    -- ruling.
    compliantEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of evidence that a compliance check ruling isn\'t available
    -- for. Evidence is inconclusive when the associated control uses Security
    -- Hub or Config as a data source but you didn\'t enable those services.
    -- This is also the case when a control uses a data source that doesn’t
    -- support compliance checks (for example, manual evidence, API calls, or
    -- CloudTrail).
    --
    -- If evidence has a compliance check status of /not applicable/ in the
    -- console, it\'s classified as /inconclusive/ in @EvidenceInsights@ data.
    inconclusiveEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of compliance check evidence that Audit Manager classified as
    -- non-compliant. This includes evidence that was collected from Security
    -- Hub with a /Fail/ ruling, or collected from Config with a
    -- /Non-compliant/ ruling.
    noncompliantEvidenceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvidenceInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliantEvidenceCount', 'evidenceInsights_compliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- compliant. This includes evidence that was collected from Security Hub
-- with a /Pass/ ruling, or collected from Config with a /Compliant/
-- ruling.
--
-- 'inconclusiveEvidenceCount', 'evidenceInsights_inconclusiveEvidenceCount' - The number of evidence that a compliance check ruling isn\'t available
-- for. Evidence is inconclusive when the associated control uses Security
-- Hub or Config as a data source but you didn\'t enable those services.
-- This is also the case when a control uses a data source that doesn’t
-- support compliance checks (for example, manual evidence, API calls, or
-- CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/ in the
-- console, it\'s classified as /inconclusive/ in @EvidenceInsights@ data.
--
-- 'noncompliantEvidenceCount', 'evidenceInsights_noncompliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- non-compliant. This includes evidence that was collected from Security
-- Hub with a /Fail/ ruling, or collected from Config with a
-- /Non-compliant/ ruling.
newEvidenceInsights ::
  EvidenceInsights
newEvidenceInsights =
  EvidenceInsights'
    { compliantEvidenceCount =
        Prelude.Nothing,
      inconclusiveEvidenceCount = Prelude.Nothing,
      noncompliantEvidenceCount = Prelude.Nothing
    }

-- | The number of compliance check evidence that Audit Manager classified as
-- compliant. This includes evidence that was collected from Security Hub
-- with a /Pass/ ruling, or collected from Config with a /Compliant/
-- ruling.
evidenceInsights_compliantEvidenceCount :: Lens.Lens' EvidenceInsights (Prelude.Maybe Prelude.Int)
evidenceInsights_compliantEvidenceCount = Lens.lens (\EvidenceInsights' {compliantEvidenceCount} -> compliantEvidenceCount) (\s@EvidenceInsights' {} a -> s {compliantEvidenceCount = a} :: EvidenceInsights)

-- | The number of evidence that a compliance check ruling isn\'t available
-- for. Evidence is inconclusive when the associated control uses Security
-- Hub or Config as a data source but you didn\'t enable those services.
-- This is also the case when a control uses a data source that doesn’t
-- support compliance checks (for example, manual evidence, API calls, or
-- CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/ in the
-- console, it\'s classified as /inconclusive/ in @EvidenceInsights@ data.
evidenceInsights_inconclusiveEvidenceCount :: Lens.Lens' EvidenceInsights (Prelude.Maybe Prelude.Int)
evidenceInsights_inconclusiveEvidenceCount = Lens.lens (\EvidenceInsights' {inconclusiveEvidenceCount} -> inconclusiveEvidenceCount) (\s@EvidenceInsights' {} a -> s {inconclusiveEvidenceCount = a} :: EvidenceInsights)

-- | The number of compliance check evidence that Audit Manager classified as
-- non-compliant. This includes evidence that was collected from Security
-- Hub with a /Fail/ ruling, or collected from Config with a
-- /Non-compliant/ ruling.
evidenceInsights_noncompliantEvidenceCount :: Lens.Lens' EvidenceInsights (Prelude.Maybe Prelude.Int)
evidenceInsights_noncompliantEvidenceCount = Lens.lens (\EvidenceInsights' {noncompliantEvidenceCount} -> noncompliantEvidenceCount) (\s@EvidenceInsights' {} a -> s {noncompliantEvidenceCount = a} :: EvidenceInsights)

instance Data.FromJSON EvidenceInsights where
  parseJSON =
    Data.withObject
      "EvidenceInsights"
      ( \x ->
          EvidenceInsights'
            Prelude.<$> (x Data..:? "compliantEvidenceCount")
            Prelude.<*> (x Data..:? "inconclusiveEvidenceCount")
            Prelude.<*> (x Data..:? "noncompliantEvidenceCount")
      )

instance Prelude.Hashable EvidenceInsights where
  hashWithSalt _salt EvidenceInsights' {..} =
    _salt `Prelude.hashWithSalt` compliantEvidenceCount
      `Prelude.hashWithSalt` inconclusiveEvidenceCount
      `Prelude.hashWithSalt` noncompliantEvidenceCount

instance Prelude.NFData EvidenceInsights where
  rnf EvidenceInsights' {..} =
    Prelude.rnf compliantEvidenceCount
      `Prelude.seq` Prelude.rnf inconclusiveEvidenceCount
      `Prelude.seq` Prelude.rnf noncompliantEvidenceCount
