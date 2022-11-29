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
-- Module      : Amazonka.AccessAnalyzer.Types.ArchiveRuleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.ArchiveRuleSummary where

import Amazonka.AccessAnalyzer.Types.Criterion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an archive rule.
--
-- /See:/ 'newArchiveRuleSummary' smart constructor.
data ArchiveRuleSummary = ArchiveRuleSummary'
  { -- | The name of the archive rule.
    ruleName :: Prelude.Text,
    -- | A filter used to define the archive rule.
    filter' :: Prelude.HashMap Prelude.Text Criterion,
    -- | The time at which the archive rule was created.
    createdAt :: Core.POSIX,
    -- | The time at which the archive rule was last updated.
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveRuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'archiveRuleSummary_ruleName' - The name of the archive rule.
--
-- 'filter'', 'archiveRuleSummary_filter' - A filter used to define the archive rule.
--
-- 'createdAt', 'archiveRuleSummary_createdAt' - The time at which the archive rule was created.
--
-- 'updatedAt', 'archiveRuleSummary_updatedAt' - The time at which the archive rule was last updated.
newArchiveRuleSummary ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  ArchiveRuleSummary
newArchiveRuleSummary
  pRuleName_
  pCreatedAt_
  pUpdatedAt_ =
    ArchiveRuleSummary'
      { ruleName = pRuleName_,
        filter' = Prelude.mempty,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | The name of the archive rule.
archiveRuleSummary_ruleName :: Lens.Lens' ArchiveRuleSummary Prelude.Text
archiveRuleSummary_ruleName = Lens.lens (\ArchiveRuleSummary' {ruleName} -> ruleName) (\s@ArchiveRuleSummary' {} a -> s {ruleName = a} :: ArchiveRuleSummary)

-- | A filter used to define the archive rule.
archiveRuleSummary_filter :: Lens.Lens' ArchiveRuleSummary (Prelude.HashMap Prelude.Text Criterion)
archiveRuleSummary_filter = Lens.lens (\ArchiveRuleSummary' {filter'} -> filter') (\s@ArchiveRuleSummary' {} a -> s {filter' = a} :: ArchiveRuleSummary) Prelude.. Lens.coerced

-- | The time at which the archive rule was created.
archiveRuleSummary_createdAt :: Lens.Lens' ArchiveRuleSummary Prelude.UTCTime
archiveRuleSummary_createdAt = Lens.lens (\ArchiveRuleSummary' {createdAt} -> createdAt) (\s@ArchiveRuleSummary' {} a -> s {createdAt = a} :: ArchiveRuleSummary) Prelude.. Core._Time

-- | The time at which the archive rule was last updated.
archiveRuleSummary_updatedAt :: Lens.Lens' ArchiveRuleSummary Prelude.UTCTime
archiveRuleSummary_updatedAt = Lens.lens (\ArchiveRuleSummary' {updatedAt} -> updatedAt) (\s@ArchiveRuleSummary' {} a -> s {updatedAt = a} :: ArchiveRuleSummary) Prelude.. Core._Time

instance Core.FromJSON ArchiveRuleSummary where
  parseJSON =
    Core.withObject
      "ArchiveRuleSummary"
      ( \x ->
          ArchiveRuleSummary'
            Prelude.<$> (x Core..: "ruleName")
            Prelude.<*> (x Core..:? "filter" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "updatedAt")
      )

instance Prelude.Hashable ArchiveRuleSummary where
  hashWithSalt _salt ArchiveRuleSummary' {..} =
    _salt `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ArchiveRuleSummary where
  rnf ArchiveRuleSummary' {..} =
    Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
