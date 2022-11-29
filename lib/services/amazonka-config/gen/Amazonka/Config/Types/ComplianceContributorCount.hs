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
-- Module      : Amazonka.Config.Types.ComplianceContributorCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ComplianceContributorCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The number of Amazon Web Services resources or Config rules responsible
-- for the current compliance of the item, up to a maximum number.
--
-- /See:/ 'newComplianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
  { -- | The number of Amazon Web Services resources or Config rules responsible
    -- for the current compliance of the item.
    cappedCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the maximum count is reached.
    capExceeded :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceContributorCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cappedCount', 'complianceContributorCount_cappedCount' - The number of Amazon Web Services resources or Config rules responsible
-- for the current compliance of the item.
--
-- 'capExceeded', 'complianceContributorCount_capExceeded' - Indicates whether the maximum count is reached.
newComplianceContributorCount ::
  ComplianceContributorCount
newComplianceContributorCount =
  ComplianceContributorCount'
    { cappedCount =
        Prelude.Nothing,
      capExceeded = Prelude.Nothing
    }

-- | The number of Amazon Web Services resources or Config rules responsible
-- for the current compliance of the item.
complianceContributorCount_cappedCount :: Lens.Lens' ComplianceContributorCount (Prelude.Maybe Prelude.Int)
complianceContributorCount_cappedCount = Lens.lens (\ComplianceContributorCount' {cappedCount} -> cappedCount) (\s@ComplianceContributorCount' {} a -> s {cappedCount = a} :: ComplianceContributorCount)

-- | Indicates whether the maximum count is reached.
complianceContributorCount_capExceeded :: Lens.Lens' ComplianceContributorCount (Prelude.Maybe Prelude.Bool)
complianceContributorCount_capExceeded = Lens.lens (\ComplianceContributorCount' {capExceeded} -> capExceeded) (\s@ComplianceContributorCount' {} a -> s {capExceeded = a} :: ComplianceContributorCount)

instance Core.FromJSON ComplianceContributorCount where
  parseJSON =
    Core.withObject
      "ComplianceContributorCount"
      ( \x ->
          ComplianceContributorCount'
            Prelude.<$> (x Core..:? "CappedCount")
            Prelude.<*> (x Core..:? "CapExceeded")
      )

instance Prelude.Hashable ComplianceContributorCount where
  hashWithSalt _salt ComplianceContributorCount' {..} =
    _salt `Prelude.hashWithSalt` cappedCount
      `Prelude.hashWithSalt` capExceeded

instance Prelude.NFData ComplianceContributorCount where
  rnf ComplianceContributorCount' {..} =
    Prelude.rnf cappedCount
      `Prelude.seq` Prelude.rnf capExceeded
