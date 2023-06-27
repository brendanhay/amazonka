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
-- Module      : Amazonka.ImageBuilder.Types.AccountAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.AccountAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | Contains counts of vulnerability findings from image scans that run when
-- you create new Image Builder images, or build new versions of existing
-- images. The vulnerability counts are grouped by severity level. The
-- counts are aggregated across resources to create the final tally for the
-- account that owns them.
--
-- /See:/ 'newAccountAggregation' smart constructor.
data AccountAggregation = AccountAggregation'
  { -- | Identifies the account that owns the aggregated resource findings.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Counts by severity level for medium severity and higher level findings,
    -- plus a total for all of the findings.
    severityCounts :: Prelude.Maybe SeverityCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountAggregation_accountId' - Identifies the account that owns the aggregated resource findings.
--
-- 'severityCounts', 'accountAggregation_severityCounts' - Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings.
newAccountAggregation ::
  AccountAggregation
newAccountAggregation =
  AccountAggregation'
    { accountId = Prelude.Nothing,
      severityCounts = Prelude.Nothing
    }

-- | Identifies the account that owns the aggregated resource findings.
accountAggregation_accountId :: Lens.Lens' AccountAggregation (Prelude.Maybe Prelude.Text)
accountAggregation_accountId = Lens.lens (\AccountAggregation' {accountId} -> accountId) (\s@AccountAggregation' {} a -> s {accountId = a} :: AccountAggregation)

-- | Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings.
accountAggregation_severityCounts :: Lens.Lens' AccountAggregation (Prelude.Maybe SeverityCounts)
accountAggregation_severityCounts = Lens.lens (\AccountAggregation' {severityCounts} -> severityCounts) (\s@AccountAggregation' {} a -> s {severityCounts = a} :: AccountAggregation)

instance Data.FromJSON AccountAggregation where
  parseJSON =
    Data.withObject
      "AccountAggregation"
      ( \x ->
          AccountAggregation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "severityCounts")
      )

instance Prelude.Hashable AccountAggregation where
  hashWithSalt _salt AccountAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` severityCounts

instance Prelude.NFData AccountAggregation where
  rnf AccountAggregation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf severityCounts
