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
-- Module      : Amazonka.GuardDuty.Types.DataSourceFreeTrial
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourceFreeTrial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which data sources are enabled for the
-- GuardDuty member account.
--
-- /See:/ 'newDataSourceFreeTrial' smart constructor.
data DataSourceFreeTrial = DataSourceFreeTrial'
  { -- | A value that specifies the number of days left to use each enabled data
    -- source.
    freeTrialDaysRemaining :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceFreeTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeTrialDaysRemaining', 'dataSourceFreeTrial_freeTrialDaysRemaining' - A value that specifies the number of days left to use each enabled data
-- source.
newDataSourceFreeTrial ::
  DataSourceFreeTrial
newDataSourceFreeTrial =
  DataSourceFreeTrial'
    { freeTrialDaysRemaining =
        Prelude.Nothing
    }

-- | A value that specifies the number of days left to use each enabled data
-- source.
dataSourceFreeTrial_freeTrialDaysRemaining :: Lens.Lens' DataSourceFreeTrial (Prelude.Maybe Prelude.Int)
dataSourceFreeTrial_freeTrialDaysRemaining = Lens.lens (\DataSourceFreeTrial' {freeTrialDaysRemaining} -> freeTrialDaysRemaining) (\s@DataSourceFreeTrial' {} a -> s {freeTrialDaysRemaining = a} :: DataSourceFreeTrial)

instance Core.FromJSON DataSourceFreeTrial where
  parseJSON =
    Core.withObject
      "DataSourceFreeTrial"
      ( \x ->
          DataSourceFreeTrial'
            Prelude.<$> (x Core..:? "freeTrialDaysRemaining")
      )

instance Prelude.Hashable DataSourceFreeTrial where
  hashWithSalt _salt DataSourceFreeTrial' {..} =
    _salt `Prelude.hashWithSalt` freeTrialDaysRemaining

instance Prelude.NFData DataSourceFreeTrial where
  rnf DataSourceFreeTrial' {..} =
    Prelude.rnf freeTrialDaysRemaining
