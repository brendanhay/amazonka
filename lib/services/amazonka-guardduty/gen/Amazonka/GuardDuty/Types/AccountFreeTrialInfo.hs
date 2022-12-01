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
-- Module      : Amazonka.GuardDuty.Types.AccountFreeTrialInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AccountFreeTrialInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.DataSourcesFreeTrial
import qualified Amazonka.Prelude as Prelude

-- | Provides details of the GuardDuty member account that uses a free trial
-- service.
--
-- /See:/ 'newAccountFreeTrialInfo' smart constructor.
data AccountFreeTrialInfo = AccountFreeTrialInfo'
  { -- | Describes the data source enabled for the GuardDuty member account.
    dataSources :: Prelude.Maybe DataSourcesFreeTrial,
    -- | The account identifier of the GuardDuty member account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountFreeTrialInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'accountFreeTrialInfo_dataSources' - Describes the data source enabled for the GuardDuty member account.
--
-- 'accountId', 'accountFreeTrialInfo_accountId' - The account identifier of the GuardDuty member account.
newAccountFreeTrialInfo ::
  AccountFreeTrialInfo
newAccountFreeTrialInfo =
  AccountFreeTrialInfo'
    { dataSources =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | Describes the data source enabled for the GuardDuty member account.
accountFreeTrialInfo_dataSources :: Lens.Lens' AccountFreeTrialInfo (Prelude.Maybe DataSourcesFreeTrial)
accountFreeTrialInfo_dataSources = Lens.lens (\AccountFreeTrialInfo' {dataSources} -> dataSources) (\s@AccountFreeTrialInfo' {} a -> s {dataSources = a} :: AccountFreeTrialInfo)

-- | The account identifier of the GuardDuty member account.
accountFreeTrialInfo_accountId :: Lens.Lens' AccountFreeTrialInfo (Prelude.Maybe Prelude.Text)
accountFreeTrialInfo_accountId = Lens.lens (\AccountFreeTrialInfo' {accountId} -> accountId) (\s@AccountFreeTrialInfo' {} a -> s {accountId = a} :: AccountFreeTrialInfo)

instance Core.FromJSON AccountFreeTrialInfo where
  parseJSON =
    Core.withObject
      "AccountFreeTrialInfo"
      ( \x ->
          AccountFreeTrialInfo'
            Prelude.<$> (x Core..:? "dataSources")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable AccountFreeTrialInfo where
  hashWithSalt _salt AccountFreeTrialInfo' {..} =
    _salt `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AccountFreeTrialInfo where
  rnf AccountFreeTrialInfo' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf accountId
