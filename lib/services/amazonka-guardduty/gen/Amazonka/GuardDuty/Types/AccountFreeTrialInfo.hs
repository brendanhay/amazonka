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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AccountFreeTrialInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourcesFreeTrial
import Amazonka.GuardDuty.Types.FreeTrialFeatureConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | Provides details of the GuardDuty member account that uses a free trial
-- service.
--
-- /See:/ 'newAccountFreeTrialInfo' smart constructor.
data AccountFreeTrialInfo = AccountFreeTrialInfo'
  { -- | The account identifier of the GuardDuty member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Describes the data source enabled for the GuardDuty member account.
    dataSources :: Prelude.Maybe DataSourcesFreeTrial,
    -- | A list of features enabled for the GuardDuty account.
    features :: Prelude.Maybe [FreeTrialFeatureConfigurationResult]
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
-- 'accountId', 'accountFreeTrialInfo_accountId' - The account identifier of the GuardDuty member account.
--
-- 'dataSources', 'accountFreeTrialInfo_dataSources' - Describes the data source enabled for the GuardDuty member account.
--
-- 'features', 'accountFreeTrialInfo_features' - A list of features enabled for the GuardDuty account.
newAccountFreeTrialInfo ::
  AccountFreeTrialInfo
newAccountFreeTrialInfo =
  AccountFreeTrialInfo'
    { accountId = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      features = Prelude.Nothing
    }

-- | The account identifier of the GuardDuty member account.
accountFreeTrialInfo_accountId :: Lens.Lens' AccountFreeTrialInfo (Prelude.Maybe Prelude.Text)
accountFreeTrialInfo_accountId = Lens.lens (\AccountFreeTrialInfo' {accountId} -> accountId) (\s@AccountFreeTrialInfo' {} a -> s {accountId = a} :: AccountFreeTrialInfo)

-- | Describes the data source enabled for the GuardDuty member account.
accountFreeTrialInfo_dataSources :: Lens.Lens' AccountFreeTrialInfo (Prelude.Maybe DataSourcesFreeTrial)
accountFreeTrialInfo_dataSources = Lens.lens (\AccountFreeTrialInfo' {dataSources} -> dataSources) (\s@AccountFreeTrialInfo' {} a -> s {dataSources = a} :: AccountFreeTrialInfo)

-- | A list of features enabled for the GuardDuty account.
accountFreeTrialInfo_features :: Lens.Lens' AccountFreeTrialInfo (Prelude.Maybe [FreeTrialFeatureConfigurationResult])
accountFreeTrialInfo_features = Lens.lens (\AccountFreeTrialInfo' {features} -> features) (\s@AccountFreeTrialInfo' {} a -> s {features = a} :: AccountFreeTrialInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AccountFreeTrialInfo where
  parseJSON =
    Data.withObject
      "AccountFreeTrialInfo"
      ( \x ->
          AccountFreeTrialInfo'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "dataSources")
            Prelude.<*> (x Data..:? "features" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AccountFreeTrialInfo where
  hashWithSalt _salt AccountFreeTrialInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` features

instance Prelude.NFData AccountFreeTrialInfo where
  rnf AccountFreeTrialInfo' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf features
