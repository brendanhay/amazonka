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
-- Module      : Amazonka.GuardDuty.Types.MemberDataSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.MemberDataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.MemberFeaturesConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | Contains information on which data sources are enabled for a member
-- account.
--
-- /See:/ 'newMemberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { -- | Contains information on the status of data sources for the account.
    dataSources :: Prelude.Maybe DataSourceConfigurationsResult,
    -- | Contains information about the status of the features for the member
    -- account.
    features :: Prelude.Maybe [MemberFeaturesConfigurationResult],
    -- | The account ID for the member account.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberDataSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'memberDataSourceConfiguration_dataSources' - Contains information on the status of data sources for the account.
--
-- 'features', 'memberDataSourceConfiguration_features' - Contains information about the status of the features for the member
-- account.
--
-- 'accountId', 'memberDataSourceConfiguration_accountId' - The account ID for the member account.
newMemberDataSourceConfiguration ::
  -- | 'accountId'
  Prelude.Text ->
  MemberDataSourceConfiguration
newMemberDataSourceConfiguration pAccountId_ =
  MemberDataSourceConfiguration'
    { dataSources =
        Prelude.Nothing,
      features = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | Contains information on the status of data sources for the account.
memberDataSourceConfiguration_dataSources :: Lens.Lens' MemberDataSourceConfiguration (Prelude.Maybe DataSourceConfigurationsResult)
memberDataSourceConfiguration_dataSources = Lens.lens (\MemberDataSourceConfiguration' {dataSources} -> dataSources) (\s@MemberDataSourceConfiguration' {} a -> s {dataSources = a} :: MemberDataSourceConfiguration)

-- | Contains information about the status of the features for the member
-- account.
memberDataSourceConfiguration_features :: Lens.Lens' MemberDataSourceConfiguration (Prelude.Maybe [MemberFeaturesConfigurationResult])
memberDataSourceConfiguration_features = Lens.lens (\MemberDataSourceConfiguration' {features} -> features) (\s@MemberDataSourceConfiguration' {} a -> s {features = a} :: MemberDataSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The account ID for the member account.
memberDataSourceConfiguration_accountId :: Lens.Lens' MemberDataSourceConfiguration Prelude.Text
memberDataSourceConfiguration_accountId = Lens.lens (\MemberDataSourceConfiguration' {accountId} -> accountId) (\s@MemberDataSourceConfiguration' {} a -> s {accountId = a} :: MemberDataSourceConfiguration)

instance Data.FromJSON MemberDataSourceConfiguration where
  parseJSON =
    Data.withObject
      "MemberDataSourceConfiguration"
      ( \x ->
          MemberDataSourceConfiguration'
            Prelude.<$> (x Data..:? "dataSources")
            Prelude.<*> (x Data..:? "features" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "accountId")
      )

instance
  Prelude.Hashable
    MemberDataSourceConfiguration
  where
  hashWithSalt _salt MemberDataSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData MemberDataSourceConfiguration where
  rnf MemberDataSourceConfiguration' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf accountId
