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
import qualified Amazonka.Prelude as Prelude

-- | Contains information on which data sources are enabled for a member
-- account.
--
-- /See:/ 'newMemberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { -- | The account ID for the member account.
    accountId :: Prelude.Text,
    -- | Contains information on the status of data sources for the account.
    dataSources :: DataSourceConfigurationsResult
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
-- 'accountId', 'memberDataSourceConfiguration_accountId' - The account ID for the member account.
--
-- 'dataSources', 'memberDataSourceConfiguration_dataSources' - Contains information on the status of data sources for the account.
newMemberDataSourceConfiguration ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'dataSources'
  DataSourceConfigurationsResult ->
  MemberDataSourceConfiguration
newMemberDataSourceConfiguration
  pAccountId_
  pDataSources_ =
    MemberDataSourceConfiguration'
      { accountId =
          pAccountId_,
        dataSources = pDataSources_
      }

-- | The account ID for the member account.
memberDataSourceConfiguration_accountId :: Lens.Lens' MemberDataSourceConfiguration Prelude.Text
memberDataSourceConfiguration_accountId = Lens.lens (\MemberDataSourceConfiguration' {accountId} -> accountId) (\s@MemberDataSourceConfiguration' {} a -> s {accountId = a} :: MemberDataSourceConfiguration)

-- | Contains information on the status of data sources for the account.
memberDataSourceConfiguration_dataSources :: Lens.Lens' MemberDataSourceConfiguration DataSourceConfigurationsResult
memberDataSourceConfiguration_dataSources = Lens.lens (\MemberDataSourceConfiguration' {dataSources} -> dataSources) (\s@MemberDataSourceConfiguration' {} a -> s {dataSources = a} :: MemberDataSourceConfiguration)

instance Data.FromJSON MemberDataSourceConfiguration where
  parseJSON =
    Data.withObject
      "MemberDataSourceConfiguration"
      ( \x ->
          MemberDataSourceConfiguration'
            Prelude.<$> (x Data..: "accountId")
            Prelude.<*> (x Data..: "dataSources")
      )

instance
  Prelude.Hashable
    MemberDataSourceConfiguration
  where
  hashWithSalt _salt MemberDataSourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` dataSources

instance Prelude.NFData MemberDataSourceConfiguration where
  rnf MemberDataSourceConfiguration' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf dataSources
