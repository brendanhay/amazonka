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
-- Module      : Amazonka.Detective.Types.MembershipDatasources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.MembershipDatasources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types.DatasourcePackage
import Amazonka.Detective.Types.DatasourcePackageIngestState
import Amazonka.Detective.Types.TimestampForCollection
import qualified Amazonka.Prelude as Prelude

-- | Details on data source packages for members of the behavior graph.
--
-- /See:/ 'newMembershipDatasources' smart constructor.
data MembershipDatasources = MembershipDatasources'
  { -- | Details on when a data source package was added to a behavior graph.
    datasourcePackageIngestHistory :: Prelude.Maybe (Prelude.HashMap DatasourcePackage (Prelude.HashMap DatasourcePackageIngestState TimestampForCollection)),
    -- | The ARN of the organization behavior graph.
    graphArn :: Prelude.Maybe Prelude.Text,
    -- | The account identifier of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MembershipDatasources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasourcePackageIngestHistory', 'membershipDatasources_datasourcePackageIngestHistory' - Details on when a data source package was added to a behavior graph.
--
-- 'graphArn', 'membershipDatasources_graphArn' - The ARN of the organization behavior graph.
--
-- 'accountId', 'membershipDatasources_accountId' - The account identifier of the Amazon Web Services account.
newMembershipDatasources ::
  MembershipDatasources
newMembershipDatasources =
  MembershipDatasources'
    { datasourcePackageIngestHistory =
        Prelude.Nothing,
      graphArn = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | Details on when a data source package was added to a behavior graph.
membershipDatasources_datasourcePackageIngestHistory :: Lens.Lens' MembershipDatasources (Prelude.Maybe (Prelude.HashMap DatasourcePackage (Prelude.HashMap DatasourcePackageIngestState TimestampForCollection)))
membershipDatasources_datasourcePackageIngestHistory = Lens.lens (\MembershipDatasources' {datasourcePackageIngestHistory} -> datasourcePackageIngestHistory) (\s@MembershipDatasources' {} a -> s {datasourcePackageIngestHistory = a} :: MembershipDatasources) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the organization behavior graph.
membershipDatasources_graphArn :: Lens.Lens' MembershipDatasources (Prelude.Maybe Prelude.Text)
membershipDatasources_graphArn = Lens.lens (\MembershipDatasources' {graphArn} -> graphArn) (\s@MembershipDatasources' {} a -> s {graphArn = a} :: MembershipDatasources)

-- | The account identifier of the Amazon Web Services account.
membershipDatasources_accountId :: Lens.Lens' MembershipDatasources (Prelude.Maybe Prelude.Text)
membershipDatasources_accountId = Lens.lens (\MembershipDatasources' {accountId} -> accountId) (\s@MembershipDatasources' {} a -> s {accountId = a} :: MembershipDatasources)

instance Core.FromJSON MembershipDatasources where
  parseJSON =
    Core.withObject
      "MembershipDatasources"
      ( \x ->
          MembershipDatasources'
            Prelude.<$> ( x Core..:? "DatasourcePackageIngestHistory"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "GraphArn")
            Prelude.<*> (x Core..:? "AccountId")
      )

instance Prelude.Hashable MembershipDatasources where
  hashWithSalt _salt MembershipDatasources' {..} =
    _salt
      `Prelude.hashWithSalt` datasourcePackageIngestHistory
      `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData MembershipDatasources where
  rnf MembershipDatasources' {..} =
    Prelude.rnf datasourcePackageIngestHistory
      `Prelude.seq` Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf accountId
