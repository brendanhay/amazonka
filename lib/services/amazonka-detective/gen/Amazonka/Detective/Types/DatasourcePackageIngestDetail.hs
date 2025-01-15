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
-- Module      : Amazonka.Detective.Types.DatasourcePackageIngestDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.DatasourcePackageIngestDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types.DatasourcePackageIngestState
import Amazonka.Detective.Types.TimestampForCollection
import qualified Amazonka.Prelude as Prelude

-- | Details about the data source packages ingested by your behavior graph.
--
-- /See:/ 'newDatasourcePackageIngestDetail' smart constructor.
data DatasourcePackageIngestDetail = DatasourcePackageIngestDetail'
  { -- | Details on which data source packages are ingested for a member account.
    datasourcePackageIngestState :: Prelude.Maybe DatasourcePackageIngestState,
    -- | The date a data source package was enabled for this account
    lastIngestStateChange :: Prelude.Maybe (Prelude.HashMap DatasourcePackageIngestState TimestampForCollection)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasourcePackageIngestDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasourcePackageIngestState', 'datasourcePackageIngestDetail_datasourcePackageIngestState' - Details on which data source packages are ingested for a member account.
--
-- 'lastIngestStateChange', 'datasourcePackageIngestDetail_lastIngestStateChange' - The date a data source package was enabled for this account
newDatasourcePackageIngestDetail ::
  DatasourcePackageIngestDetail
newDatasourcePackageIngestDetail =
  DatasourcePackageIngestDetail'
    { datasourcePackageIngestState =
        Prelude.Nothing,
      lastIngestStateChange = Prelude.Nothing
    }

-- | Details on which data source packages are ingested for a member account.
datasourcePackageIngestDetail_datasourcePackageIngestState :: Lens.Lens' DatasourcePackageIngestDetail (Prelude.Maybe DatasourcePackageIngestState)
datasourcePackageIngestDetail_datasourcePackageIngestState = Lens.lens (\DatasourcePackageIngestDetail' {datasourcePackageIngestState} -> datasourcePackageIngestState) (\s@DatasourcePackageIngestDetail' {} a -> s {datasourcePackageIngestState = a} :: DatasourcePackageIngestDetail)

-- | The date a data source package was enabled for this account
datasourcePackageIngestDetail_lastIngestStateChange :: Lens.Lens' DatasourcePackageIngestDetail (Prelude.Maybe (Prelude.HashMap DatasourcePackageIngestState TimestampForCollection))
datasourcePackageIngestDetail_lastIngestStateChange = Lens.lens (\DatasourcePackageIngestDetail' {lastIngestStateChange} -> lastIngestStateChange) (\s@DatasourcePackageIngestDetail' {} a -> s {lastIngestStateChange = a} :: DatasourcePackageIngestDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DatasourcePackageIngestDetail where
  parseJSON =
    Data.withObject
      "DatasourcePackageIngestDetail"
      ( \x ->
          DatasourcePackageIngestDetail'
            Prelude.<$> (x Data..:? "DatasourcePackageIngestState")
            Prelude.<*> ( x
                            Data..:? "LastIngestStateChange"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DatasourcePackageIngestDetail
  where
  hashWithSalt _salt DatasourcePackageIngestDetail' {..} =
    _salt
      `Prelude.hashWithSalt` datasourcePackageIngestState
      `Prelude.hashWithSalt` lastIngestStateChange

instance Prelude.NFData DatasourcePackageIngestDetail where
  rnf DatasourcePackageIngestDetail' {..} =
    Prelude.rnf datasourcePackageIngestState `Prelude.seq`
      Prelude.rnf lastIngestStateChange
