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
-- Module      : Amazonka.FinSpace.Types.KxDatabaseConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxDatabaseConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.KxDatabaseCacheConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration of data that is available for querying from this
-- database.
--
-- /See:/ 'newKxDatabaseConfiguration' smart constructor.
data KxDatabaseConfiguration = KxDatabaseConfiguration'
  { -- | Configuration details for the disk cache used to increase performance
    -- reading from a kdb database mounted to the cluster.
    cacheConfigurations :: Prelude.Maybe [KxDatabaseCacheConfiguration],
    -- | A unique identifier of the changeset that is associated with the
    -- cluster.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb database. When this parameter is specified in the
    -- structure, S3 with the whole database is included by default.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxDatabaseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheConfigurations', 'kxDatabaseConfiguration_cacheConfigurations' - Configuration details for the disk cache used to increase performance
-- reading from a kdb database mounted to the cluster.
--
-- 'changesetId', 'kxDatabaseConfiguration_changesetId' - A unique identifier of the changeset that is associated with the
-- cluster.
--
-- 'databaseName', 'kxDatabaseConfiguration_databaseName' - The name of the kdb database. When this parameter is specified in the
-- structure, S3 with the whole database is included by default.
newKxDatabaseConfiguration ::
  -- | 'databaseName'
  Prelude.Text ->
  KxDatabaseConfiguration
newKxDatabaseConfiguration pDatabaseName_ =
  KxDatabaseConfiguration'
    { cacheConfigurations =
        Prelude.Nothing,
      changesetId = Prelude.Nothing,
      databaseName = pDatabaseName_
    }

-- | Configuration details for the disk cache used to increase performance
-- reading from a kdb database mounted to the cluster.
kxDatabaseConfiguration_cacheConfigurations :: Lens.Lens' KxDatabaseConfiguration (Prelude.Maybe [KxDatabaseCacheConfiguration])
kxDatabaseConfiguration_cacheConfigurations = Lens.lens (\KxDatabaseConfiguration' {cacheConfigurations} -> cacheConfigurations) (\s@KxDatabaseConfiguration' {} a -> s {cacheConfigurations = a} :: KxDatabaseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier of the changeset that is associated with the
-- cluster.
kxDatabaseConfiguration_changesetId :: Lens.Lens' KxDatabaseConfiguration (Prelude.Maybe Prelude.Text)
kxDatabaseConfiguration_changesetId = Lens.lens (\KxDatabaseConfiguration' {changesetId} -> changesetId) (\s@KxDatabaseConfiguration' {} a -> s {changesetId = a} :: KxDatabaseConfiguration)

-- | The name of the kdb database. When this parameter is specified in the
-- structure, S3 with the whole database is included by default.
kxDatabaseConfiguration_databaseName :: Lens.Lens' KxDatabaseConfiguration Prelude.Text
kxDatabaseConfiguration_databaseName = Lens.lens (\KxDatabaseConfiguration' {databaseName} -> databaseName) (\s@KxDatabaseConfiguration' {} a -> s {databaseName = a} :: KxDatabaseConfiguration)

instance Data.FromJSON KxDatabaseConfiguration where
  parseJSON =
    Data.withObject
      "KxDatabaseConfiguration"
      ( \x ->
          KxDatabaseConfiguration'
            Prelude.<$> ( x
                            Data..:? "cacheConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "changesetId")
            Prelude.<*> (x Data..: "databaseName")
      )

instance Prelude.Hashable KxDatabaseConfiguration where
  hashWithSalt _salt KxDatabaseConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cacheConfigurations
      `Prelude.hashWithSalt` changesetId
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData KxDatabaseConfiguration where
  rnf KxDatabaseConfiguration' {..} =
    Prelude.rnf cacheConfigurations
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToJSON KxDatabaseConfiguration where
  toJSON KxDatabaseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cacheConfigurations" Data..=)
              Prelude.<$> cacheConfigurations,
            ("changesetId" Data..=) Prelude.<$> changesetId,
            Prelude.Just ("databaseName" Data..= databaseName)
          ]
      )
