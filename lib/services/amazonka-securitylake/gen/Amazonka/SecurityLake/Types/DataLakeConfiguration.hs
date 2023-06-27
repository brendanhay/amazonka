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
-- Module      : Amazonka.SecurityLake.Types.DataLakeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.DataLakeEncryptionConfiguration
import Amazonka.SecurityLake.Types.DataLakeLifecycleConfiguration
import Amazonka.SecurityLake.Types.DataLakeReplicationConfiguration

-- | Provides details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeConfiguration' smart constructor.
data DataLakeConfiguration = DataLakeConfiguration'
  { -- | Provides encryption details of Amazon Security Lake object.
    encryptionConfiguration :: Prelude.Maybe DataLakeEncryptionConfiguration,
    -- | Provides lifecycle details of Amazon Security Lake object.
    lifecycleConfiguration :: Prelude.Maybe DataLakeLifecycleConfiguration,
    -- | Provides replication details of Amazon Security Lake object.
    replicationConfiguration :: Prelude.Maybe DataLakeReplicationConfiguration,
    -- | The Amazon Web Services Regions where Security Lake is automatically
    -- enabled.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'dataLakeConfiguration_encryptionConfiguration' - Provides encryption details of Amazon Security Lake object.
--
-- 'lifecycleConfiguration', 'dataLakeConfiguration_lifecycleConfiguration' - Provides lifecycle details of Amazon Security Lake object.
--
-- 'replicationConfiguration', 'dataLakeConfiguration_replicationConfiguration' - Provides replication details of Amazon Security Lake object.
--
-- 'region', 'dataLakeConfiguration_region' - The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
newDataLakeConfiguration ::
  -- | 'region'
  Prelude.Text ->
  DataLakeConfiguration
newDataLakeConfiguration pRegion_ =
  DataLakeConfiguration'
    { encryptionConfiguration =
        Prelude.Nothing,
      lifecycleConfiguration = Prelude.Nothing,
      replicationConfiguration = Prelude.Nothing,
      region = pRegion_
    }

-- | Provides encryption details of Amazon Security Lake object.
dataLakeConfiguration_encryptionConfiguration :: Lens.Lens' DataLakeConfiguration (Prelude.Maybe DataLakeEncryptionConfiguration)
dataLakeConfiguration_encryptionConfiguration = Lens.lens (\DataLakeConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@DataLakeConfiguration' {} a -> s {encryptionConfiguration = a} :: DataLakeConfiguration)

-- | Provides lifecycle details of Amazon Security Lake object.
dataLakeConfiguration_lifecycleConfiguration :: Lens.Lens' DataLakeConfiguration (Prelude.Maybe DataLakeLifecycleConfiguration)
dataLakeConfiguration_lifecycleConfiguration = Lens.lens (\DataLakeConfiguration' {lifecycleConfiguration} -> lifecycleConfiguration) (\s@DataLakeConfiguration' {} a -> s {lifecycleConfiguration = a} :: DataLakeConfiguration)

-- | Provides replication details of Amazon Security Lake object.
dataLakeConfiguration_replicationConfiguration :: Lens.Lens' DataLakeConfiguration (Prelude.Maybe DataLakeReplicationConfiguration)
dataLakeConfiguration_replicationConfiguration = Lens.lens (\DataLakeConfiguration' {replicationConfiguration} -> replicationConfiguration) (\s@DataLakeConfiguration' {} a -> s {replicationConfiguration = a} :: DataLakeConfiguration)

-- | The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
dataLakeConfiguration_region :: Lens.Lens' DataLakeConfiguration Prelude.Text
dataLakeConfiguration_region = Lens.lens (\DataLakeConfiguration' {region} -> region) (\s@DataLakeConfiguration' {} a -> s {region = a} :: DataLakeConfiguration)

instance Prelude.Hashable DataLakeConfiguration where
  hashWithSalt _salt DataLakeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` lifecycleConfiguration
      `Prelude.hashWithSalt` replicationConfiguration
      `Prelude.hashWithSalt` region

instance Prelude.NFData DataLakeConfiguration where
  rnf DataLakeConfiguration' {..} =
    Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf lifecycleConfiguration
      `Prelude.seq` Prelude.rnf replicationConfiguration
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON DataLakeConfiguration where
  toJSON DataLakeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("lifecycleConfiguration" Data..=)
              Prelude.<$> lifecycleConfiguration,
            ("replicationConfiguration" Data..=)
              Prelude.<$> replicationConfiguration,
            Prelude.Just ("region" Data..= region)
          ]
      )
