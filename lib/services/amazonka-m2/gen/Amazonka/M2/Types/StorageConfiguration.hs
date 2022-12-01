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
-- Module      : Amazonka.M2.Types.StorageConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.StorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.EfsStorageConfiguration
import Amazonka.M2.Types.FsxStorageConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Defines the storage configuration for an environment.
--
-- /See:/ 'newStorageConfiguration' smart constructor.
data StorageConfiguration = StorageConfiguration'
  { -- | Defines the storage configuration for an Amazon EFS file system.
    efs :: Prelude.Maybe EfsStorageConfiguration,
    -- | Defines the storage configuration for an Amazon FSx file system.
    fsx :: Prelude.Maybe FsxStorageConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'efs', 'storageConfiguration_efs' - Defines the storage configuration for an Amazon EFS file system.
--
-- 'fsx', 'storageConfiguration_fsx' - Defines the storage configuration for an Amazon FSx file system.
newStorageConfiguration ::
  StorageConfiguration
newStorageConfiguration =
  StorageConfiguration'
    { efs = Prelude.Nothing,
      fsx = Prelude.Nothing
    }

-- | Defines the storage configuration for an Amazon EFS file system.
storageConfiguration_efs :: Lens.Lens' StorageConfiguration (Prelude.Maybe EfsStorageConfiguration)
storageConfiguration_efs = Lens.lens (\StorageConfiguration' {efs} -> efs) (\s@StorageConfiguration' {} a -> s {efs = a} :: StorageConfiguration)

-- | Defines the storage configuration for an Amazon FSx file system.
storageConfiguration_fsx :: Lens.Lens' StorageConfiguration (Prelude.Maybe FsxStorageConfiguration)
storageConfiguration_fsx = Lens.lens (\StorageConfiguration' {fsx} -> fsx) (\s@StorageConfiguration' {} a -> s {fsx = a} :: StorageConfiguration)

instance Core.FromJSON StorageConfiguration where
  parseJSON =
    Core.withObject
      "StorageConfiguration"
      ( \x ->
          StorageConfiguration'
            Prelude.<$> (x Core..:? "efs") Prelude.<*> (x Core..:? "fsx")
      )

instance Prelude.Hashable StorageConfiguration where
  hashWithSalt _salt StorageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` efs
      `Prelude.hashWithSalt` fsx

instance Prelude.NFData StorageConfiguration where
  rnf StorageConfiguration' {..} =
    Prelude.rnf efs `Prelude.seq` Prelude.rnf fsx

instance Core.ToJSON StorageConfiguration where
  toJSON StorageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("efs" Core..=) Prelude.<$> efs,
            ("fsx" Core..=) Prelude.<$> fsx
          ]
      )
