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
-- Module      : Amazonka.FinSpace.Types.KxSavedownStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxSavedownStorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.KxSavedownStorageType
import qualified Amazonka.Prelude as Prelude

-- | The size and type of temporary storage that is used to hold data during
-- the savedown process. All the data written to this storage space is lost
-- when the cluster node is restarted.
--
-- /See:/ 'newKxSavedownStorageConfiguration' smart constructor.
data KxSavedownStorageConfiguration = KxSavedownStorageConfiguration'
  { -- | The type of writeable storage space for temporarily storing your
    -- savedown data. The valid values are:
    --
    -- -   SDS01 – This type represents 3000 IOPS and io2 ebs volume type.
    type' :: KxSavedownStorageType,
    -- | The size of temporary storage in bytes.
    size :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxSavedownStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'kxSavedownStorageConfiguration_type' - The type of writeable storage space for temporarily storing your
-- savedown data. The valid values are:
--
-- -   SDS01 – This type represents 3000 IOPS and io2 ebs volume type.
--
-- 'size', 'kxSavedownStorageConfiguration_size' - The size of temporary storage in bytes.
newKxSavedownStorageConfiguration ::
  -- | 'type''
  KxSavedownStorageType ->
  -- | 'size'
  Prelude.Natural ->
  KxSavedownStorageConfiguration
newKxSavedownStorageConfiguration pType_ pSize_ =
  KxSavedownStorageConfiguration'
    { type' = pType_,
      size = pSize_
    }

-- | The type of writeable storage space for temporarily storing your
-- savedown data. The valid values are:
--
-- -   SDS01 – This type represents 3000 IOPS and io2 ebs volume type.
kxSavedownStorageConfiguration_type :: Lens.Lens' KxSavedownStorageConfiguration KxSavedownStorageType
kxSavedownStorageConfiguration_type = Lens.lens (\KxSavedownStorageConfiguration' {type'} -> type') (\s@KxSavedownStorageConfiguration' {} a -> s {type' = a} :: KxSavedownStorageConfiguration)

-- | The size of temporary storage in bytes.
kxSavedownStorageConfiguration_size :: Lens.Lens' KxSavedownStorageConfiguration Prelude.Natural
kxSavedownStorageConfiguration_size = Lens.lens (\KxSavedownStorageConfiguration' {size} -> size) (\s@KxSavedownStorageConfiguration' {} a -> s {size = a} :: KxSavedownStorageConfiguration)

instance Data.FromJSON KxSavedownStorageConfiguration where
  parseJSON =
    Data.withObject
      "KxSavedownStorageConfiguration"
      ( \x ->
          KxSavedownStorageConfiguration'
            Prelude.<$> (x Data..: "type")
            Prelude.<*> (x Data..: "size")
      )

instance
  Prelude.Hashable
    KxSavedownStorageConfiguration
  where
  hashWithSalt
    _salt
    KxSavedownStorageConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` size

instance
  Prelude.NFData
    KxSavedownStorageConfiguration
  where
  rnf KxSavedownStorageConfiguration' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf size

instance Data.ToJSON KxSavedownStorageConfiguration where
  toJSON KxSavedownStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("size" Data..= size)
          ]
      )
