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
-- Module      : Amazonka.WorkDocs.Types.UserStorageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UserStorageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.StorageRuleType

-- | Describes the storage for a user.
--
-- /See:/ 'newUserStorageMetadata' smart constructor.
data UserStorageMetadata = UserStorageMetadata'
  { -- | The storage for a user.
    storageRule :: Prelude.Maybe StorageRuleType,
    -- | The amount of storage used, in bytes.
    storageUtilizedInBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserStorageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageRule', 'userStorageMetadata_storageRule' - The storage for a user.
--
-- 'storageUtilizedInBytes', 'userStorageMetadata_storageUtilizedInBytes' - The amount of storage used, in bytes.
newUserStorageMetadata ::
  UserStorageMetadata
newUserStorageMetadata =
  UserStorageMetadata'
    { storageRule = Prelude.Nothing,
      storageUtilizedInBytes = Prelude.Nothing
    }

-- | The storage for a user.
userStorageMetadata_storageRule :: Lens.Lens' UserStorageMetadata (Prelude.Maybe StorageRuleType)
userStorageMetadata_storageRule = Lens.lens (\UserStorageMetadata' {storageRule} -> storageRule) (\s@UserStorageMetadata' {} a -> s {storageRule = a} :: UserStorageMetadata)

-- | The amount of storage used, in bytes.
userStorageMetadata_storageUtilizedInBytes :: Lens.Lens' UserStorageMetadata (Prelude.Maybe Prelude.Integer)
userStorageMetadata_storageUtilizedInBytes = Lens.lens (\UserStorageMetadata' {storageUtilizedInBytes} -> storageUtilizedInBytes) (\s@UserStorageMetadata' {} a -> s {storageUtilizedInBytes = a} :: UserStorageMetadata)

instance Data.FromJSON UserStorageMetadata where
  parseJSON =
    Data.withObject
      "UserStorageMetadata"
      ( \x ->
          UserStorageMetadata'
            Prelude.<$> (x Data..:? "StorageRule")
            Prelude.<*> (x Data..:? "StorageUtilizedInBytes")
      )

instance Prelude.Hashable UserStorageMetadata where
  hashWithSalt _salt UserStorageMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` storageRule
      `Prelude.hashWithSalt` storageUtilizedInBytes

instance Prelude.NFData UserStorageMetadata where
  rnf UserStorageMetadata' {..} =
    Prelude.rnf storageRule
      `Prelude.seq` Prelude.rnf storageUtilizedInBytes
