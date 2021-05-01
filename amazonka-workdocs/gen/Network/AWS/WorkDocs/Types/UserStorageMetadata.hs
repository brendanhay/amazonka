{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkDocs.Types.UserStorageMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserStorageMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.StorageRuleType

-- | Describes the storage for a user.
--
-- /See:/ 'newUserStorageMetadata' smart constructor.
data UserStorageMetadata = UserStorageMetadata'
  { -- | The storage for a user.
    storageRule :: Prelude.Maybe StorageRuleType,
    -- | The amount of storage used, in bytes.
    storageUtilizedInBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON UserStorageMetadata where
  parseJSON =
    Prelude.withObject
      "UserStorageMetadata"
      ( \x ->
          UserStorageMetadata'
            Prelude.<$> (x Prelude..:? "StorageRule")
            Prelude.<*> (x Prelude..:? "StorageUtilizedInBytes")
      )

instance Prelude.Hashable UserStorageMetadata

instance Prelude.NFData UserStorageMetadata
