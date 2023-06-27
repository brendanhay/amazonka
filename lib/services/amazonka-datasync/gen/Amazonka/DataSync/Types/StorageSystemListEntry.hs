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
-- Module      : Amazonka.DataSync.Types.StorageSystemListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.StorageSystemListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information that identifies an on-premises storage system that you\'re
-- using with DataSync Discovery.
--
-- /See:/ 'newStorageSystemListEntry' smart constructor.
data StorageSystemListEntry = StorageSystemListEntry'
  { -- | The name of an on-premises storage system that you added to DataSync
    -- Discovery.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of an on-premises storage system that
    -- you added to DataSync Discovery.
    storageSystemArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageSystemListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'storageSystemListEntry_name' - The name of an on-premises storage system that you added to DataSync
-- Discovery.
--
-- 'storageSystemArn', 'storageSystemListEntry_storageSystemArn' - The Amazon Resource Names (ARN) of an on-premises storage system that
-- you added to DataSync Discovery.
newStorageSystemListEntry ::
  StorageSystemListEntry
newStorageSystemListEntry =
  StorageSystemListEntry'
    { name = Prelude.Nothing,
      storageSystemArn = Prelude.Nothing
    }

-- | The name of an on-premises storage system that you added to DataSync
-- Discovery.
storageSystemListEntry_name :: Lens.Lens' StorageSystemListEntry (Prelude.Maybe Prelude.Text)
storageSystemListEntry_name = Lens.lens (\StorageSystemListEntry' {name} -> name) (\s@StorageSystemListEntry' {} a -> s {name = a} :: StorageSystemListEntry)

-- | The Amazon Resource Names (ARN) of an on-premises storage system that
-- you added to DataSync Discovery.
storageSystemListEntry_storageSystemArn :: Lens.Lens' StorageSystemListEntry (Prelude.Maybe Prelude.Text)
storageSystemListEntry_storageSystemArn = Lens.lens (\StorageSystemListEntry' {storageSystemArn} -> storageSystemArn) (\s@StorageSystemListEntry' {} a -> s {storageSystemArn = a} :: StorageSystemListEntry)

instance Data.FromJSON StorageSystemListEntry where
  parseJSON =
    Data.withObject
      "StorageSystemListEntry"
      ( \x ->
          StorageSystemListEntry'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "StorageSystemArn")
      )

instance Prelude.Hashable StorageSystemListEntry where
  hashWithSalt _salt StorageSystemListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` storageSystemArn

instance Prelude.NFData StorageSystemListEntry where
  rnf StorageSystemListEntry' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf storageSystemArn
