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
-- Module      : Amazonka.EC2.Types.StorageLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.StorageLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a storage location in Amazon S3.
--
-- /See:/ 'newStorageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { -- | The key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'storageLocation_key' - The key.
--
-- 'bucket', 'storageLocation_bucket' - The name of the S3 bucket.
newStorageLocation ::
  StorageLocation
newStorageLocation =
  StorageLocation'
    { key = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | The key.
storageLocation_key :: Lens.Lens' StorageLocation (Prelude.Maybe Prelude.Text)
storageLocation_key = Lens.lens (\StorageLocation' {key} -> key) (\s@StorageLocation' {} a -> s {key = a} :: StorageLocation)

-- | The name of the S3 bucket.
storageLocation_bucket :: Lens.Lens' StorageLocation (Prelude.Maybe Prelude.Text)
storageLocation_bucket = Lens.lens (\StorageLocation' {bucket} -> bucket) (\s@StorageLocation' {} a -> s {bucket = a} :: StorageLocation)

instance Prelude.Hashable StorageLocation where
  hashWithSalt _salt StorageLocation' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData StorageLocation where
  rnf StorageLocation' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf bucket

instance Core.ToQuery StorageLocation where
  toQuery StorageLocation' {..} =
    Prelude.mconcat
      ["Key" Core.=: key, "Bucket" Core.=: bucket]
