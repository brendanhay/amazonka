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
-- Module      : Amazonka.ElasticSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.StorageTypeLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Limits that are applicable for given storage type.
--
-- /See:/ 'newStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { -- | Name of storage limits that are applicable for given storage type. If
    -- @ @@StorageType@@ @ is ebs, following storage options are applicable
    --
    -- 1.  MinimumVolumeSize
    --     Minimum amount of volume size that is applicable for given storage
    --     type.It can be empty if it is not applicable.
    -- 2.  MaximumVolumeSize
    --     Maximum amount of volume size that is applicable for given storage
    --     type.It can be empty if it is not applicable.
    -- 3.  MaximumIops
    --     Maximum amount of Iops that is applicable for given storage type.It
    --     can be empty if it is not applicable.
    -- 4.  MinimumIops
    --     Minimum amount of Iops that is applicable for given storage type.It
    --     can be empty if it is not applicable.
    -- 5.  MaximumThroughput
    --     Maximum amount of Throughput that is applicable for given storage
    --     type.It can be empty if it is not applicable.
    -- 6.  MinimumThroughput
    --     Minimum amount of Throughput that is applicable for given storage
    --     type.It can be empty if it is not applicable.
    limitName :: Prelude.Maybe Prelude.Text,
    -- | Values for the @ @@StorageTypeLimit$LimitName@@ @ .
    limitValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageTypeLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitName', 'storageTypeLimit_limitName' - Name of storage limits that are applicable for given storage type. If
-- @ @@StorageType@@ @ is ebs, following storage options are applicable
--
-- 1.  MinimumVolumeSize
--     Minimum amount of volume size that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 2.  MaximumVolumeSize
--     Maximum amount of volume size that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 3.  MaximumIops
--     Maximum amount of Iops that is applicable for given storage type.It
--     can be empty if it is not applicable.
-- 4.  MinimumIops
--     Minimum amount of Iops that is applicable for given storage type.It
--     can be empty if it is not applicable.
-- 5.  MaximumThroughput
--     Maximum amount of Throughput that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 6.  MinimumThroughput
--     Minimum amount of Throughput that is applicable for given storage
--     type.It can be empty if it is not applicable.
--
-- 'limitValues', 'storageTypeLimit_limitValues' - Values for the @ @@StorageTypeLimit$LimitName@@ @ .
newStorageTypeLimit ::
  StorageTypeLimit
newStorageTypeLimit =
  StorageTypeLimit'
    { limitName = Prelude.Nothing,
      limitValues = Prelude.Nothing
    }

-- | Name of storage limits that are applicable for given storage type. If
-- @ @@StorageType@@ @ is ebs, following storage options are applicable
--
-- 1.  MinimumVolumeSize
--     Minimum amount of volume size that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 2.  MaximumVolumeSize
--     Maximum amount of volume size that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 3.  MaximumIops
--     Maximum amount of Iops that is applicable for given storage type.It
--     can be empty if it is not applicable.
-- 4.  MinimumIops
--     Minimum amount of Iops that is applicable for given storage type.It
--     can be empty if it is not applicable.
-- 5.  MaximumThroughput
--     Maximum amount of Throughput that is applicable for given storage
--     type.It can be empty if it is not applicable.
-- 6.  MinimumThroughput
--     Minimum amount of Throughput that is applicable for given storage
--     type.It can be empty if it is not applicable.
storageTypeLimit_limitName :: Lens.Lens' StorageTypeLimit (Prelude.Maybe Prelude.Text)
storageTypeLimit_limitName = Lens.lens (\StorageTypeLimit' {limitName} -> limitName) (\s@StorageTypeLimit' {} a -> s {limitName = a} :: StorageTypeLimit)

-- | Values for the @ @@StorageTypeLimit$LimitName@@ @ .
storageTypeLimit_limitValues :: Lens.Lens' StorageTypeLimit (Prelude.Maybe [Prelude.Text])
storageTypeLimit_limitValues = Lens.lens (\StorageTypeLimit' {limitValues} -> limitValues) (\s@StorageTypeLimit' {} a -> s {limitValues = a} :: StorageTypeLimit) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StorageTypeLimit where
  parseJSON =
    Data.withObject
      "StorageTypeLimit"
      ( \x ->
          StorageTypeLimit'
            Prelude.<$> (x Data..:? "LimitName")
            Prelude.<*> (x Data..:? "LimitValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StorageTypeLimit where
  hashWithSalt _salt StorageTypeLimit' {..} =
    _salt
      `Prelude.hashWithSalt` limitName
      `Prelude.hashWithSalt` limitValues

instance Prelude.NFData StorageTypeLimit where
  rnf StorageTypeLimit' {..} =
    Prelude.rnf limitName `Prelude.seq`
      Prelude.rnf limitValues
