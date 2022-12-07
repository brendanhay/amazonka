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
-- Module      : Amazonka.OpenSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.StorageTypeLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Limits that are applicable for the given Amazon OpenSearch Service
-- storage type.
--
-- /See:/ 'newStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { -- | Name of storage limits that are applicable for the given storage type.
    -- If @StorageType@ is @ebs@, the following options are available:
    --
    -- -   __MinimumVolumeSize__ - Minimum volume size that is available for
    --     the given storage type. Can be empty if not applicable.
    --
    -- -   __MaximumVolumeSize__ - Maximum volume size that is available for
    --     the given storage type. Can be empty if not applicable.
    --
    -- -   __MaximumIops__ - Maximum amount of IOPS that is available for the
    --     given the storage type. Can be empty if not applicable.
    --
    -- -   __MinimumIops__ - Minimum amount of IOPS that is available for the
    --     given the storage type. Can be empty if not applicable.
    --
    -- -   __MaximumThroughput__ - Maximum amount of throughput that is
    --     available for the given the storage type. Can be empty if not
    --     applicable.
    --
    -- -   __MinimumThroughput__ - Minimum amount of throughput that is
    --     available for the given the storage type. Can be empty if not
    --     applicable.
    limitName :: Prelude.Maybe Prelude.Text,
    -- | The limit values.
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
-- 'limitName', 'storageTypeLimit_limitName' - Name of storage limits that are applicable for the given storage type.
-- If @StorageType@ is @ebs@, the following options are available:
--
-- -   __MinimumVolumeSize__ - Minimum volume size that is available for
--     the given storage type. Can be empty if not applicable.
--
-- -   __MaximumVolumeSize__ - Maximum volume size that is available for
--     the given storage type. Can be empty if not applicable.
--
-- -   __MaximumIops__ - Maximum amount of IOPS that is available for the
--     given the storage type. Can be empty if not applicable.
--
-- -   __MinimumIops__ - Minimum amount of IOPS that is available for the
--     given the storage type. Can be empty if not applicable.
--
-- -   __MaximumThroughput__ - Maximum amount of throughput that is
--     available for the given the storage type. Can be empty if not
--     applicable.
--
-- -   __MinimumThroughput__ - Minimum amount of throughput that is
--     available for the given the storage type. Can be empty if not
--     applicable.
--
-- 'limitValues', 'storageTypeLimit_limitValues' - The limit values.
newStorageTypeLimit ::
  StorageTypeLimit
newStorageTypeLimit =
  StorageTypeLimit'
    { limitName = Prelude.Nothing,
      limitValues = Prelude.Nothing
    }

-- | Name of storage limits that are applicable for the given storage type.
-- If @StorageType@ is @ebs@, the following options are available:
--
-- -   __MinimumVolumeSize__ - Minimum volume size that is available for
--     the given storage type. Can be empty if not applicable.
--
-- -   __MaximumVolumeSize__ - Maximum volume size that is available for
--     the given storage type. Can be empty if not applicable.
--
-- -   __MaximumIops__ - Maximum amount of IOPS that is available for the
--     given the storage type. Can be empty if not applicable.
--
-- -   __MinimumIops__ - Minimum amount of IOPS that is available for the
--     given the storage type. Can be empty if not applicable.
--
-- -   __MaximumThroughput__ - Maximum amount of throughput that is
--     available for the given the storage type. Can be empty if not
--     applicable.
--
-- -   __MinimumThroughput__ - Minimum amount of throughput that is
--     available for the given the storage type. Can be empty if not
--     applicable.
storageTypeLimit_limitName :: Lens.Lens' StorageTypeLimit (Prelude.Maybe Prelude.Text)
storageTypeLimit_limitName = Lens.lens (\StorageTypeLimit' {limitName} -> limitName) (\s@StorageTypeLimit' {} a -> s {limitName = a} :: StorageTypeLimit)

-- | The limit values.
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
    _salt `Prelude.hashWithSalt` limitName
      `Prelude.hashWithSalt` limitValues

instance Prelude.NFData StorageTypeLimit where
  rnf StorageTypeLimit' {..} =
    Prelude.rnf limitName
      `Prelude.seq` Prelude.rnf limitValues
