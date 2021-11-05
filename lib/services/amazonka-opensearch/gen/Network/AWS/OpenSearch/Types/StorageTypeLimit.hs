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
-- Module      : Network.AWS.OpenSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.StorageTypeLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Limits that are applicable for the given storage type.
--
-- /See:/ 'newStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { -- | Name of storage limits that are applicable for the given storage type.
    -- If @ StorageType @ is \"ebs\", the following storage options are
    -- applicable:
    --
    -- 1.  MinimumVolumeSize
    -- 2.  MaximumVolumeSize
    -- 3.  MaximumIops
    -- 4.  MinimumIops
    limitName :: Prelude.Maybe Prelude.Text,
    -- | Values for the @ StorageTypeLimit$LimitName @ .
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
-- If @ StorageType @ is \"ebs\", the following storage options are
-- applicable:
--
-- 1.  MinimumVolumeSize
-- 2.  MaximumVolumeSize
-- 3.  MaximumIops
-- 4.  MinimumIops
--
-- 'limitValues', 'storageTypeLimit_limitValues' - Values for the @ StorageTypeLimit$LimitName @ .
newStorageTypeLimit ::
  StorageTypeLimit
newStorageTypeLimit =
  StorageTypeLimit'
    { limitName = Prelude.Nothing,
      limitValues = Prelude.Nothing
    }

-- | Name of storage limits that are applicable for the given storage type.
-- If @ StorageType @ is \"ebs\", the following storage options are
-- applicable:
--
-- 1.  MinimumVolumeSize
-- 2.  MaximumVolumeSize
-- 3.  MaximumIops
-- 4.  MinimumIops
storageTypeLimit_limitName :: Lens.Lens' StorageTypeLimit (Prelude.Maybe Prelude.Text)
storageTypeLimit_limitName = Lens.lens (\StorageTypeLimit' {limitName} -> limitName) (\s@StorageTypeLimit' {} a -> s {limitName = a} :: StorageTypeLimit)

-- | Values for the @ StorageTypeLimit$LimitName @ .
storageTypeLimit_limitValues :: Lens.Lens' StorageTypeLimit (Prelude.Maybe [Prelude.Text])
storageTypeLimit_limitValues = Lens.lens (\StorageTypeLimit' {limitValues} -> limitValues) (\s@StorageTypeLimit' {} a -> s {limitValues = a} :: StorageTypeLimit) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON StorageTypeLimit where
  parseJSON =
    Core.withObject
      "StorageTypeLimit"
      ( \x ->
          StorageTypeLimit'
            Prelude.<$> (x Core..:? "LimitName")
            Prelude.<*> (x Core..:? "LimitValues" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable StorageTypeLimit

instance Prelude.NFData StorageTypeLimit
