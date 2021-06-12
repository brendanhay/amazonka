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
-- Module      : Network.AWS.ElasticSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageTypeLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Limits that are applicable for given storage type.
--
-- /See:/ 'newStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { -- | Values for the @ StorageTypeLimit$LimitName @ .
    limitValues :: Core.Maybe [Core.Text],
    -- | Name of storage limits that are applicable for given storage type. If
    -- @ StorageType @ is ebs, following storage options are applicable
    --
    -- 1.  MinimumVolumeSize
    -- 2.  MaximumVolumeSize
    -- 3.  MaximumIops
    -- 4.  MinimumIops
    limitName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StorageTypeLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitValues', 'storageTypeLimit_limitValues' - Values for the @ StorageTypeLimit$LimitName @ .
--
-- 'limitName', 'storageTypeLimit_limitName' - Name of storage limits that are applicable for given storage type. If
-- @ StorageType @ is ebs, following storage options are applicable
--
-- 1.  MinimumVolumeSize
-- 2.  MaximumVolumeSize
-- 3.  MaximumIops
-- 4.  MinimumIops
newStorageTypeLimit ::
  StorageTypeLimit
newStorageTypeLimit =
  StorageTypeLimit'
    { limitValues = Core.Nothing,
      limitName = Core.Nothing
    }

-- | Values for the @ StorageTypeLimit$LimitName @ .
storageTypeLimit_limitValues :: Lens.Lens' StorageTypeLimit (Core.Maybe [Core.Text])
storageTypeLimit_limitValues = Lens.lens (\StorageTypeLimit' {limitValues} -> limitValues) (\s@StorageTypeLimit' {} a -> s {limitValues = a} :: StorageTypeLimit) Core.. Lens.mapping Lens._Coerce

-- | Name of storage limits that are applicable for given storage type. If
-- @ StorageType @ is ebs, following storage options are applicable
--
-- 1.  MinimumVolumeSize
-- 2.  MaximumVolumeSize
-- 3.  MaximumIops
-- 4.  MinimumIops
storageTypeLimit_limitName :: Lens.Lens' StorageTypeLimit (Core.Maybe Core.Text)
storageTypeLimit_limitName = Lens.lens (\StorageTypeLimit' {limitName} -> limitName) (\s@StorageTypeLimit' {} a -> s {limitName = a} :: StorageTypeLimit)

instance Core.FromJSON StorageTypeLimit where
  parseJSON =
    Core.withObject
      "StorageTypeLimit"
      ( \x ->
          StorageTypeLimit'
            Core.<$> (x Core..:? "LimitValues" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LimitName")
      )

instance Core.Hashable StorageTypeLimit

instance Core.NFData StorageTypeLimit
