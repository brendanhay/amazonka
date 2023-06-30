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
-- Module      : Amazonka.Lightsail.Types.DiskMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk mapping.
--
-- /See:/ 'newDiskMap' smart constructor.
data DiskMap = DiskMap'
  { -- | The new disk name (e.g., @my-new-disk@).
    newDiskName' :: Prelude.Maybe Prelude.Text,
    -- | The original disk path exposed to the instance (for example,
    -- @\/dev\/sdh@).
    originalDiskPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newDiskName'', 'diskMap_newDiskName' - The new disk name (e.g., @my-new-disk@).
--
-- 'originalDiskPath', 'diskMap_originalDiskPath' - The original disk path exposed to the instance (for example,
-- @\/dev\/sdh@).
newDiskMap ::
  DiskMap
newDiskMap =
  DiskMap'
    { newDiskName' = Prelude.Nothing,
      originalDiskPath = Prelude.Nothing
    }

-- | The new disk name (e.g., @my-new-disk@).
diskMap_newDiskName :: Lens.Lens' DiskMap (Prelude.Maybe Prelude.Text)
diskMap_newDiskName = Lens.lens (\DiskMap' {newDiskName'} -> newDiskName') (\s@DiskMap' {} a -> s {newDiskName' = a} :: DiskMap)

-- | The original disk path exposed to the instance (for example,
-- @\/dev\/sdh@).
diskMap_originalDiskPath :: Lens.Lens' DiskMap (Prelude.Maybe Prelude.Text)
diskMap_originalDiskPath = Lens.lens (\DiskMap' {originalDiskPath} -> originalDiskPath) (\s@DiskMap' {} a -> s {originalDiskPath = a} :: DiskMap)

instance Prelude.Hashable DiskMap where
  hashWithSalt _salt DiskMap' {..} =
    _salt
      `Prelude.hashWithSalt` newDiskName'
      `Prelude.hashWithSalt` originalDiskPath

instance Prelude.NFData DiskMap where
  rnf DiskMap' {..} =
    Prelude.rnf newDiskName'
      `Prelude.seq` Prelude.rnf originalDiskPath

instance Data.ToJSON DiskMap where
  toJSON DiskMap' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("newDiskName" Data..=) Prelude.<$> newDiskName',
            ("originalDiskPath" Data..=)
              Prelude.<$> originalDiskPath
          ]
      )
