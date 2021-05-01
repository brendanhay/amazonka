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
-- Module      : Network.AWS.Lightsail.Types.DiskMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskMap where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block storage disk mapping.
--
-- /See:/ 'newDiskMap' smart constructor.
data DiskMap = DiskMap'
  { -- | The original disk path exposed to the instance (for example,
    -- @\/dev\/sdh@).
    originalDiskPath :: Prelude.Maybe Prelude.Text,
    -- | The new disk name (e.g., @my-new-disk@).
    newDiskName' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DiskMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originalDiskPath', 'diskMap_originalDiskPath' - The original disk path exposed to the instance (for example,
-- @\/dev\/sdh@).
--
-- 'newDiskName'', 'diskMap_newDiskName' - The new disk name (e.g., @my-new-disk@).
newDiskMap ::
  DiskMap
newDiskMap =
  DiskMap'
    { originalDiskPath = Prelude.Nothing,
      newDiskName' = Prelude.Nothing
    }

-- | The original disk path exposed to the instance (for example,
-- @\/dev\/sdh@).
diskMap_originalDiskPath :: Lens.Lens' DiskMap (Prelude.Maybe Prelude.Text)
diskMap_originalDiskPath = Lens.lens (\DiskMap' {originalDiskPath} -> originalDiskPath) (\s@DiskMap' {} a -> s {originalDiskPath = a} :: DiskMap)

-- | The new disk name (e.g., @my-new-disk@).
diskMap_newDiskName :: Lens.Lens' DiskMap (Prelude.Maybe Prelude.Text)
diskMap_newDiskName = Lens.lens (\DiskMap' {newDiskName'} -> newDiskName') (\s@DiskMap' {} a -> s {newDiskName' = a} :: DiskMap)

instance Prelude.Hashable DiskMap

instance Prelude.NFData DiskMap

instance Prelude.ToJSON DiskMap where
  toJSON DiskMap' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("originalDiskPath" Prelude..=)
              Prelude.<$> originalDiskPath,
            ("newDiskName" Prelude..=) Prelude.<$> newDiskName'
          ]
      )
