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
-- Module      : Network.AWS.Lightsail.Types.AttachedDisk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AttachedDisk where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block storage disk that is attached to an instance, and is
-- included in an automatic snapshot.
--
-- /See:/ 'newAttachedDisk' smart constructor.
data AttachedDisk = AttachedDisk'
  { -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The path of the disk (e.g., @\/dev\/xvdf@).
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachedDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGb', 'attachedDisk_sizeInGb' - The size of the disk in GB.
--
-- 'path', 'attachedDisk_path' - The path of the disk (e.g., @\/dev\/xvdf@).
newAttachedDisk ::
  AttachedDisk
newAttachedDisk =
  AttachedDisk'
    { sizeInGb = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | The size of the disk in GB.
attachedDisk_sizeInGb :: Lens.Lens' AttachedDisk (Prelude.Maybe Prelude.Int)
attachedDisk_sizeInGb = Lens.lens (\AttachedDisk' {sizeInGb} -> sizeInGb) (\s@AttachedDisk' {} a -> s {sizeInGb = a} :: AttachedDisk)

-- | The path of the disk (e.g., @\/dev\/xvdf@).
attachedDisk_path :: Lens.Lens' AttachedDisk (Prelude.Maybe Prelude.Text)
attachedDisk_path = Lens.lens (\AttachedDisk' {path} -> path) (\s@AttachedDisk' {} a -> s {path = a} :: AttachedDisk)

instance Prelude.FromJSON AttachedDisk where
  parseJSON =
    Prelude.withObject
      "AttachedDisk"
      ( \x ->
          AttachedDisk'
            Prelude.<$> (x Prelude..:? "sizeInGb")
            Prelude.<*> (x Prelude..:? "path")
      )

instance Prelude.Hashable AttachedDisk

instance Prelude.NFData AttachedDisk
