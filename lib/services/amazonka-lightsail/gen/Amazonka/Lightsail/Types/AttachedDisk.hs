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
-- Module      : Amazonka.Lightsail.Types.AttachedDisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AttachedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk that is attached to an instance, and is
-- included in an automatic snapshot.
--
-- /See:/ 'newAttachedDisk' smart constructor.
data AttachedDisk = AttachedDisk'
  { -- | The path of the disk (e.g., @\/dev\/xvdf@).
    path :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachedDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'attachedDisk_path' - The path of the disk (e.g., @\/dev\/xvdf@).
--
-- 'sizeInGb', 'attachedDisk_sizeInGb' - The size of the disk in GB.
newAttachedDisk ::
  AttachedDisk
newAttachedDisk =
  AttachedDisk'
    { path = Prelude.Nothing,
      sizeInGb = Prelude.Nothing
    }

-- | The path of the disk (e.g., @\/dev\/xvdf@).
attachedDisk_path :: Lens.Lens' AttachedDisk (Prelude.Maybe Prelude.Text)
attachedDisk_path = Lens.lens (\AttachedDisk' {path} -> path) (\s@AttachedDisk' {} a -> s {path = a} :: AttachedDisk)

-- | The size of the disk in GB.
attachedDisk_sizeInGb :: Lens.Lens' AttachedDisk (Prelude.Maybe Prelude.Int)
attachedDisk_sizeInGb = Lens.lens (\AttachedDisk' {sizeInGb} -> sizeInGb) (\s@AttachedDisk' {} a -> s {sizeInGb = a} :: AttachedDisk)

instance Data.FromJSON AttachedDisk where
  parseJSON =
    Data.withObject
      "AttachedDisk"
      ( \x ->
          AttachedDisk'
            Prelude.<$> (x Data..:? "path")
            Prelude.<*> (x Data..:? "sizeInGb")
      )

instance Prelude.Hashable AttachedDisk where
  hashWithSalt _salt AttachedDisk' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` sizeInGb

instance Prelude.NFData AttachedDisk where
  rnf AttachedDisk' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf sizeInGb
