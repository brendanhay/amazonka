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
-- Module      : Amazonka.GuardDuty.Types.VolumeMount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.VolumeMount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container volume mount.
--
-- /See:/ 'newVolumeMount' smart constructor.
data VolumeMount = VolumeMount'
  { -- | Volume mount name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Volume mount path.
    mountPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeMount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'volumeMount_name' - Volume mount name.
--
-- 'mountPath', 'volumeMount_mountPath' - Volume mount path.
newVolumeMount ::
  VolumeMount
newVolumeMount =
  VolumeMount'
    { name = Prelude.Nothing,
      mountPath = Prelude.Nothing
    }

-- | Volume mount name.
volumeMount_name :: Lens.Lens' VolumeMount (Prelude.Maybe Prelude.Text)
volumeMount_name = Lens.lens (\VolumeMount' {name} -> name) (\s@VolumeMount' {} a -> s {name = a} :: VolumeMount)

-- | Volume mount path.
volumeMount_mountPath :: Lens.Lens' VolumeMount (Prelude.Maybe Prelude.Text)
volumeMount_mountPath = Lens.lens (\VolumeMount' {mountPath} -> mountPath) (\s@VolumeMount' {} a -> s {mountPath = a} :: VolumeMount)

instance Core.FromJSON VolumeMount where
  parseJSON =
    Core.withObject
      "VolumeMount"
      ( \x ->
          VolumeMount'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "mountPath")
      )

instance Prelude.Hashable VolumeMount where
  hashWithSalt _salt VolumeMount' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` mountPath

instance Prelude.NFData VolumeMount where
  rnf VolumeMount' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf mountPath
