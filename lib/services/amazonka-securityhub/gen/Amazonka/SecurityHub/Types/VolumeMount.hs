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
-- Module      : Amazonka.SecurityHub.Types.VolumeMount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.VolumeMount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the mounting of a volume in a container.
--
-- /See:/ 'newVolumeMount' smart constructor.
data VolumeMount = VolumeMount'
  { -- | The path in the container at which the volume should be mounted.
    mountPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the volume.
    name :: Prelude.Maybe Prelude.Text
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
-- 'mountPath', 'volumeMount_mountPath' - The path in the container at which the volume should be mounted.
--
-- 'name', 'volumeMount_name' - The name of the volume.
newVolumeMount ::
  VolumeMount
newVolumeMount =
  VolumeMount'
    { mountPath = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The path in the container at which the volume should be mounted.
volumeMount_mountPath :: Lens.Lens' VolumeMount (Prelude.Maybe Prelude.Text)
volumeMount_mountPath = Lens.lens (\VolumeMount' {mountPath} -> mountPath) (\s@VolumeMount' {} a -> s {mountPath = a} :: VolumeMount)

-- | The name of the volume.
volumeMount_name :: Lens.Lens' VolumeMount (Prelude.Maybe Prelude.Text)
volumeMount_name = Lens.lens (\VolumeMount' {name} -> name) (\s@VolumeMount' {} a -> s {name = a} :: VolumeMount)

instance Data.FromJSON VolumeMount where
  parseJSON =
    Data.withObject
      "VolumeMount"
      ( \x ->
          VolumeMount'
            Prelude.<$> (x Data..:? "MountPath")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable VolumeMount where
  hashWithSalt _salt VolumeMount' {..} =
    _salt
      `Prelude.hashWithSalt` mountPath
      `Prelude.hashWithSalt` name

instance Prelude.NFData VolumeMount where
  rnf VolumeMount' {..} =
    Prelude.rnf mountPath `Prelude.seq`
      Prelude.rnf name

instance Data.ToJSON VolumeMount where
  toJSON VolumeMount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MountPath" Data..=) Prelude.<$> mountPath,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
