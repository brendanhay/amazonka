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
-- Module      : Amazonka.GreengrassV2.Types.LambdaVolumeMount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaVolumeMount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.LambdaFilesystemPermission
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a volume that Linux processes in a container
-- can access. When you define a volume, the IoT Greengrass Core software
-- mounts the source files to the destination inside the container.
--
-- /See:/ 'newLambdaVolumeMount' smart constructor.
data LambdaVolumeMount = LambdaVolumeMount'
  { -- | Whether or not to add the IoT Greengrass user group as an owner of the
    -- volume.
    --
    -- Default: @false@
    addGroupOwner :: Prelude.Maybe Prelude.Bool,
    -- | The permission to access the volume: read\/only (@ro@) or read\/write
    -- (@rw@).
    --
    -- Default: @ro@
    permission :: Prelude.Maybe LambdaFilesystemPermission,
    -- | The path to the physical volume in the file system.
    sourcePath :: Prelude.Text,
    -- | The path to the logical volume in the file system.
    destinationPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaVolumeMount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addGroupOwner', 'lambdaVolumeMount_addGroupOwner' - Whether or not to add the IoT Greengrass user group as an owner of the
-- volume.
--
-- Default: @false@
--
-- 'permission', 'lambdaVolumeMount_permission' - The permission to access the volume: read\/only (@ro@) or read\/write
-- (@rw@).
--
-- Default: @ro@
--
-- 'sourcePath', 'lambdaVolumeMount_sourcePath' - The path to the physical volume in the file system.
--
-- 'destinationPath', 'lambdaVolumeMount_destinationPath' - The path to the logical volume in the file system.
newLambdaVolumeMount ::
  -- | 'sourcePath'
  Prelude.Text ->
  -- | 'destinationPath'
  Prelude.Text ->
  LambdaVolumeMount
newLambdaVolumeMount pSourcePath_ pDestinationPath_ =
  LambdaVolumeMount'
    { addGroupOwner = Prelude.Nothing,
      permission = Prelude.Nothing,
      sourcePath = pSourcePath_,
      destinationPath = pDestinationPath_
    }

-- | Whether or not to add the IoT Greengrass user group as an owner of the
-- volume.
--
-- Default: @false@
lambdaVolumeMount_addGroupOwner :: Lens.Lens' LambdaVolumeMount (Prelude.Maybe Prelude.Bool)
lambdaVolumeMount_addGroupOwner = Lens.lens (\LambdaVolumeMount' {addGroupOwner} -> addGroupOwner) (\s@LambdaVolumeMount' {} a -> s {addGroupOwner = a} :: LambdaVolumeMount)

-- | The permission to access the volume: read\/only (@ro@) or read\/write
-- (@rw@).
--
-- Default: @ro@
lambdaVolumeMount_permission :: Lens.Lens' LambdaVolumeMount (Prelude.Maybe LambdaFilesystemPermission)
lambdaVolumeMount_permission = Lens.lens (\LambdaVolumeMount' {permission} -> permission) (\s@LambdaVolumeMount' {} a -> s {permission = a} :: LambdaVolumeMount)

-- | The path to the physical volume in the file system.
lambdaVolumeMount_sourcePath :: Lens.Lens' LambdaVolumeMount Prelude.Text
lambdaVolumeMount_sourcePath = Lens.lens (\LambdaVolumeMount' {sourcePath} -> sourcePath) (\s@LambdaVolumeMount' {} a -> s {sourcePath = a} :: LambdaVolumeMount)

-- | The path to the logical volume in the file system.
lambdaVolumeMount_destinationPath :: Lens.Lens' LambdaVolumeMount Prelude.Text
lambdaVolumeMount_destinationPath = Lens.lens (\LambdaVolumeMount' {destinationPath} -> destinationPath) (\s@LambdaVolumeMount' {} a -> s {destinationPath = a} :: LambdaVolumeMount)

instance Prelude.Hashable LambdaVolumeMount where
  hashWithSalt _salt LambdaVolumeMount' {..} =
    _salt
      `Prelude.hashWithSalt` addGroupOwner
      `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` destinationPath

instance Prelude.NFData LambdaVolumeMount where
  rnf LambdaVolumeMount' {..} =
    Prelude.rnf addGroupOwner
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf sourcePath
      `Prelude.seq` Prelude.rnf destinationPath

instance Data.ToJSON LambdaVolumeMount where
  toJSON LambdaVolumeMount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addGroupOwner" Data..=) Prelude.<$> addGroupOwner,
            ("permission" Data..=) Prelude.<$> permission,
            Prelude.Just ("sourcePath" Data..= sourcePath),
            Prelude.Just
              ("destinationPath" Data..= destinationPath)
          ]
      )
