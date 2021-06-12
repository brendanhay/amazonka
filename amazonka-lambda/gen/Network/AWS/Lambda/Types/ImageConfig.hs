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
-- Module      : Network.AWS.Lambda.Types.ImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ImageConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration values that override the container image Dockerfile
-- settings. See
-- <https://docs.aws.amazon.com/lambda/latest/dg/images-create.html#images-parms Container settings>.
--
-- /See:/ 'newImageConfig' smart constructor.
data ImageConfig = ImageConfig'
  { -- | Specifies the working directory.
    workingDirectory :: Core.Maybe Core.Text,
    -- | Specifies the entry point to their application, which is typically the
    -- location of the runtime executable.
    entryPoint :: Core.Maybe [Core.Text],
    -- | Specifies parameters that you want to pass in with ENTRYPOINT.
    command :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workingDirectory', 'imageConfig_workingDirectory' - Specifies the working directory.
--
-- 'entryPoint', 'imageConfig_entryPoint' - Specifies the entry point to their application, which is typically the
-- location of the runtime executable.
--
-- 'command', 'imageConfig_command' - Specifies parameters that you want to pass in with ENTRYPOINT.
newImageConfig ::
  ImageConfig
newImageConfig =
  ImageConfig'
    { workingDirectory = Core.Nothing,
      entryPoint = Core.Nothing,
      command = Core.Nothing
    }

-- | Specifies the working directory.
imageConfig_workingDirectory :: Lens.Lens' ImageConfig (Core.Maybe Core.Text)
imageConfig_workingDirectory = Lens.lens (\ImageConfig' {workingDirectory} -> workingDirectory) (\s@ImageConfig' {} a -> s {workingDirectory = a} :: ImageConfig)

-- | Specifies the entry point to their application, which is typically the
-- location of the runtime executable.
imageConfig_entryPoint :: Lens.Lens' ImageConfig (Core.Maybe [Core.Text])
imageConfig_entryPoint = Lens.lens (\ImageConfig' {entryPoint} -> entryPoint) (\s@ImageConfig' {} a -> s {entryPoint = a} :: ImageConfig) Core.. Lens.mapping Lens._Coerce

-- | Specifies parameters that you want to pass in with ENTRYPOINT.
imageConfig_command :: Lens.Lens' ImageConfig (Core.Maybe [Core.Text])
imageConfig_command = Lens.lens (\ImageConfig' {command} -> command) (\s@ImageConfig' {} a -> s {command = a} :: ImageConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ImageConfig where
  parseJSON =
    Core.withObject
      "ImageConfig"
      ( \x ->
          ImageConfig'
            Core.<$> (x Core..:? "WorkingDirectory")
            Core.<*> (x Core..:? "EntryPoint" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Command" Core..!= Core.mempty)
      )

instance Core.Hashable ImageConfig

instance Core.NFData ImageConfig

instance Core.ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WorkingDirectory" Core..=)
              Core.<$> workingDirectory,
            ("EntryPoint" Core..=) Core.<$> entryPoint,
            ("Command" Core..=) Core.<$> command
          ]
      )
