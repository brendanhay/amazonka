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
-- Module      : Network.AWS.Lambda.Types.ImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ImageConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration values that override the container image Dockerfile
-- settings. See
-- <https://docs.aws.amazon.com/lambda/latest/dg/images-create.html#images-parms Container settings>.
--
-- /See:/ 'newImageConfig' smart constructor.
data ImageConfig = ImageConfig'
  { -- | Specifies the working directory.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | Specifies the entry point to their application, which is typically the
    -- location of the runtime executable.
    entryPoint :: Prelude.Maybe [Prelude.Text],
    -- | Specifies parameters that you want to pass in with ENTRYPOINT.
    command :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { workingDirectory = Prelude.Nothing,
      entryPoint = Prelude.Nothing,
      command = Prelude.Nothing
    }

-- | Specifies the working directory.
imageConfig_workingDirectory :: Lens.Lens' ImageConfig (Prelude.Maybe Prelude.Text)
imageConfig_workingDirectory = Lens.lens (\ImageConfig' {workingDirectory} -> workingDirectory) (\s@ImageConfig' {} a -> s {workingDirectory = a} :: ImageConfig)

-- | Specifies the entry point to their application, which is typically the
-- location of the runtime executable.
imageConfig_entryPoint :: Lens.Lens' ImageConfig (Prelude.Maybe [Prelude.Text])
imageConfig_entryPoint = Lens.lens (\ImageConfig' {entryPoint} -> entryPoint) (\s@ImageConfig' {} a -> s {entryPoint = a} :: ImageConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies parameters that you want to pass in with ENTRYPOINT.
imageConfig_command :: Lens.Lens' ImageConfig (Prelude.Maybe [Prelude.Text])
imageConfig_command = Lens.lens (\ImageConfig' {command} -> command) (\s@ImageConfig' {} a -> s {command = a} :: ImageConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ImageConfig where
  parseJSON =
    Prelude.withObject
      "ImageConfig"
      ( \x ->
          ImageConfig'
            Prelude.<$> (x Prelude..:? "WorkingDirectory")
            Prelude.<*> ( x Prelude..:? "EntryPoint"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Command" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ImageConfig

instance Prelude.NFData ImageConfig

instance Prelude.ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WorkingDirectory" Prelude..=)
              Prelude.<$> workingDirectory,
            ("EntryPoint" Prelude..=) Prelude.<$> entryPoint,
            ("Command" Prelude..=) Prelude.<$> command
          ]
      )
