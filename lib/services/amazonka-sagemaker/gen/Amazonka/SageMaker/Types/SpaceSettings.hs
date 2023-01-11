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
-- Module      : Amazonka.SageMaker.Types.SpaceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SpaceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings

-- | A collection of space settings.
--
-- /See:/ 'newSpaceSettings' smart constructor.
data SpaceSettings = SpaceSettings'
  { jupyterServerAppSettings :: Prelude.Maybe JupyterServerAppSettings,
    kernelGatewayAppSettings :: Prelude.Maybe KernelGatewayAppSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jupyterServerAppSettings', 'spaceSettings_jupyterServerAppSettings' - Undocumented member.
--
-- 'kernelGatewayAppSettings', 'spaceSettings_kernelGatewayAppSettings' - Undocumented member.
newSpaceSettings ::
  SpaceSettings
newSpaceSettings =
  SpaceSettings'
    { jupyterServerAppSettings =
        Prelude.Nothing,
      kernelGatewayAppSettings = Prelude.Nothing
    }

-- | Undocumented member.
spaceSettings_jupyterServerAppSettings :: Lens.Lens' SpaceSettings (Prelude.Maybe JupyterServerAppSettings)
spaceSettings_jupyterServerAppSettings = Lens.lens (\SpaceSettings' {jupyterServerAppSettings} -> jupyterServerAppSettings) (\s@SpaceSettings' {} a -> s {jupyterServerAppSettings = a} :: SpaceSettings)

-- | Undocumented member.
spaceSettings_kernelGatewayAppSettings :: Lens.Lens' SpaceSettings (Prelude.Maybe KernelGatewayAppSettings)
spaceSettings_kernelGatewayAppSettings = Lens.lens (\SpaceSettings' {kernelGatewayAppSettings} -> kernelGatewayAppSettings) (\s@SpaceSettings' {} a -> s {kernelGatewayAppSettings = a} :: SpaceSettings)

instance Data.FromJSON SpaceSettings where
  parseJSON =
    Data.withObject
      "SpaceSettings"
      ( \x ->
          SpaceSettings'
            Prelude.<$> (x Data..:? "JupyterServerAppSettings")
            Prelude.<*> (x Data..:? "KernelGatewayAppSettings")
      )

instance Prelude.Hashable SpaceSettings where
  hashWithSalt _salt SpaceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` jupyterServerAppSettings
      `Prelude.hashWithSalt` kernelGatewayAppSettings

instance Prelude.NFData SpaceSettings where
  rnf SpaceSettings' {..} =
    Prelude.rnf jupyterServerAppSettings
      `Prelude.seq` Prelude.rnf kernelGatewayAppSettings

instance Data.ToJSON SpaceSettings where
  toJSON SpaceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JupyterServerAppSettings" Data..=)
              Prelude.<$> jupyterServerAppSettings,
            ("KernelGatewayAppSettings" Data..=)
              Prelude.<$> kernelGatewayAppSettings
          ]
      )
