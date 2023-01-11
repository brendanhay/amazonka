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
-- Module      : Amazonka.AppRunner.Types.SourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.SourceConfiguration where

import Amazonka.AppRunner.Types.AuthenticationConfiguration
import Amazonka.AppRunner.Types.CodeRepository
import Amazonka.AppRunner.Types.ImageRepository
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the source deployed to an App Runner service. It can be a code
-- or an image repository.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | Describes the resources that are needed to authenticate access to some
    -- source repositories.
    authenticationConfiguration :: Prelude.Maybe AuthenticationConfiguration,
    -- | If @true@, continuous integration from the source repository is enabled
    -- for the App Runner service. Each repository change (including any source
    -- code commit or new image version) starts a deployment.
    --
    -- Default: App Runner sets to @false@ for a source image that uses an ECR
    -- Public repository or an ECR repository that\'s in an Amazon Web Services
    -- account other than the one that the service is in. App Runner sets to
    -- @true@ in all other cases (which currently include a source code
    -- repository or a source image using a same-account ECR repository).
    autoDeploymentsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The description of a source code repository.
    --
    -- You must provide either this member or @ImageRepository@ (but not both).
    codeRepository :: Prelude.Maybe CodeRepository,
    -- | The description of a source image repository.
    --
    -- You must provide either this member or @CodeRepository@ (but not both).
    imageRepository :: Prelude.Maybe ImageRepository
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationConfiguration', 'sourceConfiguration_authenticationConfiguration' - Describes the resources that are needed to authenticate access to some
-- source repositories.
--
-- 'autoDeploymentsEnabled', 'sourceConfiguration_autoDeploymentsEnabled' - If @true@, continuous integration from the source repository is enabled
-- for the App Runner service. Each repository change (including any source
-- code commit or new image version) starts a deployment.
--
-- Default: App Runner sets to @false@ for a source image that uses an ECR
-- Public repository or an ECR repository that\'s in an Amazon Web Services
-- account other than the one that the service is in. App Runner sets to
-- @true@ in all other cases (which currently include a source code
-- repository or a source image using a same-account ECR repository).
--
-- 'codeRepository', 'sourceConfiguration_codeRepository' - The description of a source code repository.
--
-- You must provide either this member or @ImageRepository@ (but not both).
--
-- 'imageRepository', 'sourceConfiguration_imageRepository' - The description of a source image repository.
--
-- You must provide either this member or @CodeRepository@ (but not both).
newSourceConfiguration ::
  SourceConfiguration
newSourceConfiguration =
  SourceConfiguration'
    { authenticationConfiguration =
        Prelude.Nothing,
      autoDeploymentsEnabled = Prelude.Nothing,
      codeRepository = Prelude.Nothing,
      imageRepository = Prelude.Nothing
    }

-- | Describes the resources that are needed to authenticate access to some
-- source repositories.
sourceConfiguration_authenticationConfiguration :: Lens.Lens' SourceConfiguration (Prelude.Maybe AuthenticationConfiguration)
sourceConfiguration_authenticationConfiguration = Lens.lens (\SourceConfiguration' {authenticationConfiguration} -> authenticationConfiguration) (\s@SourceConfiguration' {} a -> s {authenticationConfiguration = a} :: SourceConfiguration)

-- | If @true@, continuous integration from the source repository is enabled
-- for the App Runner service. Each repository change (including any source
-- code commit or new image version) starts a deployment.
--
-- Default: App Runner sets to @false@ for a source image that uses an ECR
-- Public repository or an ECR repository that\'s in an Amazon Web Services
-- account other than the one that the service is in. App Runner sets to
-- @true@ in all other cases (which currently include a source code
-- repository or a source image using a same-account ECR repository).
sourceConfiguration_autoDeploymentsEnabled :: Lens.Lens' SourceConfiguration (Prelude.Maybe Prelude.Bool)
sourceConfiguration_autoDeploymentsEnabled = Lens.lens (\SourceConfiguration' {autoDeploymentsEnabled} -> autoDeploymentsEnabled) (\s@SourceConfiguration' {} a -> s {autoDeploymentsEnabled = a} :: SourceConfiguration)

-- | The description of a source code repository.
--
-- You must provide either this member or @ImageRepository@ (but not both).
sourceConfiguration_codeRepository :: Lens.Lens' SourceConfiguration (Prelude.Maybe CodeRepository)
sourceConfiguration_codeRepository = Lens.lens (\SourceConfiguration' {codeRepository} -> codeRepository) (\s@SourceConfiguration' {} a -> s {codeRepository = a} :: SourceConfiguration)

-- | The description of a source image repository.
--
-- You must provide either this member or @CodeRepository@ (but not both).
sourceConfiguration_imageRepository :: Lens.Lens' SourceConfiguration (Prelude.Maybe ImageRepository)
sourceConfiguration_imageRepository = Lens.lens (\SourceConfiguration' {imageRepository} -> imageRepository) (\s@SourceConfiguration' {} a -> s {imageRepository = a} :: SourceConfiguration)

instance Data.FromJSON SourceConfiguration where
  parseJSON =
    Data.withObject
      "SourceConfiguration"
      ( \x ->
          SourceConfiguration'
            Prelude.<$> (x Data..:? "AuthenticationConfiguration")
            Prelude.<*> (x Data..:? "AutoDeploymentsEnabled")
            Prelude.<*> (x Data..:? "CodeRepository")
            Prelude.<*> (x Data..:? "ImageRepository")
      )

instance Prelude.Hashable SourceConfiguration where
  hashWithSalt _salt SourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationConfiguration
      `Prelude.hashWithSalt` autoDeploymentsEnabled
      `Prelude.hashWithSalt` codeRepository
      `Prelude.hashWithSalt` imageRepository

instance Prelude.NFData SourceConfiguration where
  rnf SourceConfiguration' {..} =
    Prelude.rnf authenticationConfiguration
      `Prelude.seq` Prelude.rnf autoDeploymentsEnabled
      `Prelude.seq` Prelude.rnf codeRepository
      `Prelude.seq` Prelude.rnf imageRepository

instance Data.ToJSON SourceConfiguration where
  toJSON SourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationConfiguration" Data..=)
              Prelude.<$> authenticationConfiguration,
            ("AutoDeploymentsEnabled" Data..=)
              Prelude.<$> autoDeploymentsEnabled,
            ("CodeRepository" Data..=)
              Prelude.<$> codeRepository,
            ("ImageRepository" Data..=)
              Prelude.<$> imageRepository
          ]
      )
