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
-- Module      : Amazonka.ImageBuilder.Types.TargetContainerRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.TargetContainerRepository where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types.ContainerRepositoryService
import qualified Amazonka.Prelude as Prelude

-- | The container repository where the output container image is stored.
--
-- /See:/ 'newTargetContainerRepository' smart constructor.
data TargetContainerRepository = TargetContainerRepository'
  { -- | Specifies the service in which this image was registered.
    service :: ContainerRepositoryService,
    -- | The name of the container repository where the output container image is
    -- stored. This name is prefixed by the repository location.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetContainerRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'targetContainerRepository_service' - Specifies the service in which this image was registered.
--
-- 'repositoryName', 'targetContainerRepository_repositoryName' - The name of the container repository where the output container image is
-- stored. This name is prefixed by the repository location.
newTargetContainerRepository ::
  -- | 'service'
  ContainerRepositoryService ->
  -- | 'repositoryName'
  Prelude.Text ->
  TargetContainerRepository
newTargetContainerRepository
  pService_
  pRepositoryName_ =
    TargetContainerRepository'
      { service = pService_,
        repositoryName = pRepositoryName_
      }

-- | Specifies the service in which this image was registered.
targetContainerRepository_service :: Lens.Lens' TargetContainerRepository ContainerRepositoryService
targetContainerRepository_service = Lens.lens (\TargetContainerRepository' {service} -> service) (\s@TargetContainerRepository' {} a -> s {service = a} :: TargetContainerRepository)

-- | The name of the container repository where the output container image is
-- stored. This name is prefixed by the repository location.
targetContainerRepository_repositoryName :: Lens.Lens' TargetContainerRepository Prelude.Text
targetContainerRepository_repositoryName = Lens.lens (\TargetContainerRepository' {repositoryName} -> repositoryName) (\s@TargetContainerRepository' {} a -> s {repositoryName = a} :: TargetContainerRepository)

instance Core.FromJSON TargetContainerRepository where
  parseJSON =
    Core.withObject
      "TargetContainerRepository"
      ( \x ->
          TargetContainerRepository'
            Prelude.<$> (x Core..: "service")
            Prelude.<*> (x Core..: "repositoryName")
      )

instance Prelude.Hashable TargetContainerRepository where
  hashWithSalt _salt TargetContainerRepository' {..} =
    _salt `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData TargetContainerRepository where
  rnf TargetContainerRepository' {..} =
    Prelude.rnf service
      `Prelude.seq` Prelude.rnf repositoryName

instance Core.ToJSON TargetContainerRepository where
  toJSON TargetContainerRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("service" Core..= service),
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )
