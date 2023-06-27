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
-- Module      : Amazonka.EMRContainers.Types.ContainerLogRotationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ContainerLogRotationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for container log rotation.
--
-- /See:/ 'newContainerLogRotationConfiguration' smart constructor.
data ContainerLogRotationConfiguration = ContainerLogRotationConfiguration'
  { -- | The file size at which to rotate logs. Minimum of 2KB, Maximum of 2GB.
    rotationSize :: Prelude.Text,
    -- | The number of files to keep in container after rotation.
    maxFilesToKeep :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerLogRotationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationSize', 'containerLogRotationConfiguration_rotationSize' - The file size at which to rotate logs. Minimum of 2KB, Maximum of 2GB.
--
-- 'maxFilesToKeep', 'containerLogRotationConfiguration_maxFilesToKeep' - The number of files to keep in container after rotation.
newContainerLogRotationConfiguration ::
  -- | 'rotationSize'
  Prelude.Text ->
  -- | 'maxFilesToKeep'
  Prelude.Natural ->
  ContainerLogRotationConfiguration
newContainerLogRotationConfiguration
  pRotationSize_
  pMaxFilesToKeep_ =
    ContainerLogRotationConfiguration'
      { rotationSize =
          pRotationSize_,
        maxFilesToKeep = pMaxFilesToKeep_
      }

-- | The file size at which to rotate logs. Minimum of 2KB, Maximum of 2GB.
containerLogRotationConfiguration_rotationSize :: Lens.Lens' ContainerLogRotationConfiguration Prelude.Text
containerLogRotationConfiguration_rotationSize = Lens.lens (\ContainerLogRotationConfiguration' {rotationSize} -> rotationSize) (\s@ContainerLogRotationConfiguration' {} a -> s {rotationSize = a} :: ContainerLogRotationConfiguration)

-- | The number of files to keep in container after rotation.
containerLogRotationConfiguration_maxFilesToKeep :: Lens.Lens' ContainerLogRotationConfiguration Prelude.Natural
containerLogRotationConfiguration_maxFilesToKeep = Lens.lens (\ContainerLogRotationConfiguration' {maxFilesToKeep} -> maxFilesToKeep) (\s@ContainerLogRotationConfiguration' {} a -> s {maxFilesToKeep = a} :: ContainerLogRotationConfiguration)

instance
  Data.FromJSON
    ContainerLogRotationConfiguration
  where
  parseJSON =
    Data.withObject
      "ContainerLogRotationConfiguration"
      ( \x ->
          ContainerLogRotationConfiguration'
            Prelude.<$> (x Data..: "rotationSize")
            Prelude.<*> (x Data..: "maxFilesToKeep")
      )

instance
  Prelude.Hashable
    ContainerLogRotationConfiguration
  where
  hashWithSalt
    _salt
    ContainerLogRotationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` rotationSize
        `Prelude.hashWithSalt` maxFilesToKeep

instance
  Prelude.NFData
    ContainerLogRotationConfiguration
  where
  rnf ContainerLogRotationConfiguration' {..} =
    Prelude.rnf rotationSize
      `Prelude.seq` Prelude.rnf maxFilesToKeep

instance
  Data.ToJSON
    ContainerLogRotationConfiguration
  where
  toJSON ContainerLogRotationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("rotationSize" Data..= rotationSize),
            Prelude.Just
              ("maxFilesToKeep" Data..= maxFilesToKeep)
          ]
      )
