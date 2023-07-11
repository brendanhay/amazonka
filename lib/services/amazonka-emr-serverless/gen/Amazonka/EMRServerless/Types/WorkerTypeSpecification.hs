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
-- Module      : Amazonka.EMRServerless.Types.WorkerTypeSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.WorkerTypeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.ImageConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The specifications for a worker type.
--
-- /See:/ 'newWorkerTypeSpecification' smart constructor.
data WorkerTypeSpecification = WorkerTypeSpecification'
  { -- | The image configuration for a worker type.
    imageConfiguration :: Prelude.Maybe ImageConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerTypeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfiguration', 'workerTypeSpecification_imageConfiguration' - The image configuration for a worker type.
newWorkerTypeSpecification ::
  WorkerTypeSpecification
newWorkerTypeSpecification =
  WorkerTypeSpecification'
    { imageConfiguration =
        Prelude.Nothing
    }

-- | The image configuration for a worker type.
workerTypeSpecification_imageConfiguration :: Lens.Lens' WorkerTypeSpecification (Prelude.Maybe ImageConfiguration)
workerTypeSpecification_imageConfiguration = Lens.lens (\WorkerTypeSpecification' {imageConfiguration} -> imageConfiguration) (\s@WorkerTypeSpecification' {} a -> s {imageConfiguration = a} :: WorkerTypeSpecification)

instance Data.FromJSON WorkerTypeSpecification where
  parseJSON =
    Data.withObject
      "WorkerTypeSpecification"
      ( \x ->
          WorkerTypeSpecification'
            Prelude.<$> (x Data..:? "imageConfiguration")
      )

instance Prelude.Hashable WorkerTypeSpecification where
  hashWithSalt _salt WorkerTypeSpecification' {..} =
    _salt `Prelude.hashWithSalt` imageConfiguration

instance Prelude.NFData WorkerTypeSpecification where
  rnf WorkerTypeSpecification' {..} =
    Prelude.rnf imageConfiguration
