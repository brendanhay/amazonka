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
-- Module      : Amazonka.EMRServerless.Types.WorkerTypeSpecificationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.WorkerTypeSpecificationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.ImageConfigurationInput
import qualified Amazonka.Prelude as Prelude

-- | The specifications for a worker type.
--
-- /See:/ 'newWorkerTypeSpecificationInput' smart constructor.
data WorkerTypeSpecificationInput = WorkerTypeSpecificationInput'
  { -- | The image configuration for a worker type.
    imageConfiguration :: Prelude.Maybe ImageConfigurationInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerTypeSpecificationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfiguration', 'workerTypeSpecificationInput_imageConfiguration' - The image configuration for a worker type.
newWorkerTypeSpecificationInput ::
  WorkerTypeSpecificationInput
newWorkerTypeSpecificationInput =
  WorkerTypeSpecificationInput'
    { imageConfiguration =
        Prelude.Nothing
    }

-- | The image configuration for a worker type.
workerTypeSpecificationInput_imageConfiguration :: Lens.Lens' WorkerTypeSpecificationInput (Prelude.Maybe ImageConfigurationInput)
workerTypeSpecificationInput_imageConfiguration = Lens.lens (\WorkerTypeSpecificationInput' {imageConfiguration} -> imageConfiguration) (\s@WorkerTypeSpecificationInput' {} a -> s {imageConfiguration = a} :: WorkerTypeSpecificationInput)

instance
  Prelude.Hashable
    WorkerTypeSpecificationInput
  where
  hashWithSalt _salt WorkerTypeSpecificationInput' {..} =
    _salt `Prelude.hashWithSalt` imageConfiguration

instance Prelude.NFData WorkerTypeSpecificationInput where
  rnf WorkerTypeSpecificationInput' {..} =
    Prelude.rnf imageConfiguration

instance Data.ToJSON WorkerTypeSpecificationInput where
  toJSON WorkerTypeSpecificationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("imageConfiguration" Data..=)
              Prelude.<$> imageConfiguration
          ]
      )
