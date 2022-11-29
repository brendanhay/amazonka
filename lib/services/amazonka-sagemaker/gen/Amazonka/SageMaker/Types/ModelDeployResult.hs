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
-- Module      : Amazonka.SageMaker.Types.ModelDeployResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDeployResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the endpoint of the model deployment.
--
-- /See:/ 'newModelDeployResult' smart constructor.
data ModelDeployResult = ModelDeployResult'
  { -- | The name of the endpoint to which the model has been deployed.
    --
    -- If model deployment fails, this field is omitted from the response.
    endpointName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDeployResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'modelDeployResult_endpointName' - The name of the endpoint to which the model has been deployed.
--
-- If model deployment fails, this field is omitted from the response.
newModelDeployResult ::
  ModelDeployResult
newModelDeployResult =
  ModelDeployResult' {endpointName = Prelude.Nothing}

-- | The name of the endpoint to which the model has been deployed.
--
-- If model deployment fails, this field is omitted from the response.
modelDeployResult_endpointName :: Lens.Lens' ModelDeployResult (Prelude.Maybe Prelude.Text)
modelDeployResult_endpointName = Lens.lens (\ModelDeployResult' {endpointName} -> endpointName) (\s@ModelDeployResult' {} a -> s {endpointName = a} :: ModelDeployResult)

instance Core.FromJSON ModelDeployResult where
  parseJSON =
    Core.withObject
      "ModelDeployResult"
      ( \x ->
          ModelDeployResult'
            Prelude.<$> (x Core..:? "EndpointName")
      )

instance Prelude.Hashable ModelDeployResult where
  hashWithSalt _salt ModelDeployResult' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData ModelDeployResult where
  rnf ModelDeployResult' {..} = Prelude.rnf endpointName
