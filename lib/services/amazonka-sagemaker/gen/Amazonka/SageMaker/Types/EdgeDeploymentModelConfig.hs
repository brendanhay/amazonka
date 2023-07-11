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
-- Module      : Amazonka.SageMaker.Types.EdgeDeploymentModelConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeDeploymentModelConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configuration of a model in a deployment.
--
-- /See:/ 'newEdgeDeploymentModelConfig' smart constructor.
data EdgeDeploymentModelConfig = EdgeDeploymentModelConfig'
  { -- | The name the device application uses to reference this model.
    modelHandle :: Prelude.Text,
    -- | The edge packaging job associated with this deployment.
    edgePackagingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeDeploymentModelConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelHandle', 'edgeDeploymentModelConfig_modelHandle' - The name the device application uses to reference this model.
--
-- 'edgePackagingJobName', 'edgeDeploymentModelConfig_edgePackagingJobName' - The edge packaging job associated with this deployment.
newEdgeDeploymentModelConfig ::
  -- | 'modelHandle'
  Prelude.Text ->
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  EdgeDeploymentModelConfig
newEdgeDeploymentModelConfig
  pModelHandle_
  pEdgePackagingJobName_ =
    EdgeDeploymentModelConfig'
      { modelHandle =
          pModelHandle_,
        edgePackagingJobName = pEdgePackagingJobName_
      }

-- | The name the device application uses to reference this model.
edgeDeploymentModelConfig_modelHandle :: Lens.Lens' EdgeDeploymentModelConfig Prelude.Text
edgeDeploymentModelConfig_modelHandle = Lens.lens (\EdgeDeploymentModelConfig' {modelHandle} -> modelHandle) (\s@EdgeDeploymentModelConfig' {} a -> s {modelHandle = a} :: EdgeDeploymentModelConfig)

-- | The edge packaging job associated with this deployment.
edgeDeploymentModelConfig_edgePackagingJobName :: Lens.Lens' EdgeDeploymentModelConfig Prelude.Text
edgeDeploymentModelConfig_edgePackagingJobName = Lens.lens (\EdgeDeploymentModelConfig' {edgePackagingJobName} -> edgePackagingJobName) (\s@EdgeDeploymentModelConfig' {} a -> s {edgePackagingJobName = a} :: EdgeDeploymentModelConfig)

instance Data.FromJSON EdgeDeploymentModelConfig where
  parseJSON =
    Data.withObject
      "EdgeDeploymentModelConfig"
      ( \x ->
          EdgeDeploymentModelConfig'
            Prelude.<$> (x Data..: "ModelHandle")
            Prelude.<*> (x Data..: "EdgePackagingJobName")
      )

instance Prelude.Hashable EdgeDeploymentModelConfig where
  hashWithSalt _salt EdgeDeploymentModelConfig' {..} =
    _salt
      `Prelude.hashWithSalt` modelHandle
      `Prelude.hashWithSalt` edgePackagingJobName

instance Prelude.NFData EdgeDeploymentModelConfig where
  rnf EdgeDeploymentModelConfig' {..} =
    Prelude.rnf modelHandle
      `Prelude.seq` Prelude.rnf edgePackagingJobName

instance Data.ToJSON EdgeDeploymentModelConfig where
  toJSON EdgeDeploymentModelConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ModelHandle" Data..= modelHandle),
            Prelude.Just
              ( "EdgePackagingJobName"
                  Data..= edgePackagingJobName
              )
          ]
      )
