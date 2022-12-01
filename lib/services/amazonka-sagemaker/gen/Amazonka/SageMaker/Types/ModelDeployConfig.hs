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
-- Module      : Amazonka.SageMaker.Types.ModelDeployConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDeployConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies how to generate the endpoint name for an automatic one-click
-- Autopilot model deployment.
--
-- /See:/ 'newModelDeployConfig' smart constructor.
data ModelDeployConfig = ModelDeployConfig'
  { -- | Specifies the endpoint name to use for a one-click Autopilot model
    -- deployment if the endpoint name is not generated automatically.
    --
    -- Specify the @EndpointName@ if and only if you set
    -- @AutoGenerateEndpointName@ to @False@; otherwise a 400 error is thrown.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | Set to @True@ to automatically generate an endpoint name for a one-click
    -- Autopilot model deployment; set to @False@ otherwise. The default value
    -- is @False@.
    --
    -- If you set @AutoGenerateEndpointName@ to @True@, do not specify the
    -- @EndpointName@; otherwise a 400 error is thrown.
    autoGenerateEndpointName :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDeployConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'modelDeployConfig_endpointName' - Specifies the endpoint name to use for a one-click Autopilot model
-- deployment if the endpoint name is not generated automatically.
--
-- Specify the @EndpointName@ if and only if you set
-- @AutoGenerateEndpointName@ to @False@; otherwise a 400 error is thrown.
--
-- 'autoGenerateEndpointName', 'modelDeployConfig_autoGenerateEndpointName' - Set to @True@ to automatically generate an endpoint name for a one-click
-- Autopilot model deployment; set to @False@ otherwise. The default value
-- is @False@.
--
-- If you set @AutoGenerateEndpointName@ to @True@, do not specify the
-- @EndpointName@; otherwise a 400 error is thrown.
newModelDeployConfig ::
  ModelDeployConfig
newModelDeployConfig =
  ModelDeployConfig'
    { endpointName = Prelude.Nothing,
      autoGenerateEndpointName = Prelude.Nothing
    }

-- | Specifies the endpoint name to use for a one-click Autopilot model
-- deployment if the endpoint name is not generated automatically.
--
-- Specify the @EndpointName@ if and only if you set
-- @AutoGenerateEndpointName@ to @False@; otherwise a 400 error is thrown.
modelDeployConfig_endpointName :: Lens.Lens' ModelDeployConfig (Prelude.Maybe Prelude.Text)
modelDeployConfig_endpointName = Lens.lens (\ModelDeployConfig' {endpointName} -> endpointName) (\s@ModelDeployConfig' {} a -> s {endpointName = a} :: ModelDeployConfig)

-- | Set to @True@ to automatically generate an endpoint name for a one-click
-- Autopilot model deployment; set to @False@ otherwise. The default value
-- is @False@.
--
-- If you set @AutoGenerateEndpointName@ to @True@, do not specify the
-- @EndpointName@; otherwise a 400 error is thrown.
modelDeployConfig_autoGenerateEndpointName :: Lens.Lens' ModelDeployConfig (Prelude.Maybe Prelude.Bool)
modelDeployConfig_autoGenerateEndpointName = Lens.lens (\ModelDeployConfig' {autoGenerateEndpointName} -> autoGenerateEndpointName) (\s@ModelDeployConfig' {} a -> s {autoGenerateEndpointName = a} :: ModelDeployConfig)

instance Core.FromJSON ModelDeployConfig where
  parseJSON =
    Core.withObject
      "ModelDeployConfig"
      ( \x ->
          ModelDeployConfig'
            Prelude.<$> (x Core..:? "EndpointName")
            Prelude.<*> (x Core..:? "AutoGenerateEndpointName")
      )

instance Prelude.Hashable ModelDeployConfig where
  hashWithSalt _salt ModelDeployConfig' {..} =
    _salt `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` autoGenerateEndpointName

instance Prelude.NFData ModelDeployConfig where
  rnf ModelDeployConfig' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf autoGenerateEndpointName

instance Core.ToJSON ModelDeployConfig where
  toJSON ModelDeployConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndpointName" Core..=) Prelude.<$> endpointName,
            ("AutoGenerateEndpointName" Core..=)
              Prelude.<$> autoGenerateEndpointName
          ]
      )
