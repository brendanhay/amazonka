{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.UpdateDeviceFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet of devices.
module Amazonka.SageMaker.UpdateDeviceFleet
  ( -- * Creating a Request
    UpdateDeviceFleet (..),
    newUpdateDeviceFleet,

    -- * Request Lenses
    updateDeviceFleet_description,
    updateDeviceFleet_enableIotRoleAlias,
    updateDeviceFleet_roleArn,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- * Destructuring the Response
    UpdateDeviceFleetResponse (..),
    newUpdateDeviceFleetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateDeviceFleet' smart constructor.
data UpdateDeviceFleet = UpdateDeviceFleet'
  { -- | Description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether to create an Amazon Web Services IoT Role Alias during device
    -- fleet creation. The name of the role alias generated will match this
    -- pattern: \"SageMakerEdge-{DeviceFleetName}\".
    --
    -- For example, if your device fleet is called \"demo-fleet\", the name of
    -- the role alias will be \"SageMakerEdge-demo-fleet\".
    enableIotRoleAlias :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the device.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text,
    -- | Output configuration for storing sample data collected by the fleet.
    outputConfig :: EdgeOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDeviceFleet_description' - Description of the fleet.
--
-- 'enableIotRoleAlias', 'updateDeviceFleet_enableIotRoleAlias' - Whether to create an Amazon Web Services IoT Role Alias during device
-- fleet creation. The name of the role alias generated will match this
-- pattern: \"SageMakerEdge-{DeviceFleetName}\".
--
-- For example, if your device fleet is called \"demo-fleet\", the name of
-- the role alias will be \"SageMakerEdge-demo-fleet\".
--
-- 'roleArn', 'updateDeviceFleet_roleArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'deviceFleetName', 'updateDeviceFleet_deviceFleetName' - The name of the fleet.
--
-- 'outputConfig', 'updateDeviceFleet_outputConfig' - Output configuration for storing sample data collected by the fleet.
newUpdateDeviceFleet ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  UpdateDeviceFleet
newUpdateDeviceFleet pDeviceFleetName_ pOutputConfig_ =
  UpdateDeviceFleet'
    { description = Prelude.Nothing,
      enableIotRoleAlias = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      deviceFleetName = pDeviceFleetName_,
      outputConfig = pOutputConfig_
    }

-- | Description of the fleet.
updateDeviceFleet_description :: Lens.Lens' UpdateDeviceFleet (Prelude.Maybe Prelude.Text)
updateDeviceFleet_description = Lens.lens (\UpdateDeviceFleet' {description} -> description) (\s@UpdateDeviceFleet' {} a -> s {description = a} :: UpdateDeviceFleet)

-- | Whether to create an Amazon Web Services IoT Role Alias during device
-- fleet creation. The name of the role alias generated will match this
-- pattern: \"SageMakerEdge-{DeviceFleetName}\".
--
-- For example, if your device fleet is called \"demo-fleet\", the name of
-- the role alias will be \"SageMakerEdge-demo-fleet\".
updateDeviceFleet_enableIotRoleAlias :: Lens.Lens' UpdateDeviceFleet (Prelude.Maybe Prelude.Bool)
updateDeviceFleet_enableIotRoleAlias = Lens.lens (\UpdateDeviceFleet' {enableIotRoleAlias} -> enableIotRoleAlias) (\s@UpdateDeviceFleet' {} a -> s {enableIotRoleAlias = a} :: UpdateDeviceFleet)

-- | The Amazon Resource Name (ARN) of the device.
updateDeviceFleet_roleArn :: Lens.Lens' UpdateDeviceFleet (Prelude.Maybe Prelude.Text)
updateDeviceFleet_roleArn = Lens.lens (\UpdateDeviceFleet' {roleArn} -> roleArn) (\s@UpdateDeviceFleet' {} a -> s {roleArn = a} :: UpdateDeviceFleet)

-- | The name of the fleet.
updateDeviceFleet_deviceFleetName :: Lens.Lens' UpdateDeviceFleet Prelude.Text
updateDeviceFleet_deviceFleetName = Lens.lens (\UpdateDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@UpdateDeviceFleet' {} a -> s {deviceFleetName = a} :: UpdateDeviceFleet)

-- | Output configuration for storing sample data collected by the fleet.
updateDeviceFleet_outputConfig :: Lens.Lens' UpdateDeviceFleet EdgeOutputConfig
updateDeviceFleet_outputConfig = Lens.lens (\UpdateDeviceFleet' {outputConfig} -> outputConfig) (\s@UpdateDeviceFleet' {} a -> s {outputConfig = a} :: UpdateDeviceFleet)

instance Core.AWSRequest UpdateDeviceFleet where
  type
    AWSResponse UpdateDeviceFleet =
      UpdateDeviceFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDeviceFleetResponse'

instance Prelude.Hashable UpdateDeviceFleet where
  hashWithSalt _salt UpdateDeviceFleet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enableIotRoleAlias
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData UpdateDeviceFleet where
  rnf UpdateDeviceFleet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf enableIotRoleAlias
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders UpdateDeviceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateDeviceFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeviceFleet where
  toJSON UpdateDeviceFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EnableIotRoleAlias" Data..=)
              Prelude.<$> enableIotRoleAlias,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath UpdateDeviceFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDeviceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceFleetResponse' smart constructor.
data UpdateDeviceFleetResponse = UpdateDeviceFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDeviceFleetResponse ::
  UpdateDeviceFleetResponse
newUpdateDeviceFleetResponse =
  UpdateDeviceFleetResponse'

instance Prelude.NFData UpdateDeviceFleetResponse where
  rnf _ = ()
