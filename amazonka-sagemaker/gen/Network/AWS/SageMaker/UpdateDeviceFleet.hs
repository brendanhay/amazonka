{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.UpdateDeviceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet of devices.
module Network.AWS.SageMaker.UpdateDeviceFleet
  ( -- * Creating a Request
    UpdateDeviceFleet (..),
    newUpdateDeviceFleet,

    -- * Request Lenses
    updateDeviceFleet_roleArn,
    updateDeviceFleet_description,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- * Destructuring the Response
    UpdateDeviceFleetResponse (..),
    newUpdateDeviceFleetResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateDeviceFleet' smart constructor.
data UpdateDeviceFleet = UpdateDeviceFleet'
  { -- | The Amazon Resource Name (ARN) of the device.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text,
    -- | Output configuration for storing sample data collected by the fleet.
    outputConfig :: EdgeOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateDeviceFleet_roleArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'description', 'updateDeviceFleet_description' - Description of the fleet.
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
    { roleArn = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceFleetName = pDeviceFleetName_,
      outputConfig = pOutputConfig_
    }

-- | The Amazon Resource Name (ARN) of the device.
updateDeviceFleet_roleArn :: Lens.Lens' UpdateDeviceFleet (Prelude.Maybe Prelude.Text)
updateDeviceFleet_roleArn = Lens.lens (\UpdateDeviceFleet' {roleArn} -> roleArn) (\s@UpdateDeviceFleet' {} a -> s {roleArn = a} :: UpdateDeviceFleet)

-- | Description of the fleet.
updateDeviceFleet_description :: Lens.Lens' UpdateDeviceFleet (Prelude.Maybe Prelude.Text)
updateDeviceFleet_description = Lens.lens (\UpdateDeviceFleet' {description} -> description) (\s@UpdateDeviceFleet' {} a -> s {description = a} :: UpdateDeviceFleet)

-- | The name of the fleet.
updateDeviceFleet_deviceFleetName :: Lens.Lens' UpdateDeviceFleet Prelude.Text
updateDeviceFleet_deviceFleetName = Lens.lens (\UpdateDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@UpdateDeviceFleet' {} a -> s {deviceFleetName = a} :: UpdateDeviceFleet)

-- | Output configuration for storing sample data collected by the fleet.
updateDeviceFleet_outputConfig :: Lens.Lens' UpdateDeviceFleet EdgeOutputConfig
updateDeviceFleet_outputConfig = Lens.lens (\UpdateDeviceFleet' {outputConfig} -> outputConfig) (\s@UpdateDeviceFleet' {} a -> s {outputConfig = a} :: UpdateDeviceFleet)

instance Prelude.AWSRequest UpdateDeviceFleet where
  type Rs UpdateDeviceFleet = UpdateDeviceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateDeviceFleetResponse'

instance Prelude.Hashable UpdateDeviceFleet

instance Prelude.NFData UpdateDeviceFleet

instance Prelude.ToHeaders UpdateDeviceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.UpdateDeviceFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDeviceFleet where
  toJSON UpdateDeviceFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just
              ("DeviceFleetName" Prelude..= deviceFleetName),
            Prelude.Just
              ("OutputConfig" Prelude..= outputConfig)
          ]
      )

instance Prelude.ToPath UpdateDeviceFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDeviceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceFleetResponse' smart constructor.
data UpdateDeviceFleetResponse = UpdateDeviceFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDeviceFleetResponse ::
  UpdateDeviceFleetResponse
newUpdateDeviceFleetResponse =
  UpdateDeviceFleetResponse'

instance Prelude.NFData UpdateDeviceFleetResponse
