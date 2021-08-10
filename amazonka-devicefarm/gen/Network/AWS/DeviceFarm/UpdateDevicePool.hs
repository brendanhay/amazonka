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
-- Module      : Network.AWS.DeviceFarm.UpdateDevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the name, description, and rules in a device pool given the
-- attributes and the pool ARN. Rule updates are all-or-nothing, meaning
-- they can only be updated as a whole (or not at all).
module Network.AWS.DeviceFarm.UpdateDevicePool
  ( -- * Creating a Request
    UpdateDevicePool (..),
    newUpdateDevicePool,

    -- * Request Lenses
    updateDevicePool_clearMaxDevices,
    updateDevicePool_rules,
    updateDevicePool_name,
    updateDevicePool_maxDevices,
    updateDevicePool_description,
    updateDevicePool_arn,

    -- * Destructuring the Response
    UpdateDevicePoolResponse (..),
    newUpdateDevicePoolResponse,

    -- * Response Lenses
    updateDevicePoolResponse_devicePool,
    updateDevicePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the update device pool operation.
--
-- /See:/ 'newUpdateDevicePool' smart constructor.
data UpdateDevicePool = UpdateDevicePool'
  { -- | Sets whether the @maxDevices@ parameter applies to your device pool. If
    -- you set this parameter to @true@, the @maxDevices@ parameter does not
    -- apply, and Device Farm does not limit the number of devices that it adds
    -- to your device pool. In this case, Device Farm adds all available
    -- devices that meet the criteria specified in the @rules@ parameter.
    --
    -- If you use this parameter in your request, you cannot use the
    -- @maxDevices@ parameter in the same request.
    clearMaxDevices :: Prelude.Maybe Prelude.Bool,
    -- | Represents the rules to modify for the device pool. Updating rules is
    -- optional. If you update rules for your request, the update replaces the
    -- existing rules.
    rules :: Prelude.Maybe [Rule],
    -- | A string that represents the name of the device pool to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and that meet the criteria
    -- that you assign for the @rules@ parameter. Depending on how many devices
    -- meet these constraints, your device pool might contain fewer devices
    -- than the value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    --
    -- If you use this parameter in your request, you cannot use the
    -- @clearMaxDevices@ parameter in the same request.
    maxDevices :: Prelude.Maybe Prelude.Int,
    -- | A description of the device pool to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Device Farm device pool to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clearMaxDevices', 'updateDevicePool_clearMaxDevices' - Sets whether the @maxDevices@ parameter applies to your device pool. If
-- you set this parameter to @true@, the @maxDevices@ parameter does not
-- apply, and Device Farm does not limit the number of devices that it adds
-- to your device pool. In this case, Device Farm adds all available
-- devices that meet the criteria specified in the @rules@ parameter.
--
-- If you use this parameter in your request, you cannot use the
-- @maxDevices@ parameter in the same request.
--
-- 'rules', 'updateDevicePool_rules' - Represents the rules to modify for the device pool. Updating rules is
-- optional. If you update rules for your request, the update replaces the
-- existing rules.
--
-- 'name', 'updateDevicePool_name' - A string that represents the name of the device pool to update.
--
-- 'maxDevices', 'updateDevicePool_maxDevices' - The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and that meet the criteria
-- that you assign for the @rules@ parameter. Depending on how many devices
-- meet these constraints, your device pool might contain fewer devices
-- than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
--
-- If you use this parameter in your request, you cannot use the
-- @clearMaxDevices@ parameter in the same request.
--
-- 'description', 'updateDevicePool_description' - A description of the device pool to update.
--
-- 'arn', 'updateDevicePool_arn' - The Amazon Resource Name (ARN) of the Device Farm device pool to update.
newUpdateDevicePool ::
  -- | 'arn'
  Prelude.Text ->
  UpdateDevicePool
newUpdateDevicePool pArn_ =
  UpdateDevicePool'
    { clearMaxDevices =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      name = Prelude.Nothing,
      maxDevices = Prelude.Nothing,
      description = Prelude.Nothing,
      arn = pArn_
    }

-- | Sets whether the @maxDevices@ parameter applies to your device pool. If
-- you set this parameter to @true@, the @maxDevices@ parameter does not
-- apply, and Device Farm does not limit the number of devices that it adds
-- to your device pool. In this case, Device Farm adds all available
-- devices that meet the criteria specified in the @rules@ parameter.
--
-- If you use this parameter in your request, you cannot use the
-- @maxDevices@ parameter in the same request.
updateDevicePool_clearMaxDevices :: Lens.Lens' UpdateDevicePool (Prelude.Maybe Prelude.Bool)
updateDevicePool_clearMaxDevices = Lens.lens (\UpdateDevicePool' {clearMaxDevices} -> clearMaxDevices) (\s@UpdateDevicePool' {} a -> s {clearMaxDevices = a} :: UpdateDevicePool)

-- | Represents the rules to modify for the device pool. Updating rules is
-- optional. If you update rules for your request, the update replaces the
-- existing rules.
updateDevicePool_rules :: Lens.Lens' UpdateDevicePool (Prelude.Maybe [Rule])
updateDevicePool_rules = Lens.lens (\UpdateDevicePool' {rules} -> rules) (\s@UpdateDevicePool' {} a -> s {rules = a} :: UpdateDevicePool) Prelude.. Lens.mapping Lens._Coerce

-- | A string that represents the name of the device pool to update.
updateDevicePool_name :: Lens.Lens' UpdateDevicePool (Prelude.Maybe Prelude.Text)
updateDevicePool_name = Lens.lens (\UpdateDevicePool' {name} -> name) (\s@UpdateDevicePool' {} a -> s {name = a} :: UpdateDevicePool)

-- | The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and that meet the criteria
-- that you assign for the @rules@ parameter. Depending on how many devices
-- meet these constraints, your device pool might contain fewer devices
-- than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
--
-- If you use this parameter in your request, you cannot use the
-- @clearMaxDevices@ parameter in the same request.
updateDevicePool_maxDevices :: Lens.Lens' UpdateDevicePool (Prelude.Maybe Prelude.Int)
updateDevicePool_maxDevices = Lens.lens (\UpdateDevicePool' {maxDevices} -> maxDevices) (\s@UpdateDevicePool' {} a -> s {maxDevices = a} :: UpdateDevicePool)

-- | A description of the device pool to update.
updateDevicePool_description :: Lens.Lens' UpdateDevicePool (Prelude.Maybe Prelude.Text)
updateDevicePool_description = Lens.lens (\UpdateDevicePool' {description} -> description) (\s@UpdateDevicePool' {} a -> s {description = a} :: UpdateDevicePool)

-- | The Amazon Resource Name (ARN) of the Device Farm device pool to update.
updateDevicePool_arn :: Lens.Lens' UpdateDevicePool Prelude.Text
updateDevicePool_arn = Lens.lens (\UpdateDevicePool' {arn} -> arn) (\s@UpdateDevicePool' {} a -> s {arn = a} :: UpdateDevicePool)

instance Core.AWSRequest UpdateDevicePool where
  type
    AWSResponse UpdateDevicePool =
      UpdateDevicePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDevicePoolResponse'
            Prelude.<$> (x Core..?> "devicePool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDevicePool

instance Prelude.NFData UpdateDevicePool

instance Core.ToHeaders UpdateDevicePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateDevicePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDevicePool where
  toJSON UpdateDevicePool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clearMaxDevices" Core..=)
              Prelude.<$> clearMaxDevices,
            ("rules" Core..=) Prelude.<$> rules,
            ("name" Core..=) Prelude.<$> name,
            ("maxDevices" Core..=) Prelude.<$> maxDevices,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateDevicePool where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDevicePool where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of an update device pool request.
--
-- /See:/ 'newUpdateDevicePoolResponse' smart constructor.
data UpdateDevicePoolResponse = UpdateDevicePoolResponse'
  { -- | The device pool you just updated.
    devicePool :: Prelude.Maybe DevicePool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devicePool', 'updateDevicePoolResponse_devicePool' - The device pool you just updated.
--
-- 'httpStatus', 'updateDevicePoolResponse_httpStatus' - The response's http status code.
newUpdateDevicePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDevicePoolResponse
newUpdateDevicePoolResponse pHttpStatus_ =
  UpdateDevicePoolResponse'
    { devicePool =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device pool you just updated.
updateDevicePoolResponse_devicePool :: Lens.Lens' UpdateDevicePoolResponse (Prelude.Maybe DevicePool)
updateDevicePoolResponse_devicePool = Lens.lens (\UpdateDevicePoolResponse' {devicePool} -> devicePool) (\s@UpdateDevicePoolResponse' {} a -> s {devicePool = a} :: UpdateDevicePoolResponse)

-- | The response's http status code.
updateDevicePoolResponse_httpStatus :: Lens.Lens' UpdateDevicePoolResponse Prelude.Int
updateDevicePoolResponse_httpStatus = Lens.lens (\UpdateDevicePoolResponse' {httpStatus} -> httpStatus) (\s@UpdateDevicePoolResponse' {} a -> s {httpStatus = a} :: UpdateDevicePoolResponse)

instance Prelude.NFData UpdateDevicePoolResponse
