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
-- Module      : Network.AWS.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
module Network.AWS.DeviceFarm.CreateDevicePool
  ( -- * Creating a Request
    CreateDevicePool (..),
    newCreateDevicePool,

    -- * Request Lenses
    createDevicePool_maxDevices,
    createDevicePool_description,
    createDevicePool_projectArn,
    createDevicePool_name,
    createDevicePool_rules,

    -- * Destructuring the Response
    CreateDevicePoolResponse (..),
    newCreateDevicePoolResponse,

    -- * Response Lenses
    createDevicePoolResponse_devicePool,
    createDevicePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create device pool operation.
--
-- /See:/ 'newCreateDevicePool' smart constructor.
data CreateDevicePool = CreateDevicePool'
  { -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and meet the criteria that
    -- you assign for the @rules@ parameter. Depending on how many devices meet
    -- these constraints, your device pool might contain fewer devices than the
    -- value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    maxDevices :: Core.Maybe Core.Int,
    -- | The device pool\'s description.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the project for the device pool.
    projectArn :: Core.Text,
    -- | The device pool\'s name.
    name :: Core.Text,
    -- | The device pool\'s rules.
    rules :: [Rule]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxDevices', 'createDevicePool_maxDevices' - The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
--
-- 'description', 'createDevicePool_description' - The device pool\'s description.
--
-- 'projectArn', 'createDevicePool_projectArn' - The ARN of the project for the device pool.
--
-- 'name', 'createDevicePool_name' - The device pool\'s name.
--
-- 'rules', 'createDevicePool_rules' - The device pool\'s rules.
newCreateDevicePool ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateDevicePool
newCreateDevicePool pProjectArn_ pName_ =
  CreateDevicePool'
    { maxDevices = Core.Nothing,
      description = Core.Nothing,
      projectArn = pProjectArn_,
      name = pName_,
      rules = Core.mempty
    }

-- | The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
createDevicePool_maxDevices :: Lens.Lens' CreateDevicePool (Core.Maybe Core.Int)
createDevicePool_maxDevices = Lens.lens (\CreateDevicePool' {maxDevices} -> maxDevices) (\s@CreateDevicePool' {} a -> s {maxDevices = a} :: CreateDevicePool)

-- | The device pool\'s description.
createDevicePool_description :: Lens.Lens' CreateDevicePool (Core.Maybe Core.Text)
createDevicePool_description = Lens.lens (\CreateDevicePool' {description} -> description) (\s@CreateDevicePool' {} a -> s {description = a} :: CreateDevicePool)

-- | The ARN of the project for the device pool.
createDevicePool_projectArn :: Lens.Lens' CreateDevicePool Core.Text
createDevicePool_projectArn = Lens.lens (\CreateDevicePool' {projectArn} -> projectArn) (\s@CreateDevicePool' {} a -> s {projectArn = a} :: CreateDevicePool)

-- | The device pool\'s name.
createDevicePool_name :: Lens.Lens' CreateDevicePool Core.Text
createDevicePool_name = Lens.lens (\CreateDevicePool' {name} -> name) (\s@CreateDevicePool' {} a -> s {name = a} :: CreateDevicePool)

-- | The device pool\'s rules.
createDevicePool_rules :: Lens.Lens' CreateDevicePool [Rule]
createDevicePool_rules = Lens.lens (\CreateDevicePool' {rules} -> rules) (\s@CreateDevicePool' {} a -> s {rules = a} :: CreateDevicePool) Core.. Lens._Coerce

instance Core.AWSRequest CreateDevicePool where
  type
    AWSResponse CreateDevicePool =
      CreateDevicePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDevicePoolResponse'
            Core.<$> (x Core..?> "devicePool")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDevicePool

instance Core.NFData CreateDevicePool

instance Core.ToHeaders CreateDevicePool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateDevicePool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDevicePool where
  toJSON CreateDevicePool' {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxDevices" Core..=) Core.<$> maxDevices,
            ("description" Core..=) Core.<$> description,
            Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("name" Core..= name),
            Core.Just ("rules" Core..= rules)
          ]
      )

instance Core.ToPath CreateDevicePool where
  toPath = Core.const "/"

instance Core.ToQuery CreateDevicePool where
  toQuery = Core.const Core.mempty

-- | Represents the result of a create device pool request.
--
-- /See:/ 'newCreateDevicePoolResponse' smart constructor.
data CreateDevicePoolResponse = CreateDevicePoolResponse'
  { -- | The newly created device pool.
    devicePool :: Core.Maybe DevicePool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devicePool', 'createDevicePoolResponse_devicePool' - The newly created device pool.
--
-- 'httpStatus', 'createDevicePoolResponse_httpStatus' - The response's http status code.
newCreateDevicePoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDevicePoolResponse
newCreateDevicePoolResponse pHttpStatus_ =
  CreateDevicePoolResponse'
    { devicePool =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created device pool.
createDevicePoolResponse_devicePool :: Lens.Lens' CreateDevicePoolResponse (Core.Maybe DevicePool)
createDevicePoolResponse_devicePool = Lens.lens (\CreateDevicePoolResponse' {devicePool} -> devicePool) (\s@CreateDevicePoolResponse' {} a -> s {devicePool = a} :: CreateDevicePoolResponse)

-- | The response's http status code.
createDevicePoolResponse_httpStatus :: Lens.Lens' CreateDevicePoolResponse Core.Int
createDevicePoolResponse_httpStatus = Lens.lens (\CreateDevicePoolResponse' {httpStatus} -> httpStatus) (\s@CreateDevicePoolResponse' {} a -> s {httpStatus = a} :: CreateDevicePoolResponse)

instance Core.NFData CreateDevicePoolResponse
