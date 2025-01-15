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
-- Module      : Amazonka.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
module Amazonka.DeviceFarm.CreateDevicePool
  ( -- * Creating a Request
    CreateDevicePool (..),
    newCreateDevicePool,

    -- * Request Lenses
    createDevicePool_description,
    createDevicePool_maxDevices,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the create device pool operation.
--
-- /See:/ 'newCreateDevicePool' smart constructor.
data CreateDevicePool = CreateDevicePool'
  { -- | The device pool\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and meet the criteria that
    -- you assign for the @rules@ parameter. Depending on how many devices meet
    -- these constraints, your device pool might contain fewer devices than the
    -- value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    maxDevices :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the project for the device pool.
    projectArn :: Prelude.Text,
    -- | The device pool\'s name.
    name :: Prelude.Text,
    -- | The device pool\'s rules.
    rules :: [Rule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDevicePool_description' - The device pool\'s description.
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
-- 'projectArn', 'createDevicePool_projectArn' - The ARN of the project for the device pool.
--
-- 'name', 'createDevicePool_name' - The device pool\'s name.
--
-- 'rules', 'createDevicePool_rules' - The device pool\'s rules.
newCreateDevicePool ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateDevicePool
newCreateDevicePool pProjectArn_ pName_ =
  CreateDevicePool'
    { description = Prelude.Nothing,
      maxDevices = Prelude.Nothing,
      projectArn = pProjectArn_,
      name = pName_,
      rules = Prelude.mempty
    }

-- | The device pool\'s description.
createDevicePool_description :: Lens.Lens' CreateDevicePool (Prelude.Maybe Prelude.Text)
createDevicePool_description = Lens.lens (\CreateDevicePool' {description} -> description) (\s@CreateDevicePool' {} a -> s {description = a} :: CreateDevicePool)

-- | The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
createDevicePool_maxDevices :: Lens.Lens' CreateDevicePool (Prelude.Maybe Prelude.Int)
createDevicePool_maxDevices = Lens.lens (\CreateDevicePool' {maxDevices} -> maxDevices) (\s@CreateDevicePool' {} a -> s {maxDevices = a} :: CreateDevicePool)

-- | The ARN of the project for the device pool.
createDevicePool_projectArn :: Lens.Lens' CreateDevicePool Prelude.Text
createDevicePool_projectArn = Lens.lens (\CreateDevicePool' {projectArn} -> projectArn) (\s@CreateDevicePool' {} a -> s {projectArn = a} :: CreateDevicePool)

-- | The device pool\'s name.
createDevicePool_name :: Lens.Lens' CreateDevicePool Prelude.Text
createDevicePool_name = Lens.lens (\CreateDevicePool' {name} -> name) (\s@CreateDevicePool' {} a -> s {name = a} :: CreateDevicePool)

-- | The device pool\'s rules.
createDevicePool_rules :: Lens.Lens' CreateDevicePool [Rule]
createDevicePool_rules = Lens.lens (\CreateDevicePool' {rules} -> rules) (\s@CreateDevicePool' {} a -> s {rules = a} :: CreateDevicePool) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDevicePool where
  type
    AWSResponse CreateDevicePool =
      CreateDevicePoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDevicePoolResponse'
            Prelude.<$> (x Data..?> "devicePool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDevicePool where
  hashWithSalt _salt CreateDevicePool' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxDevices
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateDevicePool where
  rnf CreateDevicePool' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf maxDevices `Prelude.seq`
        Prelude.rnf projectArn `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf rules

instance Data.ToHeaders CreateDevicePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.CreateDevicePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDevicePool where
  toJSON CreateDevicePool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("maxDevices" Data..=) Prelude.<$> maxDevices,
            Prelude.Just ("projectArn" Data..= projectArn),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("rules" Data..= rules)
          ]
      )

instance Data.ToPath CreateDevicePool where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDevicePool where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a create device pool request.
--
-- /See:/ 'newCreateDevicePoolResponse' smart constructor.
data CreateDevicePoolResponse = CreateDevicePoolResponse'
  { -- | The newly created device pool.
    devicePool :: Prelude.Maybe DevicePool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateDevicePoolResponse
newCreateDevicePoolResponse pHttpStatus_ =
  CreateDevicePoolResponse'
    { devicePool =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created device pool.
createDevicePoolResponse_devicePool :: Lens.Lens' CreateDevicePoolResponse (Prelude.Maybe DevicePool)
createDevicePoolResponse_devicePool = Lens.lens (\CreateDevicePoolResponse' {devicePool} -> devicePool) (\s@CreateDevicePoolResponse' {} a -> s {devicePool = a} :: CreateDevicePoolResponse)

-- | The response's http status code.
createDevicePoolResponse_httpStatus :: Lens.Lens' CreateDevicePoolResponse Prelude.Int
createDevicePoolResponse_httpStatus = Lens.lens (\CreateDevicePoolResponse' {httpStatus} -> httpStatus) (\s@CreateDevicePoolResponse' {} a -> s {httpStatus = a} :: CreateDevicePoolResponse)

instance Prelude.NFData CreateDevicePoolResponse where
  rnf CreateDevicePoolResponse' {..} =
    Prelude.rnf devicePool `Prelude.seq`
      Prelude.rnf httpStatus
