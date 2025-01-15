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
-- Module      : Amazonka.Lightsail.StopInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
-- When you start a stopped instance, Lightsail assigns a new public IP
-- address to the instance. To use the same IP address after stopping and
-- starting an instance, create a static IP address and attach it to the
-- instance. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/lightsail-create-static-ip Amazon Lightsail Developer Guide>.
--
-- The @stop instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.StopInstance
  ( -- * Creating a Request
    StopInstance (..),
    newStopInstance,

    -- * Request Lenses
    stopInstance_force,
    stopInstance_instanceName,

    -- * Destructuring the Response
    StopInstanceResponse (..),
    newStopInstanceResponse,

    -- * Response Lenses
    stopInstanceResponse_operations,
    stopInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopInstance' smart constructor.
data StopInstance = StopInstance'
  { -- | When set to @True@, forces a Lightsail instance that is stuck in a
    -- @stopping@ state to stop.
    --
    -- Only use the @force@ parameter if your instance is stuck in the
    -- @stopping@ state. In any other state, your instance should stop normally
    -- without adding this parameter to your API request.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the instance (a virtual private server) to stop.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'stopInstance_force' - When set to @True@, forces a Lightsail instance that is stuck in a
-- @stopping@ state to stop.
--
-- Only use the @force@ parameter if your instance is stuck in the
-- @stopping@ state. In any other state, your instance should stop normally
-- without adding this parameter to your API request.
--
-- 'instanceName', 'stopInstance_instanceName' - The name of the instance (a virtual private server) to stop.
newStopInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  StopInstance
newStopInstance pInstanceName_ =
  StopInstance'
    { force = Prelude.Nothing,
      instanceName = pInstanceName_
    }

-- | When set to @True@, forces a Lightsail instance that is stuck in a
-- @stopping@ state to stop.
--
-- Only use the @force@ parameter if your instance is stuck in the
-- @stopping@ state. In any other state, your instance should stop normally
-- without adding this parameter to your API request.
stopInstance_force :: Lens.Lens' StopInstance (Prelude.Maybe Prelude.Bool)
stopInstance_force = Lens.lens (\StopInstance' {force} -> force) (\s@StopInstance' {} a -> s {force = a} :: StopInstance)

-- | The name of the instance (a virtual private server) to stop.
stopInstance_instanceName :: Lens.Lens' StopInstance Prelude.Text
stopInstance_instanceName = Lens.lens (\StopInstance' {instanceName} -> instanceName) (\s@StopInstance' {} a -> s {instanceName = a} :: StopInstance)

instance Core.AWSRequest StopInstance where
  type AWSResponse StopInstance = StopInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopInstanceResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopInstance where
  hashWithSalt _salt StopInstance' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData StopInstance where
  rnf StopInstance' {..} =
    Prelude.rnf force `Prelude.seq`
      Prelude.rnf instanceName

instance Data.ToHeaders StopInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.StopInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopInstance where
  toJSON StopInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("force" Data..=) Prelude.<$> force,
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath StopInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StopInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'stopInstanceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'stopInstanceResponse_httpStatus' - The response's http status code.
newStopInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopInstanceResponse
newStopInstanceResponse pHttpStatus_ =
  StopInstanceResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
stopInstanceResponse_operations :: Lens.Lens' StopInstanceResponse (Prelude.Maybe [Operation])
stopInstanceResponse_operations = Lens.lens (\StopInstanceResponse' {operations} -> operations) (\s@StopInstanceResponse' {} a -> s {operations = a} :: StopInstanceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopInstanceResponse_httpStatus :: Lens.Lens' StopInstanceResponse Prelude.Int
stopInstanceResponse_httpStatus = Lens.lens (\StopInstanceResponse' {httpStatus} -> httpStatus) (\s@StopInstanceResponse' {} a -> s {httpStatus = a} :: StopInstanceResponse)

instance Prelude.NFData StopInstanceResponse where
  rnf StopInstanceResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
