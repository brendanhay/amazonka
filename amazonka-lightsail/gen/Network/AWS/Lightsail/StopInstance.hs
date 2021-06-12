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
-- Module      : Network.AWS.Lightsail.StopInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
-- When you start a stopped instance, Lightsail assigns a new public IP
-- address to the instance. To use the same IP address after stopping and
-- starting an instance, create a static IP address and attach it to the
-- instance. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/lightsail-create-static-ip Lightsail Dev Guide>.
--
-- The @stop instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.StopInstance
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopInstance' smart constructor.
data StopInstance = StopInstance'
  { -- | When set to @True@, forces a Lightsail instance that is stuck in a
    -- @stopping@ state to stop.
    --
    -- Only use the @force@ parameter if your instance is stuck in the
    -- @stopping@ state. In any other state, your instance should stop normally
    -- without adding this parameter to your API request.
    force :: Core.Maybe Core.Bool,
    -- | The name of the instance (a virtual private server) to stop.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopInstance
newStopInstance pInstanceName_ =
  StopInstance'
    { force = Core.Nothing,
      instanceName = pInstanceName_
    }

-- | When set to @True@, forces a Lightsail instance that is stuck in a
-- @stopping@ state to stop.
--
-- Only use the @force@ parameter if your instance is stuck in the
-- @stopping@ state. In any other state, your instance should stop normally
-- without adding this parameter to your API request.
stopInstance_force :: Lens.Lens' StopInstance (Core.Maybe Core.Bool)
stopInstance_force = Lens.lens (\StopInstance' {force} -> force) (\s@StopInstance' {} a -> s {force = a} :: StopInstance)

-- | The name of the instance (a virtual private server) to stop.
stopInstance_instanceName :: Lens.Lens' StopInstance Core.Text
stopInstance_instanceName = Lens.lens (\StopInstance' {instanceName} -> instanceName) (\s@StopInstance' {} a -> s {instanceName = a} :: StopInstance)

instance Core.AWSRequest StopInstance where
  type AWSResponse StopInstance = StopInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopInstanceResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopInstance

instance Core.NFData StopInstance

instance Core.ToHeaders StopInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.StopInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopInstance where
  toJSON StopInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("force" Core..=) Core.<$> force,
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath StopInstance where
  toPath = Core.const "/"

instance Core.ToQuery StopInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopInstanceResponse
newStopInstanceResponse pHttpStatus_ =
  StopInstanceResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
stopInstanceResponse_operations :: Lens.Lens' StopInstanceResponse (Core.Maybe [Operation])
stopInstanceResponse_operations = Lens.lens (\StopInstanceResponse' {operations} -> operations) (\s@StopInstanceResponse' {} a -> s {operations = a} :: StopInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopInstanceResponse_httpStatus :: Lens.Lens' StopInstanceResponse Core.Int
stopInstanceResponse_httpStatus = Lens.lens (\StopInstanceResponse' {httpStatus} -> httpStatus) (\s@StopInstanceResponse' {} a -> s {httpStatus = a} :: StopInstanceResponse)

instance Core.NFData StopInstanceResponse
