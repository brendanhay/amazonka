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
-- Module      : Network.AWS.SSM.DeregisterManagedInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the server or virtual machine from the list of registered
-- servers. You can reregister the instance again at any time. If you
-- don\'t plan to use Run Command on the server, we suggest uninstalling
-- SSM Agent first.
module Network.AWS.SSM.DeregisterManagedInstance
  ( -- * Creating a Request
    DeregisterManagedInstance (..),
    newDeregisterManagedInstance,

    -- * Request Lenses
    deregisterManagedInstance_instanceId,

    -- * Destructuring the Response
    DeregisterManagedInstanceResponse (..),
    newDeregisterManagedInstanceResponse,

    -- * Response Lenses
    deregisterManagedInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeregisterManagedInstance' smart constructor.
data DeregisterManagedInstance = DeregisterManagedInstance'
  { -- | The ID assigned to the managed instance when you registered it using the
    -- activation process.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterManagedInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deregisterManagedInstance_instanceId' - The ID assigned to the managed instance when you registered it using the
-- activation process.
newDeregisterManagedInstance ::
  -- | 'instanceId'
  Core.Text ->
  DeregisterManagedInstance
newDeregisterManagedInstance pInstanceId_ =
  DeregisterManagedInstance'
    { instanceId =
        pInstanceId_
    }

-- | The ID assigned to the managed instance when you registered it using the
-- activation process.
deregisterManagedInstance_instanceId :: Lens.Lens' DeregisterManagedInstance Core.Text
deregisterManagedInstance_instanceId = Lens.lens (\DeregisterManagedInstance' {instanceId} -> instanceId) (\s@DeregisterManagedInstance' {} a -> s {instanceId = a} :: DeregisterManagedInstance)

instance Core.AWSRequest DeregisterManagedInstance where
  type
    AWSResponse DeregisterManagedInstance =
      DeregisterManagedInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterManagedInstanceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterManagedInstance

instance Core.NFData DeregisterManagedInstance

instance Core.ToHeaders DeregisterManagedInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeregisterManagedInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterManagedInstance where
  toJSON DeregisterManagedInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("InstanceId" Core..= instanceId)]
      )

instance Core.ToPath DeregisterManagedInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterManagedInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterManagedInstanceResponse' smart constructor.
data DeregisterManagedInstanceResponse = DeregisterManagedInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterManagedInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterManagedInstanceResponse_httpStatus' - The response's http status code.
newDeregisterManagedInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterManagedInstanceResponse
newDeregisterManagedInstanceResponse pHttpStatus_ =
  DeregisterManagedInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterManagedInstanceResponse_httpStatus :: Lens.Lens' DeregisterManagedInstanceResponse Core.Int
deregisterManagedInstanceResponse_httpStatus = Lens.lens (\DeregisterManagedInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterManagedInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterManagedInstanceResponse)

instance
  Core.NFData
    DeregisterManagedInstanceResponse
