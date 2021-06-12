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
-- Module      : Network.AWS.Lightsail.GetInstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a specific instance. Works on one instance at a
-- time.
module Network.AWS.Lightsail.GetInstanceState
  ( -- * Creating a Request
    GetInstanceState (..),
    newGetInstanceState,

    -- * Request Lenses
    getInstanceState_instanceName,

    -- * Destructuring the Response
    GetInstanceStateResponse (..),
    newGetInstanceStateResponse,

    -- * Response Lenses
    getInstanceStateResponse_state,
    getInstanceStateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstanceState' smart constructor.
data GetInstanceState = GetInstanceState'
  { -- | The name of the instance to get state information about.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getInstanceState_instanceName' - The name of the instance to get state information about.
newGetInstanceState ::
  -- | 'instanceName'
  Core.Text ->
  GetInstanceState
newGetInstanceState pInstanceName_ =
  GetInstanceState' {instanceName = pInstanceName_}

-- | The name of the instance to get state information about.
getInstanceState_instanceName :: Lens.Lens' GetInstanceState Core.Text
getInstanceState_instanceName = Lens.lens (\GetInstanceState' {instanceName} -> instanceName) (\s@GetInstanceState' {} a -> s {instanceName = a} :: GetInstanceState)

instance Core.AWSRequest GetInstanceState where
  type
    AWSResponse GetInstanceState =
      GetInstanceStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceStateResponse'
            Core.<$> (x Core..?> "state")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstanceState

instance Core.NFData GetInstanceState

instance Core.ToHeaders GetInstanceState where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstanceState" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstanceState where
  toJSON GetInstanceState' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath GetInstanceState where
  toPath = Core.const "/"

instance Core.ToQuery GetInstanceState where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstanceStateResponse' smart constructor.
data GetInstanceStateResponse = GetInstanceStateResponse'
  { -- | The state of the instance.
    state :: Core.Maybe InstanceState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstanceStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'getInstanceStateResponse_state' - The state of the instance.
--
-- 'httpStatus', 'getInstanceStateResponse_httpStatus' - The response's http status code.
newGetInstanceStateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInstanceStateResponse
newGetInstanceStateResponse pHttpStatus_ =
  GetInstanceStateResponse'
    { state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the instance.
getInstanceStateResponse_state :: Lens.Lens' GetInstanceStateResponse (Core.Maybe InstanceState)
getInstanceStateResponse_state = Lens.lens (\GetInstanceStateResponse' {state} -> state) (\s@GetInstanceStateResponse' {} a -> s {state = a} :: GetInstanceStateResponse)

-- | The response's http status code.
getInstanceStateResponse_httpStatus :: Lens.Lens' GetInstanceStateResponse Core.Int
getInstanceStateResponse_httpStatus = Lens.lens (\GetInstanceStateResponse' {httpStatus} -> httpStatus) (\s@GetInstanceStateResponse' {} a -> s {httpStatus = a} :: GetInstanceStateResponse)

instance Core.NFData GetInstanceStateResponse
