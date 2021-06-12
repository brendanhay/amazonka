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
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is
-- a virtual private server.
module Network.AWS.Lightsail.GetInstance
  ( -- * Creating a Request
    GetInstance (..),
    newGetInstance,

    -- * Request Lenses
    getInstance_instanceName,

    -- * Destructuring the Response
    GetInstanceResponse (..),
    newGetInstanceResponse,

    -- * Response Lenses
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstance' smart constructor.
data GetInstance = GetInstance'
  { -- | The name of the instance.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getInstance_instanceName' - The name of the instance.
newGetInstance ::
  -- | 'instanceName'
  Core.Text ->
  GetInstance
newGetInstance pInstanceName_ =
  GetInstance' {instanceName = pInstanceName_}

-- | The name of the instance.
getInstance_instanceName :: Lens.Lens' GetInstance Core.Text
getInstance_instanceName = Lens.lens (\GetInstance' {instanceName} -> instanceName) (\s@GetInstance' {} a -> s {instanceName = a} :: GetInstance)

instance Core.AWSRequest GetInstance where
  type AWSResponse GetInstance = GetInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Core.<$> (x Core..?> "instance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstance

instance Core.NFData GetInstance

instance Core.ToHeaders GetInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath GetInstance where
  toPath = Core.const "/"

instance Core.ToQuery GetInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { -- | An array of key-value pairs containing information about the specified
    -- instance.
    instance' :: Core.Maybe Instance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instance'', 'getInstanceResponse_instance' - An array of key-value pairs containing information about the specified
-- instance.
--
-- 'httpStatus', 'getInstanceResponse_httpStatus' - The response's http status code.
newGetInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInstanceResponse
newGetInstanceResponse pHttpStatus_ =
  GetInstanceResponse'
    { instance' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the specified
-- instance.
getInstanceResponse_instance :: Lens.Lens' GetInstanceResponse (Core.Maybe Instance)
getInstanceResponse_instance = Lens.lens (\GetInstanceResponse' {instance'} -> instance') (\s@GetInstanceResponse' {} a -> s {instance' = a} :: GetInstanceResponse)

-- | The response's http status code.
getInstanceResponse_httpStatus :: Lens.Lens' GetInstanceResponse Core.Int
getInstanceResponse_httpStatus = Lens.lens (\GetInstanceResponse' {httpStatus} -> httpStatus) (\s@GetInstanceResponse' {} a -> s {httpStatus = a} :: GetInstanceResponse)

instance Core.NFData GetInstanceResponse
