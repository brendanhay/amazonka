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
-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an on-premises instance.
module Network.AWS.CodeDeploy.GetOnPremisesInstance
  ( -- * Creating a Request
    GetOnPremisesInstance (..),
    newGetOnPremisesInstance,

    -- * Request Lenses
    getOnPremisesInstance_instanceName,

    -- * Destructuring the Response
    GetOnPremisesInstanceResponse (..),
    newGetOnPremisesInstanceResponse,

    -- * Response Lenses
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'newGetOnPremisesInstance' smart constructor.
data GetOnPremisesInstance = GetOnPremisesInstance'
  { -- | The name of the on-premises instance about which to get information.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOnPremisesInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getOnPremisesInstance_instanceName' - The name of the on-premises instance about which to get information.
newGetOnPremisesInstance ::
  -- | 'instanceName'
  Core.Text ->
  GetOnPremisesInstance
newGetOnPremisesInstance pInstanceName_ =
  GetOnPremisesInstance'
    { instanceName =
        pInstanceName_
    }

-- | The name of the on-premises instance about which to get information.
getOnPremisesInstance_instanceName :: Lens.Lens' GetOnPremisesInstance Core.Text
getOnPremisesInstance_instanceName = Lens.lens (\GetOnPremisesInstance' {instanceName} -> instanceName) (\s@GetOnPremisesInstance' {} a -> s {instanceName = a} :: GetOnPremisesInstance)

instance Core.AWSRequest GetOnPremisesInstance where
  type
    AWSResponse GetOnPremisesInstance =
      GetOnPremisesInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOnPremisesInstanceResponse'
            Core.<$> (x Core..?> "instanceInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOnPremisesInstance

instance Core.NFData GetOnPremisesInstance

instance Core.ToHeaders GetOnPremisesInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetOnPremisesInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetOnPremisesInstance where
  toJSON GetOnPremisesInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath GetOnPremisesInstance where
  toPath = Core.const "/"

instance Core.ToQuery GetOnPremisesInstance where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'newGetOnPremisesInstanceResponse' smart constructor.
data GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'
  { -- | Information about the on-premises instance.
    instanceInfo :: Core.Maybe InstanceInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOnPremisesInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInfo', 'getOnPremisesInstanceResponse_instanceInfo' - Information about the on-premises instance.
--
-- 'httpStatus', 'getOnPremisesInstanceResponse_httpStatus' - The response's http status code.
newGetOnPremisesInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOnPremisesInstanceResponse
newGetOnPremisesInstanceResponse pHttpStatus_ =
  GetOnPremisesInstanceResponse'
    { instanceInfo =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the on-premises instance.
getOnPremisesInstanceResponse_instanceInfo :: Lens.Lens' GetOnPremisesInstanceResponse (Core.Maybe InstanceInfo)
getOnPremisesInstanceResponse_instanceInfo = Lens.lens (\GetOnPremisesInstanceResponse' {instanceInfo} -> instanceInfo) (\s@GetOnPremisesInstanceResponse' {} a -> s {instanceInfo = a} :: GetOnPremisesInstanceResponse)

-- | The response's http status code.
getOnPremisesInstanceResponse_httpStatus :: Lens.Lens' GetOnPremisesInstanceResponse Core.Int
getOnPremisesInstanceResponse_httpStatus = Lens.lens (\GetOnPremisesInstanceResponse' {httpStatus} -> httpStatus) (\s@GetOnPremisesInstanceResponse' {} a -> s {httpStatus = a} :: GetOnPremisesInstanceResponse)

instance Core.NFData GetOnPremisesInstanceResponse
