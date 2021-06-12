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
-- Module      : Network.AWS.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control
-- via resource tags applied to the resource identified by @instanceName@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CloseInstancePublicPorts
  ( -- * Creating a Request
    CloseInstancePublicPorts (..),
    newCloseInstancePublicPorts,

    -- * Request Lenses
    closeInstancePublicPorts_portInfo,
    closeInstancePublicPorts_instanceName,

    -- * Destructuring the Response
    CloseInstancePublicPortsResponse (..),
    newCloseInstancePublicPortsResponse,

    -- * Response Lenses
    closeInstancePublicPortsResponse_operation,
    closeInstancePublicPortsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCloseInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { -- | An object to describe the ports to close for the specified instance.
    portInfo :: PortInfo,
    -- | The name of the instance for which to close ports.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloseInstancePublicPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portInfo', 'closeInstancePublicPorts_portInfo' - An object to describe the ports to close for the specified instance.
--
-- 'instanceName', 'closeInstancePublicPorts_instanceName' - The name of the instance for which to close ports.
newCloseInstancePublicPorts ::
  -- | 'portInfo'
  PortInfo ->
  -- | 'instanceName'
  Core.Text ->
  CloseInstancePublicPorts
newCloseInstancePublicPorts pPortInfo_ pInstanceName_ =
  CloseInstancePublicPorts'
    { portInfo = pPortInfo_,
      instanceName = pInstanceName_
    }

-- | An object to describe the ports to close for the specified instance.
closeInstancePublicPorts_portInfo :: Lens.Lens' CloseInstancePublicPorts PortInfo
closeInstancePublicPorts_portInfo = Lens.lens (\CloseInstancePublicPorts' {portInfo} -> portInfo) (\s@CloseInstancePublicPorts' {} a -> s {portInfo = a} :: CloseInstancePublicPorts)

-- | The name of the instance for which to close ports.
closeInstancePublicPorts_instanceName :: Lens.Lens' CloseInstancePublicPorts Core.Text
closeInstancePublicPorts_instanceName = Lens.lens (\CloseInstancePublicPorts' {instanceName} -> instanceName) (\s@CloseInstancePublicPorts' {} a -> s {instanceName = a} :: CloseInstancePublicPorts)

instance Core.AWSRequest CloseInstancePublicPorts where
  type
    AWSResponse CloseInstancePublicPorts =
      CloseInstancePublicPortsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CloseInstancePublicPortsResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CloseInstancePublicPorts

instance Core.NFData CloseInstancePublicPorts

instance Core.ToHeaders CloseInstancePublicPorts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CloseInstancePublicPorts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CloseInstancePublicPorts where
  toJSON CloseInstancePublicPorts' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("portInfo" Core..= portInfo),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath CloseInstancePublicPorts where
  toPath = Core.const "/"

instance Core.ToQuery CloseInstancePublicPorts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCloseInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloseInstancePublicPortsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'closeInstancePublicPortsResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'closeInstancePublicPortsResponse_httpStatus' - The response's http status code.
newCloseInstancePublicPortsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CloseInstancePublicPortsResponse
newCloseInstancePublicPortsResponse pHttpStatus_ =
  CloseInstancePublicPortsResponse'
    { operation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
closeInstancePublicPortsResponse_operation :: Lens.Lens' CloseInstancePublicPortsResponse (Core.Maybe Operation)
closeInstancePublicPortsResponse_operation = Lens.lens (\CloseInstancePublicPortsResponse' {operation} -> operation) (\s@CloseInstancePublicPortsResponse' {} a -> s {operation = a} :: CloseInstancePublicPortsResponse)

-- | The response's http status code.
closeInstancePublicPortsResponse_httpStatus :: Lens.Lens' CloseInstancePublicPortsResponse Core.Int
closeInstancePublicPortsResponse_httpStatus = Lens.lens (\CloseInstancePublicPortsResponse' {httpStatus} -> httpStatus) (\s@CloseInstancePublicPortsResponse' {} a -> s {httpStatus = a} :: CloseInstancePublicPortsResponse)

instance Core.NFData CloseInstancePublicPortsResponse
