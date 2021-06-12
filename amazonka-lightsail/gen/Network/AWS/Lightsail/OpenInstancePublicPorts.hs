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
-- Module      : Network.AWS.Lightsail.OpenInstancePublicPorts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the
-- IP addresses allowed to connect to the instance through the ports, and
-- the protocol.
--
-- The @OpenInstancePublicPorts@ action supports tag-based access control
-- via resource tags applied to the resource identified by @instanceName@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.OpenInstancePublicPorts
  ( -- * Creating a Request
    OpenInstancePublicPorts (..),
    newOpenInstancePublicPorts,

    -- * Request Lenses
    openInstancePublicPorts_portInfo,
    openInstancePublicPorts_instanceName,

    -- * Destructuring the Response
    OpenInstancePublicPortsResponse (..),
    newOpenInstancePublicPortsResponse,

    -- * Response Lenses
    openInstancePublicPortsResponse_operation,
    openInstancePublicPortsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newOpenInstancePublicPorts' smart constructor.
data OpenInstancePublicPorts = OpenInstancePublicPorts'
  { -- | An object to describe the ports to open for the specified instance.
    portInfo :: PortInfo,
    -- | The name of the instance for which to open ports.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpenInstancePublicPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portInfo', 'openInstancePublicPorts_portInfo' - An object to describe the ports to open for the specified instance.
--
-- 'instanceName', 'openInstancePublicPorts_instanceName' - The name of the instance for which to open ports.
newOpenInstancePublicPorts ::
  -- | 'portInfo'
  PortInfo ->
  -- | 'instanceName'
  Core.Text ->
  OpenInstancePublicPorts
newOpenInstancePublicPorts pPortInfo_ pInstanceName_ =
  OpenInstancePublicPorts'
    { portInfo = pPortInfo_,
      instanceName = pInstanceName_
    }

-- | An object to describe the ports to open for the specified instance.
openInstancePublicPorts_portInfo :: Lens.Lens' OpenInstancePublicPorts PortInfo
openInstancePublicPorts_portInfo = Lens.lens (\OpenInstancePublicPorts' {portInfo} -> portInfo) (\s@OpenInstancePublicPorts' {} a -> s {portInfo = a} :: OpenInstancePublicPorts)

-- | The name of the instance for which to open ports.
openInstancePublicPorts_instanceName :: Lens.Lens' OpenInstancePublicPorts Core.Text
openInstancePublicPorts_instanceName = Lens.lens (\OpenInstancePublicPorts' {instanceName} -> instanceName) (\s@OpenInstancePublicPorts' {} a -> s {instanceName = a} :: OpenInstancePublicPorts)

instance Core.AWSRequest OpenInstancePublicPorts where
  type
    AWSResponse OpenInstancePublicPorts =
      OpenInstancePublicPortsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          OpenInstancePublicPortsResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable OpenInstancePublicPorts

instance Core.NFData OpenInstancePublicPorts

instance Core.ToHeaders OpenInstancePublicPorts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.OpenInstancePublicPorts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON OpenInstancePublicPorts where
  toJSON OpenInstancePublicPorts' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("portInfo" Core..= portInfo),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath OpenInstancePublicPorts where
  toPath = Core.const "/"

instance Core.ToQuery OpenInstancePublicPorts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newOpenInstancePublicPortsResponse' smart constructor.
data OpenInstancePublicPortsResponse = OpenInstancePublicPortsResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpenInstancePublicPortsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'openInstancePublicPortsResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'openInstancePublicPortsResponse_httpStatus' - The response's http status code.
newOpenInstancePublicPortsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  OpenInstancePublicPortsResponse
newOpenInstancePublicPortsResponse pHttpStatus_ =
  OpenInstancePublicPortsResponse'
    { operation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
openInstancePublicPortsResponse_operation :: Lens.Lens' OpenInstancePublicPortsResponse (Core.Maybe Operation)
openInstancePublicPortsResponse_operation = Lens.lens (\OpenInstancePublicPortsResponse' {operation} -> operation) (\s@OpenInstancePublicPortsResponse' {} a -> s {operation = a} :: OpenInstancePublicPortsResponse)

-- | The response's http status code.
openInstancePublicPortsResponse_httpStatus :: Lens.Lens' OpenInstancePublicPortsResponse Core.Int
openInstancePublicPortsResponse_httpStatus = Lens.lens (\OpenInstancePublicPortsResponse' {httpStatus} -> httpStatus) (\s@OpenInstancePublicPortsResponse' {} a -> s {httpStatus = a} :: OpenInstancePublicPortsResponse)

instance Core.NFData OpenInstancePublicPortsResponse
