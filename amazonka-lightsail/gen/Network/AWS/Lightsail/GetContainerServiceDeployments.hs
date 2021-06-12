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
-- Module      : Network.AWS.Lightsail.GetContainerServiceDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the deployments for your Amazon Lightsail container service
--
-- A deployment specifies the settings, such as the ports and launch
-- command, of containers that are deployed to your container service.
--
-- The deployments are ordered by version in ascending order. The newest
-- version is listed at the top of the response.
--
-- A set number of deployments are kept before the oldest one is replaced
-- with the newest one. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail endpoints and quotas>
-- in the /AWS General Reference/.
module Network.AWS.Lightsail.GetContainerServiceDeployments
  ( -- * Creating a Request
    GetContainerServiceDeployments (..),
    newGetContainerServiceDeployments,

    -- * Request Lenses
    getContainerServiceDeployments_serviceName,

    -- * Destructuring the Response
    GetContainerServiceDeploymentsResponse (..),
    newGetContainerServiceDeploymentsResponse,

    -- * Response Lenses
    getContainerServiceDeploymentsResponse_deployments,
    getContainerServiceDeploymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerServiceDeployments' smart constructor.
data GetContainerServiceDeployments = GetContainerServiceDeployments'
  { -- | The name of the container service for which to return deployments.
    serviceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServiceDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'getContainerServiceDeployments_serviceName' - The name of the container service for which to return deployments.
newGetContainerServiceDeployments ::
  -- | 'serviceName'
  Core.Text ->
  GetContainerServiceDeployments
newGetContainerServiceDeployments pServiceName_ =
  GetContainerServiceDeployments'
    { serviceName =
        pServiceName_
    }

-- | The name of the container service for which to return deployments.
getContainerServiceDeployments_serviceName :: Lens.Lens' GetContainerServiceDeployments Core.Text
getContainerServiceDeployments_serviceName = Lens.lens (\GetContainerServiceDeployments' {serviceName} -> serviceName) (\s@GetContainerServiceDeployments' {} a -> s {serviceName = a} :: GetContainerServiceDeployments)

instance
  Core.AWSRequest
    GetContainerServiceDeployments
  where
  type
    AWSResponse GetContainerServiceDeployments =
      GetContainerServiceDeploymentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServiceDeploymentsResponse'
            Core.<$> (x Core..?> "deployments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetContainerServiceDeployments

instance Core.NFData GetContainerServiceDeployments

instance
  Core.ToHeaders
    GetContainerServiceDeployments
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerServiceDeployments" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetContainerServiceDeployments where
  toJSON GetContainerServiceDeployments' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("serviceName" Core..= serviceName)]
      )

instance Core.ToPath GetContainerServiceDeployments where
  toPath = Core.const "/"

instance Core.ToQuery GetContainerServiceDeployments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetContainerServiceDeploymentsResponse' smart constructor.
data GetContainerServiceDeploymentsResponse = GetContainerServiceDeploymentsResponse'
  { -- | An array of objects that describe deployments for a container service.
    deployments :: Core.Maybe [ContainerServiceDeployment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServiceDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployments', 'getContainerServiceDeploymentsResponse_deployments' - An array of objects that describe deployments for a container service.
--
-- 'httpStatus', 'getContainerServiceDeploymentsResponse_httpStatus' - The response's http status code.
newGetContainerServiceDeploymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetContainerServiceDeploymentsResponse
newGetContainerServiceDeploymentsResponse
  pHttpStatus_ =
    GetContainerServiceDeploymentsResponse'
      { deployments =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe deployments for a container service.
getContainerServiceDeploymentsResponse_deployments :: Lens.Lens' GetContainerServiceDeploymentsResponse (Core.Maybe [ContainerServiceDeployment])
getContainerServiceDeploymentsResponse_deployments = Lens.lens (\GetContainerServiceDeploymentsResponse' {deployments} -> deployments) (\s@GetContainerServiceDeploymentsResponse' {} a -> s {deployments = a} :: GetContainerServiceDeploymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContainerServiceDeploymentsResponse_httpStatus :: Lens.Lens' GetContainerServiceDeploymentsResponse Core.Int
getContainerServiceDeploymentsResponse_httpStatus = Lens.lens (\GetContainerServiceDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetContainerServiceDeploymentsResponse' {} a -> s {httpStatus = a} :: GetContainerServiceDeploymentsResponse)

instance
  Core.NFData
    GetContainerServiceDeploymentsResponse
