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
-- Module      : Network.AWS.Lightsail.GetContainerServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail container
-- services.
module Network.AWS.Lightsail.GetContainerServices
  ( -- * Creating a Request
    GetContainerServices (..),
    newGetContainerServices,

    -- * Request Lenses
    getContainerServices_serviceName,

    -- * Destructuring the Response
    GetContainerServicesResponse (..),
    newGetContainerServicesResponse,

    -- * Response Lenses
    getContainerServicesResponse_containerServices,
    getContainerServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerServices' smart constructor.
data GetContainerServices = GetContainerServices'
  { -- | The name of the container service for which to return information.
    --
    -- When omitted, the response includes all of your container services in
    -- the AWS Region where the request is made.
    serviceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'getContainerServices_serviceName' - The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in
-- the AWS Region where the request is made.
newGetContainerServices ::
  GetContainerServices
newGetContainerServices =
  GetContainerServices' {serviceName = Core.Nothing}

-- | The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in
-- the AWS Region where the request is made.
getContainerServices_serviceName :: Lens.Lens' GetContainerServices (Core.Maybe Core.Text)
getContainerServices_serviceName = Lens.lens (\GetContainerServices' {serviceName} -> serviceName) (\s@GetContainerServices' {} a -> s {serviceName = a} :: GetContainerServices)

instance Core.AWSRequest GetContainerServices where
  type
    AWSResponse GetContainerServices =
      GetContainerServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicesResponse'
            Core.<$> (x Core..?> "containerServices" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetContainerServices

instance Core.NFData GetContainerServices

instance Core.ToHeaders GetContainerServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerServices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetContainerServices where
  toJSON GetContainerServices' {..} =
    Core.object
      ( Core.catMaybes
          [("serviceName" Core..=) Core.<$> serviceName]
      )

instance Core.ToPath GetContainerServices where
  toPath = Core.const "/"

instance Core.ToQuery GetContainerServices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetContainerServicesResponse' smart constructor.
data GetContainerServicesResponse = GetContainerServicesResponse'
  { -- | An array of objects that describe one or more container services.
    containerServices :: Core.Maybe [ContainerService],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerServices', 'getContainerServicesResponse_containerServices' - An array of objects that describe one or more container services.
--
-- 'httpStatus', 'getContainerServicesResponse_httpStatus' - The response's http status code.
newGetContainerServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetContainerServicesResponse
newGetContainerServicesResponse pHttpStatus_ =
  GetContainerServicesResponse'
    { containerServices =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe one or more container services.
getContainerServicesResponse_containerServices :: Lens.Lens' GetContainerServicesResponse (Core.Maybe [ContainerService])
getContainerServicesResponse_containerServices = Lens.lens (\GetContainerServicesResponse' {containerServices} -> containerServices) (\s@GetContainerServicesResponse' {} a -> s {containerServices = a} :: GetContainerServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContainerServicesResponse_httpStatus :: Lens.Lens' GetContainerServicesResponse Core.Int
getContainerServicesResponse_httpStatus = Lens.lens (\GetContainerServicesResponse' {httpStatus} -> httpStatus) (\s@GetContainerServicesResponse' {} a -> s {httpStatus = a} :: GetContainerServicesResponse)

instance Core.NFData GetContainerServicesResponse
