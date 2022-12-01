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
-- Module      : Amazonka.Lightsail.GetContainerServices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail container
-- services.
module Amazonka.Lightsail.GetContainerServices
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerServices' smart constructor.
data GetContainerServices = GetContainerServices'
  { -- | The name of the container service for which to return information.
    --
    -- When omitted, the response includes all of your container services in
    -- the Amazon Web Services Region where the request is made.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- the Amazon Web Services Region where the request is made.
newGetContainerServices ::
  GetContainerServices
newGetContainerServices =
  GetContainerServices'
    { serviceName =
        Prelude.Nothing
    }

-- | The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in
-- the Amazon Web Services Region where the request is made.
getContainerServices_serviceName :: Lens.Lens' GetContainerServices (Prelude.Maybe Prelude.Text)
getContainerServices_serviceName = Lens.lens (\GetContainerServices' {serviceName} -> serviceName) (\s@GetContainerServices' {} a -> s {serviceName = a} :: GetContainerServices)

instance Core.AWSRequest GetContainerServices where
  type
    AWSResponse GetContainerServices =
      GetContainerServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicesResponse'
            Prelude.<$> ( x Core..?> "containerServices"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerServices where
  hashWithSalt _salt GetContainerServices' {..} =
    _salt `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GetContainerServices where
  rnf GetContainerServices' {..} =
    Prelude.rnf serviceName

instance Core.ToHeaders GetContainerServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetContainerServices where
  toJSON GetContainerServices' {..} =
    Core.object
      ( Prelude.catMaybes
          [("serviceName" Core..=) Prelude.<$> serviceName]
      )

instance Core.ToPath GetContainerServices where
  toPath = Prelude.const "/"

instance Core.ToQuery GetContainerServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerServicesResponse' smart constructor.
data GetContainerServicesResponse = GetContainerServicesResponse'
  { -- | An array of objects that describe one or more container services.
    containerServices :: Prelude.Maybe [ContainerService],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetContainerServicesResponse
newGetContainerServicesResponse pHttpStatus_ =
  GetContainerServicesResponse'
    { containerServices =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe one or more container services.
getContainerServicesResponse_containerServices :: Lens.Lens' GetContainerServicesResponse (Prelude.Maybe [ContainerService])
getContainerServicesResponse_containerServices = Lens.lens (\GetContainerServicesResponse' {containerServices} -> containerServices) (\s@GetContainerServicesResponse' {} a -> s {containerServices = a} :: GetContainerServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getContainerServicesResponse_httpStatus :: Lens.Lens' GetContainerServicesResponse Prelude.Int
getContainerServicesResponse_httpStatus = Lens.lens (\GetContainerServicesResponse' {httpStatus} -> httpStatus) (\s@GetContainerServicesResponse' {} a -> s {httpStatus = a} :: GetContainerServicesResponse)

instance Prelude.NFData GetContainerServicesResponse where
  rnf GetContainerServicesResponse' {..} =
    Prelude.rnf containerServices
      `Prelude.seq` Prelude.rnf httpStatus
