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
-- Module      : Amazonka.IoTDeviceAdvisor.GetEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an Device Advisor endpoint.
module Amazonka.IoTDeviceAdvisor.GetEndpoint
  ( -- * Creating a Request
    GetEndpoint (..),
    newGetEndpoint,

    -- * Request Lenses
    getEndpoint_thingArn,
    getEndpoint_certificateArn,

    -- * Destructuring the Response
    GetEndpointResponse (..),
    newGetEndpointResponse,

    -- * Response Lenses
    getEndpointResponse_endpoint,
    getEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEndpoint' smart constructor.
data GetEndpoint = GetEndpoint'
  { -- | The thing ARN of the device. This is an optional parameter.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The certificate ARN of the device. This is an optional parameter.
    certificateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'getEndpoint_thingArn' - The thing ARN of the device. This is an optional parameter.
--
-- 'certificateArn', 'getEndpoint_certificateArn' - The certificate ARN of the device. This is an optional parameter.
newGetEndpoint ::
  GetEndpoint
newGetEndpoint =
  GetEndpoint'
    { thingArn = Prelude.Nothing,
      certificateArn = Prelude.Nothing
    }

-- | The thing ARN of the device. This is an optional parameter.
getEndpoint_thingArn :: Lens.Lens' GetEndpoint (Prelude.Maybe Prelude.Text)
getEndpoint_thingArn = Lens.lens (\GetEndpoint' {thingArn} -> thingArn) (\s@GetEndpoint' {} a -> s {thingArn = a} :: GetEndpoint)

-- | The certificate ARN of the device. This is an optional parameter.
getEndpoint_certificateArn :: Lens.Lens' GetEndpoint (Prelude.Maybe Prelude.Text)
getEndpoint_certificateArn = Lens.lens (\GetEndpoint' {certificateArn} -> certificateArn) (\s@GetEndpoint' {} a -> s {certificateArn = a} :: GetEndpoint)

instance Core.AWSRequest GetEndpoint where
  type AWSResponse GetEndpoint = GetEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEndpointResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEndpoint where
  hashWithSalt _salt GetEndpoint' {..} =
    _salt `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData GetEndpoint where
  rnf GetEndpoint' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf certificateArn

instance Data.ToHeaders GetEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEndpoint where
  toPath = Prelude.const "/endpoint"

instance Data.ToQuery GetEndpoint where
  toQuery GetEndpoint' {..} =
    Prelude.mconcat
      [ "thingArn" Data.=: thingArn,
        "certificateArn" Data.=: certificateArn
      ]

-- | /See:/ 'newGetEndpointResponse' smart constructor.
data GetEndpointResponse = GetEndpointResponse'
  { -- | The response of an Device Advisor endpoint.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'getEndpointResponse_endpoint' - The response of an Device Advisor endpoint.
--
-- 'httpStatus', 'getEndpointResponse_httpStatus' - The response's http status code.
newGetEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEndpointResponse
newGetEndpointResponse pHttpStatus_ =
  GetEndpointResponse'
    { endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response of an Device Advisor endpoint.
getEndpointResponse_endpoint :: Lens.Lens' GetEndpointResponse (Prelude.Maybe Prelude.Text)
getEndpointResponse_endpoint = Lens.lens (\GetEndpointResponse' {endpoint} -> endpoint) (\s@GetEndpointResponse' {} a -> s {endpoint = a} :: GetEndpointResponse)

-- | The response's http status code.
getEndpointResponse_httpStatus :: Lens.Lens' GetEndpointResponse Prelude.Int
getEndpointResponse_httpStatus = Lens.lens (\GetEndpointResponse' {httpStatus} -> httpStatus) (\s@GetEndpointResponse' {} a -> s {httpStatus = a} :: GetEndpointResponse)

instance Prelude.NFData GetEndpointResponse where
  rnf GetEndpointResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
