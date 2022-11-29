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
-- Module      : Amazonka.IoTFleetWise.GetLoggingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the logging options.
module Amazonka.IoTFleetWise.GetLoggingOptions
  ( -- * Creating a Request
    GetLoggingOptions (..),
    newGetLoggingOptions,

    -- * Destructuring the Response
    GetLoggingOptionsResponse (..),
    newGetLoggingOptionsResponse,

    -- * Response Lenses
    getLoggingOptionsResponse_httpStatus,
    getLoggingOptionsResponse_cloudWatchLogDelivery,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoggingOptions' smart constructor.
data GetLoggingOptions = GetLoggingOptions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetLoggingOptions ::
  GetLoggingOptions
newGetLoggingOptions = GetLoggingOptions'

instance Core.AWSRequest GetLoggingOptions where
  type
    AWSResponse GetLoggingOptions =
      GetLoggingOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "cloudWatchLogDelivery")
      )

instance Prelude.Hashable GetLoggingOptions where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetLoggingOptions where
  rnf _ = ()

instance Core.ToHeaders GetLoggingOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.GetLoggingOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLoggingOptions where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetLoggingOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about log delivery to Amazon CloudWatch Logs.
    cloudWatchLogDelivery :: CloudWatchLogDeliveryOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getLoggingOptionsResponse_httpStatus' - The response's http status code.
--
-- 'cloudWatchLogDelivery', 'getLoggingOptionsResponse_cloudWatchLogDelivery' - Returns information about log delivery to Amazon CloudWatch Logs.
newGetLoggingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cloudWatchLogDelivery'
  CloudWatchLogDeliveryOptions ->
  GetLoggingOptionsResponse
newGetLoggingOptionsResponse
  pHttpStatus_
  pCloudWatchLogDelivery_ =
    GetLoggingOptionsResponse'
      { httpStatus =
          pHttpStatus_,
        cloudWatchLogDelivery = pCloudWatchLogDelivery_
      }

-- | The response's http status code.
getLoggingOptionsResponse_httpStatus :: Lens.Lens' GetLoggingOptionsResponse Prelude.Int
getLoggingOptionsResponse_httpStatus = Lens.lens (\GetLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@GetLoggingOptionsResponse' {} a -> s {httpStatus = a} :: GetLoggingOptionsResponse)

-- | Returns information about log delivery to Amazon CloudWatch Logs.
getLoggingOptionsResponse_cloudWatchLogDelivery :: Lens.Lens' GetLoggingOptionsResponse CloudWatchLogDeliveryOptions
getLoggingOptionsResponse_cloudWatchLogDelivery = Lens.lens (\GetLoggingOptionsResponse' {cloudWatchLogDelivery} -> cloudWatchLogDelivery) (\s@GetLoggingOptionsResponse' {} a -> s {cloudWatchLogDelivery = a} :: GetLoggingOptionsResponse)

instance Prelude.NFData GetLoggingOptionsResponse where
  rnf GetLoggingOptionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cloudWatchLogDelivery
