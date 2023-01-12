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
-- Module      : Amazonka.IoTFleetWise.PutLoggingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the logging option.
module Amazonka.IoTFleetWise.PutLoggingOptions
  ( -- * Creating a Request
    PutLoggingOptions (..),
    newPutLoggingOptions,

    -- * Request Lenses
    putLoggingOptions_cloudWatchLogDelivery,

    -- * Destructuring the Response
    PutLoggingOptionsResponse (..),
    newPutLoggingOptionsResponse,

    -- * Response Lenses
    putLoggingOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLoggingOptions' smart constructor.
data PutLoggingOptions = PutLoggingOptions'
  { -- | Creates or updates the log delivery option to Amazon CloudWatch Logs.
    cloudWatchLogDelivery :: CloudWatchLogDeliveryOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogDelivery', 'putLoggingOptions_cloudWatchLogDelivery' - Creates or updates the log delivery option to Amazon CloudWatch Logs.
newPutLoggingOptions ::
  -- | 'cloudWatchLogDelivery'
  CloudWatchLogDeliveryOptions ->
  PutLoggingOptions
newPutLoggingOptions pCloudWatchLogDelivery_ =
  PutLoggingOptions'
    { cloudWatchLogDelivery =
        pCloudWatchLogDelivery_
    }

-- | Creates or updates the log delivery option to Amazon CloudWatch Logs.
putLoggingOptions_cloudWatchLogDelivery :: Lens.Lens' PutLoggingOptions CloudWatchLogDeliveryOptions
putLoggingOptions_cloudWatchLogDelivery = Lens.lens (\PutLoggingOptions' {cloudWatchLogDelivery} -> cloudWatchLogDelivery) (\s@PutLoggingOptions' {} a -> s {cloudWatchLogDelivery = a} :: PutLoggingOptions)

instance Core.AWSRequest PutLoggingOptions where
  type
    AWSResponse PutLoggingOptions =
      PutLoggingOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLoggingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLoggingOptions where
  hashWithSalt _salt PutLoggingOptions' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogDelivery

instance Prelude.NFData PutLoggingOptions where
  rnf PutLoggingOptions' {..} =
    Prelude.rnf cloudWatchLogDelivery

instance Data.ToHeaders PutLoggingOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.PutLoggingOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutLoggingOptions where
  toJSON PutLoggingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cloudWatchLogDelivery"
                  Data..= cloudWatchLogDelivery
              )
          ]
      )

instance Data.ToPath PutLoggingOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery PutLoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLoggingOptionsResponse' smart constructor.
data PutLoggingOptionsResponse = PutLoggingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putLoggingOptionsResponse_httpStatus' - The response's http status code.
newPutLoggingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLoggingOptionsResponse
newPutLoggingOptionsResponse pHttpStatus_ =
  PutLoggingOptionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putLoggingOptionsResponse_httpStatus :: Lens.Lens' PutLoggingOptionsResponse Prelude.Int
putLoggingOptionsResponse_httpStatus = Lens.lens (\PutLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@PutLoggingOptionsResponse' {} a -> s {httpStatus = a} :: PutLoggingOptionsResponse)

instance Prelude.NFData PutLoggingOptionsResponse where
  rnf PutLoggingOptionsResponse' {..} =
    Prelude.rnf httpStatus
