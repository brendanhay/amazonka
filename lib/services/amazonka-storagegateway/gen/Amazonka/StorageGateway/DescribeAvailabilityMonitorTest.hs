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
-- Module      : Amazonka.StorageGateway.DescribeAvailabilityMonitorTest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the most recent high availability monitoring
-- test that was performed on the host in a cluster. If a test isn\'t
-- performed, the status and start time in the response would be null.
module Amazonka.StorageGateway.DescribeAvailabilityMonitorTest
  ( -- * Creating a Request
    DescribeAvailabilityMonitorTest (..),
    newDescribeAvailabilityMonitorTest,

    -- * Request Lenses
    describeAvailabilityMonitorTest_gatewayARN,

    -- * Destructuring the Response
    DescribeAvailabilityMonitorTestResponse (..),
    newDescribeAvailabilityMonitorTestResponse,

    -- * Response Lenses
    describeAvailabilityMonitorTestResponse_status,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDescribeAvailabilityMonitorTest' smart constructor.
data DescribeAvailabilityMonitorTest = DescribeAvailabilityMonitorTest'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityMonitorTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeAvailabilityMonitorTest_gatewayARN' - Undocumented member.
newDescribeAvailabilityMonitorTest ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeAvailabilityMonitorTest
newDescribeAvailabilityMonitorTest pGatewayARN_ =
  DescribeAvailabilityMonitorTest'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeAvailabilityMonitorTest_gatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTest Prelude.Text
describeAvailabilityMonitorTest_gatewayARN = Lens.lens (\DescribeAvailabilityMonitorTest' {gatewayARN} -> gatewayARN) (\s@DescribeAvailabilityMonitorTest' {} a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTest)

instance
  Core.AWSRequest
    DescribeAvailabilityMonitorTest
  where
  type
    AWSResponse DescribeAvailabilityMonitorTest =
      DescribeAvailabilityMonitorTestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailabilityMonitorTestResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAvailabilityMonitorTest
  where
  hashWithSalt
    _salt
    DescribeAvailabilityMonitorTest' {..} =
      _salt `Prelude.hashWithSalt` gatewayARN

instance
  Prelude.NFData
    DescribeAvailabilityMonitorTest
  where
  rnf DescribeAvailabilityMonitorTest' {..} =
    Prelude.rnf gatewayARN

instance
  Data.ToHeaders
    DescribeAvailabilityMonitorTest
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeAvailabilityMonitorTest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAvailabilityMonitorTest where
  toJSON DescribeAvailabilityMonitorTest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DescribeAvailabilityMonitorTest where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAvailabilityMonitorTest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAvailabilityMonitorTestResponse' smart constructor.
data DescribeAvailabilityMonitorTestResponse = DescribeAvailabilityMonitorTestResponse'
  { -- | The status of the high availability monitoring test. If a test hasn\'t
    -- been performed, the value of this field is null.
    status :: Prelude.Maybe AvailabilityMonitorTestStatus,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The time the high availability monitoring test was started. If a test
    -- hasn\'t been performed, the value of this field is null.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityMonitorTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeAvailabilityMonitorTestResponse_status' - The status of the high availability monitoring test. If a test hasn\'t
-- been performed, the value of this field is null.
--
-- 'gatewayARN', 'describeAvailabilityMonitorTestResponse_gatewayARN' - Undocumented member.
--
-- 'startTime', 'describeAvailabilityMonitorTestResponse_startTime' - The time the high availability monitoring test was started. If a test
-- hasn\'t been performed, the value of this field is null.
--
-- 'httpStatus', 'describeAvailabilityMonitorTestResponse_httpStatus' - The response's http status code.
newDescribeAvailabilityMonitorTestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAvailabilityMonitorTestResponse
newDescribeAvailabilityMonitorTestResponse
  pHttpStatus_ =
    DescribeAvailabilityMonitorTestResponse'
      { status =
          Prelude.Nothing,
        gatewayARN = Prelude.Nothing,
        startTime = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the high availability monitoring test. If a test hasn\'t
-- been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_status :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe AvailabilityMonitorTestStatus)
describeAvailabilityMonitorTestResponse_status = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {status} -> status) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {status = a} :: DescribeAvailabilityMonitorTestResponse)

-- | Undocumented member.
describeAvailabilityMonitorTestResponse_gatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe Prelude.Text)
describeAvailabilityMonitorTestResponse_gatewayARN = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {gatewayARN} -> gatewayARN) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTestResponse)

-- | The time the high availability monitoring test was started. If a test
-- hasn\'t been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_startTime :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe Prelude.UTCTime)
describeAvailabilityMonitorTestResponse_startTime = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {startTime} -> startTime) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {startTime = a} :: DescribeAvailabilityMonitorTestResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeAvailabilityMonitorTestResponse_httpStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse Prelude.Int
describeAvailabilityMonitorTestResponse_httpStatus = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {httpStatus = a} :: DescribeAvailabilityMonitorTestResponse)

instance
  Prelude.NFData
    DescribeAvailabilityMonitorTestResponse
  where
  rnf DescribeAvailabilityMonitorTestResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus
