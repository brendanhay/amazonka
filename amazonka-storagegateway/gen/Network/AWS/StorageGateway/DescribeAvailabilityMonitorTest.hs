{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the most recent High Availability monitoring
-- test that was performed on the host in a cluster. If a test isn\'t
-- performed, the status and start time in the response would be null.
module Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
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
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeAvailabilityMonitorTest' smart constructor.
data DescribeAvailabilityMonitorTest = DescribeAvailabilityMonitorTest'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DescribeAvailabilityMonitorTest
  where
  type
    Rs DescribeAvailabilityMonitorTest =
      DescribeAvailabilityMonitorTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailabilityMonitorTestResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "StartTime")
            Prelude.<*> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAvailabilityMonitorTest

instance
  Prelude.NFData
    DescribeAvailabilityMonitorTest

instance
  Prelude.ToHeaders
    DescribeAvailabilityMonitorTest
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DescribeAvailabilityMonitorTest" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeAvailabilityMonitorTest
  where
  toJSON DescribeAvailabilityMonitorTest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance
  Prelude.ToPath
    DescribeAvailabilityMonitorTest
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeAvailabilityMonitorTest
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAvailabilityMonitorTestResponse' smart constructor.
data DescribeAvailabilityMonitorTestResponse = DescribeAvailabilityMonitorTestResponse'
  { -- | The status of the High Availability monitoring test. If a test hasn\'t
    -- been performed, the value of this field is null.
    status :: Prelude.Maybe AvailabilityMonitorTestStatus,
    -- | The time the High Availability monitoring test was started. If a test
    -- hasn\'t been performed, the value of this field is null.
    startTime :: Prelude.Maybe Prelude.POSIX,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityMonitorTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeAvailabilityMonitorTestResponse_status' - The status of the High Availability monitoring test. If a test hasn\'t
-- been performed, the value of this field is null.
--
-- 'startTime', 'describeAvailabilityMonitorTestResponse_startTime' - The time the High Availability monitoring test was started. If a test
-- hasn\'t been performed, the value of this field is null.
--
-- 'gatewayARN', 'describeAvailabilityMonitorTestResponse_gatewayARN' - Undocumented member.
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
        startTime = Prelude.Nothing,
        gatewayARN = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the High Availability monitoring test. If a test hasn\'t
-- been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_status :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe AvailabilityMonitorTestStatus)
describeAvailabilityMonitorTestResponse_status = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {status} -> status) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {status = a} :: DescribeAvailabilityMonitorTestResponse)

-- | The time the High Availability monitoring test was started. If a test
-- hasn\'t been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_startTime :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe Prelude.UTCTime)
describeAvailabilityMonitorTestResponse_startTime = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {startTime} -> startTime) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {startTime = a} :: DescribeAvailabilityMonitorTestResponse) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
describeAvailabilityMonitorTestResponse_gatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Prelude.Maybe Prelude.Text)
describeAvailabilityMonitorTestResponse_gatewayARN = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {gatewayARN} -> gatewayARN) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTestResponse)

-- | The response's http status code.
describeAvailabilityMonitorTestResponse_httpStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse Prelude.Int
describeAvailabilityMonitorTestResponse_httpStatus = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {httpStatus = a} :: DescribeAvailabilityMonitorTestResponse)

instance
  Prelude.NFData
    DescribeAvailabilityMonitorTestResponse
