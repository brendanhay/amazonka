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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeAvailabilityMonitorTest' smart constructor.
data DescribeAvailabilityMonitorTest = DescribeAvailabilityMonitorTest'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAvailabilityMonitorTest
newDescribeAvailabilityMonitorTest pGatewayARN_ =
  DescribeAvailabilityMonitorTest'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeAvailabilityMonitorTest_gatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTest Core.Text
describeAvailabilityMonitorTest_gatewayARN = Lens.lens (\DescribeAvailabilityMonitorTest' {gatewayARN} -> gatewayARN) (\s@DescribeAvailabilityMonitorTest' {} a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTest)

instance
  Core.AWSRequest
    DescribeAvailabilityMonitorTest
  where
  type
    AWSResponse DescribeAvailabilityMonitorTest =
      DescribeAvailabilityMonitorTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailabilityMonitorTestResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "StartTime")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAvailabilityMonitorTest

instance Core.NFData DescribeAvailabilityMonitorTest

instance
  Core.ToHeaders
    DescribeAvailabilityMonitorTest
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeAvailabilityMonitorTest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAvailabilityMonitorTest where
  toJSON DescribeAvailabilityMonitorTest' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeAvailabilityMonitorTest where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAvailabilityMonitorTest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAvailabilityMonitorTestResponse' smart constructor.
data DescribeAvailabilityMonitorTestResponse = DescribeAvailabilityMonitorTestResponse'
  { -- | The status of the High Availability monitoring test. If a test hasn\'t
    -- been performed, the value of this field is null.
    status :: Core.Maybe AvailabilityMonitorTestStatus,
    -- | The time the High Availability monitoring test was started. If a test
    -- hasn\'t been performed, the value of this field is null.
    startTime :: Core.Maybe Core.POSIX,
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAvailabilityMonitorTestResponse
newDescribeAvailabilityMonitorTestResponse
  pHttpStatus_ =
    DescribeAvailabilityMonitorTestResponse'
      { status =
          Core.Nothing,
        startTime = Core.Nothing,
        gatewayARN = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the High Availability monitoring test. If a test hasn\'t
-- been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_status :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe AvailabilityMonitorTestStatus)
describeAvailabilityMonitorTestResponse_status = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {status} -> status) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {status = a} :: DescribeAvailabilityMonitorTestResponse)

-- | The time the High Availability monitoring test was started. If a test
-- hasn\'t been performed, the value of this field is null.
describeAvailabilityMonitorTestResponse_startTime :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe Core.UTCTime)
describeAvailabilityMonitorTestResponse_startTime = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {startTime} -> startTime) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {startTime = a} :: DescribeAvailabilityMonitorTestResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeAvailabilityMonitorTestResponse_gatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe Core.Text)
describeAvailabilityMonitorTestResponse_gatewayARN = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {gatewayARN} -> gatewayARN) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTestResponse)

-- | The response's http status code.
describeAvailabilityMonitorTestResponse_httpStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse Core.Int
describeAvailabilityMonitorTestResponse_httpStatus = Lens.lens (\DescribeAvailabilityMonitorTestResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailabilityMonitorTestResponse' {} a -> s {httpStatus = a} :: DescribeAvailabilityMonitorTestResponse)

instance
  Core.NFData
    DescribeAvailabilityMonitorTestResponse
