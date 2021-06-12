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
-- Module      : Network.AWS.StorageGateway.StartAvailabilityMonitorTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a test that verifies that the specified gateway is configured for
-- High Availability monitoring in your host environment. This request only
-- initiates the test and that a successful response only indicates that
-- the test was started. It doesn\'t indicate that the test passed. For the
-- status of the test, invoke the @DescribeAvailabilityMonitorTest@ API.
--
-- Starting this test will cause your gateway to go offline for a brief
-- period.
module Network.AWS.StorageGateway.StartAvailabilityMonitorTest
  ( -- * Creating a Request
    StartAvailabilityMonitorTest (..),
    newStartAvailabilityMonitorTest,

    -- * Request Lenses
    startAvailabilityMonitorTest_gatewayARN,

    -- * Destructuring the Response
    StartAvailabilityMonitorTestResponse (..),
    newStartAvailabilityMonitorTestResponse,

    -- * Response Lenses
    startAvailabilityMonitorTestResponse_gatewayARN,
    startAvailabilityMonitorTestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newStartAvailabilityMonitorTest' smart constructor.
data StartAvailabilityMonitorTest = StartAvailabilityMonitorTest'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartAvailabilityMonitorTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'startAvailabilityMonitorTest_gatewayARN' - Undocumented member.
newStartAvailabilityMonitorTest ::
  -- | 'gatewayARN'
  Core.Text ->
  StartAvailabilityMonitorTest
newStartAvailabilityMonitorTest pGatewayARN_ =
  StartAvailabilityMonitorTest'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
startAvailabilityMonitorTest_gatewayARN :: Lens.Lens' StartAvailabilityMonitorTest Core.Text
startAvailabilityMonitorTest_gatewayARN = Lens.lens (\StartAvailabilityMonitorTest' {gatewayARN} -> gatewayARN) (\s@StartAvailabilityMonitorTest' {} a -> s {gatewayARN = a} :: StartAvailabilityMonitorTest)

instance Core.AWSRequest StartAvailabilityMonitorTest where
  type
    AWSResponse StartAvailabilityMonitorTest =
      StartAvailabilityMonitorTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAvailabilityMonitorTestResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartAvailabilityMonitorTest

instance Core.NFData StartAvailabilityMonitorTest

instance Core.ToHeaders StartAvailabilityMonitorTest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.StartAvailabilityMonitorTest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartAvailabilityMonitorTest where
  toJSON StartAvailabilityMonitorTest' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath StartAvailabilityMonitorTest where
  toPath = Core.const "/"

instance Core.ToQuery StartAvailabilityMonitorTest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartAvailabilityMonitorTestResponse' smart constructor.
data StartAvailabilityMonitorTestResponse = StartAvailabilityMonitorTestResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartAvailabilityMonitorTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'startAvailabilityMonitorTestResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'startAvailabilityMonitorTestResponse_httpStatus' - The response's http status code.
newStartAvailabilityMonitorTestResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartAvailabilityMonitorTestResponse
newStartAvailabilityMonitorTestResponse pHttpStatus_ =
  StartAvailabilityMonitorTestResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startAvailabilityMonitorTestResponse_gatewayARN :: Lens.Lens' StartAvailabilityMonitorTestResponse (Core.Maybe Core.Text)
startAvailabilityMonitorTestResponse_gatewayARN = Lens.lens (\StartAvailabilityMonitorTestResponse' {gatewayARN} -> gatewayARN) (\s@StartAvailabilityMonitorTestResponse' {} a -> s {gatewayARN = a} :: StartAvailabilityMonitorTestResponse)

-- | The response's http status code.
startAvailabilityMonitorTestResponse_httpStatus :: Lens.Lens' StartAvailabilityMonitorTestResponse Core.Int
startAvailabilityMonitorTestResponse_httpStatus = Lens.lens (\StartAvailabilityMonitorTestResponse' {httpStatus} -> httpStatus) (\s@StartAvailabilityMonitorTestResponse' {} a -> s {httpStatus = a} :: StartAvailabilityMonitorTestResponse)

instance
  Core.NFData
    StartAvailabilityMonitorTestResponse
