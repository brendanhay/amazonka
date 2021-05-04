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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newStartAvailabilityMonitorTest' smart constructor.
data StartAvailabilityMonitorTest = StartAvailabilityMonitorTest'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StartAvailabilityMonitorTest
newStartAvailabilityMonitorTest pGatewayARN_ =
  StartAvailabilityMonitorTest'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
startAvailabilityMonitorTest_gatewayARN :: Lens.Lens' StartAvailabilityMonitorTest Prelude.Text
startAvailabilityMonitorTest_gatewayARN = Lens.lens (\StartAvailabilityMonitorTest' {gatewayARN} -> gatewayARN) (\s@StartAvailabilityMonitorTest' {} a -> s {gatewayARN = a} :: StartAvailabilityMonitorTest)

instance
  Prelude.AWSRequest
    StartAvailabilityMonitorTest
  where
  type
    Rs StartAvailabilityMonitorTest =
      StartAvailabilityMonitorTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAvailabilityMonitorTestResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartAvailabilityMonitorTest

instance Prelude.NFData StartAvailabilityMonitorTest

instance
  Prelude.ToHeaders
    StartAvailabilityMonitorTest
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.StartAvailabilityMonitorTest" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartAvailabilityMonitorTest where
  toJSON StartAvailabilityMonitorTest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath StartAvailabilityMonitorTest where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartAvailabilityMonitorTest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAvailabilityMonitorTestResponse' smart constructor.
data StartAvailabilityMonitorTestResponse = StartAvailabilityMonitorTestResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StartAvailabilityMonitorTestResponse
newStartAvailabilityMonitorTestResponse pHttpStatus_ =
  StartAvailabilityMonitorTestResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startAvailabilityMonitorTestResponse_gatewayARN :: Lens.Lens' StartAvailabilityMonitorTestResponse (Prelude.Maybe Prelude.Text)
startAvailabilityMonitorTestResponse_gatewayARN = Lens.lens (\StartAvailabilityMonitorTestResponse' {gatewayARN} -> gatewayARN) (\s@StartAvailabilityMonitorTestResponse' {} a -> s {gatewayARN = a} :: StartAvailabilityMonitorTestResponse)

-- | The response's http status code.
startAvailabilityMonitorTestResponse_httpStatus :: Lens.Lens' StartAvailabilityMonitorTestResponse Prelude.Int
startAvailabilityMonitorTestResponse_httpStatus = Lens.lens (\StartAvailabilityMonitorTestResponse' {httpStatus} -> httpStatus) (\s@StartAvailabilityMonitorTestResponse' {} a -> s {httpStatus = a} :: StartAvailabilityMonitorTestResponse)

instance
  Prelude.NFData
    StartAvailabilityMonitorTestResponse
