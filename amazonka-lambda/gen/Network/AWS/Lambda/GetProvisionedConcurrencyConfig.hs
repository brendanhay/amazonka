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
-- Module      : Network.AWS.Lambda.GetProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the provisioned concurrency configuration for a function\'s
-- alias or version.
module Network.AWS.Lambda.GetProvisionedConcurrencyConfig
  ( -- * Creating a Request
    GetProvisionedConcurrencyConfig (..),
    newGetProvisionedConcurrencyConfig,

    -- * Request Lenses
    getProvisionedConcurrencyConfig_functionName,
    getProvisionedConcurrencyConfig_qualifier,

    -- * Destructuring the Response
    GetProvisionedConcurrencyConfigResponse (..),
    newGetProvisionedConcurrencyConfigResponse,

    -- * Response Lenses
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Core.Text,
    -- | The version number or alias name.
    qualifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProvisionedConcurrencyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'getProvisionedConcurrencyConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
--
-- 'qualifier', 'getProvisionedConcurrencyConfig_qualifier' - The version number or alias name.
newGetProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Core.Text ->
  -- | 'qualifier'
  Core.Text ->
  GetProvisionedConcurrencyConfig
newGetProvisionedConcurrencyConfig
  pFunctionName_
  pQualifier_ =
    GetProvisionedConcurrencyConfig'
      { functionName =
          pFunctionName_,
        qualifier = pQualifier_
      }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getProvisionedConcurrencyConfig_functionName :: Lens.Lens' GetProvisionedConcurrencyConfig Core.Text
getProvisionedConcurrencyConfig_functionName = Lens.lens (\GetProvisionedConcurrencyConfig' {functionName} -> functionName) (\s@GetProvisionedConcurrencyConfig' {} a -> s {functionName = a} :: GetProvisionedConcurrencyConfig)

-- | The version number or alias name.
getProvisionedConcurrencyConfig_qualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Core.Text
getProvisionedConcurrencyConfig_qualifier = Lens.lens (\GetProvisionedConcurrencyConfig' {qualifier} -> qualifier) (\s@GetProvisionedConcurrencyConfig' {} a -> s {qualifier = a} :: GetProvisionedConcurrencyConfig)

instance
  Core.AWSRequest
    GetProvisionedConcurrencyConfig
  where
  type
    AWSResponse GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> ( x
                         Core..?> "AvailableProvisionedConcurrentExecutions"
                     )
            Core.<*> ( x
                         Core..?> "RequestedProvisionedConcurrentExecutions"
                     )
            Core.<*> ( x
                         Core..?> "AllocatedProvisionedConcurrentExecutions"
                     )
            Core.<*> (x Core..?> "LastModified")
            Core.<*> (x Core..?> "StatusReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetProvisionedConcurrencyConfig

instance Core.NFData GetProvisionedConcurrencyConfig

instance
  Core.ToHeaders
    GetProvisionedConcurrencyConfig
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetProvisionedConcurrencyConfig where
  toPath GetProvisionedConcurrencyConfig' {..} =
    Core.mconcat
      [ "/2019-09-30/functions/",
        Core.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Core.ToQuery GetProvisionedConcurrencyConfig where
  toQuery GetProvisionedConcurrencyConfig' {..} =
    Core.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { -- | The status of the allocation process.
    status :: Core.Maybe ProvisionedConcurrencyStatusEnum,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Core.Maybe Core.Text,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProvisionedConcurrencyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getProvisionedConcurrencyConfigResponse_status' - The status of the allocation process.
--
-- 'availableProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- 'requestedProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- 'allocatedProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- 'lastModified', 'getProvisionedConcurrencyConfigResponse_lastModified' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
--
-- 'statusReason', 'getProvisionedConcurrencyConfigResponse_statusReason' - For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
--
-- 'httpStatus', 'getProvisionedConcurrencyConfigResponse_httpStatus' - The response's http status code.
newGetProvisionedConcurrencyConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetProvisionedConcurrencyConfigResponse
newGetProvisionedConcurrencyConfigResponse
  pHttpStatus_ =
    GetProvisionedConcurrencyConfigResponse'
      { status =
          Core.Nothing,
        availableProvisionedConcurrentExecutions =
          Core.Nothing,
        requestedProvisionedConcurrentExecutions =
          Core.Nothing,
        allocatedProvisionedConcurrentExecutions =
          Core.Nothing,
        lastModified = Core.Nothing,
        statusReason = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the allocation process.
getProvisionedConcurrencyConfigResponse_status :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe ProvisionedConcurrencyStatusEnum)
getProvisionedConcurrencyConfigResponse_status = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {status} -> status) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {status = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency available.
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {availableProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency requested.
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency allocated.
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
getProvisionedConcurrencyConfigResponse_lastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Text)
getProvisionedConcurrencyConfigResponse_lastModified = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {lastModified} -> lastModified) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {lastModified = a} :: GetProvisionedConcurrencyConfigResponse)

-- | For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
getProvisionedConcurrencyConfigResponse_statusReason :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Text)
getProvisionedConcurrencyConfigResponse_statusReason = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {statusReason} -> statusReason) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {statusReason = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The response's http status code.
getProvisionedConcurrencyConfigResponse_httpStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse Core.Int
getProvisionedConcurrencyConfigResponse_httpStatus = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {httpStatus} -> httpStatus) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {httpStatus = a} :: GetProvisionedConcurrencyConfigResponse)

instance
  Core.NFData
    GetProvisionedConcurrencyConfigResponse
