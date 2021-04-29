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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    functionName :: Prelude.Text,
    -- | The version number or alias name.
    qualifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'qualifier'
  Prelude.Text ->
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
getProvisionedConcurrencyConfig_functionName :: Lens.Lens' GetProvisionedConcurrencyConfig Prelude.Text
getProvisionedConcurrencyConfig_functionName = Lens.lens (\GetProvisionedConcurrencyConfig' {functionName} -> functionName) (\s@GetProvisionedConcurrencyConfig' {} a -> s {functionName = a} :: GetProvisionedConcurrencyConfig)

-- | The version number or alias name.
getProvisionedConcurrencyConfig_qualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Prelude.Text
getProvisionedConcurrencyConfig_qualifier = Lens.lens (\GetProvisionedConcurrencyConfig' {qualifier} -> qualifier) (\s@GetProvisionedConcurrencyConfig' {} a -> s {qualifier = a} :: GetProvisionedConcurrencyConfig)

instance
  Prelude.AWSRequest
    GetProvisionedConcurrencyConfig
  where
  type
    Rs GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> ( x
                            Prelude..?> "AvailableProvisionedConcurrentExecutions"
                        )
            Prelude.<*> ( x
                            Prelude..?> "RequestedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> ( x
                            Prelude..?> "AllocatedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Prelude..?> "LastModified")
            Prelude.<*> (x Prelude..?> "StatusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetProvisionedConcurrencyConfig

instance
  Prelude.NFData
    GetProvisionedConcurrencyConfig

instance
  Prelude.ToHeaders
    GetProvisionedConcurrencyConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    GetProvisionedConcurrencyConfig
  where
  toPath GetProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Prelude.toBS functionName,
        "/provisioned-concurrency"
      ]

instance
  Prelude.ToQuery
    GetProvisionedConcurrencyConfig
  where
  toQuery GetProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat ["Qualifier" Prelude.=: qualifier]

-- | /See:/ 'newGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { -- | The status of the allocation process.
    status :: Prelude.Maybe ProvisionedConcurrencyStatusEnum,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetProvisionedConcurrencyConfigResponse
newGetProvisionedConcurrencyConfigResponse
  pHttpStatus_ =
    GetProvisionedConcurrencyConfigResponse'
      { status =
          Prelude.Nothing,
        availableProvisionedConcurrentExecutions =
          Prelude.Nothing,
        requestedProvisionedConcurrentExecutions =
          Prelude.Nothing,
        allocatedProvisionedConcurrentExecutions =
          Prelude.Nothing,
        lastModified = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the allocation process.
getProvisionedConcurrencyConfigResponse_status :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe ProvisionedConcurrencyStatusEnum)
getProvisionedConcurrencyConfigResponse_status = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {status} -> status) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {status = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency available.
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {availableProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency requested.
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency allocated.
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
getProvisionedConcurrencyConfigResponse_lastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Text)
getProvisionedConcurrencyConfigResponse_lastModified = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {lastModified} -> lastModified) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {lastModified = a} :: GetProvisionedConcurrencyConfigResponse)

-- | For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
getProvisionedConcurrencyConfigResponse_statusReason :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Text)
getProvisionedConcurrencyConfigResponse_statusReason = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {statusReason} -> statusReason) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {statusReason = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The response's http status code.
getProvisionedConcurrencyConfigResponse_httpStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse Prelude.Int
getProvisionedConcurrencyConfigResponse_httpStatus = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {httpStatus} -> httpStatus) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {httpStatus = a} :: GetProvisionedConcurrencyConfigResponse)

instance
  Prelude.NFData
    GetProvisionedConcurrencyConfigResponse
