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
-- Module      : Amazonka.Lambda.GetProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the provisioned concurrency configuration for a function\'s
-- alias or version.
module Amazonka.Lambda.GetProvisionedConcurrencyConfig
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
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text,
    -- | The version number or alias name.
    qualifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
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
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getProvisionedConcurrencyConfig_functionName :: Lens.Lens' GetProvisionedConcurrencyConfig Prelude.Text
getProvisionedConcurrencyConfig_functionName = Lens.lens (\GetProvisionedConcurrencyConfig' {functionName} -> functionName) (\s@GetProvisionedConcurrencyConfig' {} a -> s {functionName = a} :: GetProvisionedConcurrencyConfig)

-- | The version number or alias name.
getProvisionedConcurrencyConfig_qualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Prelude.Text
getProvisionedConcurrencyConfig_qualifier = Lens.lens (\GetProvisionedConcurrencyConfig' {qualifier} -> qualifier) (\s@GetProvisionedConcurrencyConfig' {} a -> s {qualifier = a} :: GetProvisionedConcurrencyConfig)

instance
  Core.AWSRequest
    GetProvisionedConcurrencyConfig
  where
  type
    AWSResponse GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            Prelude.<$> ( x
                            Data..?> "AllocatedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> ( x
                            Data..?> "AvailableProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Data..?> "LastModified")
            Prelude.<*> ( x
                            Data..?> "RequestedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetProvisionedConcurrencyConfig
  where
  hashWithSalt
    _salt
    GetProvisionedConcurrencyConfig' {..} =
      _salt
        `Prelude.hashWithSalt` functionName
        `Prelude.hashWithSalt` qualifier

instance
  Prelude.NFData
    GetProvisionedConcurrencyConfig
  where
  rnf GetProvisionedConcurrencyConfig' {..} =
    Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf qualifier

instance
  Data.ToHeaders
    GetProvisionedConcurrencyConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetProvisionedConcurrencyConfig where
  toPath GetProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Data.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Data.ToQuery GetProvisionedConcurrencyConfig where
  toQuery GetProvisionedConcurrencyConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { -- | The amount of provisioned concurrency allocated. When a weighted alias
    -- is used during linear and canary deployments, this value fluctuates
    -- depending on the amount of concurrency that is provisioned for the
    -- function versions.
    allocatedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The status of the allocation process.
    status :: Prelude.Maybe ProvisionedConcurrencyStatusEnum,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProvisionedConcurrencyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated. When a weighted alias
-- is used during linear and canary deployments, this value fluctuates
-- depending on the amount of concurrency that is provisioned for the
-- function versions.
--
-- 'availableProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- 'lastModified', 'getProvisionedConcurrencyConfigResponse_lastModified' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
--
-- 'requestedProvisionedConcurrentExecutions', 'getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- 'status', 'getProvisionedConcurrencyConfigResponse_status' - The status of the allocation process.
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
      { allocatedProvisionedConcurrentExecutions =
          Prelude.Nothing,
        availableProvisionedConcurrentExecutions =
          Prelude.Nothing,
        lastModified = Prelude.Nothing,
        requestedProvisionedConcurrentExecutions =
          Prelude.Nothing,
        status = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of provisioned concurrency allocated. When a weighted alias
-- is used during linear and canary deployments, this value fluctuates
-- depending on the amount of concurrency that is provisioned for the
-- function versions.
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency available.
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {availableProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
getProvisionedConcurrencyConfigResponse_lastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Text)
getProvisionedConcurrencyConfigResponse_lastModified = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {lastModified} -> lastModified) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {lastModified = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The amount of provisioned concurrency requested.
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe Prelude.Natural)
getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)

-- | The status of the allocation process.
getProvisionedConcurrencyConfigResponse_status :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Prelude.Maybe ProvisionedConcurrencyStatusEnum)
getProvisionedConcurrencyConfigResponse_status = Lens.lens (\GetProvisionedConcurrencyConfigResponse' {status} -> status) (\s@GetProvisionedConcurrencyConfigResponse' {} a -> s {status = a} :: GetProvisionedConcurrencyConfigResponse)

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
  where
  rnf GetProvisionedConcurrencyConfigResponse' {..} =
    Prelude.rnf
      allocatedProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf availableProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf requestedProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
