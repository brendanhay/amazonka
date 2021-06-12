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
-- Module      : Network.AWS.Connect.DescribeHoursOfOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the hours of operation.
module Network.AWS.Connect.DescribeHoursOfOperation
  ( -- * Creating a Request
    DescribeHoursOfOperation (..),
    newDescribeHoursOfOperation,

    -- * Request Lenses
    describeHoursOfOperation_instanceId,
    describeHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    DescribeHoursOfOperationResponse (..),
    newDescribeHoursOfOperationResponse,

    -- * Response Lenses
    describeHoursOfOperationResponse_hoursOfOperation,
    describeHoursOfOperationResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHoursOfOperation' smart constructor.
data DescribeHoursOfOperation = DescribeHoursOfOperation'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'hoursOfOperationId', 'describeHoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
newDescribeHoursOfOperation ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'hoursOfOperationId'
  Core.Text ->
  DescribeHoursOfOperation
newDescribeHoursOfOperation
  pInstanceId_
  pHoursOfOperationId_ =
    DescribeHoursOfOperation'
      { instanceId =
          pInstanceId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The identifier of the Amazon Connect instance.
describeHoursOfOperation_instanceId :: Lens.Lens' DescribeHoursOfOperation Core.Text
describeHoursOfOperation_instanceId = Lens.lens (\DescribeHoursOfOperation' {instanceId} -> instanceId) (\s@DescribeHoursOfOperation' {} a -> s {instanceId = a} :: DescribeHoursOfOperation)

-- | The identifier for the hours of operation.
describeHoursOfOperation_hoursOfOperationId :: Lens.Lens' DescribeHoursOfOperation Core.Text
describeHoursOfOperation_hoursOfOperationId = Lens.lens (\DescribeHoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@DescribeHoursOfOperation' {} a -> s {hoursOfOperationId = a} :: DescribeHoursOfOperation)

instance Core.AWSRequest DescribeHoursOfOperation where
  type
    AWSResponse DescribeHoursOfOperation =
      DescribeHoursOfOperationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHoursOfOperationResponse'
            Core.<$> (x Core..?> "HoursOfOperation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeHoursOfOperation

instance Core.NFData DescribeHoursOfOperation

instance Core.ToHeaders DescribeHoursOfOperation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeHoursOfOperation where
  toPath DescribeHoursOfOperation' {..} =
    Core.mconcat
      [ "/hours-of-operations/",
        Core.toBS instanceId,
        "/",
        Core.toBS hoursOfOperationId
      ]

instance Core.ToQuery DescribeHoursOfOperation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeHoursOfOperationResponse' smart constructor.
data DescribeHoursOfOperationResponse = DescribeHoursOfOperationResponse'
  { -- | The hours of operation.
    hoursOfOperation :: Core.Maybe HoursOfOperation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hoursOfOperation', 'describeHoursOfOperationResponse_hoursOfOperation' - The hours of operation.
--
-- 'httpStatus', 'describeHoursOfOperationResponse_httpStatus' - The response's http status code.
newDescribeHoursOfOperationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHoursOfOperationResponse
newDescribeHoursOfOperationResponse pHttpStatus_ =
  DescribeHoursOfOperationResponse'
    { hoursOfOperation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The hours of operation.
describeHoursOfOperationResponse_hoursOfOperation :: Lens.Lens' DescribeHoursOfOperationResponse (Core.Maybe HoursOfOperation)
describeHoursOfOperationResponse_hoursOfOperation = Lens.lens (\DescribeHoursOfOperationResponse' {hoursOfOperation} -> hoursOfOperation) (\s@DescribeHoursOfOperationResponse' {} a -> s {hoursOfOperation = a} :: DescribeHoursOfOperationResponse)

-- | The response's http status code.
describeHoursOfOperationResponse_httpStatus :: Lens.Lens' DescribeHoursOfOperationResponse Core.Int
describeHoursOfOperationResponse_httpStatus = Lens.lens (\DescribeHoursOfOperationResponse' {httpStatus} -> httpStatus) (\s@DescribeHoursOfOperationResponse' {} a -> s {httpStatus = a} :: DescribeHoursOfOperationResponse)

instance Core.NFData DescribeHoursOfOperationResponse
