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
-- Module      : Network.AWS.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being
-- returned to AWS.
module Network.AWS.Snowball.DescribeReturnShippingLabel
  ( -- * Creating a Request
    DescribeReturnShippingLabel (..),
    newDescribeReturnShippingLabel,

    -- * Request Lenses
    describeReturnShippingLabel_jobId,

    -- * Destructuring the Response
    DescribeReturnShippingLabelResponse (..),
    newDescribeReturnShippingLabelResponse,

    -- * Response Lenses
    describeReturnShippingLabelResponse_status,
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeReturnShippingLabel' smart constructor.
data DescribeReturnShippingLabel = DescribeReturnShippingLabel'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReturnShippingLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeReturnShippingLabel_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newDescribeReturnShippingLabel ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeReturnShippingLabel
newDescribeReturnShippingLabel pJobId_ =
  DescribeReturnShippingLabel' {jobId = pJobId_}

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
describeReturnShippingLabel_jobId :: Lens.Lens' DescribeReturnShippingLabel Prelude.Text
describeReturnShippingLabel_jobId = Lens.lens (\DescribeReturnShippingLabel' {jobId} -> jobId) (\s@DescribeReturnShippingLabel' {} a -> s {jobId = a} :: DescribeReturnShippingLabel)

instance Core.AWSRequest DescribeReturnShippingLabel where
  type
    AWSResponse DescribeReturnShippingLabel =
      DescribeReturnShippingLabelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReturnShippingLabelResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ExpirationDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReturnShippingLabel

instance Prelude.NFData DescribeReturnShippingLabel

instance Core.ToHeaders DescribeReturnShippingLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.DescribeReturnShippingLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeReturnShippingLabel where
  toJSON DescribeReturnShippingLabel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeReturnShippingLabel where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReturnShippingLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { -- | The status information of the task on a Snow device that is being
    -- returned to AWS.
    status :: Prelude.Maybe ShippingLabelStatus,
    -- | The expiration date of the current return shipping label.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReturnShippingLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeReturnShippingLabelResponse_status' - The status information of the task on a Snow device that is being
-- returned to AWS.
--
-- 'expirationDate', 'describeReturnShippingLabelResponse_expirationDate' - The expiration date of the current return shipping label.
--
-- 'httpStatus', 'describeReturnShippingLabelResponse_httpStatus' - The response's http status code.
newDescribeReturnShippingLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReturnShippingLabelResponse
newDescribeReturnShippingLabelResponse pHttpStatus_ =
  DescribeReturnShippingLabelResponse'
    { status =
        Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status information of the task on a Snow device that is being
-- returned to AWS.
describeReturnShippingLabelResponse_status :: Lens.Lens' DescribeReturnShippingLabelResponse (Prelude.Maybe ShippingLabelStatus)
describeReturnShippingLabelResponse_status = Lens.lens (\DescribeReturnShippingLabelResponse' {status} -> status) (\s@DescribeReturnShippingLabelResponse' {} a -> s {status = a} :: DescribeReturnShippingLabelResponse)

-- | The expiration date of the current return shipping label.
describeReturnShippingLabelResponse_expirationDate :: Lens.Lens' DescribeReturnShippingLabelResponse (Prelude.Maybe Prelude.UTCTime)
describeReturnShippingLabelResponse_expirationDate = Lens.lens (\DescribeReturnShippingLabelResponse' {expirationDate} -> expirationDate) (\s@DescribeReturnShippingLabelResponse' {} a -> s {expirationDate = a} :: DescribeReturnShippingLabelResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeReturnShippingLabelResponse_httpStatus :: Lens.Lens' DescribeReturnShippingLabelResponse Prelude.Int
describeReturnShippingLabelResponse_httpStatus = Lens.lens (\DescribeReturnShippingLabelResponse' {httpStatus} -> httpStatus) (\s@DescribeReturnShippingLabelResponse' {} a -> s {httpStatus = a} :: DescribeReturnShippingLabelResponse)

instance
  Prelude.NFData
    DescribeReturnShippingLabelResponse
