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
-- Module      : Amazonka.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being
-- returned to Amazon Web Services.
module Amazonka.Snowball.DescribeReturnShippingLabel
  ( -- * Creating a Request
    DescribeReturnShippingLabel (..),
    newDescribeReturnShippingLabel,

    -- * Request Lenses
    describeReturnShippingLabel_jobId,

    -- * Destructuring the Response
    DescribeReturnShippingLabelResponse (..),
    newDescribeReturnShippingLabelResponse,

    -- * Response Lenses
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_returnShippingLabelURI,
    describeReturnShippingLabelResponse_status,
    describeReturnShippingLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReturnShippingLabelResponse'
            Prelude.<$> (x Data..?> "ExpirationDate")
            Prelude.<*> (x Data..?> "ReturnShippingLabelURI")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReturnShippingLabel where
  hashWithSalt _salt DescribeReturnShippingLabel' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeReturnShippingLabel where
  rnf DescribeReturnShippingLabel' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders DescribeReturnShippingLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.DescribeReturnShippingLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReturnShippingLabel where
  toJSON DescribeReturnShippingLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeReturnShippingLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReturnShippingLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { -- | The expiration date of the current return shipping label.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | The pre-signed Amazon S3 URI used to download the return shipping label.
    returnShippingLabelURI :: Prelude.Maybe Prelude.Text,
    -- | The status information of the task on a Snow device that is being
    -- returned to Amazon Web Services.
    status :: Prelude.Maybe ShippingLabelStatus,
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
-- 'expirationDate', 'describeReturnShippingLabelResponse_expirationDate' - The expiration date of the current return shipping label.
--
-- 'returnShippingLabelURI', 'describeReturnShippingLabelResponse_returnShippingLabelURI' - The pre-signed Amazon S3 URI used to download the return shipping label.
--
-- 'status', 'describeReturnShippingLabelResponse_status' - The status information of the task on a Snow device that is being
-- returned to Amazon Web Services.
--
-- 'httpStatus', 'describeReturnShippingLabelResponse_httpStatus' - The response's http status code.
newDescribeReturnShippingLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReturnShippingLabelResponse
newDescribeReturnShippingLabelResponse pHttpStatus_ =
  DescribeReturnShippingLabelResponse'
    { expirationDate =
        Prelude.Nothing,
      returnShippingLabelURI =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The expiration date of the current return shipping label.
describeReturnShippingLabelResponse_expirationDate :: Lens.Lens' DescribeReturnShippingLabelResponse (Prelude.Maybe Prelude.UTCTime)
describeReturnShippingLabelResponse_expirationDate = Lens.lens (\DescribeReturnShippingLabelResponse' {expirationDate} -> expirationDate) (\s@DescribeReturnShippingLabelResponse' {} a -> s {expirationDate = a} :: DescribeReturnShippingLabelResponse) Prelude.. Lens.mapping Data._Time

-- | The pre-signed Amazon S3 URI used to download the return shipping label.
describeReturnShippingLabelResponse_returnShippingLabelURI :: Lens.Lens' DescribeReturnShippingLabelResponse (Prelude.Maybe Prelude.Text)
describeReturnShippingLabelResponse_returnShippingLabelURI = Lens.lens (\DescribeReturnShippingLabelResponse' {returnShippingLabelURI} -> returnShippingLabelURI) (\s@DescribeReturnShippingLabelResponse' {} a -> s {returnShippingLabelURI = a} :: DescribeReturnShippingLabelResponse)

-- | The status information of the task on a Snow device that is being
-- returned to Amazon Web Services.
describeReturnShippingLabelResponse_status :: Lens.Lens' DescribeReturnShippingLabelResponse (Prelude.Maybe ShippingLabelStatus)
describeReturnShippingLabelResponse_status = Lens.lens (\DescribeReturnShippingLabelResponse' {status} -> status) (\s@DescribeReturnShippingLabelResponse' {} a -> s {status = a} :: DescribeReturnShippingLabelResponse)

-- | The response's http status code.
describeReturnShippingLabelResponse_httpStatus :: Lens.Lens' DescribeReturnShippingLabelResponse Prelude.Int
describeReturnShippingLabelResponse_httpStatus = Lens.lens (\DescribeReturnShippingLabelResponse' {httpStatus} -> httpStatus) (\s@DescribeReturnShippingLabelResponse' {} a -> s {httpStatus = a} :: DescribeReturnShippingLabelResponse)

instance
  Prelude.NFData
    DescribeReturnShippingLabelResponse
  where
  rnf DescribeReturnShippingLabelResponse' {..} =
    Prelude.rnf expirationDate `Prelude.seq`
      Prelude.rnf returnShippingLabelURI `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf httpStatus
