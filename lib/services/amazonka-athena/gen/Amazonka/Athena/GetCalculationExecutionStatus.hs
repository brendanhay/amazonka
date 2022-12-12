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
-- Module      : Amazonka.Athena.GetCalculationExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a current calculation.
module Amazonka.Athena.GetCalculationExecutionStatus
  ( -- * Creating a Request
    GetCalculationExecutionStatus (..),
    newGetCalculationExecutionStatus,

    -- * Request Lenses
    getCalculationExecutionStatus_calculationExecutionId,

    -- * Destructuring the Response
    GetCalculationExecutionStatusResponse (..),
    newGetCalculationExecutionStatusResponse,

    -- * Response Lenses
    getCalculationExecutionStatusResponse_statistics,
    getCalculationExecutionStatusResponse_status,
    getCalculationExecutionStatusResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCalculationExecutionStatus' smart constructor.
data GetCalculationExecutionStatus = GetCalculationExecutionStatus'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'getCalculationExecutionStatus_calculationExecutionId' - The calculation execution UUID.
newGetCalculationExecutionStatus ::
  -- | 'calculationExecutionId'
  Prelude.Text ->
  GetCalculationExecutionStatus
newGetCalculationExecutionStatus
  pCalculationExecutionId_ =
    GetCalculationExecutionStatus'
      { calculationExecutionId =
          pCalculationExecutionId_
      }

-- | The calculation execution UUID.
getCalculationExecutionStatus_calculationExecutionId :: Lens.Lens' GetCalculationExecutionStatus Prelude.Text
getCalculationExecutionStatus_calculationExecutionId = Lens.lens (\GetCalculationExecutionStatus' {calculationExecutionId} -> calculationExecutionId) (\s@GetCalculationExecutionStatus' {} a -> s {calculationExecutionId = a} :: GetCalculationExecutionStatus)

instance
  Core.AWSRequest
    GetCalculationExecutionStatus
  where
  type
    AWSResponse GetCalculationExecutionStatus =
      GetCalculationExecutionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalculationExecutionStatusResponse'
            Prelude.<$> (x Data..?> "Statistics")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCalculationExecutionStatus
  where
  hashWithSalt _salt GetCalculationExecutionStatus' {..} =
    _salt `Prelude.hashWithSalt` calculationExecutionId

instance Prelude.NFData GetCalculationExecutionStatus where
  rnf GetCalculationExecutionStatus' {..} =
    Prelude.rnf calculationExecutionId

instance Data.ToHeaders GetCalculationExecutionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetCalculationExecutionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCalculationExecutionStatus where
  toJSON GetCalculationExecutionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CalculationExecutionId"
                  Data..= calculationExecutionId
              )
          ]
      )

instance Data.ToPath GetCalculationExecutionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCalculationExecutionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalculationExecutionStatusResponse' smart constructor.
data GetCalculationExecutionStatusResponse = GetCalculationExecutionStatusResponse'
  { -- | Contains information about the DPU execution time and progress.
    statistics :: Prelude.Maybe CalculationStatistics,
    -- | Contains information about the calculation execution status.
    status :: Prelude.Maybe CalculationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecutionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistics', 'getCalculationExecutionStatusResponse_statistics' - Contains information about the DPU execution time and progress.
--
-- 'status', 'getCalculationExecutionStatusResponse_status' - Contains information about the calculation execution status.
--
-- 'httpStatus', 'getCalculationExecutionStatusResponse_httpStatus' - The response's http status code.
newGetCalculationExecutionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalculationExecutionStatusResponse
newGetCalculationExecutionStatusResponse pHttpStatus_ =
  GetCalculationExecutionStatusResponse'
    { statistics =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the DPU execution time and progress.
getCalculationExecutionStatusResponse_statistics :: Lens.Lens' GetCalculationExecutionStatusResponse (Prelude.Maybe CalculationStatistics)
getCalculationExecutionStatusResponse_statistics = Lens.lens (\GetCalculationExecutionStatusResponse' {statistics} -> statistics) (\s@GetCalculationExecutionStatusResponse' {} a -> s {statistics = a} :: GetCalculationExecutionStatusResponse)

-- | Contains information about the calculation execution status.
getCalculationExecutionStatusResponse_status :: Lens.Lens' GetCalculationExecutionStatusResponse (Prelude.Maybe CalculationStatus)
getCalculationExecutionStatusResponse_status = Lens.lens (\GetCalculationExecutionStatusResponse' {status} -> status) (\s@GetCalculationExecutionStatusResponse' {} a -> s {status = a} :: GetCalculationExecutionStatusResponse)

-- | The response's http status code.
getCalculationExecutionStatusResponse_httpStatus :: Lens.Lens' GetCalculationExecutionStatusResponse Prelude.Int
getCalculationExecutionStatusResponse_httpStatus = Lens.lens (\GetCalculationExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@GetCalculationExecutionStatusResponse' {} a -> s {httpStatus = a} :: GetCalculationExecutionStatusResponse)

instance
  Prelude.NFData
    GetCalculationExecutionStatusResponse
  where
  rnf GetCalculationExecutionStatusResponse' {..} =
    Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
