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
-- Module      : Amazonka.Athena.GetCalculationExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a previously submitted calculation execution.
module Amazonka.Athena.GetCalculationExecution
  ( -- * Creating a Request
    GetCalculationExecution (..),
    newGetCalculationExecution,

    -- * Request Lenses
    getCalculationExecution_calculationExecutionId,

    -- * Destructuring the Response
    GetCalculationExecutionResponse (..),
    newGetCalculationExecutionResponse,

    -- * Response Lenses
    getCalculationExecutionResponse_calculationExecutionId,
    getCalculationExecutionResponse_description,
    getCalculationExecutionResponse_result,
    getCalculationExecutionResponse_sessionId,
    getCalculationExecutionResponse_statistics,
    getCalculationExecutionResponse_status,
    getCalculationExecutionResponse_workingDirectory,
    getCalculationExecutionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCalculationExecution' smart constructor.
data GetCalculationExecution = GetCalculationExecution'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'getCalculationExecution_calculationExecutionId' - The calculation execution UUID.
newGetCalculationExecution ::
  -- | 'calculationExecutionId'
  Prelude.Text ->
  GetCalculationExecution
newGetCalculationExecution pCalculationExecutionId_ =
  GetCalculationExecution'
    { calculationExecutionId =
        pCalculationExecutionId_
    }

-- | The calculation execution UUID.
getCalculationExecution_calculationExecutionId :: Lens.Lens' GetCalculationExecution Prelude.Text
getCalculationExecution_calculationExecutionId = Lens.lens (\GetCalculationExecution' {calculationExecutionId} -> calculationExecutionId) (\s@GetCalculationExecution' {} a -> s {calculationExecutionId = a} :: GetCalculationExecution)

instance Core.AWSRequest GetCalculationExecution where
  type
    AWSResponse GetCalculationExecution =
      GetCalculationExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalculationExecutionResponse'
            Prelude.<$> (x Data..?> "CalculationExecutionId")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Result")
            Prelude.<*> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "Statistics")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "WorkingDirectory")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCalculationExecution where
  hashWithSalt _salt GetCalculationExecution' {..} =
    _salt `Prelude.hashWithSalt` calculationExecutionId

instance Prelude.NFData GetCalculationExecution where
  rnf GetCalculationExecution' {..} =
    Prelude.rnf calculationExecutionId

instance Data.ToHeaders GetCalculationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetCalculationExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCalculationExecution where
  toJSON GetCalculationExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CalculationExecutionId"
                  Data..= calculationExecutionId
              )
          ]
      )

instance Data.ToPath GetCalculationExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCalculationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalculationExecutionResponse' smart constructor.
data GetCalculationExecutionResponse = GetCalculationExecutionResponse'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The description of the calculation execution.
    description :: Prelude.Maybe Prelude.Text,
    -- | Contains result information. This field is populated only if the
    -- calculation is completed.
    result :: Prelude.Maybe CalculationResult,
    -- | The session ID that the calculation ran in.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the data processing unit (DPU) execution time
    -- and progress. This field is populated only when statistics are
    -- available.
    statistics :: Prelude.Maybe CalculationStatistics,
    -- | Contains information about the status of the calculation.
    status :: Prelude.Maybe CalculationStatus,
    -- | The Amazon S3 location in which calculation results are stored.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'getCalculationExecutionResponse_calculationExecutionId' - The calculation execution UUID.
--
-- 'description', 'getCalculationExecutionResponse_description' - The description of the calculation execution.
--
-- 'result', 'getCalculationExecutionResponse_result' - Contains result information. This field is populated only if the
-- calculation is completed.
--
-- 'sessionId', 'getCalculationExecutionResponse_sessionId' - The session ID that the calculation ran in.
--
-- 'statistics', 'getCalculationExecutionResponse_statistics' - Contains information about the data processing unit (DPU) execution time
-- and progress. This field is populated only when statistics are
-- available.
--
-- 'status', 'getCalculationExecutionResponse_status' - Contains information about the status of the calculation.
--
-- 'workingDirectory', 'getCalculationExecutionResponse_workingDirectory' - The Amazon S3 location in which calculation results are stored.
--
-- 'httpStatus', 'getCalculationExecutionResponse_httpStatus' - The response's http status code.
newGetCalculationExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalculationExecutionResponse
newGetCalculationExecutionResponse pHttpStatus_ =
  GetCalculationExecutionResponse'
    { calculationExecutionId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      result = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      statistics = Prelude.Nothing,
      status = Prelude.Nothing,
      workingDirectory = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The calculation execution UUID.
getCalculationExecutionResponse_calculationExecutionId :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe Prelude.Text)
getCalculationExecutionResponse_calculationExecutionId = Lens.lens (\GetCalculationExecutionResponse' {calculationExecutionId} -> calculationExecutionId) (\s@GetCalculationExecutionResponse' {} a -> s {calculationExecutionId = a} :: GetCalculationExecutionResponse)

-- | The description of the calculation execution.
getCalculationExecutionResponse_description :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe Prelude.Text)
getCalculationExecutionResponse_description = Lens.lens (\GetCalculationExecutionResponse' {description} -> description) (\s@GetCalculationExecutionResponse' {} a -> s {description = a} :: GetCalculationExecutionResponse)

-- | Contains result information. This field is populated only if the
-- calculation is completed.
getCalculationExecutionResponse_result :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe CalculationResult)
getCalculationExecutionResponse_result = Lens.lens (\GetCalculationExecutionResponse' {result} -> result) (\s@GetCalculationExecutionResponse' {} a -> s {result = a} :: GetCalculationExecutionResponse)

-- | The session ID that the calculation ran in.
getCalculationExecutionResponse_sessionId :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe Prelude.Text)
getCalculationExecutionResponse_sessionId = Lens.lens (\GetCalculationExecutionResponse' {sessionId} -> sessionId) (\s@GetCalculationExecutionResponse' {} a -> s {sessionId = a} :: GetCalculationExecutionResponse)

-- | Contains information about the data processing unit (DPU) execution time
-- and progress. This field is populated only when statistics are
-- available.
getCalculationExecutionResponse_statistics :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe CalculationStatistics)
getCalculationExecutionResponse_statistics = Lens.lens (\GetCalculationExecutionResponse' {statistics} -> statistics) (\s@GetCalculationExecutionResponse' {} a -> s {statistics = a} :: GetCalculationExecutionResponse)

-- | Contains information about the status of the calculation.
getCalculationExecutionResponse_status :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe CalculationStatus)
getCalculationExecutionResponse_status = Lens.lens (\GetCalculationExecutionResponse' {status} -> status) (\s@GetCalculationExecutionResponse' {} a -> s {status = a} :: GetCalculationExecutionResponse)

-- | The Amazon S3 location in which calculation results are stored.
getCalculationExecutionResponse_workingDirectory :: Lens.Lens' GetCalculationExecutionResponse (Prelude.Maybe Prelude.Text)
getCalculationExecutionResponse_workingDirectory = Lens.lens (\GetCalculationExecutionResponse' {workingDirectory} -> workingDirectory) (\s@GetCalculationExecutionResponse' {} a -> s {workingDirectory = a} :: GetCalculationExecutionResponse)

-- | The response's http status code.
getCalculationExecutionResponse_httpStatus :: Lens.Lens' GetCalculationExecutionResponse Prelude.Int
getCalculationExecutionResponse_httpStatus = Lens.lens (\GetCalculationExecutionResponse' {httpStatus} -> httpStatus) (\s@GetCalculationExecutionResponse' {} a -> s {httpStatus = a} :: GetCalculationExecutionResponse)

instance
  Prelude.NFData
    GetCalculationExecutionResponse
  where
  rnf GetCalculationExecutionResponse' {..} =
    Prelude.rnf calculationExecutionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf httpStatus
