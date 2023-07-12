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
-- Module      : Amazonka.LicenseManager.GetLicenseConversionTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified license type conversion task.
module Amazonka.LicenseManager.GetLicenseConversionTask
  ( -- * Creating a Request
    GetLicenseConversionTask (..),
    newGetLicenseConversionTask,

    -- * Request Lenses
    getLicenseConversionTask_licenseConversionTaskId,

    -- * Destructuring the Response
    GetLicenseConversionTaskResponse (..),
    newGetLicenseConversionTaskResponse,

    -- * Response Lenses
    getLicenseConversionTaskResponse_destinationLicenseContext,
    getLicenseConversionTaskResponse_endTime,
    getLicenseConversionTaskResponse_licenseConversionTaskId,
    getLicenseConversionTaskResponse_licenseConversionTime,
    getLicenseConversionTaskResponse_resourceArn,
    getLicenseConversionTaskResponse_sourceLicenseContext,
    getLicenseConversionTaskResponse_startTime,
    getLicenseConversionTaskResponse_status,
    getLicenseConversionTaskResponse_statusMessage,
    getLicenseConversionTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLicenseConversionTask' smart constructor.
data GetLicenseConversionTask = GetLicenseConversionTask'
  { -- | ID of the license type conversion task to retrieve information on.
    licenseConversionTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseConversionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConversionTaskId', 'getLicenseConversionTask_licenseConversionTaskId' - ID of the license type conversion task to retrieve information on.
newGetLicenseConversionTask ::
  -- | 'licenseConversionTaskId'
  Prelude.Text ->
  GetLicenseConversionTask
newGetLicenseConversionTask pLicenseConversionTaskId_ =
  GetLicenseConversionTask'
    { licenseConversionTaskId =
        pLicenseConversionTaskId_
    }

-- | ID of the license type conversion task to retrieve information on.
getLicenseConversionTask_licenseConversionTaskId :: Lens.Lens' GetLicenseConversionTask Prelude.Text
getLicenseConversionTask_licenseConversionTaskId = Lens.lens (\GetLicenseConversionTask' {licenseConversionTaskId} -> licenseConversionTaskId) (\s@GetLicenseConversionTask' {} a -> s {licenseConversionTaskId = a} :: GetLicenseConversionTask)

instance Core.AWSRequest GetLicenseConversionTask where
  type
    AWSResponse GetLicenseConversionTask =
      GetLicenseConversionTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseConversionTaskResponse'
            Prelude.<$> (x Data..?> "DestinationLicenseContext")
            Prelude.<*> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "LicenseConversionTaskId")
            Prelude.<*> (x Data..?> "LicenseConversionTime")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "SourceLicenseContext")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLicenseConversionTask where
  hashWithSalt _salt GetLicenseConversionTask' {..} =
    _salt
      `Prelude.hashWithSalt` licenseConversionTaskId

instance Prelude.NFData GetLicenseConversionTask where
  rnf GetLicenseConversionTask' {..} =
    Prelude.rnf licenseConversionTaskId

instance Data.ToHeaders GetLicenseConversionTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.GetLicenseConversionTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLicenseConversionTask where
  toJSON GetLicenseConversionTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseConversionTaskId"
                  Data..= licenseConversionTaskId
              )
          ]
      )

instance Data.ToPath GetLicenseConversionTask where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLicenseConversionTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseConversionTaskResponse' smart constructor.
data GetLicenseConversionTaskResponse = GetLicenseConversionTaskResponse'
  { -- | Information about the license type converted to.
    destinationLicenseContext :: Prelude.Maybe LicenseConversionContext,
    -- | Time at which the license type conversion task was completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | ID of the license type conversion task.
    licenseConversionTaskId :: Prelude.Maybe Prelude.Text,
    -- | Amount of time to complete the license type conversion.
    licenseConversionTime :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Names (ARN) of the resources the license conversion task
    -- is associated with.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the license type converted from.
    sourceLicenseContext :: Prelude.Maybe LicenseConversionContext,
    -- | Time at which the license type conversion task was started .
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the license type conversion task.
    status :: Prelude.Maybe LicenseConversionTaskStatus,
    -- | The status message for the conversion task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseConversionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationLicenseContext', 'getLicenseConversionTaskResponse_destinationLicenseContext' - Information about the license type converted to.
--
-- 'endTime', 'getLicenseConversionTaskResponse_endTime' - Time at which the license type conversion task was completed.
--
-- 'licenseConversionTaskId', 'getLicenseConversionTaskResponse_licenseConversionTaskId' - ID of the license type conversion task.
--
-- 'licenseConversionTime', 'getLicenseConversionTaskResponse_licenseConversionTime' - Amount of time to complete the license type conversion.
--
-- 'resourceArn', 'getLicenseConversionTaskResponse_resourceArn' - Amazon Resource Names (ARN) of the resources the license conversion task
-- is associated with.
--
-- 'sourceLicenseContext', 'getLicenseConversionTaskResponse_sourceLicenseContext' - Information about the license type converted from.
--
-- 'startTime', 'getLicenseConversionTaskResponse_startTime' - Time at which the license type conversion task was started .
--
-- 'status', 'getLicenseConversionTaskResponse_status' - Status of the license type conversion task.
--
-- 'statusMessage', 'getLicenseConversionTaskResponse_statusMessage' - The status message for the conversion task.
--
-- 'httpStatus', 'getLicenseConversionTaskResponse_httpStatus' - The response's http status code.
newGetLicenseConversionTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLicenseConversionTaskResponse
newGetLicenseConversionTaskResponse pHttpStatus_ =
  GetLicenseConversionTaskResponse'
    { destinationLicenseContext =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      licenseConversionTaskId = Prelude.Nothing,
      licenseConversionTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      sourceLicenseContext = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the license type converted to.
getLicenseConversionTaskResponse_destinationLicenseContext :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionContext)
getLicenseConversionTaskResponse_destinationLicenseContext = Lens.lens (\GetLicenseConversionTaskResponse' {destinationLicenseContext} -> destinationLicenseContext) (\s@GetLicenseConversionTaskResponse' {} a -> s {destinationLicenseContext = a} :: GetLicenseConversionTaskResponse)

-- | Time at which the license type conversion task was completed.
getLicenseConversionTaskResponse_endTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_endTime = Lens.lens (\GetLicenseConversionTaskResponse' {endTime} -> endTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {endTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Data._Time

-- | ID of the license type conversion task.
getLicenseConversionTaskResponse_licenseConversionTaskId :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_licenseConversionTaskId = Lens.lens (\GetLicenseConversionTaskResponse' {licenseConversionTaskId} -> licenseConversionTaskId) (\s@GetLicenseConversionTaskResponse' {} a -> s {licenseConversionTaskId = a} :: GetLicenseConversionTaskResponse)

-- | Amount of time to complete the license type conversion.
getLicenseConversionTaskResponse_licenseConversionTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_licenseConversionTime = Lens.lens (\GetLicenseConversionTaskResponse' {licenseConversionTime} -> licenseConversionTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {licenseConversionTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Names (ARN) of the resources the license conversion task
-- is associated with.
getLicenseConversionTaskResponse_resourceArn :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_resourceArn = Lens.lens (\GetLicenseConversionTaskResponse' {resourceArn} -> resourceArn) (\s@GetLicenseConversionTaskResponse' {} a -> s {resourceArn = a} :: GetLicenseConversionTaskResponse)

-- | Information about the license type converted from.
getLicenseConversionTaskResponse_sourceLicenseContext :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionContext)
getLicenseConversionTaskResponse_sourceLicenseContext = Lens.lens (\GetLicenseConversionTaskResponse' {sourceLicenseContext} -> sourceLicenseContext) (\s@GetLicenseConversionTaskResponse' {} a -> s {sourceLicenseContext = a} :: GetLicenseConversionTaskResponse)

-- | Time at which the license type conversion task was started .
getLicenseConversionTaskResponse_startTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_startTime = Lens.lens (\GetLicenseConversionTaskResponse' {startTime} -> startTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {startTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Data._Time

-- | Status of the license type conversion task.
getLicenseConversionTaskResponse_status :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionTaskStatus)
getLicenseConversionTaskResponse_status = Lens.lens (\GetLicenseConversionTaskResponse' {status} -> status) (\s@GetLicenseConversionTaskResponse' {} a -> s {status = a} :: GetLicenseConversionTaskResponse)

-- | The status message for the conversion task.
getLicenseConversionTaskResponse_statusMessage :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_statusMessage = Lens.lens (\GetLicenseConversionTaskResponse' {statusMessage} -> statusMessage) (\s@GetLicenseConversionTaskResponse' {} a -> s {statusMessage = a} :: GetLicenseConversionTaskResponse)

-- | The response's http status code.
getLicenseConversionTaskResponse_httpStatus :: Lens.Lens' GetLicenseConversionTaskResponse Prelude.Int
getLicenseConversionTaskResponse_httpStatus = Lens.lens (\GetLicenseConversionTaskResponse' {httpStatus} -> httpStatus) (\s@GetLicenseConversionTaskResponse' {} a -> s {httpStatus = a} :: GetLicenseConversionTaskResponse)

instance
  Prelude.NFData
    GetLicenseConversionTaskResponse
  where
  rnf GetLicenseConversionTaskResponse' {..} =
    Prelude.rnf destinationLicenseContext
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf licenseConversionTaskId
      `Prelude.seq` Prelude.rnf licenseConversionTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf sourceLicenseContext
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
