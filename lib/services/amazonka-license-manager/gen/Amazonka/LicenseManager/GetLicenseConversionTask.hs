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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getLicenseConversionTaskResponse_status,
    getLicenseConversionTaskResponse_startTime,
    getLicenseConversionTaskResponse_destinationLicenseContext,
    getLicenseConversionTaskResponse_licenseConversionTaskId,
    getLicenseConversionTaskResponse_resourceArn,
    getLicenseConversionTaskResponse_statusMessage,
    getLicenseConversionTaskResponse_endTime,
    getLicenseConversionTaskResponse_licenseConversionTime,
    getLicenseConversionTaskResponse_sourceLicenseContext,
    getLicenseConversionTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseConversionTaskResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "StartTime")
            Prelude.<*> (x Core..?> "DestinationLicenseContext")
            Prelude.<*> (x Core..?> "LicenseConversionTaskId")
            Prelude.<*> (x Core..?> "ResourceArn")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "EndTime")
            Prelude.<*> (x Core..?> "LicenseConversionTime")
            Prelude.<*> (x Core..?> "SourceLicenseContext")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLicenseConversionTask

instance Prelude.NFData GetLicenseConversionTask

instance Core.ToHeaders GetLicenseConversionTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.GetLicenseConversionTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLicenseConversionTask where
  toJSON GetLicenseConversionTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseConversionTaskId"
                  Core..= licenseConversionTaskId
              )
          ]
      )

instance Core.ToPath GetLicenseConversionTask where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLicenseConversionTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseConversionTaskResponse' smart constructor.
data GetLicenseConversionTaskResponse = GetLicenseConversionTaskResponse'
  { -- | Status of the license type conversion task.
    status :: Prelude.Maybe LicenseConversionTaskStatus,
    -- | Time at which the license type conversion task was started .
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the license type converted to.
    destinationLicenseContext :: Prelude.Maybe LicenseConversionContext,
    -- | ID of the license type conversion task.
    licenseConversionTaskId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Names (ARN) of the resources the license conversion task
    -- is associated with.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The status message for the conversion task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Time at which the license type conversion task was completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Amount of time to complete the license type conversion.
    licenseConversionTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the license type converted from.
    sourceLicenseContext :: Prelude.Maybe LicenseConversionContext,
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
-- 'status', 'getLicenseConversionTaskResponse_status' - Status of the license type conversion task.
--
-- 'startTime', 'getLicenseConversionTaskResponse_startTime' - Time at which the license type conversion task was started .
--
-- 'destinationLicenseContext', 'getLicenseConversionTaskResponse_destinationLicenseContext' - Information about the license type converted to.
--
-- 'licenseConversionTaskId', 'getLicenseConversionTaskResponse_licenseConversionTaskId' - ID of the license type conversion task.
--
-- 'resourceArn', 'getLicenseConversionTaskResponse_resourceArn' - Amazon Resource Names (ARN) of the resources the license conversion task
-- is associated with.
--
-- 'statusMessage', 'getLicenseConversionTaskResponse_statusMessage' - The status message for the conversion task.
--
-- 'endTime', 'getLicenseConversionTaskResponse_endTime' - Time at which the license type conversion task was completed.
--
-- 'licenseConversionTime', 'getLicenseConversionTaskResponse_licenseConversionTime' - Amount of time to complete the license type conversion.
--
-- 'sourceLicenseContext', 'getLicenseConversionTaskResponse_sourceLicenseContext' - Information about the license type converted from.
--
-- 'httpStatus', 'getLicenseConversionTaskResponse_httpStatus' - The response's http status code.
newGetLicenseConversionTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLicenseConversionTaskResponse
newGetLicenseConversionTaskResponse pHttpStatus_ =
  GetLicenseConversionTaskResponse'
    { status =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      destinationLicenseContext =
        Prelude.Nothing,
      licenseConversionTaskId = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      endTime = Prelude.Nothing,
      licenseConversionTime = Prelude.Nothing,
      sourceLicenseContext = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Status of the license type conversion task.
getLicenseConversionTaskResponse_status :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionTaskStatus)
getLicenseConversionTaskResponse_status = Lens.lens (\GetLicenseConversionTaskResponse' {status} -> status) (\s@GetLicenseConversionTaskResponse' {} a -> s {status = a} :: GetLicenseConversionTaskResponse)

-- | Time at which the license type conversion task was started .
getLicenseConversionTaskResponse_startTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_startTime = Lens.lens (\GetLicenseConversionTaskResponse' {startTime} -> startTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {startTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | Information about the license type converted to.
getLicenseConversionTaskResponse_destinationLicenseContext :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionContext)
getLicenseConversionTaskResponse_destinationLicenseContext = Lens.lens (\GetLicenseConversionTaskResponse' {destinationLicenseContext} -> destinationLicenseContext) (\s@GetLicenseConversionTaskResponse' {} a -> s {destinationLicenseContext = a} :: GetLicenseConversionTaskResponse)

-- | ID of the license type conversion task.
getLicenseConversionTaskResponse_licenseConversionTaskId :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_licenseConversionTaskId = Lens.lens (\GetLicenseConversionTaskResponse' {licenseConversionTaskId} -> licenseConversionTaskId) (\s@GetLicenseConversionTaskResponse' {} a -> s {licenseConversionTaskId = a} :: GetLicenseConversionTaskResponse)

-- | Amazon Resource Names (ARN) of the resources the license conversion task
-- is associated with.
getLicenseConversionTaskResponse_resourceArn :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_resourceArn = Lens.lens (\GetLicenseConversionTaskResponse' {resourceArn} -> resourceArn) (\s@GetLicenseConversionTaskResponse' {} a -> s {resourceArn = a} :: GetLicenseConversionTaskResponse)

-- | The status message for the conversion task.
getLicenseConversionTaskResponse_statusMessage :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.Text)
getLicenseConversionTaskResponse_statusMessage = Lens.lens (\GetLicenseConversionTaskResponse' {statusMessage} -> statusMessage) (\s@GetLicenseConversionTaskResponse' {} a -> s {statusMessage = a} :: GetLicenseConversionTaskResponse)

-- | Time at which the license type conversion task was completed.
getLicenseConversionTaskResponse_endTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_endTime = Lens.lens (\GetLicenseConversionTaskResponse' {endTime} -> endTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {endTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | Amount of time to complete the license type conversion.
getLicenseConversionTaskResponse_licenseConversionTime :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getLicenseConversionTaskResponse_licenseConversionTime = Lens.lens (\GetLicenseConversionTaskResponse' {licenseConversionTime} -> licenseConversionTime) (\s@GetLicenseConversionTaskResponse' {} a -> s {licenseConversionTime = a} :: GetLicenseConversionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | Information about the license type converted from.
getLicenseConversionTaskResponse_sourceLicenseContext :: Lens.Lens' GetLicenseConversionTaskResponse (Prelude.Maybe LicenseConversionContext)
getLicenseConversionTaskResponse_sourceLicenseContext = Lens.lens (\GetLicenseConversionTaskResponse' {sourceLicenseContext} -> sourceLicenseContext) (\s@GetLicenseConversionTaskResponse' {} a -> s {sourceLicenseContext = a} :: GetLicenseConversionTaskResponse)

-- | The response's http status code.
getLicenseConversionTaskResponse_httpStatus :: Lens.Lens' GetLicenseConversionTaskResponse Prelude.Int
getLicenseConversionTaskResponse_httpStatus = Lens.lens (\GetLicenseConversionTaskResponse' {httpStatus} -> httpStatus) (\s@GetLicenseConversionTaskResponse' {} a -> s {httpStatus = a} :: GetLicenseConversionTaskResponse)

instance
  Prelude.NFData
    GetLicenseConversionTaskResponse
