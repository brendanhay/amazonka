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
-- Module      : Network.AWS.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a
-- specified @JobId@.
module Network.AWS.Snowball.GetSoftwareUpdates
  ( -- * Creating a Request
    GetSoftwareUpdates (..),
    newGetSoftwareUpdates,

    -- * Request Lenses
    getSoftwareUpdates_jobId,

    -- * Destructuring the Response
    GetSoftwareUpdatesResponse (..),
    newGetSoftwareUpdatesResponse,

    -- * Response Lenses
    getSoftwareUpdatesResponse_updatesURI,
    getSoftwareUpdatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newGetSoftwareUpdates' smart constructor.
data GetSoftwareUpdates = GetSoftwareUpdates'
  { -- | The ID for a job that you want to get the software update file for, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSoftwareUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getSoftwareUpdates_jobId' - The ID for a job that you want to get the software update file for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
newGetSoftwareUpdates ::
  -- | 'jobId'
  Core.Text ->
  GetSoftwareUpdates
newGetSoftwareUpdates pJobId_ =
  GetSoftwareUpdates' {jobId = pJobId_}

-- | The ID for a job that you want to get the software update file for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
getSoftwareUpdates_jobId :: Lens.Lens' GetSoftwareUpdates Core.Text
getSoftwareUpdates_jobId = Lens.lens (\GetSoftwareUpdates' {jobId} -> jobId) (\s@GetSoftwareUpdates' {} a -> s {jobId = a} :: GetSoftwareUpdates)

instance Core.AWSRequest GetSoftwareUpdates where
  type
    AWSResponse GetSoftwareUpdates =
      GetSoftwareUpdatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSoftwareUpdatesResponse'
            Core.<$> (x Core..?> "UpdatesURI")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSoftwareUpdates

instance Core.NFData GetSoftwareUpdates

instance Core.ToHeaders GetSoftwareUpdates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.GetSoftwareUpdates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSoftwareUpdates where
  toJSON GetSoftwareUpdates' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath GetSoftwareUpdates where
  toPath = Core.const "/"

instance Core.ToQuery GetSoftwareUpdates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { -- | The Amazon S3 presigned URL for the update file associated with the
    -- specified @JobId@ value. The software update will be available for 2
    -- days after this request is made. To access an update after the 2 days
    -- have passed, you\'ll have to make another call to @GetSoftwareUpdates@.
    updatesURI :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSoftwareUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatesURI', 'getSoftwareUpdatesResponse_updatesURI' - The Amazon S3 presigned URL for the update file associated with the
-- specified @JobId@ value. The software update will be available for 2
-- days after this request is made. To access an update after the 2 days
-- have passed, you\'ll have to make another call to @GetSoftwareUpdates@.
--
-- 'httpStatus', 'getSoftwareUpdatesResponse_httpStatus' - The response's http status code.
newGetSoftwareUpdatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSoftwareUpdatesResponse
newGetSoftwareUpdatesResponse pHttpStatus_ =
  GetSoftwareUpdatesResponse'
    { updatesURI =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 presigned URL for the update file associated with the
-- specified @JobId@ value. The software update will be available for 2
-- days after this request is made. To access an update after the 2 days
-- have passed, you\'ll have to make another call to @GetSoftwareUpdates@.
getSoftwareUpdatesResponse_updatesURI :: Lens.Lens' GetSoftwareUpdatesResponse (Core.Maybe Core.Text)
getSoftwareUpdatesResponse_updatesURI = Lens.lens (\GetSoftwareUpdatesResponse' {updatesURI} -> updatesURI) (\s@GetSoftwareUpdatesResponse' {} a -> s {updatesURI = a} :: GetSoftwareUpdatesResponse)

-- | The response's http status code.
getSoftwareUpdatesResponse_httpStatus :: Lens.Lens' GetSoftwareUpdatesResponse Core.Int
getSoftwareUpdatesResponse_httpStatus = Lens.lens (\GetSoftwareUpdatesResponse' {httpStatus} -> httpStatus) (\s@GetSoftwareUpdatesResponse' {} a -> s {httpStatus = a} :: GetSoftwareUpdatesResponse)

instance Core.NFData GetSoftwareUpdatesResponse
