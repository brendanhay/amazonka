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
-- Module      : Amazonka.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a
-- specified @JobId@.
module Amazonka.Snowball.GetSoftwareUpdates
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newGetSoftwareUpdates' smart constructor.
data GetSoftwareUpdates = GetSoftwareUpdates'
  { -- | The ID for a job that you want to get the software update file for, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetSoftwareUpdates
newGetSoftwareUpdates pJobId_ =
  GetSoftwareUpdates' {jobId = pJobId_}

-- | The ID for a job that you want to get the software update file for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
getSoftwareUpdates_jobId :: Lens.Lens' GetSoftwareUpdates Prelude.Text
getSoftwareUpdates_jobId = Lens.lens (\GetSoftwareUpdates' {jobId} -> jobId) (\s@GetSoftwareUpdates' {} a -> s {jobId = a} :: GetSoftwareUpdates)

instance Core.AWSRequest GetSoftwareUpdates where
  type
    AWSResponse GetSoftwareUpdates =
      GetSoftwareUpdatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSoftwareUpdatesResponse'
            Prelude.<$> (x Data..?> "UpdatesURI")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSoftwareUpdates where
  hashWithSalt _salt GetSoftwareUpdates' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetSoftwareUpdates where
  rnf GetSoftwareUpdates' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetSoftwareUpdates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.GetSoftwareUpdates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSoftwareUpdates where
  toJSON GetSoftwareUpdates' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath GetSoftwareUpdates where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSoftwareUpdates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { -- | The Amazon S3 presigned URL for the update file associated with the
    -- specified @JobId@ value. The software update will be available for 2
    -- days after this request is made. To access an update after the 2 days
    -- have passed, you\'ll have to make another call to @GetSoftwareUpdates@.
    updatesURI :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetSoftwareUpdatesResponse
newGetSoftwareUpdatesResponse pHttpStatus_ =
  GetSoftwareUpdatesResponse'
    { updatesURI =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 presigned URL for the update file associated with the
-- specified @JobId@ value. The software update will be available for 2
-- days after this request is made. To access an update after the 2 days
-- have passed, you\'ll have to make another call to @GetSoftwareUpdates@.
getSoftwareUpdatesResponse_updatesURI :: Lens.Lens' GetSoftwareUpdatesResponse (Prelude.Maybe Prelude.Text)
getSoftwareUpdatesResponse_updatesURI = Lens.lens (\GetSoftwareUpdatesResponse' {updatesURI} -> updatesURI) (\s@GetSoftwareUpdatesResponse' {} a -> s {updatesURI = a} :: GetSoftwareUpdatesResponse)

-- | The response's http status code.
getSoftwareUpdatesResponse_httpStatus :: Lens.Lens' GetSoftwareUpdatesResponse Prelude.Int
getSoftwareUpdatesResponse_httpStatus = Lens.lens (\GetSoftwareUpdatesResponse' {httpStatus} -> httpStatus) (\s@GetSoftwareUpdatesResponse' {} a -> s {httpStatus = a} :: GetSoftwareUpdatesResponse)

instance Prelude.NFData GetSoftwareUpdatesResponse where
  rnf GetSoftwareUpdatesResponse' {..} =
    Prelude.rnf updatesURI
      `Prelude.seq` Prelude.rnf httpStatus
