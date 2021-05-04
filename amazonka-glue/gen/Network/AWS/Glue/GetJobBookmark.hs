{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.GetJobBookmark
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a job bookmark entry.
module Network.AWS.Glue.GetJobBookmark
  ( -- * Creating a Request
    GetJobBookmark (..),
    newGetJobBookmark,

    -- * Request Lenses
    getJobBookmark_runId,
    getJobBookmark_jobName,

    -- * Destructuring the Response
    GetJobBookmarkResponse (..),
    newGetJobBookmarkResponse,

    -- * Response Lenses
    getJobBookmarkResponse_jobBookmarkEntry,
    getJobBookmarkResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobBookmark' smart constructor.
data GetJobBookmark = GetJobBookmark'
  { -- | The unique run identifier associated with this job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of the job in question.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetJobBookmark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'getJobBookmark_runId' - The unique run identifier associated with this job run.
--
-- 'jobName', 'getJobBookmark_jobName' - The name of the job in question.
newGetJobBookmark ::
  -- | 'jobName'
  Prelude.Text ->
  GetJobBookmark
newGetJobBookmark pJobName_ =
  GetJobBookmark'
    { runId = Prelude.Nothing,
      jobName = pJobName_
    }

-- | The unique run identifier associated with this job run.
getJobBookmark_runId :: Lens.Lens' GetJobBookmark (Prelude.Maybe Prelude.Text)
getJobBookmark_runId = Lens.lens (\GetJobBookmark' {runId} -> runId) (\s@GetJobBookmark' {} a -> s {runId = a} :: GetJobBookmark)

-- | The name of the job in question.
getJobBookmark_jobName :: Lens.Lens' GetJobBookmark Prelude.Text
getJobBookmark_jobName = Lens.lens (\GetJobBookmark' {jobName} -> jobName) (\s@GetJobBookmark' {} a -> s {jobName = a} :: GetJobBookmark)

instance Prelude.AWSRequest GetJobBookmark where
  type Rs GetJobBookmark = GetJobBookmarkResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobBookmarkResponse'
            Prelude.<$> (x Prelude..?> "JobBookmarkEntry")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobBookmark

instance Prelude.NFData GetJobBookmark

instance Prelude.ToHeaders GetJobBookmark where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetJobBookmark" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetJobBookmark where
  toJSON GetJobBookmark' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RunId" Prelude..=) Prelude.<$> runId,
            Prelude.Just ("JobName" Prelude..= jobName)
          ]
      )

instance Prelude.ToPath GetJobBookmark where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetJobBookmark where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobBookmarkResponse' smart constructor.
data GetJobBookmarkResponse = GetJobBookmarkResponse'
  { -- | A structure that defines a point that a job can resume processing.
    jobBookmarkEntry :: Prelude.Maybe JobBookmarkEntry,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetJobBookmarkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobBookmarkEntry', 'getJobBookmarkResponse_jobBookmarkEntry' - A structure that defines a point that a job can resume processing.
--
-- 'httpStatus', 'getJobBookmarkResponse_httpStatus' - The response's http status code.
newGetJobBookmarkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobBookmarkResponse
newGetJobBookmarkResponse pHttpStatus_ =
  GetJobBookmarkResponse'
    { jobBookmarkEntry =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that defines a point that a job can resume processing.
getJobBookmarkResponse_jobBookmarkEntry :: Lens.Lens' GetJobBookmarkResponse (Prelude.Maybe JobBookmarkEntry)
getJobBookmarkResponse_jobBookmarkEntry = Lens.lens (\GetJobBookmarkResponse' {jobBookmarkEntry} -> jobBookmarkEntry) (\s@GetJobBookmarkResponse' {} a -> s {jobBookmarkEntry = a} :: GetJobBookmarkResponse)

-- | The response's http status code.
getJobBookmarkResponse_httpStatus :: Lens.Lens' GetJobBookmarkResponse Prelude.Int
getJobBookmarkResponse_httpStatus = Lens.lens (\GetJobBookmarkResponse' {httpStatus} -> httpStatus) (\s@GetJobBookmarkResponse' {} a -> s {httpStatus = a} :: GetJobBookmarkResponse)

instance Prelude.NFData GetJobBookmarkResponse
