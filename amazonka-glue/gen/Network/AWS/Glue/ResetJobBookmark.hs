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
-- Module      : Network.AWS.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
module Network.AWS.Glue.ResetJobBookmark
  ( -- * Creating a Request
    ResetJobBookmark (..),
    newResetJobBookmark,

    -- * Request Lenses
    resetJobBookmark_runId,
    resetJobBookmark_jobName,

    -- * Destructuring the Response
    ResetJobBookmarkResponse (..),
    newResetJobBookmarkResponse,

    -- * Response Lenses
    resetJobBookmarkResponse_jobBookmarkEntry,
    resetJobBookmarkResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetJobBookmark' smart constructor.
data ResetJobBookmark = ResetJobBookmark'
  { -- | The unique run identifier associated with this job run.
    runId :: Core.Maybe Core.Text,
    -- | The name of the job in question.
    jobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetJobBookmark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'resetJobBookmark_runId' - The unique run identifier associated with this job run.
--
-- 'jobName', 'resetJobBookmark_jobName' - The name of the job in question.
newResetJobBookmark ::
  -- | 'jobName'
  Core.Text ->
  ResetJobBookmark
newResetJobBookmark pJobName_ =
  ResetJobBookmark'
    { runId = Core.Nothing,
      jobName = pJobName_
    }

-- | The unique run identifier associated with this job run.
resetJobBookmark_runId :: Lens.Lens' ResetJobBookmark (Core.Maybe Core.Text)
resetJobBookmark_runId = Lens.lens (\ResetJobBookmark' {runId} -> runId) (\s@ResetJobBookmark' {} a -> s {runId = a} :: ResetJobBookmark)

-- | The name of the job in question.
resetJobBookmark_jobName :: Lens.Lens' ResetJobBookmark Core.Text
resetJobBookmark_jobName = Lens.lens (\ResetJobBookmark' {jobName} -> jobName) (\s@ResetJobBookmark' {} a -> s {jobName = a} :: ResetJobBookmark)

instance Core.AWSRequest ResetJobBookmark where
  type
    AWSResponse ResetJobBookmark =
      ResetJobBookmarkResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetJobBookmarkResponse'
            Core.<$> (x Core..?> "JobBookmarkEntry")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResetJobBookmark

instance Core.NFData ResetJobBookmark

instance Core.ToHeaders ResetJobBookmark where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ResetJobBookmark" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResetJobBookmark where
  toJSON ResetJobBookmark' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RunId" Core..=) Core.<$> runId,
            Core.Just ("JobName" Core..= jobName)
          ]
      )

instance Core.ToPath ResetJobBookmark where
  toPath = Core.const "/"

instance Core.ToQuery ResetJobBookmark where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newResetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { -- | The reset bookmark entry.
    jobBookmarkEntry :: Core.Maybe JobBookmarkEntry,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetJobBookmarkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobBookmarkEntry', 'resetJobBookmarkResponse_jobBookmarkEntry' - The reset bookmark entry.
--
-- 'httpStatus', 'resetJobBookmarkResponse_httpStatus' - The response's http status code.
newResetJobBookmarkResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResetJobBookmarkResponse
newResetJobBookmarkResponse pHttpStatus_ =
  ResetJobBookmarkResponse'
    { jobBookmarkEntry =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The reset bookmark entry.
resetJobBookmarkResponse_jobBookmarkEntry :: Lens.Lens' ResetJobBookmarkResponse (Core.Maybe JobBookmarkEntry)
resetJobBookmarkResponse_jobBookmarkEntry = Lens.lens (\ResetJobBookmarkResponse' {jobBookmarkEntry} -> jobBookmarkEntry) (\s@ResetJobBookmarkResponse' {} a -> s {jobBookmarkEntry = a} :: ResetJobBookmarkResponse)

-- | The response's http status code.
resetJobBookmarkResponse_httpStatus :: Lens.Lens' ResetJobBookmarkResponse Core.Int
resetJobBookmarkResponse_httpStatus = Lens.lens (\ResetJobBookmarkResponse' {httpStatus} -> httpStatus) (\s@ResetJobBookmarkResponse' {} a -> s {httpStatus = a} :: ResetJobBookmarkResponse)

instance Core.NFData ResetJobBookmarkResponse
