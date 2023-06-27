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
-- Module      : Amazonka.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
--
-- For more information about enabling and using job bookmarks, see:
--
-- -   <https://docs.aws.amazon.com/glue/latest/dg/monitor-continuations.html Tracking processed data using job bookmarks>
--
-- -   <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Job parameters used by Glue>
--
-- -   <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-jobs-job.html#aws-glue-api-jobs-job-Job Job structure>
module Amazonka.Glue.ResetJobBookmark
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetJobBookmark' smart constructor.
data ResetJobBookmark = ResetJobBookmark'
  { -- | The unique run identifier associated with this job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of the job in question.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ResetJobBookmark
newResetJobBookmark pJobName_ =
  ResetJobBookmark'
    { runId = Prelude.Nothing,
      jobName = pJobName_
    }

-- | The unique run identifier associated with this job run.
resetJobBookmark_runId :: Lens.Lens' ResetJobBookmark (Prelude.Maybe Prelude.Text)
resetJobBookmark_runId = Lens.lens (\ResetJobBookmark' {runId} -> runId) (\s@ResetJobBookmark' {} a -> s {runId = a} :: ResetJobBookmark)

-- | The name of the job in question.
resetJobBookmark_jobName :: Lens.Lens' ResetJobBookmark Prelude.Text
resetJobBookmark_jobName = Lens.lens (\ResetJobBookmark' {jobName} -> jobName) (\s@ResetJobBookmark' {} a -> s {jobName = a} :: ResetJobBookmark)

instance Core.AWSRequest ResetJobBookmark where
  type
    AWSResponse ResetJobBookmark =
      ResetJobBookmarkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetJobBookmarkResponse'
            Prelude.<$> (x Data..?> "JobBookmarkEntry")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetJobBookmark where
  hashWithSalt _salt ResetJobBookmark' {..} =
    _salt
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData ResetJobBookmark where
  rnf ResetJobBookmark' {..} =
    Prelude.rnf runId `Prelude.seq` Prelude.rnf jobName

instance Data.ToHeaders ResetJobBookmark where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ResetJobBookmark" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetJobBookmark where
  toJSON ResetJobBookmark' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RunId" Data..=) Prelude.<$> runId,
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )

instance Data.ToPath ResetJobBookmark where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetJobBookmark where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { -- | The reset bookmark entry.
    jobBookmarkEntry :: Prelude.Maybe JobBookmarkEntry,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ResetJobBookmarkResponse
newResetJobBookmarkResponse pHttpStatus_ =
  ResetJobBookmarkResponse'
    { jobBookmarkEntry =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The reset bookmark entry.
resetJobBookmarkResponse_jobBookmarkEntry :: Lens.Lens' ResetJobBookmarkResponse (Prelude.Maybe JobBookmarkEntry)
resetJobBookmarkResponse_jobBookmarkEntry = Lens.lens (\ResetJobBookmarkResponse' {jobBookmarkEntry} -> jobBookmarkEntry) (\s@ResetJobBookmarkResponse' {} a -> s {jobBookmarkEntry = a} :: ResetJobBookmarkResponse)

-- | The response's http status code.
resetJobBookmarkResponse_httpStatus :: Lens.Lens' ResetJobBookmarkResponse Prelude.Int
resetJobBookmarkResponse_httpStatus = Lens.lens (\ResetJobBookmarkResponse' {httpStatus} -> httpStatus) (\s@ResetJobBookmarkResponse' {} a -> s {httpStatus = a} :: ResetJobBookmarkResponse)

instance Prelude.NFData ResetJobBookmarkResponse where
  rnf ResetJobBookmarkResponse' {..} =
    Prelude.rnf jobBookmarkEntry
      `Prelude.seq` Prelude.rnf httpStatus
