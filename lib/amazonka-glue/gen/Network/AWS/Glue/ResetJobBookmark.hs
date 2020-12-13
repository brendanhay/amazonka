{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
module Network.AWS.Glue.ResetJobBookmark
  ( -- * Creating a request
    ResetJobBookmark (..),
    mkResetJobBookmark,

    -- ** Request lenses
    rjbJobName,
    rjbRunId,

    -- * Destructuring the response
    ResetJobBookmarkResponse (..),
    mkResetJobBookmarkResponse,

    -- ** Response lenses
    rjbrsJobBookmarkEntry,
    rjbrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetJobBookmark' smart constructor.
data ResetJobBookmark = ResetJobBookmark'
  { -- | The name of the job in question.
    jobName :: Lude.Text,
    -- | The unique run identifier associated with this job run.
    runId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetJobBookmark' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job in question.
-- * 'runId' - The unique run identifier associated with this job run.
mkResetJobBookmark ::
  -- | 'jobName'
  Lude.Text ->
  ResetJobBookmark
mkResetJobBookmark pJobName_ =
  ResetJobBookmark' {jobName = pJobName_, runId = Lude.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbJobName :: Lens.Lens' ResetJobBookmark Lude.Text
rjbJobName = Lens.lens (jobName :: ResetJobBookmark -> Lude.Text) (\s a -> s {jobName = a} :: ResetJobBookmark)
{-# DEPRECATED rjbJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbRunId :: Lens.Lens' ResetJobBookmark (Lude.Maybe Lude.Text)
rjbRunId = Lens.lens (runId :: ResetJobBookmark -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: ResetJobBookmark)
{-# DEPRECATED rjbRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest ResetJobBookmark where
  type Rs ResetJobBookmark = ResetJobBookmarkResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResetJobBookmarkResponse'
            Lude.<$> (x Lude..?> "JobBookmarkEntry")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetJobBookmark where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ResetJobBookmark" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetJobBookmark where
  toJSON ResetJobBookmark' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobName" Lude..= jobName),
            ("RunId" Lude..=) Lude.<$> runId
          ]
      )

instance Lude.ToPath ResetJobBookmark where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetJobBookmark where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { -- | The reset bookmark entry.
    jobBookmarkEntry :: Lude.Maybe JobBookmarkEntry,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetJobBookmarkResponse' with the minimum fields required to make a request.
--
-- * 'jobBookmarkEntry' - The reset bookmark entry.
-- * 'responseStatus' - The response status code.
mkResetJobBookmarkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetJobBookmarkResponse
mkResetJobBookmarkResponse pResponseStatus_ =
  ResetJobBookmarkResponse'
    { jobBookmarkEntry = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The reset bookmark entry.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrsJobBookmarkEntry :: Lens.Lens' ResetJobBookmarkResponse (Lude.Maybe JobBookmarkEntry)
rjbrsJobBookmarkEntry = Lens.lens (jobBookmarkEntry :: ResetJobBookmarkResponse -> Lude.Maybe JobBookmarkEntry) (\s a -> s {jobBookmarkEntry = a} :: ResetJobBookmarkResponse)
{-# DEPRECATED rjbrsJobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrsResponseStatus :: Lens.Lens' ResetJobBookmarkResponse Lude.Int
rjbrsResponseStatus = Lens.lens (responseStatus :: ResetJobBookmarkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetJobBookmarkResponse)
{-# DEPRECATED rjbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
