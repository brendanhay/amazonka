{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a job bookmark entry.
module Network.AWS.Glue.GetJobBookmark
  ( -- * Creating a request
    GetJobBookmark (..),
    mkGetJobBookmark,

    -- ** Request lenses
    gjbJobName,
    gjbRunId,

    -- * Destructuring the response
    GetJobBookmarkResponse (..),
    mkGetJobBookmarkResponse,

    -- ** Response lenses
    gjbrsJobBookmarkEntry,
    gjbrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobBookmark' smart constructor.
data GetJobBookmark = GetJobBookmark'
  { -- | The name of the job in question.
    jobName :: Lude.Text,
    -- | The unique run identifier associated with this job run.
    runId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobBookmark' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job in question.
-- * 'runId' - The unique run identifier associated with this job run.
mkGetJobBookmark ::
  -- | 'jobName'
  Lude.Text ->
  GetJobBookmark
mkGetJobBookmark pJobName_ =
  GetJobBookmark' {jobName = pJobName_, runId = Lude.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbJobName :: Lens.Lens' GetJobBookmark Lude.Text
gjbJobName = Lens.lens (jobName :: GetJobBookmark -> Lude.Text) (\s a -> s {jobName = a} :: GetJobBookmark)
{-# DEPRECATED gjbJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbRunId :: Lens.Lens' GetJobBookmark (Lude.Maybe Lude.Text)
gjbRunId = Lens.lens (runId :: GetJobBookmark -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: GetJobBookmark)
{-# DEPRECATED gjbRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest GetJobBookmark where
  type Rs GetJobBookmark = GetJobBookmarkResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobBookmarkResponse'
            Lude.<$> (x Lude..?> "JobBookmarkEntry")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobBookmark where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetJobBookmark" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobBookmark where
  toJSON GetJobBookmark' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobName" Lude..= jobName),
            ("RunId" Lude..=) Lude.<$> runId
          ]
      )

instance Lude.ToPath GetJobBookmark where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobBookmark where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobBookmarkResponse' smart constructor.
data GetJobBookmarkResponse = GetJobBookmarkResponse'
  { -- | A structure that defines a point that a job can resume processing.
    jobBookmarkEntry :: Lude.Maybe JobBookmarkEntry,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobBookmarkResponse' with the minimum fields required to make a request.
--
-- * 'jobBookmarkEntry' - A structure that defines a point that a job can resume processing.
-- * 'responseStatus' - The response status code.
mkGetJobBookmarkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobBookmarkResponse
mkGetJobBookmarkResponse pResponseStatus_ =
  GetJobBookmarkResponse'
    { jobBookmarkEntry = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that defines a point that a job can resume processing.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrsJobBookmarkEntry :: Lens.Lens' GetJobBookmarkResponse (Lude.Maybe JobBookmarkEntry)
gjbrsJobBookmarkEntry = Lens.lens (jobBookmarkEntry :: GetJobBookmarkResponse -> Lude.Maybe JobBookmarkEntry) (\s a -> s {jobBookmarkEntry = a} :: GetJobBookmarkResponse)
{-# DEPRECATED gjbrsJobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrsResponseStatus :: Lens.Lens' GetJobBookmarkResponse Lude.Int
gjbrsResponseStatus = Lens.lens (responseStatus :: GetJobBookmarkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobBookmarkResponse)
{-# DEPRECATED gjbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
