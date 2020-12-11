{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the triggers associated with a job.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTriggers
  ( -- * Creating a request
    GetTriggers (..),
    mkGetTriggers,

    -- ** Request lenses
    gtNextToken,
    gtMaxResults,
    gtDependentJobName,

    -- * Destructuring the response
    GetTriggersResponse (..),
    mkGetTriggersResponse,

    -- ** Response lenses
    gttrsTriggers,
    gttrsNextToken,
    gttrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    dependentJobName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTriggers' with the minimum fields required to make a request.
--
-- * 'dependentJobName' - The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
-- * 'maxResults' - The maximum size of the response.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkGetTriggers ::
  GetTriggers
mkGetTriggers =
  GetTriggers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      dependentJobName = Lude.Nothing
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtNextToken :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Text)
gtNextToken = Lens.lens (nextToken :: GetTriggers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTriggers)
{-# DEPRECATED gtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtMaxResults :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Natural)
gtMaxResults = Lens.lens (maxResults :: GetTriggers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTriggers)
{-# DEPRECATED gtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDependentJobName :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Text)
gtDependentJobName = Lens.lens (dependentJobName :: GetTriggers -> Lude.Maybe Lude.Text) (\s a -> s {dependentJobName = a} :: GetTriggers)
{-# DEPRECATED gtDependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead." #-}

instance Page.AWSPager GetTriggers where
  page rq rs
    | Page.stop (rs Lens.^. gttrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gttrsTriggers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtNextToken Lens..~ rs Lens.^. gttrsNextToken

instance Lude.AWSRequest GetTriggers where
  type Rs GetTriggers = GetTriggersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTriggersResponse'
            Lude.<$> (x Lude..?> "Triggers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTriggers where
  toJSON GetTriggers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("DependentJobName" Lude..=) Lude.<$> dependentJobName
          ]
      )

instance Lude.ToPath GetTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTriggers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { triggers ::
      Lude.Maybe [Trigger],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTriggersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if not all the requested triggers have yet been returned.
-- * 'responseStatus' - The response status code.
-- * 'triggers' - A list of triggers for the specified job.
mkGetTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTriggersResponse
mkGetTriggersResponse pResponseStatus_ =
  GetTriggersResponse'
    { triggers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of triggers for the specified job.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttrsTriggers :: Lens.Lens' GetTriggersResponse (Lude.Maybe [Trigger])
gttrsTriggers = Lens.lens (triggers :: GetTriggersResponse -> Lude.Maybe [Trigger]) (\s a -> s {triggers = a} :: GetTriggersResponse)
{-# DEPRECATED gttrsTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | A continuation token, if not all the requested triggers have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttrsNextToken :: Lens.Lens' GetTriggersResponse (Lude.Maybe Lude.Text)
gttrsNextToken = Lens.lens (nextToken :: GetTriggersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTriggersResponse)
{-# DEPRECATED gttrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttrsResponseStatus :: Lens.Lens' GetTriggersResponse Lude.Int
gttrsResponseStatus = Lens.lens (responseStatus :: GetTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTriggersResponse)
{-# DEPRECATED gttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
