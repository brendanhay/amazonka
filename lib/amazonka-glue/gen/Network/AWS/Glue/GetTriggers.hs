{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gNextToken,
    gMaxResults,
    gDependentJobName,

    -- * Destructuring the response
    GetTriggersResponse (..),
    mkGetTriggersResponse,

    -- ** Response lenses
    gtgrsTriggers,
    gtgrsNextToken,
    gtgrsResponseStatus,
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
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
    dependentJobName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTriggers' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'maxResults' - The maximum size of the response.
-- * 'dependentJobName' - The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
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
gNextToken :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Text)
gNextToken = Lens.lens (nextToken :: GetTriggers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTriggers)
{-# DEPRECATED gNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gMaxResults :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Natural)
gMaxResults = Lens.lens (maxResults :: GetTriggers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTriggers)
{-# DEPRECATED gMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDependentJobName :: Lens.Lens' GetTriggers (Lude.Maybe Lude.Text)
gDependentJobName = Lens.lens (dependentJobName :: GetTriggers -> Lude.Maybe Lude.Text) (\s a -> s {dependentJobName = a} :: GetTriggers)
{-# DEPRECATED gDependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead." #-}

instance Page.AWSPager GetTriggers where
  page rq rs
    | Page.stop (rs Lens.^. gtgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtgrsTriggers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gNextToken Lens..~ rs Lens.^. gtgrsNextToken

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
  { -- | A list of triggers for the specified job.
    triggers :: Lude.Maybe [Trigger],
    -- | A continuation token, if not all the requested triggers have yet been returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTriggersResponse' with the minimum fields required to make a request.
--
-- * 'triggers' - A list of triggers for the specified job.
-- * 'nextToken' - A continuation token, if not all the requested triggers have yet been returned.
-- * 'responseStatus' - The response status code.
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
gtgrsTriggers :: Lens.Lens' GetTriggersResponse (Lude.Maybe [Trigger])
gtgrsTriggers = Lens.lens (triggers :: GetTriggersResponse -> Lude.Maybe [Trigger]) (\s a -> s {triggers = a} :: GetTriggersResponse)
{-# DEPRECATED gtgrsTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | A continuation token, if not all the requested triggers have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrsNextToken :: Lens.Lens' GetTriggersResponse (Lude.Maybe Lude.Text)
gtgrsNextToken = Lens.lens (nextToken :: GetTriggersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTriggersResponse)
{-# DEPRECATED gtgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrsResponseStatus :: Lens.Lens' GetTriggersResponse Lude.Int
gtgrsResponseStatus = Lens.lens (responseStatus :: GetTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTriggersResponse)
{-# DEPRECATED gtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
