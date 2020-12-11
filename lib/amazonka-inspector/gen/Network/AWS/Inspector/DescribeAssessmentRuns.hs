{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment runs that are specified by the ARNs of the assessment runs.
module Network.AWS.Inspector.DescribeAssessmentRuns
  ( -- * Creating a request
    DescribeAssessmentRuns (..),
    mkDescribeAssessmentRuns,

    -- ** Request lenses
    darAssessmentRunARNs,

    -- * Destructuring the response
    DescribeAssessmentRunsResponse (..),
    mkDescribeAssessmentRunsResponse,

    -- ** Response lenses
    darrsResponseStatus,
    darrsAssessmentRuns,
    darrsFailedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAssessmentRuns' smart constructor.
newtype DescribeAssessmentRuns = DescribeAssessmentRuns'
  { assessmentRunARNs ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentRuns' with the minimum fields required to make a request.
--
-- * 'assessmentRunARNs' - The ARN that specifies the assessment run that you want to describe.
mkDescribeAssessmentRuns ::
  -- | 'assessmentRunARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeAssessmentRuns
mkDescribeAssessmentRuns pAssessmentRunARNs_ =
  DescribeAssessmentRuns' {assessmentRunARNs = pAssessmentRunARNs_}

-- | The ARN that specifies the assessment run that you want to describe.
--
-- /Note:/ Consider using 'assessmentRunARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darAssessmentRunARNs :: Lens.Lens' DescribeAssessmentRuns (Lude.NonEmpty Lude.Text)
darAssessmentRunARNs = Lens.lens (assessmentRunARNs :: DescribeAssessmentRuns -> Lude.NonEmpty Lude.Text) (\s a -> s {assessmentRunARNs = a} :: DescribeAssessmentRuns)
{-# DEPRECATED darAssessmentRunARNs "Use generic-lens or generic-optics with 'assessmentRunARNs' instead." #-}

instance Lude.AWSRequest DescribeAssessmentRuns where
  type Rs DescribeAssessmentRuns = DescribeAssessmentRunsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssessmentRunsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "assessmentRuns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeAssessmentRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeAssessmentRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssessmentRuns where
  toJSON DescribeAssessmentRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("assessmentRunArns" Lude..= assessmentRunARNs)]
      )

instance Lude.ToPath DescribeAssessmentRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssessmentRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssessmentRunsResponse' smart constructor.
data DescribeAssessmentRunsResponse = DescribeAssessmentRunsResponse'
  { responseStatus ::
      Lude.Int,
    assessmentRuns ::
      [AssessmentRun],
    failedItems ::
      Lude.HashMap
        Lude.Text
        (FailedItemDetails)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentRunsResponse' with the minimum fields required to make a request.
--
-- * 'assessmentRuns' - Information about the assessment run.
-- * 'failedItems' - Assessment run details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkDescribeAssessmentRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssessmentRunsResponse
mkDescribeAssessmentRunsResponse pResponseStatus_ =
  DescribeAssessmentRunsResponse'
    { responseStatus =
        pResponseStatus_,
      assessmentRuns = Lude.mempty,
      failedItems = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAssessmentRunsResponse Lude.Int
darrsResponseStatus = Lens.lens (responseStatus :: DescribeAssessmentRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssessmentRunsResponse)
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the assessment run.
--
-- /Note:/ Consider using 'assessmentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAssessmentRuns :: Lens.Lens' DescribeAssessmentRunsResponse [AssessmentRun]
darrsAssessmentRuns = Lens.lens (assessmentRuns :: DescribeAssessmentRunsResponse -> [AssessmentRun]) (\s a -> s {assessmentRuns = a} :: DescribeAssessmentRunsResponse)
{-# DEPRECATED darrsAssessmentRuns "Use generic-lens or generic-optics with 'assessmentRuns' instead." #-}

-- | Assessment run details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsFailedItems :: Lens.Lens' DescribeAssessmentRunsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
darrsFailedItems = Lens.lens (failedItems :: DescribeAssessmentRunsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeAssessmentRunsResponse)
{-# DEPRECATED darrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}
