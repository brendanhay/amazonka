{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment targets that are specified by the ARNs of the assessment targets.
module Network.AWS.Inspector.DescribeAssessmentTargets
  ( -- * Creating a request
    DescribeAssessmentTargets (..),
    mkDescribeAssessmentTargets,

    -- ** Request lenses
    datAssessmentTargetARNs,

    -- * Destructuring the response
    DescribeAssessmentTargetsResponse (..),
    mkDescribeAssessmentTargetsResponse,

    -- ** Response lenses
    drsAssessmentTargets,
    drsFailedItems,
    drsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAssessmentTargets' smart constructor.
newtype DescribeAssessmentTargets = DescribeAssessmentTargets'
  { -- | The ARNs that specifies the assessment targets that you want to describe.
    assessmentTargetARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentTargets' with the minimum fields required to make a request.
--
-- * 'assessmentTargetARNs' - The ARNs that specifies the assessment targets that you want to describe.
mkDescribeAssessmentTargets ::
  -- | 'assessmentTargetARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeAssessmentTargets
mkDescribeAssessmentTargets pAssessmentTargetARNs_ =
  DescribeAssessmentTargets'
    { assessmentTargetARNs =
        pAssessmentTargetARNs_
    }

-- | The ARNs that specifies the assessment targets that you want to describe.
--
-- /Note:/ Consider using 'assessmentTargetARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTargetARNs :: Lens.Lens' DescribeAssessmentTargets (Lude.NonEmpty Lude.Text)
datAssessmentTargetARNs = Lens.lens (assessmentTargetARNs :: DescribeAssessmentTargets -> Lude.NonEmpty Lude.Text) (\s a -> s {assessmentTargetARNs = a} :: DescribeAssessmentTargets)
{-# DEPRECATED datAssessmentTargetARNs "Use generic-lens or generic-optics with 'assessmentTargetARNs' instead." #-}

instance Lude.AWSRequest DescribeAssessmentTargets where
  type
    Rs DescribeAssessmentTargets =
      DescribeAssessmentTargetsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssessmentTargetsResponse'
            Lude.<$> (x Lude..?> "assessmentTargets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAssessmentTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeAssessmentTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssessmentTargets where
  toJSON DescribeAssessmentTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("assessmentTargetArns" Lude..= assessmentTargetARNs)]
      )

instance Lude.ToPath DescribeAssessmentTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssessmentTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssessmentTargetsResponse' smart constructor.
data DescribeAssessmentTargetsResponse = DescribeAssessmentTargetsResponse'
  { -- | Information about the assessment targets.
    assessmentTargets :: [AssessmentTarget],
    -- | Assessment target details that cannot be described. An error code is provided for each failed item.
    failedItems :: Lude.HashMap Lude.Text (FailedItemDetails),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentTargetsResponse' with the minimum fields required to make a request.
--
-- * 'assessmentTargets' - Information about the assessment targets.
-- * 'failedItems' - Assessment target details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkDescribeAssessmentTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssessmentTargetsResponse
mkDescribeAssessmentTargetsResponse pResponseStatus_ =
  DescribeAssessmentTargetsResponse'
    { assessmentTargets =
        Lude.mempty,
      failedItems = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Information about the assessment targets.
--
-- /Note:/ Consider using 'assessmentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAssessmentTargets :: Lens.Lens' DescribeAssessmentTargetsResponse [AssessmentTarget]
drsAssessmentTargets = Lens.lens (assessmentTargets :: DescribeAssessmentTargetsResponse -> [AssessmentTarget]) (\s a -> s {assessmentTargets = a} :: DescribeAssessmentTargetsResponse)
{-# DEPRECATED drsAssessmentTargets "Use generic-lens or generic-optics with 'assessmentTargets' instead." #-}

-- | Assessment target details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFailedItems :: Lens.Lens' DescribeAssessmentTargetsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
drsFailedItems = Lens.lens (failedItems :: DescribeAssessmentTargetsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeAssessmentTargetsResponse)
{-# DEPRECATED drsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAssessmentTargetsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeAssessmentTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssessmentTargetsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
