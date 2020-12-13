{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For more information about assessment targets, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets> .
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTargets
  ( -- * Creating a request
    ListAssessmentTargets (..),
    mkListAssessmentTargets,

    -- ** Request lenses
    latNextToken,
    latFilter,
    latMaxResults,

    -- * Destructuring the response
    ListAssessmentTargetsResponse (..),
    mkListAssessmentTargetsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsAssessmentTargetARNs,
    lrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | You can use this parameter to specify a subset of data to be included in the action's response.
    --
    -- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
    filter :: Lude.Maybe AssessmentTargetFilter,
    -- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentTargets' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'filter' - You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
mkListAssessmentTargets ::
  ListAssessmentTargets
mkListAssessmentTargets =
  ListAssessmentTargets'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListAssessmentTargets (Lude.Maybe Lude.Text)
latNextToken = Lens.lens (nextToken :: ListAssessmentTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentTargets)
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latFilter :: Lens.Lens' ListAssessmentTargets (Lude.Maybe AssessmentTargetFilter)
latFilter = Lens.lens (filter :: ListAssessmentTargets -> Lude.Maybe AssessmentTargetFilter) (\s a -> s {filter = a} :: ListAssessmentTargets)
{-# DEPRECATED latFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaxResults :: Lens.Lens' ListAssessmentTargets (Lude.Maybe Lude.Int)
latMaxResults = Lens.lens (maxResults :: ListAssessmentTargets -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAssessmentTargets)
{-# DEPRECATED latMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAssessmentTargets where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsAssessmentTargetARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& latNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListAssessmentTargets where
  type Rs ListAssessmentTargets = ListAssessmentTargetsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssessmentTargetsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "assessmentTargetArns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssessmentTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListAssessmentTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssessmentTargets where
  toJSON ListAssessmentTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAssessmentTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssessmentTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssessmentTargetsResponse' smart constructor.
data ListAssessmentTargetsResponse = ListAssessmentTargetsResponse'
  { -- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of ARNs that specifies the assessment targets that are returned by the action.
    assessmentTargetARNs :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentTargetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'assessmentTargetARNs' - A list of ARNs that specifies the assessment targets that are returned by the action.
-- * 'responseStatus' - The response status code.
mkListAssessmentTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssessmentTargetsResponse
mkListAssessmentTargetsResponse pResponseStatus_ =
  ListAssessmentTargetsResponse'
    { nextToken = Lude.Nothing,
      assessmentTargetARNs = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListAssessmentTargetsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListAssessmentTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentTargetsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of ARNs that specifies the assessment targets that are returned by the action.
--
-- /Note:/ Consider using 'assessmentTargetARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsAssessmentTargetARNs :: Lens.Lens' ListAssessmentTargetsResponse [Lude.Text]
lrsAssessmentTargetARNs = Lens.lens (assessmentTargetARNs :: ListAssessmentTargetsResponse -> [Lude.Text]) (\s a -> s {assessmentTargetARNs = a} :: ListAssessmentTargetsResponse)
{-# DEPRECATED lrsAssessmentTargetARNs "Use generic-lens or generic-optics with 'assessmentTargetARNs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListAssessmentTargetsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListAssessmentTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssessmentTargetsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
