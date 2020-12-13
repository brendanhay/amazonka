{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentRunAgents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the agents of the assessment runs that are specified by the ARNs of the assessment runs.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRunAgents
  ( -- * Creating a request
    ListAssessmentRunAgents (..),
    mkListAssessmentRunAgents,

    -- ** Request lenses
    laraNextToken,
    laraAssessmentRunARN,
    laraFilter,
    laraMaxResults,

    -- * Destructuring the response
    ListAssessmentRunAgentsResponse (..),
    mkListAssessmentRunAgentsResponse,

    -- ** Response lenses
    lararsAssessmentRunAgents,
    lararsNextToken,
    lararsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssessmentRunAgents' smart constructor.
data ListAssessmentRunAgents = ListAssessmentRunAgents'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRunAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ARN that specifies the assessment run whose agents you want to list.
    assessmentRunARN :: Lude.Text,
    -- | You can use this parameter to specify a subset of data to be included in the action's response.
    --
    -- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
    filter :: Lude.Maybe AgentFilter,
    -- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentRunAgents' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRunAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'assessmentRunARN' - The ARN that specifies the assessment run whose agents you want to list.
-- * 'filter' - You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
mkListAssessmentRunAgents ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  ListAssessmentRunAgents
mkListAssessmentRunAgents pAssessmentRunARN_ =
  ListAssessmentRunAgents'
    { nextToken = Lude.Nothing,
      assessmentRunARN = pAssessmentRunARN_,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRunAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraNextToken :: Lens.Lens' ListAssessmentRunAgents (Lude.Maybe Lude.Text)
laraNextToken = Lens.lens (nextToken :: ListAssessmentRunAgents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentRunAgents)
{-# DEPRECATED laraNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN that specifies the assessment run whose agents you want to list.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraAssessmentRunARN :: Lens.Lens' ListAssessmentRunAgents Lude.Text
laraAssessmentRunARN = Lens.lens (assessmentRunARN :: ListAssessmentRunAgents -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: ListAssessmentRunAgents)
{-# DEPRECATED laraAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraFilter :: Lens.Lens' ListAssessmentRunAgents (Lude.Maybe AgentFilter)
laraFilter = Lens.lens (filter :: ListAssessmentRunAgents -> Lude.Maybe AgentFilter) (\s a -> s {filter = a} :: ListAssessmentRunAgents)
{-# DEPRECATED laraFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraMaxResults :: Lens.Lens' ListAssessmentRunAgents (Lude.Maybe Lude.Int)
laraMaxResults = Lens.lens (maxResults :: ListAssessmentRunAgents -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAssessmentRunAgents)
{-# DEPRECATED laraMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAssessmentRunAgents where
  page rq rs
    | Page.stop (rs Lens.^. lararsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lararsAssessmentRunAgents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laraNextToken Lens..~ rs Lens.^. lararsNextToken

instance Lude.AWSRequest ListAssessmentRunAgents where
  type Rs ListAssessmentRunAgents = ListAssessmentRunAgentsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssessmentRunAgentsResponse'
            Lude.<$> (x Lude..?> "assessmentRunAgents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssessmentRunAgents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListAssessmentRunAgents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssessmentRunAgents where
  toJSON ListAssessmentRunAgents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("assessmentRunArn" Lude..= assessmentRunARN),
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAssessmentRunAgents where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssessmentRunAgents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssessmentRunAgentsResponse' smart constructor.
data ListAssessmentRunAgentsResponse = ListAssessmentRunAgentsResponse'
  { -- | A list of ARNs that specifies the agents returned by the action.
    assessmentRunAgents :: [AssessmentRunAgent],
    -- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentRunAgentsResponse' with the minimum fields required to make a request.
--
-- * 'assessmentRunAgents' - A list of ARNs that specifies the agents returned by the action.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkListAssessmentRunAgentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssessmentRunAgentsResponse
mkListAssessmentRunAgentsResponse pResponseStatus_ =
  ListAssessmentRunAgentsResponse'
    { assessmentRunAgents =
        Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of ARNs that specifies the agents returned by the action.
--
-- /Note:/ Consider using 'assessmentRunAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararsAssessmentRunAgents :: Lens.Lens' ListAssessmentRunAgentsResponse [AssessmentRunAgent]
lararsAssessmentRunAgents = Lens.lens (assessmentRunAgents :: ListAssessmentRunAgentsResponse -> [AssessmentRunAgent]) (\s a -> s {assessmentRunAgents = a} :: ListAssessmentRunAgentsResponse)
{-# DEPRECATED lararsAssessmentRunAgents "Use generic-lens or generic-optics with 'assessmentRunAgents' instead." #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararsNextToken :: Lens.Lens' ListAssessmentRunAgentsResponse (Lude.Maybe Lude.Text)
lararsNextToken = Lens.lens (nextToken :: ListAssessmentRunAgentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentRunAgentsResponse)
{-# DEPRECATED lararsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararsResponseStatus :: Lens.Lens' ListAssessmentRunAgentsResponse Lude.Int
lararsResponseStatus = Lens.lens (responseStatus :: ListAssessmentRunAgentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssessmentRunAgentsResponse)
{-# DEPRECATED lararsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
