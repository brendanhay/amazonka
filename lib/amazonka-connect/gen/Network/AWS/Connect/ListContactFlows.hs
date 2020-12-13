{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListContactFlows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the contact flows for the specified Amazon Connect instance.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
-- For more information about contact flows, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html Contact Flows> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListContactFlows
  ( -- * Creating a request
    ListContactFlows (..),
    mkListContactFlows,

    -- ** Request lenses
    lcfInstanceId,
    lcfContactFlowTypes,
    lcfNextToken,
    lcfMaxResults,

    -- * Destructuring the response
    ListContactFlowsResponse (..),
    mkListContactFlowsResponse,

    -- ** Response lenses
    lcfrsContactFlowSummaryList,
    lcfrsNextToken,
    lcfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The type of contact flow.
    contactFlowTypes :: Lude.Maybe [ContactFlowType],
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContactFlows' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'contactFlowTypes' - The type of contact flow.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListContactFlows ::
  -- | 'instanceId'
  Lude.Text ->
  ListContactFlows
mkListContactFlows pInstanceId_ =
  ListContactFlows'
    { instanceId = pInstanceId_,
      contactFlowTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfInstanceId :: Lens.Lens' ListContactFlows Lude.Text
lcfInstanceId = Lens.lens (instanceId :: ListContactFlows -> Lude.Text) (\s a -> s {instanceId = a} :: ListContactFlows)
{-# DEPRECATED lcfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of contact flow.
--
-- /Note:/ Consider using 'contactFlowTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfContactFlowTypes :: Lens.Lens' ListContactFlows (Lude.Maybe [ContactFlowType])
lcfContactFlowTypes = Lens.lens (contactFlowTypes :: ListContactFlows -> Lude.Maybe [ContactFlowType]) (\s a -> s {contactFlowTypes = a} :: ListContactFlows)
{-# DEPRECATED lcfContactFlowTypes "Use generic-lens or generic-optics with 'contactFlowTypes' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfNextToken :: Lens.Lens' ListContactFlows (Lude.Maybe Lude.Text)
lcfNextToken = Lens.lens (nextToken :: ListContactFlows -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContactFlows)
{-# DEPRECATED lcfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfMaxResults :: Lens.Lens' ListContactFlows (Lude.Maybe Lude.Natural)
lcfMaxResults = Lens.lens (maxResults :: ListContactFlows -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListContactFlows)
{-# DEPRECATED lcfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListContactFlows where
  page rq rs
    | Page.stop (rs Lens.^. lcfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcfrsContactFlowSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcfNextToken Lens..~ rs Lens.^. lcfrsNextToken

instance Lude.AWSRequest ListContactFlows where
  type Rs ListContactFlows = ListContactFlowsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListContactFlowsResponse'
            Lude.<$> (x Lude..?> "ContactFlowSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListContactFlows where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListContactFlows where
  toPath ListContactFlows' {..} =
    Lude.mconcat ["/contact-flows-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListContactFlows where
  toQuery ListContactFlows' {..} =
    Lude.mconcat
      [ "contactFlowTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> contactFlowTypes),
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { -- | Information about the contact flows.
    contactFlowSummaryList :: Lude.Maybe [ContactFlowSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContactFlowsResponse' with the minimum fields required to make a request.
--
-- * 'contactFlowSummaryList' - Information about the contact flows.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListContactFlowsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListContactFlowsResponse
mkListContactFlowsResponse pResponseStatus_ =
  ListContactFlowsResponse'
    { contactFlowSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the contact flows.
--
-- /Note:/ Consider using 'contactFlowSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrsContactFlowSummaryList :: Lens.Lens' ListContactFlowsResponse (Lude.Maybe [ContactFlowSummary])
lcfrsContactFlowSummaryList = Lens.lens (contactFlowSummaryList :: ListContactFlowsResponse -> Lude.Maybe [ContactFlowSummary]) (\s a -> s {contactFlowSummaryList = a} :: ListContactFlowsResponse)
{-# DEPRECATED lcfrsContactFlowSummaryList "Use generic-lens or generic-optics with 'contactFlowSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrsNextToken :: Lens.Lens' ListContactFlowsResponse (Lude.Maybe Lude.Text)
lcfrsNextToken = Lens.lens (nextToken :: ListContactFlowsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContactFlowsResponse)
{-# DEPRECATED lcfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrsResponseStatus :: Lens.Lens' ListContactFlowsResponse Lude.Int
lcfrsResponseStatus = Lens.lens (responseStatus :: ListContactFlowsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListContactFlowsResponse)
{-# DEPRECATED lcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
