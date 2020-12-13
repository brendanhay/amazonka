{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicySummary@ objects.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListPolicies
  ( -- * Creating a request
    ListPolicies (..),
    mkListPolicies,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,

    -- * Destructuring the response
    ListPoliciesResponse (..),
    mkListPoliciesResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsPolicyList,
    lprsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
-- * 'maxResults' - Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
mkListPolicies ::
  ListPolicies
mkListPolicies =
  ListPolicies'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPolicies)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPolicies)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPolicyList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PolicyList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.ListPolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPolicies where
  toJSON ListPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @PolicySummary@ objects.
    policyList :: Lude.Maybe [PolicySummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
-- * 'policyList' - An array of @PolicySummary@ objects.
-- * 'responseStatus' - The response status code.
mkListPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesResponse
mkListPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { nextToken = Lude.Nothing,
      policyList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPoliciesResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'policyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPolicyList :: Lens.Lens' ListPoliciesResponse (Lude.Maybe [PolicySummary])
lprsPolicyList = Lens.lens (policyList :: ListPoliciesResponse -> Lude.Maybe [PolicySummary]) (\s a -> s {policyList = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsPolicyList "Use generic-lens or generic-optics with 'policyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPoliciesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
