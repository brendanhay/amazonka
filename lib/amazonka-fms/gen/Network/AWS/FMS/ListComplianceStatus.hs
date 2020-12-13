{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListComplianceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicyComplianceStatus@ objects. Use @PolicyComplianceStatus@ to get a summary of which member accounts are protected by the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListComplianceStatus
  ( -- * Creating a request
    ListComplianceStatus (..),
    mkListComplianceStatus,

    -- ** Request lenses
    lcsPolicyId,
    lcsNextToken,
    lcsMaxResults,

    -- * Destructuring the response
    ListComplianceStatusResponse (..),
    mkListComplianceStatusResponse,

    -- ** Response lenses
    lcsrsNextToken,
    lcsrsPolicyComplianceStatusList,
    lcsrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListComplianceStatus' smart constructor.
data ListComplianceStatus = ListComplianceStatus'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    policyId :: Lude.Text,
    -- | If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListComplianceStatus' with the minimum fields required to make a request.
--
-- * 'policyId' - The ID of the AWS Firewall Manager policy that you want the details for.
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
-- * 'maxResults' - Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
mkListComplianceStatus ::
  -- | 'policyId'
  Lude.Text ->
  ListComplianceStatus
mkListComplianceStatus pPolicyId_ =
  ListComplianceStatus'
    { policyId = pPolicyId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the AWS Firewall Manager policy that you want the details for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsPolicyId :: Lens.Lens' ListComplianceStatus Lude.Text
lcsPolicyId = Lens.lens (policyId :: ListComplianceStatus -> Lude.Text) (\s a -> s {policyId = a} :: ListComplianceStatus)
{-# DEPRECATED lcsPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListComplianceStatus (Lude.Maybe Lude.Text)
lcsNextToken = Lens.lens (nextToken :: ListComplianceStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceStatus)
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxResults :: Lens.Lens' ListComplianceStatus (Lude.Maybe Lude.Natural)
lcsMaxResults = Lens.lens (maxResults :: ListComplianceStatus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListComplianceStatus)
{-# DEPRECATED lcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListComplianceStatus where
  page rq rs
    | Page.stop (rs Lens.^. lcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcsrsPolicyComplianceStatusList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcsNextToken Lens..~ rs Lens.^. lcsrsNextToken

instance Lude.AWSRequest ListComplianceStatus where
  type Rs ListComplianceStatus = ListComplianceStatusResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListComplianceStatusResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PolicyComplianceStatusList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListComplianceStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.ListComplianceStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListComplianceStatus where
  toJSON ListComplianceStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyId" Lude..= policyId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListComplianceStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery ListComplianceStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListComplianceStatusResponse' smart constructor.
data ListComplianceStatusResponse = ListComplianceStatusResponse'
  { -- | If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @PolicyComplianceStatus@ objects.
    policyComplianceStatusList :: Lude.Maybe [PolicyComplianceStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListComplianceStatusResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
-- * 'policyComplianceStatusList' - An array of @PolicyComplianceStatus@ objects.
-- * 'responseStatus' - The response status code.
mkListComplianceStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListComplianceStatusResponse
mkListComplianceStatusResponse pResponseStatus_ =
  ListComplianceStatusResponse'
    { nextToken = Lude.Nothing,
      policyComplianceStatusList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsNextToken :: Lens.Lens' ListComplianceStatusResponse (Lude.Maybe Lude.Text)
lcsrsNextToken = Lens.lens (nextToken :: ListComplianceStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceStatusResponse)
{-# DEPRECATED lcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'policyComplianceStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsPolicyComplianceStatusList :: Lens.Lens' ListComplianceStatusResponse (Lude.Maybe [PolicyComplianceStatus])
lcsrsPolicyComplianceStatusList = Lens.lens (policyComplianceStatusList :: ListComplianceStatusResponse -> Lude.Maybe [PolicyComplianceStatus]) (\s a -> s {policyComplianceStatusList = a} :: ListComplianceStatusResponse)
{-# DEPRECATED lcsrsPolicyComplianceStatusList "Use generic-lens or generic-optics with 'policyComplianceStatusList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsResponseStatus :: Lens.Lens' ListComplianceStatusResponse Lude.Int
lcsrsResponseStatus = Lens.lens (responseStatus :: ListComplianceStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListComplianceStatusResponse)
{-# DEPRECATED lcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
