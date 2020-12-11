{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account creation requests that match the specified status that is currently being tracked for the organization.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListCreateAccountStatus
  ( -- * Creating a request
    ListCreateAccountStatus (..),
    mkListCreateAccountStatus,

    -- ** Request lenses
    lcasStates,
    lcasNextToken,
    lcasMaxResults,

    -- * Destructuring the response
    ListCreateAccountStatusResponse (..),
    mkListCreateAccountStatusResponse,

    -- ** Response lenses
    lcasrsCreateAccountStatuses,
    lcasrsNextToken,
    lcasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCreateAccountStatus' smart constructor.
data ListCreateAccountStatus = ListCreateAccountStatus'
  { states ::
      Lude.Maybe [CreateAccountState],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCreateAccountStatus' with the minimum fields required to make a request.
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'states' - A list of one or more states that you want included in the response. If this parameter isn't present, all requests are included in the response.
mkListCreateAccountStatus ::
  ListCreateAccountStatus
mkListCreateAccountStatus =
  ListCreateAccountStatus'
    { states = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of one or more states that you want included in the response. If this parameter isn't present, all requests are included in the response.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasStates :: Lens.Lens' ListCreateAccountStatus (Lude.Maybe [CreateAccountState])
lcasStates = Lens.lens (states :: ListCreateAccountStatus -> Lude.Maybe [CreateAccountState]) (\s a -> s {states = a} :: ListCreateAccountStatus)
{-# DEPRECATED lcasStates "Use generic-lens or generic-optics with 'states' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasNextToken :: Lens.Lens' ListCreateAccountStatus (Lude.Maybe Lude.Text)
lcasNextToken = Lens.lens (nextToken :: ListCreateAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCreateAccountStatus)
{-# DEPRECATED lcasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasMaxResults :: Lens.Lens' ListCreateAccountStatus (Lude.Maybe Lude.Natural)
lcasMaxResults = Lens.lens (maxResults :: ListCreateAccountStatus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCreateAccountStatus)
{-# DEPRECATED lcasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCreateAccountStatus where
  page rq rs
    | Page.stop (rs Lens.^. lcasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcasrsCreateAccountStatuses) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcasNextToken Lens..~ rs Lens.^. lcasrsNextToken

instance Lude.AWSRequest ListCreateAccountStatus where
  type Rs ListCreateAccountStatus = ListCreateAccountStatusResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCreateAccountStatusResponse'
            Lude.<$> (x Lude..?> "CreateAccountStatuses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCreateAccountStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListCreateAccountStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCreateAccountStatus where
  toJSON ListCreateAccountStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("States" Lude..=) Lude.<$> states,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCreateAccountStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCreateAccountStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCreateAccountStatusResponse' smart constructor.
data ListCreateAccountStatusResponse = ListCreateAccountStatusResponse'
  { createAccountStatuses ::
      Lude.Maybe
        [CreateAccountStatus],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCreateAccountStatusResponse' with the minimum fields required to make a request.
--
-- * 'createAccountStatuses' - A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListCreateAccountStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCreateAccountStatusResponse
mkListCreateAccountStatusResponse pResponseStatus_ =
  ListCreateAccountStatusResponse'
    { createAccountStatuses =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
--
-- /Note:/ Consider using 'createAccountStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrsCreateAccountStatuses :: Lens.Lens' ListCreateAccountStatusResponse (Lude.Maybe [CreateAccountStatus])
lcasrsCreateAccountStatuses = Lens.lens (createAccountStatuses :: ListCreateAccountStatusResponse -> Lude.Maybe [CreateAccountStatus]) (\s a -> s {createAccountStatuses = a} :: ListCreateAccountStatusResponse)
{-# DEPRECATED lcasrsCreateAccountStatuses "Use generic-lens or generic-optics with 'createAccountStatuses' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrsNextToken :: Lens.Lens' ListCreateAccountStatusResponse (Lude.Maybe Lude.Text)
lcasrsNextToken = Lens.lens (nextToken :: ListCreateAccountStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCreateAccountStatusResponse)
{-# DEPRECATED lcasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrsResponseStatus :: Lens.Lens' ListCreateAccountStatusResponse Lude.Int
lcasrsResponseStatus = Lens.lens (responseStatus :: ListCreateAccountStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCreateAccountStatusResponse)
{-# DEPRECATED lcasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
