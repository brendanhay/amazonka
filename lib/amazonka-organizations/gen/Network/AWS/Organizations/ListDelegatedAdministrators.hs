{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListDelegatedAdministrators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS accounts that are designated as delegated administrators in this organization.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedAdministrators
  ( -- * Creating a request
    ListDelegatedAdministrators (..),
    mkListDelegatedAdministrators,

    -- ** Request lenses
    ldaServicePrincipal,
    ldaNextToken,
    ldaMaxResults,

    -- * Destructuring the response
    ListDelegatedAdministratorsResponse (..),
    mkListDelegatedAdministratorsResponse,

    -- ** Response lenses
    ldarsDelegatedAdministrators,
    ldarsNextToken,
    ldarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { -- | Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service.
    --
    -- If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
    servicePrincipal :: Lude.Maybe Lude.Text,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDelegatedAdministrators' with the minimum fields required to make a request.
--
-- * 'servicePrincipal' - Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service.
--
-- If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
mkListDelegatedAdministrators ::
  ListDelegatedAdministrators
mkListDelegatedAdministrators =
  ListDelegatedAdministrators'
    { servicePrincipal = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service.
--
-- If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaServicePrincipal :: Lens.Lens' ListDelegatedAdministrators (Lude.Maybe Lude.Text)
ldaServicePrincipal = Lens.lens (servicePrincipal :: ListDelegatedAdministrators -> Lude.Maybe Lude.Text) (\s a -> s {servicePrincipal = a} :: ListDelegatedAdministrators)
{-# DEPRECATED ldaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaNextToken :: Lens.Lens' ListDelegatedAdministrators (Lude.Maybe Lude.Text)
ldaNextToken = Lens.lens (nextToken :: ListDelegatedAdministrators -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDelegatedAdministrators)
{-# DEPRECATED ldaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaMaxResults :: Lens.Lens' ListDelegatedAdministrators (Lude.Maybe Lude.Natural)
ldaMaxResults = Lens.lens (maxResults :: ListDelegatedAdministrators -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDelegatedAdministrators)
{-# DEPRECATED ldaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDelegatedAdministrators where
  page rq rs
    | Page.stop (rs Lens.^. ldarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldarsDelegatedAdministrators) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldaNextToken Lens..~ rs Lens.^. ldarsNextToken

instance Lude.AWSRequest ListDelegatedAdministrators where
  type
    Rs ListDelegatedAdministrators =
      ListDelegatedAdministratorsResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDelegatedAdministratorsResponse'
            Lude.<$> (x Lude..?> "DelegatedAdministrators" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDelegatedAdministrators where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListDelegatedAdministrators" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDelegatedAdministrators where
  toJSON ListDelegatedAdministrators' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServicePrincipal" Lude..=) Lude.<$> servicePrincipal,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDelegatedAdministrators where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDelegatedAdministrators where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { -- | The list of delegated administrators in your organization.
    delegatedAdministrators :: Lude.Maybe [DelegatedAdministrator],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDelegatedAdministratorsResponse' with the minimum fields required to make a request.
--
-- * 'delegatedAdministrators' - The list of delegated administrators in your organization.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListDelegatedAdministratorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDelegatedAdministratorsResponse
mkListDelegatedAdministratorsResponse pResponseStatus_ =
  ListDelegatedAdministratorsResponse'
    { delegatedAdministrators =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of delegated administrators in your organization.
--
-- /Note:/ Consider using 'delegatedAdministrators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarsDelegatedAdministrators :: Lens.Lens' ListDelegatedAdministratorsResponse (Lude.Maybe [DelegatedAdministrator])
ldarsDelegatedAdministrators = Lens.lens (delegatedAdministrators :: ListDelegatedAdministratorsResponse -> Lude.Maybe [DelegatedAdministrator]) (\s a -> s {delegatedAdministrators = a} :: ListDelegatedAdministratorsResponse)
{-# DEPRECATED ldarsDelegatedAdministrators "Use generic-lens or generic-optics with 'delegatedAdministrators' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarsNextToken :: Lens.Lens' ListDelegatedAdministratorsResponse (Lude.Maybe Lude.Text)
ldarsNextToken = Lens.lens (nextToken :: ListDelegatedAdministratorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDelegatedAdministratorsResponse)
{-# DEPRECATED ldarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarsResponseStatus :: Lens.Lens' ListDelegatedAdministratorsResponse Lude.Int
ldarsResponseStatus = Lens.lens (responseStatus :: ListDelegatedAdministratorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDelegatedAdministratorsResponse)
{-# DEPRECATED ldarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
