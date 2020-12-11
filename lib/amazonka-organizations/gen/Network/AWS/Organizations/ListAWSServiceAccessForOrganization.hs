{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListAWSServiceAccessForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the AWS services that you enabled to integrate with your organization. After a service on this list creates the resources that it requires for the integration, it can perform operations on your organization and its accounts.
--
-- For more information about integrating other services with AWS Organizations, including the list of services that currently work with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAWSServiceAccessForOrganization
  ( -- * Creating a request
    ListAWSServiceAccessForOrganization (..),
    mkListAWSServiceAccessForOrganization,

    -- ** Request lenses
    lasafoNextToken,
    lasafoMaxResults,

    -- * Destructuring the response
    ListAWSServiceAccessForOrganizationResponse (..),
    mkListAWSServiceAccessForOrganizationResponse,

    -- ** Response lenses
    lasaforsNextToken,
    lasaforsEnabledServicePrincipals,
    lasaforsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAWSServiceAccessForOrganization' smart constructor.
data ListAWSServiceAccessForOrganization = ListAWSServiceAccessForOrganization'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAWSServiceAccessForOrganization' with the minimum fields required to make a request.
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
mkListAWSServiceAccessForOrganization ::
  ListAWSServiceAccessForOrganization
mkListAWSServiceAccessForOrganization =
  ListAWSServiceAccessForOrganization'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasafoNextToken :: Lens.Lens' ListAWSServiceAccessForOrganization (Lude.Maybe Lude.Text)
lasafoNextToken = Lens.lens (nextToken :: ListAWSServiceAccessForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAWSServiceAccessForOrganization)
{-# DEPRECATED lasafoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasafoMaxResults :: Lens.Lens' ListAWSServiceAccessForOrganization (Lude.Maybe Lude.Natural)
lasafoMaxResults = Lens.lens (maxResults :: ListAWSServiceAccessForOrganization -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAWSServiceAccessForOrganization)
{-# DEPRECATED lasafoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAWSServiceAccessForOrganization where
  page rq rs
    | Page.stop (rs Lens.^. lasaforsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasaforsEnabledServicePrincipals) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasafoNextToken Lens..~ rs Lens.^. lasaforsNextToken

instance Lude.AWSRequest ListAWSServiceAccessForOrganization where
  type
    Rs ListAWSServiceAccessForOrganization =
      ListAWSServiceAccessForOrganizationResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAWSServiceAccessForOrganizationResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EnabledServicePrincipals" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAWSServiceAccessForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListAWSServiceAccessForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAWSServiceAccessForOrganization where
  toJSON ListAWSServiceAccessForOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAWSServiceAccessForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAWSServiceAccessForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAWSServiceAccessForOrganizationResponse' smart constructor.
data ListAWSServiceAccessForOrganizationResponse = ListAWSServiceAccessForOrganizationResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    enabledServicePrincipals ::
      Lude.Maybe
        [EnabledServicePrincipal],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAWSServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'enabledServicePrincipals' - A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListAWSServiceAccessForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAWSServiceAccessForOrganizationResponse
mkListAWSServiceAccessForOrganizationResponse pResponseStatus_ =
  ListAWSServiceAccessForOrganizationResponse'
    { nextToken =
        Lude.Nothing,
      enabledServicePrincipals = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaforsNextToken :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Lude.Maybe Lude.Text)
lasaforsNextToken = Lens.lens (nextToken :: ListAWSServiceAccessForOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAWSServiceAccessForOrganizationResponse)
{-# DEPRECATED lasaforsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
--
-- /Note:/ Consider using 'enabledServicePrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaforsEnabledServicePrincipals :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Lude.Maybe [EnabledServicePrincipal])
lasaforsEnabledServicePrincipals = Lens.lens (enabledServicePrincipals :: ListAWSServiceAccessForOrganizationResponse -> Lude.Maybe [EnabledServicePrincipal]) (\s a -> s {enabledServicePrincipals = a} :: ListAWSServiceAccessForOrganizationResponse)
{-# DEPRECATED lasaforsEnabledServicePrincipals "Use generic-lens or generic-optics with 'enabledServicePrincipals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaforsResponseStatus :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse Lude.Int
lasaforsResponseStatus = Lens.lens (responseStatus :: ListAWSServiceAccessForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAWSServiceAccessForOrganizationResponse)
{-# DEPRECATED lasaforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
