{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lawssafoMaxResults,
    lawssafoNextToken,

    -- * Destructuring the response
    ListAWSServiceAccessForOrganizationResponse (..),
    mkListAWSServiceAccessForOrganizationResponse,

    -- ** Response lenses
    lawssaforrsEnabledServicePrincipals,
    lawssaforrsNextToken,
    lawssaforrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAWSServiceAccessForOrganization' smart constructor.
data ListAWSServiceAccessForOrganization = ListAWSServiceAccessForOrganization'
  { -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAWSServiceAccessForOrganization' value with any optional fields omitted.
mkListAWSServiceAccessForOrganization ::
  ListAWSServiceAccessForOrganization
mkListAWSServiceAccessForOrganization =
  ListAWSServiceAccessForOrganization'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lawssafoMaxResults :: Lens.Lens' ListAWSServiceAccessForOrganization (Core.Maybe Core.Natural)
lawssafoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lawssafoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lawssafoNextToken :: Lens.Lens' ListAWSServiceAccessForOrganization (Core.Maybe Types.NextToken)
lawssafoNextToken = Lens.field @"nextToken"
{-# DEPRECATED lawssafoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAWSServiceAccessForOrganization where
  toJSON ListAWSServiceAccessForOrganization {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAWSServiceAccessForOrganization where
  type
    Rs ListAWSServiceAccessForOrganization =
      ListAWSServiceAccessForOrganizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.ListAWSServiceAccessForOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAWSServiceAccessForOrganizationResponse'
            Core.<$> (x Core..:? "EnabledServicePrincipals")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAWSServiceAccessForOrganization where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"enabledServicePrincipals" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAWSServiceAccessForOrganizationResponse' smart constructor.
data ListAWSServiceAccessForOrganizationResponse = ListAWSServiceAccessForOrganizationResponse'
  { -- | A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
    enabledServicePrincipals :: Core.Maybe [Types.EnabledServicePrincipal],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAWSServiceAccessForOrganizationResponse' value with any optional fields omitted.
mkListAWSServiceAccessForOrganizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAWSServiceAccessForOrganizationResponse
mkListAWSServiceAccessForOrganizationResponse responseStatus =
  ListAWSServiceAccessForOrganizationResponse'
    { enabledServicePrincipals =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
--
-- /Note:/ Consider using 'enabledServicePrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lawssaforrsEnabledServicePrincipals :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Core.Maybe [Types.EnabledServicePrincipal])
lawssaforrsEnabledServicePrincipals = Lens.field @"enabledServicePrincipals"
{-# DEPRECATED lawssaforrsEnabledServicePrincipals "Use generic-lens or generic-optics with 'enabledServicePrincipals' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lawssaforrsNextToken :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Core.Maybe Types.NextToken)
lawssaforrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lawssaforrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lawssaforrsResponseStatus :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse Core.Int
lawssaforrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lawssaforrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
