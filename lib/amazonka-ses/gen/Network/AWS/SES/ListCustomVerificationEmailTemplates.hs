{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListCustomVerificationEmailTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing custom verification email templates for your account in the current AWS Region.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListCustomVerificationEmailTemplates
  ( -- * Creating a request
    ListCustomVerificationEmailTemplates (..),
    mkListCustomVerificationEmailTemplates,

    -- ** Request lenses
    lcvetMaxResults,
    lcvetNextToken,

    -- * Destructuring the response
    ListCustomVerificationEmailTemplatesResponse (..),
    mkListCustomVerificationEmailTemplatesResponse,

    -- ** Response lenses
    lcvetrrsCustomVerificationEmailTemplates,
    lcvetrrsNextToken,
    lcvetrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to list the existing custom verification email templates for your account.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- /See:/ 'mkListCustomVerificationEmailTemplates' smart constructor.
data ListCustomVerificationEmailTemplates = ListCustomVerificationEmailTemplates'
  { -- | The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
    maxResults :: Core.Maybe Core.Natural,
    -- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCustomVerificationEmailTemplates' value with any optional fields omitted.
mkListCustomVerificationEmailTemplates ::
  ListCustomVerificationEmailTemplates
mkListCustomVerificationEmailTemplates =
  ListCustomVerificationEmailTemplates'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetMaxResults :: Lens.Lens' ListCustomVerificationEmailTemplates (Core.Maybe Core.Natural)
lcvetMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcvetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetNextToken :: Lens.Lens' ListCustomVerificationEmailTemplates (Core.Maybe Types.NextToken)
lcvetNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcvetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListCustomVerificationEmailTemplates where
  type
    Rs ListCustomVerificationEmailTemplates =
      ListCustomVerificationEmailTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListCustomVerificationEmailTemplates")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListCustomVerificationEmailTemplatesResult"
      ( \s h x ->
          ListCustomVerificationEmailTemplatesResponse'
            Core.<$> ( x Core..@? "CustomVerificationEmailTemplates"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCustomVerificationEmailTemplates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"customVerificationEmailTemplates" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | A paginated list of custom verification email templates.
--
-- /See:/ 'mkListCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { -- | A list of the custom verification email templates that exist in your account.
    customVerificationEmailTemplates :: Core.Maybe [Types.CustomVerificationEmailTemplate],
    -- | A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCustomVerificationEmailTemplatesResponse' value with any optional fields omitted.
mkListCustomVerificationEmailTemplatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCustomVerificationEmailTemplatesResponse
mkListCustomVerificationEmailTemplatesResponse responseStatus =
  ListCustomVerificationEmailTemplatesResponse'
    { customVerificationEmailTemplates =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the custom verification email templates that exist in your account.
--
-- /Note:/ Consider using 'customVerificationEmailTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrrsCustomVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Core.Maybe [Types.CustomVerificationEmailTemplate])
lcvetrrsCustomVerificationEmailTemplates = Lens.field @"customVerificationEmailTemplates"
{-# DEPRECATED lcvetrrsCustomVerificationEmailTemplates "Use generic-lens or generic-optics with 'customVerificationEmailTemplates' instead." #-}

-- | A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrrsNextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Core.Maybe Types.NextToken)
lcvetrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcvetrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrrsResponseStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Core.Int
lcvetrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcvetrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
