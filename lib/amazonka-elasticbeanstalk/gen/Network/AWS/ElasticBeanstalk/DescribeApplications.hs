{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
module Network.AWS.ElasticBeanstalk.DescribeApplications
  ( -- * Creating a request
    DescribeApplications (..),
    mkDescribeApplications,

    -- ** Request lenses
    daApplicationNames,

    -- * Destructuring the response
    DescribeApplicationsResponse (..),
    mkDescribeApplicationsResponse,

    -- ** Response lenses
    darrsApplications,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe one or more applications.
--
-- /See:/ 'mkDescribeApplications' smart constructor.
newtype DescribeApplications = DescribeApplications'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
    applicationNames :: Core.Maybe [Types.ApplicationName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplications' value with any optional fields omitted.
mkDescribeApplications ::
  DescribeApplications
mkDescribeApplications =
  DescribeApplications' {applicationNames = Core.Nothing}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationNames :: Lens.Lens' DescribeApplications (Core.Maybe [Types.ApplicationName])
daApplicationNames = Lens.field @"applicationNames"
{-# DEPRECATED daApplicationNames "Use generic-lens or generic-optics with 'applicationNames' instead." #-}

instance Core.AWSRequest DescribeApplications where
  type Rs DescribeApplications = DescribeApplicationsResponse
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
            ( Core.pure ("Action", "DescribeApplications")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "ApplicationNames"
                            (Core.toQueryList "member" Core.<$> applicationNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationsResult"
      ( \s h x ->
          DescribeApplicationsResponse'
            Core.<$> (x Core..@? "Applications" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'mkDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { -- | This parameter contains a list of 'ApplicationDescription' .
    applications :: Core.Maybe [Types.ApplicationDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeApplicationsResponse' value with any optional fields omitted.
mkDescribeApplicationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeApplicationsResponse
mkDescribeApplicationsResponse responseStatus =
  DescribeApplicationsResponse'
    { applications = Core.Nothing,
      responseStatus
    }

-- | This parameter contains a list of 'ApplicationDescription' .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsApplications :: Lens.Lens' DescribeApplicationsResponse (Core.Maybe [Types.ApplicationDescription])
darrsApplications = Lens.field @"applications"
{-# DEPRECATED darrsApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeApplicationsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
