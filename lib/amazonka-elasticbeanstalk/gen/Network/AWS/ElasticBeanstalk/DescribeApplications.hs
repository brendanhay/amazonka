{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeApplications (..)
    , mkDescribeApplications
    -- ** Request lenses
    , daApplicationNames

    -- * Destructuring the response
    , DescribeApplicationsResponse (..)
    , mkDescribeApplicationsResponse
    -- ** Response lenses
    , darrsApplications
    , darrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe one or more applications.
--
-- /See:/ 'mkDescribeApplications' smart constructor.
newtype DescribeApplications = DescribeApplications'
  { applicationNames :: Core.Maybe [Types.ApplicationName]
    -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplications' value with any optional fields omitted.
mkDescribeApplications
    :: DescribeApplications
mkDescribeApplications
  = DescribeApplications'{applicationNames = Core.Nothing}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationNames :: Lens.Lens' DescribeApplications (Core.Maybe [Types.ApplicationName])
daApplicationNames = Lens.field @"applicationNames"
{-# INLINEABLE daApplicationNames #-}
{-# DEPRECATED applicationNames "Use generic-lens or generic-optics with 'applicationNames' instead"  #-}

instance Core.ToQuery DescribeApplications where
        toQuery DescribeApplications{..}
          = Core.toQueryPair "Action" ("DescribeApplications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ApplicationNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   applicationNames)

instance Core.ToHeaders DescribeApplications where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeApplications where
        type Rs DescribeApplications = DescribeApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeApplicationsResult"
              (\ s h x ->
                 DescribeApplicationsResponse' Core.<$>
                   (x Core..@? "Applications" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'mkDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { applications :: Core.Maybe [Types.ApplicationDescription]
    -- ^ This parameter contains a list of 'ApplicationDescription' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeApplicationsResponse' value with any optional fields omitted.
mkDescribeApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeApplicationsResponse
mkDescribeApplicationsResponse responseStatus
  = DescribeApplicationsResponse'{applications = Core.Nothing,
                                  responseStatus}

-- | This parameter contains a list of 'ApplicationDescription' .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsApplications :: Lens.Lens' DescribeApplicationsResponse (Core.Maybe [Types.ApplicationDescription])
darrsApplications = Lens.field @"applications"
{-# INLINEABLE darrsApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeApplicationsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
