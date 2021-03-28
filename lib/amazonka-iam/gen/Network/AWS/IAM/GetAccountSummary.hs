{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the AWS account.
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.GetAccountSummary
    (
    -- * Creating a request
      GetAccountSummary (..)
    , mkGetAccountSummary

    -- * Destructuring the response
    , GetAccountSummaryResponse (..)
    , mkGetAccountSummaryResponse
    -- ** Response lenses
    , gasrrsSummaryMap
    , gasrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAccountSummary' smart constructor.
data GetAccountSummary = GetAccountSummary'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSummary' value with any optional fields omitted.
mkGetAccountSummary
    :: GetAccountSummary
mkGetAccountSummary = GetAccountSummary'

instance Core.ToQuery GetAccountSummary where
        toQuery GetAccountSummary{..}
          = Core.toQueryPair "Action" ("GetAccountSummary" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)

instance Core.ToHeaders GetAccountSummary where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccountSummary where
        type Rs GetAccountSummary = GetAccountSummaryResponse
        toRequest x@_
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
          = Response.receiveXMLWrapper "GetAccountSummaryResult"
              (\ s h x ->
                 GetAccountSummaryResponse' Core.<$>
                   (x Core..@? "SummaryMap" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetAccountSummary' request. 
--
-- /See:/ 'mkGetAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
  { summaryMap :: Core.Maybe (Core.HashMap Types.SummaryKeyType Core.Int)
    -- ^ A set of key–value pairs containing information about IAM entity usage and IAM quotas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSummaryResponse' value with any optional fields omitted.
mkGetAccountSummaryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccountSummaryResponse
mkGetAccountSummaryResponse responseStatus
  = GetAccountSummaryResponse'{summaryMap = Core.Nothing,
                               responseStatus}

-- | A set of key–value pairs containing information about IAM entity usage and IAM quotas.
--
-- /Note:/ Consider using 'summaryMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsSummaryMap :: Lens.Lens' GetAccountSummaryResponse (Core.Maybe (Core.HashMap Types.SummaryKeyType Core.Int))
gasrrsSummaryMap = Lens.field @"summaryMap"
{-# INLINEABLE gasrrsSummaryMap #-}
{-# DEPRECATED summaryMap "Use generic-lens or generic-optics with 'summaryMap' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAccountSummaryResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
