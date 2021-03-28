{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the attributes for a customer account. The attributes include Amazon RDS quotas for the account, such as the number of DB instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.
--
-- This command doesn't take any parameters.
module Network.AWS.RDS.DescribeAccountAttributes
    (
    -- * Creating a request
      DescribeAccountAttributes (..)
    , mkDescribeAccountAttributes

    -- * Destructuring the response
    , DescribeAccountAttributesResponse (..)
    , mkDescribeAccountAttributesResponse
    -- ** Response lenses
    , daarrsAccountQuotas
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes
    :: DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Core.ToQuery DescribeAccountAttributes where
        toQuery DescribeAccountAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeAccountAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)

instance Core.ToHeaders DescribeAccountAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
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
          = Response.receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' Core.<$>
                   (x Core..@? "AccountQuotas" Core..<@>
                      Core.parseXMLList "AccountQuota")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Data returned by the __DescribeAccountAttributes__ action.
--
-- /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountQuotas :: Core.Maybe [Types.AccountQuota]
    -- ^ A list of @AccountQuota@ objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributesResponse' value with any optional fields omitted.
mkDescribeAccountAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse responseStatus
  = DescribeAccountAttributesResponse'{accountQuotas = Core.Nothing,
                                       responseStatus}

-- | A list of @AccountQuota@ objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
--
-- /Note:/ Consider using 'accountQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAccountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [Types.AccountQuota])
daarrsAccountQuotas = Lens.field @"accountQuotas"
{-# INLINEABLE daarrsAccountQuotas #-}
{-# DEPRECATED accountQuotas "Use generic-lens or generic-optics with 'accountQuotas' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
