{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of an asynchronous request to create an account.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeCreateAccountStatus
    (
    -- * Creating a request
      DescribeCreateAccountStatus (..)
    , mkDescribeCreateAccountStatus
    -- ** Request lenses
    , dcasCreateAccountRequestId

    -- * Destructuring the response
    , DescribeCreateAccountStatusResponse (..)
    , mkDescribeCreateAccountStatusResponse
    -- ** Response lenses
    , dcasrrsCreateAccountStatus
    , dcasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCreateAccountStatus' smart constructor.
newtype DescribeCreateAccountStatus = DescribeCreateAccountStatus'
  { createAccountRequestId :: Types.CreateAccountRequestId
    -- ^ Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCreateAccountStatus' value with any optional fields omitted.
mkDescribeCreateAccountStatus
    :: Types.CreateAccountRequestId -- ^ 'createAccountRequestId'
    -> DescribeCreateAccountStatus
mkDescribeCreateAccountStatus createAccountRequestId
  = DescribeCreateAccountStatus'{createAccountRequestId}

-- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'createAccountRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasCreateAccountRequestId :: Lens.Lens' DescribeCreateAccountStatus Types.CreateAccountRequestId
dcasCreateAccountRequestId = Lens.field @"createAccountRequestId"
{-# INLINEABLE dcasCreateAccountRequestId #-}
{-# DEPRECATED createAccountRequestId "Use generic-lens or generic-optics with 'createAccountRequestId' instead"  #-}

instance Core.ToQuery DescribeCreateAccountStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCreateAccountStatus where
        toHeaders DescribeCreateAccountStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.DescribeCreateAccountStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCreateAccountStatus where
        toJSON DescribeCreateAccountStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CreateAccountRequestId" Core..= createAccountRequestId)])

instance Core.AWSRequest DescribeCreateAccountStatus where
        type Rs DescribeCreateAccountStatus =
             DescribeCreateAccountStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCreateAccountStatusResponse' Core.<$>
                   (x Core..:? "CreateAccountStatus") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { createAccountStatus :: Core.Maybe Types.CreateAccountStatus
    -- ^ A structure that contains the current status of an account creation request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCreateAccountStatusResponse' value with any optional fields omitted.
mkDescribeCreateAccountStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCreateAccountStatusResponse
mkDescribeCreateAccountStatusResponse responseStatus
  = DescribeCreateAccountStatusResponse'{createAccountStatus =
                                           Core.Nothing,
                                         responseStatus}

-- | A structure that contains the current status of an account creation request.
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrrsCreateAccountStatus :: Lens.Lens' DescribeCreateAccountStatusResponse (Core.Maybe Types.CreateAccountStatus)
dcasrrsCreateAccountStatus = Lens.field @"createAccountStatus"
{-# INLINEABLE dcasrrsCreateAccountStatus #-}
{-# DEPRECATED createAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrrsResponseStatus :: Lens.Lens' DescribeCreateAccountStatusResponse Core.Int
dcasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
