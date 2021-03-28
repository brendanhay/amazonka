{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves AWS Organizations-related information about the specified account.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeAccount
    (
    -- * Creating a request
      DescribeAccount (..)
    , mkDescribeAccount
    -- ** Request lenses
    , daAccountId

    -- * Destructuring the response
    , DescribeAccountResponse (..)
    , mkDescribeAccountResponse
    -- ** Response lenses
    , darrsAccount
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccount' smart constructor.
newtype DescribeAccount = DescribeAccount'
  { accountId :: Types.AccountId
    -- ^ The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccount' value with any optional fields omitted.
mkDescribeAccount
    :: Types.AccountId -- ^ 'accountId'
    -> DescribeAccount
mkDescribeAccount accountId = DescribeAccount'{accountId}

-- | The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAccountId :: Lens.Lens' DescribeAccount Types.AccountId
daAccountId = Lens.field @"accountId"
{-# INLINEABLE daAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

instance Core.ToQuery DescribeAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAccount where
        toHeaders DescribeAccount{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DescribeAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAccount where
        toJSON DescribeAccount{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AccountId" Core..= accountId)])

instance Core.AWSRequest DescribeAccount where
        type Rs DescribeAccount = DescribeAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAccountResponse' Core.<$>
                   (x Core..:? "Account") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { account :: Core.Maybe Types.Account
    -- ^ A structure that contains information about the requested account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAccountResponse' value with any optional fields omitted.
mkDescribeAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountResponse
mkDescribeAccountResponse responseStatus
  = DescribeAccountResponse'{account = Core.Nothing, responseStatus}

-- | A structure that contains information about the requested account.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAccount :: Lens.Lens' DescribeAccountResponse (Core.Maybe Types.Account)
darrsAccount = Lens.field @"account"
{-# INLINEABLE darrsAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAccountResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
