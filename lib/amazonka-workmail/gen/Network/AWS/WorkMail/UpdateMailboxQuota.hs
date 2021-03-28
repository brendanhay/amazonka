{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateMailboxQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's current mailbox quota for a specified organization and user.
module Network.AWS.WorkMail.UpdateMailboxQuota
    (
    -- * Creating a request
      UpdateMailboxQuota (..)
    , mkUpdateMailboxQuota
    -- ** Request lenses
    , umqOrganizationId
    , umqUserId
    , umqMailboxQuota

    -- * Destructuring the response
    , UpdateMailboxQuotaResponse (..)
    , mkUpdateMailboxQuotaResponse
    -- ** Response lenses
    , umqrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkUpdateMailboxQuota' smart constructor.
data UpdateMailboxQuota = UpdateMailboxQuota'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization that contains the user for whom to update the mailbox quota.
  , userId :: Types.WorkMailIdentifier
    -- ^ The identifer for the user for whom to update the mailbox quota.
  , mailboxQuota :: Core.Natural
    -- ^ The updated mailbox quota, in MB, for the specified user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMailboxQuota' value with any optional fields omitted.
mkUpdateMailboxQuota
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.WorkMailIdentifier -- ^ 'userId'
    -> Core.Natural -- ^ 'mailboxQuota'
    -> UpdateMailboxQuota
mkUpdateMailboxQuota organizationId userId mailboxQuota
  = UpdateMailboxQuota'{organizationId, userId, mailboxQuota}

-- | The identifier for the organization that contains the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqOrganizationId :: Lens.Lens' UpdateMailboxQuota Types.OrganizationId
umqOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE umqOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifer for the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqUserId :: Lens.Lens' UpdateMailboxQuota Types.WorkMailIdentifier
umqUserId = Lens.field @"userId"
{-# INLINEABLE umqUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The updated mailbox quota, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqMailboxQuota :: Lens.Lens' UpdateMailboxQuota Core.Natural
umqMailboxQuota = Lens.field @"mailboxQuota"
{-# INLINEABLE umqMailboxQuota #-}
{-# DEPRECATED mailboxQuota "Use generic-lens or generic-optics with 'mailboxQuota' instead"  #-}

instance Core.ToQuery UpdateMailboxQuota where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMailboxQuota where
        toHeaders UpdateMailboxQuota{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.UpdateMailboxQuota")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMailboxQuota where
        toJSON UpdateMailboxQuota{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("UserId" Core..= userId),
                  Core.Just ("MailboxQuota" Core..= mailboxQuota)])

instance Core.AWSRequest UpdateMailboxQuota where
        type Rs UpdateMailboxQuota = UpdateMailboxQuotaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateMailboxQuotaResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateMailboxQuotaResponse' smart constructor.
newtype UpdateMailboxQuotaResponse = UpdateMailboxQuotaResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMailboxQuotaResponse' value with any optional fields omitted.
mkUpdateMailboxQuotaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMailboxQuotaResponse
mkUpdateMailboxQuotaResponse responseStatus
  = UpdateMailboxQuotaResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqrrsResponseStatus :: Lens.Lens' UpdateMailboxQuotaResponse Core.Int
umqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
