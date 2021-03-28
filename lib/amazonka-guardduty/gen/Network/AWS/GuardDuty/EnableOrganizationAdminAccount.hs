{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
    (
    -- * Creating a request
      EnableOrganizationAdminAccount (..)
    , mkEnableOrganizationAdminAccount
    -- ** Request lenses
    , eoaaAdminAccountId

    -- * Destructuring the response
    , EnableOrganizationAdminAccountResponse (..)
    , mkEnableOrganizationAdminAccountResponse
    -- ** Response lenses
    , eoaarrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableOrganizationAdminAccount' smart constructor.
newtype EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { adminAccountId :: Core.Text
    -- ^ The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableOrganizationAdminAccount' value with any optional fields omitted.
mkEnableOrganizationAdminAccount
    :: Core.Text -- ^ 'adminAccountId'
    -> EnableOrganizationAdminAccount
mkEnableOrganizationAdminAccount adminAccountId
  = EnableOrganizationAdminAccount'{adminAccountId}

-- | The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaaAdminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Core.Text
eoaaAdminAccountId = Lens.field @"adminAccountId"
{-# INLINEABLE eoaaAdminAccountId #-}
{-# DEPRECATED adminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead"  #-}

instance Core.ToQuery EnableOrganizationAdminAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableOrganizationAdminAccount where
        toHeaders EnableOrganizationAdminAccount{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableOrganizationAdminAccount where
        toJSON EnableOrganizationAdminAccount{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("adminAccountId" Core..= adminAccountId)])

instance Core.AWSRequest EnableOrganizationAdminAccount where
        type Rs EnableOrganizationAdminAccount =
             EnableOrganizationAdminAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/admin/enable",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableOrganizationAdminAccountResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableOrganizationAdminAccountResponse' smart constructor.
newtype EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableOrganizationAdminAccountResponse' value with any optional fields omitted.
mkEnableOrganizationAdminAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableOrganizationAdminAccountResponse
mkEnableOrganizationAdminAccountResponse responseStatus
  = EnableOrganizationAdminAccountResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaarrsResponseStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Core.Int
eoaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eoaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
