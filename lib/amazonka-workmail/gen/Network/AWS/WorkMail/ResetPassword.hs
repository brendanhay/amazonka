{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ResetPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the administrator to reset the password for a user.
module Network.AWS.WorkMail.ResetPassword
    (
    -- * Creating a request
      ResetPassword (..)
    , mkResetPassword
    -- ** Request lenses
    , rpOrganizationId
    , rpUserId
    , rpPassword

    -- * Destructuring the response
    , ResetPasswordResponse (..)
    , mkResetPasswordResponse
    -- ** Response lenses
    , rprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkResetPassword' smart constructor.
data ResetPassword = ResetPassword'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier of the organization that contains the user for which the password is reset.
  , userId :: Types.WorkMailIdentifier
    -- ^ The identifier of the user for whom the password is reset.
  , password :: Types.Password
    -- ^ The new password for the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetPassword' value with any optional fields omitted.
mkResetPassword
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.WorkMailIdentifier -- ^ 'userId'
    -> Types.Password -- ^ 'password'
    -> ResetPassword
mkResetPassword organizationId userId password
  = ResetPassword'{organizationId, userId, password}

-- | The identifier of the organization that contains the user for which the password is reset.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpOrganizationId :: Lens.Lens' ResetPassword Types.OrganizationId
rpOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE rpOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the user for whom the password is reset.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpUserId :: Lens.Lens' ResetPassword Types.WorkMailIdentifier
rpUserId = Lens.field @"userId"
{-# INLINEABLE rpUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The new password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPassword :: Lens.Lens' ResetPassword Types.Password
rpPassword = Lens.field @"password"
{-# INLINEABLE rpPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

instance Core.ToQuery ResetPassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetPassword where
        toHeaders ResetPassword{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.ResetPassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetPassword where
        toJSON ResetPassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("UserId" Core..= userId),
                  Core.Just ("Password" Core..= password)])

instance Core.AWSRequest ResetPassword where
        type Rs ResetPassword = ResetPasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ResetPasswordResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetPasswordResponse' smart constructor.
newtype ResetPasswordResponse = ResetPasswordResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetPasswordResponse' value with any optional fields omitted.
mkResetPasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetPasswordResponse
mkResetPasswordResponse responseStatus
  = ResetPasswordResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsResponseStatus :: Lens.Lens' ResetPasswordResponse Core.Int
rprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
