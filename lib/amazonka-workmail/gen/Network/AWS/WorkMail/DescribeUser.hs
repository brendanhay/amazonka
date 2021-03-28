{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information regarding the user.
module Network.AWS.WorkMail.DescribeUser
    (
    -- * Creating a request
      DescribeUser (..)
    , mkDescribeUser
    -- ** Request lenses
    , duOrganizationId
    , duUserId

    -- * Destructuring the response
    , DescribeUserResponse (..)
    , mkDescribeUserResponse
    -- ** Response lenses
    , durrsDisabledDate
    , durrsDisplayName
    , durrsEmail
    , durrsEnabledDate
    , durrsName
    , durrsState
    , durrsUserId
    , durrsUserRole
    , durrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the user exists.
  , userId :: Types.UserId
    -- ^ The identifier for the user to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUser' value with any optional fields omitted.
mkDescribeUser
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.UserId -- ^ 'userId'
    -> DescribeUser
mkDescribeUser organizationId userId
  = DescribeUser'{organizationId, userId}

-- | The identifier for the organization under which the user exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUser Types.OrganizationId
duOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE duOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier for the user to be described.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUser Types.UserId
duUserId = Lens.field @"userId"
{-# INLINEABLE duUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DescribeUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUser where
        toHeaders DescribeUser{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.DescribeUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUser where
        toJSON DescribeUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("UserId" Core..= userId)])

instance Core.AWSRequest DescribeUser where
        type Rs DescribeUser = DescribeUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUserResponse' Core.<$>
                   (x Core..:? "DisabledDate") Core.<*> x Core..:? "DisplayName"
                     Core.<*> x Core..:? "Email"
                     Core.<*> x Core..:? "EnabledDate"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "UserId"
                     Core.<*> x Core..:? "UserRole"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { disabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
  , displayName :: Core.Maybe Core.Text
    -- ^ The display name of the user.
  , email :: Core.Maybe Types.Email
    -- ^ The email of the user.
  , enabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
  , name :: Core.Maybe Types.UserName
    -- ^ The name for the user.
  , state :: Core.Maybe Types.EntityState
    -- ^ The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
  , userId :: Core.Maybe Types.WorkMailIdentifier
    -- ^ The identifier for the described user.
  , userRole :: Core.Maybe Types.UserRole
    -- ^ In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserResponse' value with any optional fields omitted.
mkDescribeUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserResponse
mkDescribeUserResponse responseStatus
  = DescribeUserResponse'{disabledDate = Core.Nothing,
                          displayName = Core.Nothing, email = Core.Nothing,
                          enabledDate = Core.Nothing, name = Core.Nothing,
                          state = Core.Nothing, userId = Core.Nothing,
                          userRole = Core.Nothing, responseStatus}

-- | The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsDisabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.NominalDiffTime)
durrsDisabledDate = Lens.field @"disabledDate"
{-# INLINEABLE durrsDisabledDate #-}
{-# DEPRECATED disabledDate "Use generic-lens or generic-optics with 'disabledDate' instead"  #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsDisplayName :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
durrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE durrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsEmail :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.Email)
durrsEmail = Lens.field @"email"
{-# INLINEABLE durrsEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsEnabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.NominalDiffTime)
durrsEnabledDate = Lens.field @"enabledDate"
{-# INLINEABLE durrsEnabledDate #-}
{-# DEPRECATED enabledDate "Use generic-lens or generic-optics with 'enabledDate' instead"  #-}

-- | The name for the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsName :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.UserName)
durrsName = Lens.field @"name"
{-# INLINEABLE durrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsState :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.EntityState)
durrsState = Lens.field @"state"
{-# INLINEABLE durrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The identifier for the described user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUserId :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.WorkMailIdentifier)
durrsUserId = Lens.field @"userId"
{-# INLINEABLE durrsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUserRole :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.UserRole)
durrsUserRole = Lens.field @"userRole"
{-# INLINEABLE durrsUserRole #-}
{-# DEPRECATED userRole "Use generic-lens or generic-optics with 'userRole' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUserResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE durrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
