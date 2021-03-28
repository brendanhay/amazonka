{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user who can be used in Amazon WorkMail by calling the 'RegisterToWorkMail' operation.
module Network.AWS.WorkMail.CreateUser
    (
    -- * Creating a request
      CreateUser (..)
    , mkCreateUser
    -- ** Request lenses
    , cuOrganizationId
    , cuName
    , cuDisplayName
    , cuPassword

    -- * Destructuring the response
    , CreateUserResponse (..)
    , mkCreateUserResponse
    -- ** Response lenses
    , currsUserId
    , currsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier of the organization for which the user is created.
  , name :: Types.UserName
    -- ^ The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
  , displayName :: Core.Text
    -- ^ The display name for the new user.
  , password :: Types.Password
    -- ^ The password for the new user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.UserName -- ^ 'name'
    -> Core.Text -- ^ 'displayName'
    -> Types.Password -- ^ 'password'
    -> CreateUser
mkCreateUser organizationId name displayName password
  = CreateUser'{organizationId, name, displayName, password}

-- | The identifier of the organization for which the user is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser Types.OrganizationId
cuOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE cuOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuName :: Lens.Lens' CreateUser Types.UserName
cuName = Lens.field @"name"
{-# INLINEABLE cuName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The display name for the new user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDisplayName :: Lens.Lens' CreateUser Core.Text
cuDisplayName = Lens.field @"displayName"
{-# INLINEABLE cuDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The password for the new user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser Types.Password
cuPassword = Lens.field @"password"
{-# INLINEABLE cuPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

instance Core.ToQuery CreateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUser where
        toHeaders CreateUser{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.CreateUser") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUser where
        toJSON CreateUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("Name" Core..= name),
                  Core.Just ("DisplayName" Core..= displayName),
                  Core.Just ("Password" Core..= password)])

instance Core.AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserResponse' Core.<$>
                   (x Core..:? "UserId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { userId :: Core.Maybe Types.WorkMailIdentifier
    -- ^ The identifier for the new user.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserResponse
mkCreateUserResponse responseStatus
  = CreateUserResponse'{userId = Core.Nothing, responseStatus}

-- | The identifier for the new user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserId :: Lens.Lens' CreateUserResponse (Core.Maybe Types.WorkMailIdentifier)
currsUserId = Lens.field @"userId"
{-# INLINEABLE currsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE currsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
