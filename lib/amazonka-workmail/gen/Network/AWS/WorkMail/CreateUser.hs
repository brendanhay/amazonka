{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuOrganizationId,
    cuName,
    cuDisplayName,
    cuPassword,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    currsUserId,
    currsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The identifier of the organization for which the user is created.
    organizationId :: Types.OrganizationId,
    -- | The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
    name :: Types.UserName,
    -- | The display name for the new user.
    displayName :: Types.String,
    -- | The password for the new user.
    password :: Types.Password
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'name'
  Types.UserName ->
  -- | 'displayName'
  Types.String ->
  -- | 'password'
  Types.Password ->
  CreateUser
mkCreateUser organizationId name displayName password =
  CreateUser' {organizationId, name, displayName, password}

-- | The identifier of the organization for which the user is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser Types.OrganizationId
cuOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED cuOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuName :: Lens.Lens' CreateUser Types.UserName
cuName = Lens.field @"name"
{-# DEPRECATED cuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The display name for the new user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDisplayName :: Lens.Lens' CreateUser Types.String
cuDisplayName = Lens.field @"displayName"
{-# DEPRECATED cuDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The password for the new user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser Types.Password
cuPassword = Lens.field @"password"
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON CreateUser where
  toJSON CreateUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name),
            Core.Just ("DisplayName" Core..= displayName),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.CreateUser")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..:? "UserId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The identifier for the new user.
    userId :: Core.Maybe Types.WorkMailIdentifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserResponse
mkCreateUserResponse responseStatus =
  CreateUserResponse' {userId = Core.Nothing, responseStatus}

-- | The identifier for the new user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserId :: Lens.Lens' CreateUserResponse (Core.Maybe Types.WorkMailIdentifier)
currsUserId = Lens.field @"userId"
{-# DEPRECATED currsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
