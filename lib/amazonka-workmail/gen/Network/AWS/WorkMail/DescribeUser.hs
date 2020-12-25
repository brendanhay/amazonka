{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeUser (..),
    mkDescribeUser,

    -- ** Request lenses
    duOrganizationId,
    duUserId,

    -- * Destructuring the response
    DescribeUserResponse (..),
    mkDescribeUserResponse,

    -- ** Response lenses
    durrsDisabledDate,
    durrsDisplayName,
    durrsEmail,
    durrsEnabledDate,
    durrsName,
    durrsState,
    durrsUserId,
    durrsUserRole,
    durrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the user to be described.
    userId :: Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUser' value with any optional fields omitted.
mkDescribeUser ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'userId'
  Types.UserId ->
  DescribeUser
mkDescribeUser organizationId userId =
  DescribeUser' {organizationId, userId}

-- | The identifier for the organization under which the user exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUser Types.OrganizationId
duOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED duOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user to be described.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUser Types.UserId
duUserId = Lens.field @"userId"
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromJSON DescribeUser where
  toJSON DescribeUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId)
          ]
      )

instance Core.AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.DescribeUser")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Core.<$> (x Core..:? "DisabledDate")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "Email")
            Core.<*> (x Core..:? "EnabledDate")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "UserId")
            Core.<*> (x Core..:? "UserRole")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
    disabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The display name of the user.
    displayName :: Core.Maybe Types.String,
    -- | The email of the user.
    email :: Core.Maybe Types.Email,
    -- | The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
    enabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name for the user.
    name :: Core.Maybe Types.UserName,
    -- | The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
    state :: Core.Maybe Types.EntityState,
    -- | The identifier for the described user.
    userId :: Core.Maybe Types.WorkMailIdentifier,
    -- | In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
    userRole :: Core.Maybe Types.UserRole,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUserResponse' value with any optional fields omitted.
mkDescribeUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserResponse
mkDescribeUserResponse responseStatus =
  DescribeUserResponse'
    { disabledDate = Core.Nothing,
      displayName = Core.Nothing,
      email = Core.Nothing,
      enabledDate = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing,
      userId = Core.Nothing,
      userRole = Core.Nothing,
      responseStatus
    }

-- | The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsDisabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.NominalDiffTime)
durrsDisabledDate = Lens.field @"disabledDate"
{-# DEPRECATED durrsDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsDisplayName :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.String)
durrsDisplayName = Lens.field @"displayName"
{-# DEPRECATED durrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsEmail :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.Email)
durrsEmail = Lens.field @"email"
{-# DEPRECATED durrsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsEnabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.NominalDiffTime)
durrsEnabledDate = Lens.field @"enabledDate"
{-# DEPRECATED durrsEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The name for the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsName :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.UserName)
durrsName = Lens.field @"name"
{-# DEPRECATED durrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsState :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.EntityState)
durrsState = Lens.field @"state"
{-# DEPRECATED durrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier for the described user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUserId :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.WorkMailIdentifier)
durrsUserId = Lens.field @"userId"
{-# DEPRECATED durrsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUserRole :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.UserRole)
durrsUserRole = Lens.field @"userRole"
{-# DEPRECATED durrsUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUserResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED durrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
