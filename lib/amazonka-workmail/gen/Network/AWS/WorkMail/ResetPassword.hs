{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ResetPassword (..),
    mkResetPassword,

    -- ** Request lenses
    rpOrganizationId,
    rpUserId,
    rpPassword,

    -- * Destructuring the response
    ResetPasswordResponse (..),
    mkResetPasswordResponse,

    -- ** Response lenses
    rprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkResetPassword' smart constructor.
data ResetPassword = ResetPassword'
  { -- | The identifier of the organization that contains the user for which the password is reset.
    organizationId :: Types.OrganizationId,
    -- | The identifier of the user for whom the password is reset.
    userId :: Types.WorkMailIdentifier,
    -- | The new password for the user.
    password :: Types.Password
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetPassword' value with any optional fields omitted.
mkResetPassword ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'userId'
  Types.WorkMailIdentifier ->
  -- | 'password'
  Types.Password ->
  ResetPassword
mkResetPassword organizationId userId password =
  ResetPassword' {organizationId, userId, password}

-- | The identifier of the organization that contains the user for which the password is reset.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpOrganizationId :: Lens.Lens' ResetPassword Types.OrganizationId
rpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED rpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user for whom the password is reset.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpUserId :: Lens.Lens' ResetPassword Types.WorkMailIdentifier
rpUserId = Lens.field @"userId"
{-# DEPRECATED rpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The new password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPassword :: Lens.Lens' ResetPassword Types.Password
rpPassword = Lens.field @"password"
{-# DEPRECATED rpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON ResetPassword where
  toJSON ResetPassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.AWSRequest ResetPassword where
  type Rs ResetPassword = ResetPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.ResetPassword")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetPasswordResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResetPasswordResponse' smart constructor.
newtype ResetPasswordResponse = ResetPasswordResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetPasswordResponse' value with any optional fields omitted.
mkResetPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResetPasswordResponse
mkResetPasswordResponse responseStatus =
  ResetPasswordResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsResponseStatus :: Lens.Lens' ResetPasswordResponse Core.Int
rprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
