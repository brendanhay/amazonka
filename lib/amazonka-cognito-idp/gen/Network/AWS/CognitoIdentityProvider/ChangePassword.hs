{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
module Network.AWS.CognitoIdentityProvider.ChangePassword
  ( -- * Creating a request
    ChangePassword (..),
    mkChangePassword,

    -- ** Request lenses
    cpPreviousPassword,
    cpProposedPassword,
    cpAccessToken,

    -- * Destructuring the response
    ChangePasswordResponse (..),
    mkChangePasswordResponse,

    -- ** Response lenses
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to change a user password.
--
-- /See:/ 'mkChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The old password.
    previousPassword :: Types.PasswordType,
    -- | The new password.
    proposedPassword :: Types.PasswordType,
    -- | The access token.
    accessToken :: Types.AccessToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePassword' value with any optional fields omitted.
mkChangePassword ::
  -- | 'previousPassword'
  Types.PasswordType ->
  -- | 'proposedPassword'
  Types.PasswordType ->
  -- | 'accessToken'
  Types.AccessToken ->
  ChangePassword
mkChangePassword previousPassword proposedPassword accessToken =
  ChangePassword' {previousPassword, proposedPassword, accessToken}

-- | The old password.
--
-- /Note:/ Consider using 'previousPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPreviousPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpPreviousPassword = Lens.field @"previousPassword"
{-# DEPRECATED cpPreviousPassword "Use generic-lens or generic-optics with 'previousPassword' instead." #-}

-- | The new password.
--
-- /Note:/ Consider using 'proposedPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProposedPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpProposedPassword = Lens.field @"proposedPassword"
{-# DEPRECATED cpProposedPassword "Use generic-lens or generic-optics with 'proposedPassword' instead." #-}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAccessToken :: Lens.Lens' ChangePassword Types.AccessToken
cpAccessToken = Lens.field @"accessToken"
{-# DEPRECATED cpAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Core.FromJSON ChangePassword where
  toJSON ChangePassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PreviousPassword" Core..= previousPassword),
            Core.Just ("ProposedPassword" Core..= proposedPassword),
            Core.Just ("AccessToken" Core..= accessToken)
          ]
      )

instance Core.AWSRequest ChangePassword where
  type Rs ChangePassword = ChangePasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.ChangePassword"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangePasswordResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response from the server to the change password request.
--
-- /See:/ 'mkChangePasswordResponse' smart constructor.
newtype ChangePasswordResponse = ChangePasswordResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePasswordResponse' value with any optional fields omitted.
mkChangePasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ChangePasswordResponse
mkChangePasswordResponse responseStatus =
  ChangePasswordResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' ChangePasswordResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
