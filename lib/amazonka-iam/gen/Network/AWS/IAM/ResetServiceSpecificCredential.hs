{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResetServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a service-specific credential. The new password is AWS generated and cryptographically strong. It cannot be configured by the user. Resetting the password immediately invalidates the previous password associated with this user.
module Network.AWS.IAM.ResetServiceSpecificCredential
  ( -- * Creating a request
    ResetServiceSpecificCredential (..),
    mkResetServiceSpecificCredential,

    -- ** Request lenses
    rsscServiceSpecificCredentialId,
    rsscUserName,

    -- * Destructuring the response
    ResetServiceSpecificCredentialResponse (..),
    mkResetServiceSpecificCredentialResponse,

    -- ** Response lenses
    rsscrrsServiceSpecificCredential,
    rsscrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetServiceSpecificCredential' smart constructor.
data ResetServiceSpecificCredential = ResetServiceSpecificCredential'
  { -- | The unique identifier of the service-specific credential.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId,
    -- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetServiceSpecificCredential' value with any optional fields omitted.
mkResetServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Types.ServiceSpecificCredentialId ->
  ResetServiceSpecificCredential
mkResetServiceSpecificCredential serviceSpecificCredentialId =
  ResetServiceSpecificCredential'
    { serviceSpecificCredentialId,
      userName = Core.Nothing
    }

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscServiceSpecificCredentialId :: Lens.Lens' ResetServiceSpecificCredential Types.ServiceSpecificCredentialId
rsscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# DEPRECATED rsscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscUserName :: Lens.Lens' ResetServiceSpecificCredential (Core.Maybe Types.UserName)
rsscUserName = Lens.field @"userName"
{-# DEPRECATED rsscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest ResetServiceSpecificCredential where
  type
    Rs ResetServiceSpecificCredential =
      ResetServiceSpecificCredentialResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ResetServiceSpecificCredential")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> ( Core.toQueryValue
                            "ServiceSpecificCredentialId"
                            serviceSpecificCredentialId
                        )
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ResetServiceSpecificCredentialResult"
      ( \s h x ->
          ResetServiceSpecificCredentialResponse'
            Core.<$> (x Core..@? "ServiceSpecificCredential")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResetServiceSpecificCredentialResponse' smart constructor.
data ResetServiceSpecificCredentialResponse = ResetServiceSpecificCredentialResponse'
  { -- | A structure with details about the updated service-specific credential, including the new password.
    --
    -- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
    serviceSpecificCredential :: Core.Maybe Types.ServiceSpecificCredential,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResetServiceSpecificCredentialResponse' value with any optional fields omitted.
mkResetServiceSpecificCredentialResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResetServiceSpecificCredentialResponse
mkResetServiceSpecificCredentialResponse responseStatus =
  ResetServiceSpecificCredentialResponse'
    { serviceSpecificCredential =
        Core.Nothing,
      responseStatus
    }

-- | A structure with details about the updated service-specific credential, including the new password.
--
-- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
--
-- /Note:/ Consider using 'serviceSpecificCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrrsServiceSpecificCredential :: Lens.Lens' ResetServiceSpecificCredentialResponse (Core.Maybe Types.ServiceSpecificCredential)
rsscrrsServiceSpecificCredential = Lens.field @"serviceSpecificCredential"
{-# DEPRECATED rsscrrsServiceSpecificCredential "Use generic-lens or generic-optics with 'serviceSpecificCredential' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrrsResponseStatus :: Lens.Lens' ResetServiceSpecificCredentialResponse Core.Int
rsscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
