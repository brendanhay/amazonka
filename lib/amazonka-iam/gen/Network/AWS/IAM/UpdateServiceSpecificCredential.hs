{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of a service-specific credential to @Active@ or @Inactive@ . Service-specific credentials that are inactive cannot be used for authentication to the service. This operation can be used to disable a user's service-specific credential as part of a credential rotation work flow.
module Network.AWS.IAM.UpdateServiceSpecificCredential
  ( -- * Creating a request
    UpdateServiceSpecificCredential (..),
    mkUpdateServiceSpecificCredential,

    -- ** Request lenses
    usscServiceSpecificCredentialId,
    usscStatus,
    usscUserName,

    -- * Destructuring the response
    UpdateServiceSpecificCredentialResponse (..),
    mkUpdateServiceSpecificCredentialResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServiceSpecificCredential' smart constructor.
data UpdateServiceSpecificCredential = UpdateServiceSpecificCredential'
  { -- | The unique identifier of the service-specific credential.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId,
    -- | The status to be assigned to the service-specific credential.
    status :: Types.StatusType,
    -- | The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceSpecificCredential' value with any optional fields omitted.
mkUpdateServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Types.ServiceSpecificCredentialId ->
  -- | 'status'
  Types.StatusType ->
  UpdateServiceSpecificCredential
mkUpdateServiceSpecificCredential
  serviceSpecificCredentialId
  status =
    UpdateServiceSpecificCredential'
      { serviceSpecificCredentialId,
        status,
        userName = Core.Nothing
      }

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscServiceSpecificCredentialId :: Lens.Lens' UpdateServiceSpecificCredential Types.ServiceSpecificCredentialId
usscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# DEPRECATED usscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The status to be assigned to the service-specific credential.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscStatus :: Lens.Lens' UpdateServiceSpecificCredential Types.StatusType
usscStatus = Lens.field @"status"
{-# DEPRECATED usscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscUserName :: Lens.Lens' UpdateServiceSpecificCredential (Core.Maybe Types.UserNameType)
usscUserName = Lens.field @"userName"
{-# DEPRECATED usscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest UpdateServiceSpecificCredential where
  type
    Rs UpdateServiceSpecificCredential =
      UpdateServiceSpecificCredentialResponse
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
            ( Core.pure ("Action", "UpdateServiceSpecificCredential")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> ( Core.toQueryValue
                            "ServiceSpecificCredentialId"
                            serviceSpecificCredentialId
                        )
                Core.<> (Core.toQueryValue "Status" status)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveNull UpdateServiceSpecificCredentialResponse'

-- | /See:/ 'mkUpdateServiceSpecificCredentialResponse' smart constructor.
data UpdateServiceSpecificCredentialResponse = UpdateServiceSpecificCredentialResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceSpecificCredentialResponse' value with any optional fields omitted.
mkUpdateServiceSpecificCredentialResponse ::
  UpdateServiceSpecificCredentialResponse
mkUpdateServiceSpecificCredentialResponse =
  UpdateServiceSpecificCredentialResponse'
