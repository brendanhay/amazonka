{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
module Network.AWS.IAM.DeleteServiceSpecificCredential
  ( -- * Creating a request
    DeleteServiceSpecificCredential (..),
    mkDeleteServiceSpecificCredential,

    -- ** Request lenses
    dsscServiceSpecificCredentialId,
    dsscUserName,

    -- * Destructuring the response
    DeleteServiceSpecificCredentialResponse (..),
    mkDeleteServiceSpecificCredentialResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteServiceSpecificCredential' smart constructor.
data DeleteServiceSpecificCredential = DeleteServiceSpecificCredential'
  { -- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId,
    -- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceSpecificCredential' value with any optional fields omitted.
mkDeleteServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Types.ServiceSpecificCredentialId ->
  DeleteServiceSpecificCredential
mkDeleteServiceSpecificCredential serviceSpecificCredentialId =
  DeleteServiceSpecificCredential'
    { serviceSpecificCredentialId,
      userName = Core.Nothing
    }

-- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscServiceSpecificCredentialId :: Lens.Lens' DeleteServiceSpecificCredential Types.ServiceSpecificCredentialId
dsscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# DEPRECATED dsscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscUserName :: Lens.Lens' DeleteServiceSpecificCredential (Core.Maybe Types.UserNameType)
dsscUserName = Lens.field @"userName"
{-# DEPRECATED dsscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest DeleteServiceSpecificCredential where
  type
    Rs DeleteServiceSpecificCredential =
      DeleteServiceSpecificCredentialResponse
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
            ( Core.pure ("Action", "DeleteServiceSpecificCredential")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> ( Core.toQueryValue
                            "ServiceSpecificCredentialId"
                            serviceSpecificCredentialId
                        )
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveNull DeleteServiceSpecificCredentialResponse'

-- | /See:/ 'mkDeleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse = DeleteServiceSpecificCredentialResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceSpecificCredentialResponse' value with any optional fields omitted.
mkDeleteServiceSpecificCredentialResponse ::
  DeleteServiceSpecificCredentialResponse
mkDeleteServiceSpecificCredentialResponse =
  DeleteServiceSpecificCredentialResponse'
