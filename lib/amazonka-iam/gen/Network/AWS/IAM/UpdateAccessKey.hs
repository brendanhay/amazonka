{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified access key from Active to Inactive, or vice versa. This operation can be used to disable a user's key as part of a key rotation workflow.
--
-- If the @UserName@ is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
-- For information about rotating keys, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing Keys and Certificates> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateAccessKey
  ( -- * Creating a request
    UpdateAccessKey (..),
    mkUpdateAccessKey,

    -- ** Request lenses
    uakAccessKeyId,
    uakStatus,
    uakUserName,

    -- * Destructuring the response
    UpdateAccessKeyResponse (..),
    mkUpdateAccessKeyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAccessKey' smart constructor.
data UpdateAccessKey = UpdateAccessKey'
  { -- | The access key ID of the secret access key you want to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Types.AccessKeyId,
    -- | The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
    status :: Types.StatusType,
    -- | The name of the user whose key you want to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccessKey' value with any optional fields omitted.
mkUpdateAccessKey ::
  -- | 'accessKeyId'
  Types.AccessKeyId ->
  -- | 'status'
  Types.StatusType ->
  UpdateAccessKey
mkUpdateAccessKey accessKeyId status =
  UpdateAccessKey' {accessKeyId, status, userName = Core.Nothing}

-- | The access key ID of the secret access key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakAccessKeyId :: Lens.Lens' UpdateAccessKey Types.AccessKeyId
uakAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED uakAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakStatus :: Lens.Lens' UpdateAccessKey Types.StatusType
uakStatus = Lens.field @"status"
{-# DEPRECATED uakStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the user whose key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakUserName :: Lens.Lens' UpdateAccessKey (Core.Maybe Types.UserName)
uakUserName = Lens.field @"userName"
{-# DEPRECATED uakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest UpdateAccessKey where
  type Rs UpdateAccessKey = UpdateAccessKeyResponse
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
            ( Core.pure ("Action", "UpdateAccessKey")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "AccessKeyId" accessKeyId)
                Core.<> (Core.toQueryValue "Status" status)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response = Response.receiveNull UpdateAccessKeyResponse'

-- | /See:/ 'mkUpdateAccessKeyResponse' smart constructor.
data UpdateAccessKeyResponse = UpdateAccessKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccessKeyResponse' value with any optional fields omitted.
mkUpdateAccessKeyResponse ::
  UpdateAccessKeyResponse
mkUpdateAccessKeyResponse = UpdateAccessKeyResponse'
