{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccessKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access key pair associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.DeleteAccessKey
  ( -- * Creating a request
    DeleteAccessKey (..),
    mkDeleteAccessKey,

    -- ** Request lenses
    dakAccessKeyId,
    dakUserName,

    -- * Destructuring the response
    DeleteAccessKeyResponse (..),
    mkDeleteAccessKeyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccessKey' smart constructor.
data DeleteAccessKey = DeleteAccessKey'
  { -- | The access key ID for the access key ID and secret access key you want to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Types.AccessKeyId,
    -- | The name of the user whose access key pair you want to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessKey' value with any optional fields omitted.
mkDeleteAccessKey ::
  -- | 'accessKeyId'
  Types.AccessKeyId ->
  DeleteAccessKey
mkDeleteAccessKey accessKeyId =
  DeleteAccessKey' {accessKeyId, userName = Core.Nothing}

-- | The access key ID for the access key ID and secret access key you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakAccessKeyId :: Lens.Lens' DeleteAccessKey Types.AccessKeyId
dakAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED dakAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The name of the user whose access key pair you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakUserName :: Lens.Lens' DeleteAccessKey (Core.Maybe Types.UserName)
dakUserName = Lens.field @"userName"
{-# DEPRECATED dakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest DeleteAccessKey where
  type Rs DeleteAccessKey = DeleteAccessKeyResponse
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
            ( Core.pure ("Action", "DeleteAccessKey")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "AccessKeyId" accessKeyId)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response = Response.receiveNull DeleteAccessKeyResponse'

-- | /See:/ 'mkDeleteAccessKeyResponse' smart constructor.
data DeleteAccessKeyResponse = DeleteAccessKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessKeyResponse' value with any optional fields omitted.
mkDeleteAccessKeyResponse ::
  DeleteAccessKeyResponse
mkDeleteAccessKeyResponse = DeleteAccessKeyResponse'
