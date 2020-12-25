{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified SSH public key.
--
-- The SSH public key deleted by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.DeleteSSHPublicKey
  ( -- * Creating a request
    DeleteSSHPublicKey (..),
    mkDeleteSSHPublicKey,

    -- ** Request lenses
    dsshpkUserName,
    dsshpkSSHPublicKeyId,

    -- * Destructuring the response
    DeleteSSHPublicKeyResponse (..),
    mkDeleteSSHPublicKeyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSSHPublicKey' smart constructor.
data DeleteSSHPublicKey = DeleteSSHPublicKey'
  { -- | The name of the IAM user associated with the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | The unique identifier for the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    sSHPublicKeyId :: Types.PublicKeyIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSSHPublicKey' value with any optional fields omitted.
mkDeleteSSHPublicKey ::
  -- | 'userName'
  Types.UserName ->
  -- | 'sSHPublicKeyId'
  Types.PublicKeyIdType ->
  DeleteSSHPublicKey
mkDeleteSSHPublicKey userName sSHPublicKeyId =
  DeleteSSHPublicKey' {userName, sSHPublicKeyId}

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsshpkUserName :: Lens.Lens' DeleteSSHPublicKey Types.UserName
dsshpkUserName = Lens.field @"userName"
{-# DEPRECATED dsshpkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'sSHPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsshpkSSHPublicKeyId :: Lens.Lens' DeleteSSHPublicKey Types.PublicKeyIdType
dsshpkSSHPublicKeyId = Lens.field @"sSHPublicKeyId"
{-# DEPRECATED dsshpkSSHPublicKeyId "Use generic-lens or generic-optics with 'sSHPublicKeyId' instead." #-}

instance Core.AWSRequest DeleteSSHPublicKey where
  type Rs DeleteSSHPublicKey = DeleteSSHPublicKeyResponse
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
            ( Core.pure ("Action", "DeleteSSHPublicKey")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "SSHPublicKeyId" sSHPublicKeyId)
            )
      }
  response = Response.receiveNull DeleteSSHPublicKeyResponse'

-- | /See:/ 'mkDeleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSSHPublicKeyResponse' value with any optional fields omitted.
mkDeleteSSHPublicKeyResponse ::
  DeleteSSHPublicKeyResponse
mkDeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
