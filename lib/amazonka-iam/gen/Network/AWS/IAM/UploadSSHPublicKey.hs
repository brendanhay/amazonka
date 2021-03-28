{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an SSH public key and associates it with the specified IAM user.
--
-- The SSH public key uploaded by this operation can be used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.UploadSSHPublicKey
    (
    -- * Creating a request
      UploadSSHPublicKey (..)
    , mkUploadSSHPublicKey
    -- ** Request lenses
    , usshpkfUserName
    , usshpkfSSHPublicKeyBody

    -- * Destructuring the response
    , UploadSSHPublicKeyResponse (..)
    , mkUploadSSHPublicKeyResponse
    -- ** Response lenses
    , usshpkrrsSSHPublicKey
    , usshpkrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUploadSSHPublicKey' smart constructor.
data UploadSSHPublicKey = UploadSSHPublicKey'
  { userName :: Types.UserName
    -- ^ The name of the IAM user to associate the SSH public key with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , sSHPublicKeyBody :: Types.SSHPublicKeyBody
    -- ^ The SSH public key. The public key must be encoded in ssh-rsa format or PEM format. The minimum bit-length of the public key is 2048 bits. For example, you can generate a 2048-bit key, and the resulting PEM file is 1679 bytes long.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadSSHPublicKey' value with any optional fields omitted.
mkUploadSSHPublicKey
    :: Types.UserName -- ^ 'userName'
    -> Types.SSHPublicKeyBody -- ^ 'sSHPublicKeyBody'
    -> UploadSSHPublicKey
mkUploadSSHPublicKey userName sSHPublicKeyBody
  = UploadSSHPublicKey'{userName, sSHPublicKeyBody}

-- | The name of the IAM user to associate the SSH public key with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkfUserName :: Lens.Lens' UploadSSHPublicKey Types.UserName
usshpkfUserName = Lens.field @"userName"
{-# INLINEABLE usshpkfUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The SSH public key. The public key must be encoded in ssh-rsa format or PEM format. The minimum bit-length of the public key is 2048 bits. For example, you can generate a 2048-bit key, and the resulting PEM file is 1679 bytes long.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'sSHPublicKeyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkfSSHPublicKeyBody :: Lens.Lens' UploadSSHPublicKey Types.SSHPublicKeyBody
usshpkfSSHPublicKeyBody = Lens.field @"sSHPublicKeyBody"
{-# INLINEABLE usshpkfSSHPublicKeyBody #-}
{-# DEPRECATED sSHPublicKeyBody "Use generic-lens or generic-optics with 'sSHPublicKeyBody' instead"  #-}

instance Core.ToQuery UploadSSHPublicKey where
        toQuery UploadSSHPublicKey{..}
          = Core.toQueryPair "Action" ("UploadSSHPublicKey" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "SSHPublicKeyBody" sSHPublicKeyBody

instance Core.ToHeaders UploadSSHPublicKey where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UploadSSHPublicKey where
        type Rs UploadSSHPublicKey = UploadSSHPublicKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UploadSSHPublicKeyResult"
              (\ s h x ->
                 UploadSSHPublicKeyResponse' Core.<$>
                   (x Core..@? "SSHPublicKey") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'UploadSSHPublicKey' request.
--
-- /See:/ 'mkUploadSSHPublicKeyResponse' smart constructor.
data UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse'
  { sSHPublicKey :: Core.Maybe Types.SSHPublicKey
    -- ^ Contains information about the SSH public key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UploadSSHPublicKeyResponse' value with any optional fields omitted.
mkUploadSSHPublicKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UploadSSHPublicKeyResponse
mkUploadSSHPublicKeyResponse responseStatus
  = UploadSSHPublicKeyResponse'{sSHPublicKey = Core.Nothing,
                                responseStatus}

-- | Contains information about the SSH public key.
--
-- /Note:/ Consider using 'sSHPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkrrsSSHPublicKey :: Lens.Lens' UploadSSHPublicKeyResponse (Core.Maybe Types.SSHPublicKey)
usshpkrrsSSHPublicKey = Lens.field @"sSHPublicKey"
{-# INLINEABLE usshpkrrsSSHPublicKey #-}
{-# DEPRECATED sSHPublicKey "Use generic-lens or generic-optics with 'sSHPublicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkrrsResponseStatus :: Lens.Lens' UploadSSHPublicKeyResponse Core.Int
usshpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usshpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
