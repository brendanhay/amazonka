{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified SSH public key, including metadata about the key.
--
-- The SSH public key retrieved by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.GetSSHPublicKey
    (
    -- * Creating a request
      GetSSHPublicKey (..)
    , mkGetSSHPublicKey
    -- ** Request lenses
    , gsshpkUserName
    , gsshpkSSHPublicKeyId
    , gsshpkEncoding

    -- * Destructuring the response
    , GetSSHPublicKeyResponse (..)
    , mkGetSSHPublicKeyResponse
    -- ** Response lenses
    , gsshpkrrsSSHPublicKey
    , gsshpkrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSSHPublicKey' smart constructor.
data GetSSHPublicKey = GetSSHPublicKey'
  { userName :: Types.UserNameType
    -- ^ The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , sSHPublicKeyId :: Types.PublicKeyIdType
    -- ^ The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  , encoding :: Types.EncodingType
    -- ^ Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSSHPublicKey' value with any optional fields omitted.
mkGetSSHPublicKey
    :: Types.UserNameType -- ^ 'userName'
    -> Types.PublicKeyIdType -- ^ 'sSHPublicKeyId'
    -> Types.EncodingType -- ^ 'encoding'
    -> GetSSHPublicKey
mkGetSSHPublicKey userName sSHPublicKeyId encoding
  = GetSSHPublicKey'{userName, sSHPublicKeyId, encoding}

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsshpkUserName :: Lens.Lens' GetSSHPublicKey Types.UserNameType
gsshpkUserName = Lens.field @"userName"
{-# INLINEABLE gsshpkUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'sSHPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsshpkSSHPublicKeyId :: Lens.Lens' GetSSHPublicKey Types.PublicKeyIdType
gsshpkSSHPublicKeyId = Lens.field @"sSHPublicKeyId"
{-# INLINEABLE gsshpkSSHPublicKeyId #-}
{-# DEPRECATED sSHPublicKeyId "Use generic-lens or generic-optics with 'sSHPublicKeyId' instead"  #-}

-- | Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
--
-- /Note:/ Consider using 'encoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsshpkEncoding :: Lens.Lens' GetSSHPublicKey Types.EncodingType
gsshpkEncoding = Lens.field @"encoding"
{-# INLINEABLE gsshpkEncoding #-}
{-# DEPRECATED encoding "Use generic-lens or generic-optics with 'encoding' instead"  #-}

instance Core.ToQuery GetSSHPublicKey where
        toQuery GetSSHPublicKey{..}
          = Core.toQueryPair "Action" ("GetSSHPublicKey" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "SSHPublicKeyId" sSHPublicKeyId
              Core.<> Core.toQueryPair "Encoding" encoding

instance Core.ToHeaders GetSSHPublicKey where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetSSHPublicKey where
        type Rs GetSSHPublicKey = GetSSHPublicKeyResponse
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
          = Response.receiveXMLWrapper "GetSSHPublicKeyResult"
              (\ s h x ->
                 GetSSHPublicKeyResponse' Core.<$>
                   (x Core..@? "SSHPublicKey") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetSSHPublicKey' request.
--
-- /See:/ 'mkGetSSHPublicKeyResponse' smart constructor.
data GetSSHPublicKeyResponse = GetSSHPublicKeyResponse'
  { sSHPublicKey :: Core.Maybe Types.SSHPublicKey
    -- ^ A structure containing details about the SSH public key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSSHPublicKeyResponse' value with any optional fields omitted.
mkGetSSHPublicKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSSHPublicKeyResponse
mkGetSSHPublicKeyResponse responseStatus
  = GetSSHPublicKeyResponse'{sSHPublicKey = Core.Nothing,
                             responseStatus}

-- | A structure containing details about the SSH public key.
--
-- /Note:/ Consider using 'sSHPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsshpkrrsSSHPublicKey :: Lens.Lens' GetSSHPublicKeyResponse (Core.Maybe Types.SSHPublicKey)
gsshpkrrsSSHPublicKey = Lens.field @"sSHPublicKey"
{-# INLINEABLE gsshpkrrsSSHPublicKey #-}
{-# DEPRECATED sSHPublicKey "Use generic-lens or generic-optics with 'sSHPublicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsshpkrrsResponseStatus :: Lens.Lens' GetSSHPublicKeyResponse Core.Int
gsshpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsshpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
