{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified server certificate stored in IAM.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
-- /Important:/ You should understand the implications of changing a server certificate's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts Renaming a Server Certificate> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateServerCertificate
    (
    -- * Creating a request
      UpdateServerCertificate (..)
    , mkUpdateServerCertificate
    -- ** Request lenses
    , uServerCertificateName
    , uNewPath
    , uNewServerCertificateName

    -- * Destructuring the response
    , UpdateServerCertificateResponse (..)
    , mkUpdateServerCertificateResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServerCertificate' smart constructor.
data UpdateServerCertificate = UpdateServerCertificate'
  { serverCertificateName :: Types.ServerCertificateNameType
    -- ^ The name of the server certificate that you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , newPath :: Core.Maybe Types.PathType
    -- ^ The new path for the server certificate. Include this only if you are updating the server certificate's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  , newServerCertificateName :: Core.Maybe Types.ServerCertificateNameType
    -- ^ The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServerCertificate' value with any optional fields omitted.
mkUpdateServerCertificate
    :: Types.ServerCertificateNameType -- ^ 'serverCertificateName'
    -> UpdateServerCertificate
mkUpdateServerCertificate serverCertificateName
  = UpdateServerCertificate'{serverCertificateName,
                             newPath = Core.Nothing, newServerCertificateName = Core.Nothing}

-- | The name of the server certificate that you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uServerCertificateName :: Lens.Lens' UpdateServerCertificate Types.ServerCertificateNameType
uServerCertificateName = Lens.field @"serverCertificateName"
{-# INLINEABLE uServerCertificateName #-}
{-# DEPRECATED serverCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead"  #-}

-- | The new path for the server certificate. Include this only if you are updating the server certificate's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uNewPath :: Lens.Lens' UpdateServerCertificate (Core.Maybe Types.PathType)
uNewPath = Lens.field @"newPath"
{-# INLINEABLE uNewPath #-}
{-# DEPRECATED newPath "Use generic-lens or generic-optics with 'newPath' instead"  #-}

-- | The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'newServerCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uNewServerCertificateName :: Lens.Lens' UpdateServerCertificate (Core.Maybe Types.ServerCertificateNameType)
uNewServerCertificateName = Lens.field @"newServerCertificateName"
{-# INLINEABLE uNewServerCertificateName #-}
{-# DEPRECATED newServerCertificateName "Use generic-lens or generic-optics with 'newServerCertificateName' instead"  #-}

instance Core.ToQuery UpdateServerCertificate where
        toQuery UpdateServerCertificate{..}
          = Core.toQueryPair "Action"
              ("UpdateServerCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServerCertificateName" serverCertificateName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "NewPath") newPath
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "NewServerCertificateName")
                newServerCertificateName

instance Core.ToHeaders UpdateServerCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateServerCertificate where
        type Rs UpdateServerCertificate = UpdateServerCertificateResponse
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
          = Response.receiveNull UpdateServerCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse = UpdateServerCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServerCertificateResponse' value with any optional fields omitted.
mkUpdateServerCertificateResponse
    :: UpdateServerCertificateResponse
mkUpdateServerCertificateResponse
  = UpdateServerCertificateResponse'
