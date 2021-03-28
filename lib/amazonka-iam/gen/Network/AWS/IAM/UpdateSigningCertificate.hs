{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified user signing certificate from active to disabled, or vice versa. This operation can be used to disable an IAM user's signing certificate as part of a certificate rotation work flow.
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.UpdateSigningCertificate
    (
    -- * Creating a request
      UpdateSigningCertificate (..)
    , mkUpdateSigningCertificate
    -- ** Request lenses
    , uscCertificateId
    , uscStatus
    , uscUserName

    -- * Destructuring the response
    , UpdateSigningCertificateResponse (..)
    , mkUpdateSigningCertificateResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSigningCertificate' smart constructor.
data UpdateSigningCertificate = UpdateSigningCertificate'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the signing certificate you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  , status :: Types.StatusType
    -- ^ The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
  , userName :: Core.Maybe Types.UserName
    -- ^ The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSigningCertificate' value with any optional fields omitted.
mkUpdateSigningCertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> Types.StatusType -- ^ 'status'
    -> UpdateSigningCertificate
mkUpdateSigningCertificate certificateId status
  = UpdateSigningCertificate'{certificateId, status,
                              userName = Core.Nothing}

-- | The ID of the signing certificate you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCertificateId :: Lens.Lens' UpdateSigningCertificate Types.CertificateId
uscCertificateId = Lens.field @"certificateId"
{-# INLINEABLE uscCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscStatus :: Lens.Lens' UpdateSigningCertificate Types.StatusType
uscStatus = Lens.field @"status"
{-# INLINEABLE uscStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscUserName :: Lens.Lens' UpdateSigningCertificate (Core.Maybe Types.UserName)
uscUserName = Lens.field @"userName"
{-# INLINEABLE uscUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery UpdateSigningCertificate where
        toQuery UpdateSigningCertificate{..}
          = Core.toQueryPair "Action"
              ("UpdateSigningCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "CertificateId" certificateId
              Core.<> Core.toQueryPair "Status" status
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders UpdateSigningCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateSigningCertificate where
        type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse
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
          = Response.receiveNull UpdateSigningCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSigningCertificateResponse' smart constructor.
data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSigningCertificateResponse' value with any optional fields omitted.
mkUpdateSigningCertificateResponse
    :: UpdateSigningCertificateResponse
mkUpdateSigningCertificateResponse
  = UpdateSigningCertificateResponse'
