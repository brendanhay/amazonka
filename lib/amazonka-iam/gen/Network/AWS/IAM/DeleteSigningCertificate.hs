{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a signing certificate associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated IAM users.
module Network.AWS.IAM.DeleteSigningCertificate
  ( -- * Creating a request
    DeleteSigningCertificate (..),
    mkDeleteSigningCertificate,

    -- ** Request lenses
    dscCertificateId,
    dscUserName,

    -- * Destructuring the response
    DeleteSigningCertificateResponse (..),
    mkDeleteSigningCertificateResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSigningCertificate' smart constructor.
data DeleteSigningCertificate = DeleteSigningCertificate'
  { -- | The ID of the signing certificate to delete.
    --
    -- The format of this parameter, as described by its <http://wikipedia.org/wiki/regex regex> pattern, is a string of characters that can be upper- or lower-cased letters or digits.
    certificateId :: Types.CertificateId,
    -- | The name of the user the signing certificate belongs to.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSigningCertificate' value with any optional fields omitted.
mkDeleteSigningCertificate ::
  -- | 'certificateId'
  Types.CertificateId ->
  DeleteSigningCertificate
mkDeleteSigningCertificate certificateId =
  DeleteSigningCertificate' {certificateId, userName = Core.Nothing}

-- | The ID of the signing certificate to delete.
--
-- The format of this parameter, as described by its <http://wikipedia.org/wiki/regex regex> pattern, is a string of characters that can be upper- or lower-cased letters or digits.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscCertificateId :: Lens.Lens' DeleteSigningCertificate Types.CertificateId
dscCertificateId = Lens.field @"certificateId"
{-# DEPRECATED dscCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The name of the user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscUserName :: Lens.Lens' DeleteSigningCertificate (Core.Maybe Types.UserName)
dscUserName = Lens.field @"userName"
{-# DEPRECATED dscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest DeleteSigningCertificate where
  type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse
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
            ( Core.pure ("Action", "DeleteSigningCertificate")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "CertificateId" certificateId)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response = Response.receiveNull DeleteSigningCertificateResponse'

-- | /See:/ 'mkDeleteSigningCertificateResponse' smart constructor.
data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSigningCertificateResponse' value with any optional fields omitted.
mkDeleteSigningCertificateResponse ::
  DeleteSigningCertificateResponse
mkDeleteSigningCertificateResponse =
  DeleteSigningCertificateResponse'
