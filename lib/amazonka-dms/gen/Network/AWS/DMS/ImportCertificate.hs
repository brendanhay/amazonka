{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ImportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified certificate.
module Network.AWS.DMS.ImportCertificate
  ( -- * Creating a request
    ImportCertificate (..),
    mkImportCertificate,

    -- ** Request lenses
    icCertificateIdentifier,
    icCertificatePem,
    icCertificateWallet,
    icTags,

    -- * Destructuring the response
    ImportCertificateResponse (..),
    mkImportCertificateResponse,

    -- ** Response lenses
    icrrsCertificate,
    icrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { -- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
    certificateIdentifier :: Types.String,
    -- | The contents of a @.pem@ file, which contains an X.509 certificate.
    certificatePem :: Core.Maybe Types.String,
    -- | The location of an imported Oracle Wallet certificate for use with SSL.
    certificateWallet :: Core.Maybe Core.Base64,
    -- | The tags associated with the certificate.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificate' value with any optional fields omitted.
mkImportCertificate ::
  -- | 'certificateIdentifier'
  Types.String ->
  ImportCertificate
mkImportCertificate certificateIdentifier =
  ImportCertificate'
    { certificateIdentifier,
      certificatePem = Core.Nothing,
      certificateWallet = Core.Nothing,
      tags = Core.Nothing
    }

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateIdentifier :: Lens.Lens' ImportCertificate Types.String
icCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# DEPRECATED icCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificatePem :: Lens.Lens' ImportCertificate (Core.Maybe Types.String)
icCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED icCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateWallet :: Lens.Lens' ImportCertificate (Core.Maybe Core.Base64)
icCertificateWallet = Lens.field @"certificateWallet"
{-# DEPRECATED icCertificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead." #-}

-- | The tags associated with the certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTags :: Lens.Lens' ImportCertificate (Core.Maybe [Types.Tag])
icTags = Lens.field @"tags"
{-# DEPRECATED icTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ImportCertificate where
  toJSON ImportCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CertificateIdentifier" Core..= certificateIdentifier),
            ("CertificatePem" Core..=) Core.<$> certificatePem,
            ("CertificateWallet" Core..=) Core.<$> certificateWallet,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ImportCertificate where
  type Rs ImportCertificate = ImportCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.ImportCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Core.<$> (x Core..:? "Certificate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { -- | The certificate to be uploaded.
    certificate :: Core.Maybe Types.Certificate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImportCertificateResponse' value with any optional fields omitted.
mkImportCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportCertificateResponse
mkImportCertificateResponse responseStatus =
  ImportCertificateResponse'
    { certificate = Core.Nothing,
      responseStatus
    }

-- | The certificate to be uploaded.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsCertificate :: Lens.Lens' ImportCertificateResponse (Core.Maybe Types.Certificate)
icrrsCertificate = Lens.field @"certificate"
{-# DEPRECATED icrrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsResponseStatus :: Lens.Lens' ImportCertificateResponse Core.Int
icrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED icrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
