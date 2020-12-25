{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves information about an HSM client.
module Network.AWS.CloudHSM.DescribeLunaClient
  ( -- * Creating a request
    DescribeLunaClient (..),
    mkDescribeLunaClient,

    -- ** Request lenses
    dlcCertificateFingerprint,
    dlcClientArn,

    -- * Destructuring the response
    DescribeLunaClientResponse (..),
    mkDescribeLunaClientResponse,

    -- ** Response lenses
    dlcrfrsCertificate,
    dlcrfrsCertificateFingerprint,
    dlcrfrsClientArn,
    dlcrfrsLabel,
    dlcrfrsLastModifiedTimestamp,
    dlcrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLunaClient' smart constructor.
data DescribeLunaClient = DescribeLunaClient'
  { -- | The certificate fingerprint.
    certificateFingerprint :: Core.Maybe Types.CertificateFingerprint,
    -- | The ARN of the client.
    clientArn :: Core.Maybe Types.ClientArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLunaClient' value with any optional fields omitted.
mkDescribeLunaClient ::
  DescribeLunaClient
mkDescribeLunaClient =
  DescribeLunaClient'
    { certificateFingerprint = Core.Nothing,
      clientArn = Core.Nothing
    }

-- | The certificate fingerprint.
--
-- /Note:/ Consider using 'certificateFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcCertificateFingerprint :: Lens.Lens' DescribeLunaClient (Core.Maybe Types.CertificateFingerprint)
dlcCertificateFingerprint = Lens.field @"certificateFingerprint"
{-# DEPRECATED dlcCertificateFingerprint "Use generic-lens or generic-optics with 'certificateFingerprint' instead." #-}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcClientArn :: Lens.Lens' DescribeLunaClient (Core.Maybe Types.ClientArn)
dlcClientArn = Lens.field @"clientArn"
{-# DEPRECATED dlcClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

instance Core.FromJSON DescribeLunaClient where
  toJSON DescribeLunaClient {..} =
    Core.object
      ( Core.catMaybes
          [ ("CertificateFingerprint" Core..=)
              Core.<$> certificateFingerprint,
            ("ClientArn" Core..=) Core.<$> clientArn
          ]
      )

instance Core.AWSRequest DescribeLunaClient where
  type Rs DescribeLunaClient = DescribeLunaClientResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.DescribeLunaClient")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLunaClientResponse'
            Core.<$> (x Core..:? "Certificate")
            Core.<*> (x Core..:? "CertificateFingerprint")
            Core.<*> (x Core..:? "ClientArn")
            Core.<*> (x Core..:? "Label")
            Core.<*> (x Core..:? "LastModifiedTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLunaClientResponse' smart constructor.
data DescribeLunaClientResponse = DescribeLunaClientResponse'
  { -- | The certificate installed on the HSMs used by this client.
    certificate :: Core.Maybe Types.Certificate,
    -- | The certificate fingerprint.
    certificateFingerprint :: Core.Maybe Types.CertificateFingerprint,
    -- | The ARN of the client.
    clientArn :: Core.Maybe Types.ClientArn,
    -- | The label of the client.
    label :: Core.Maybe Types.Label,
    -- | The date and time the client was last modified.
    lastModifiedTimestamp :: Core.Maybe Types.LastModifiedTimestamp,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLunaClientResponse' value with any optional fields omitted.
mkDescribeLunaClientResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLunaClientResponse
mkDescribeLunaClientResponse responseStatus =
  DescribeLunaClientResponse'
    { certificate = Core.Nothing,
      certificateFingerprint = Core.Nothing,
      clientArn = Core.Nothing,
      label = Core.Nothing,
      lastModifiedTimestamp = Core.Nothing,
      responseStatus
    }

-- | The certificate installed on the HSMs used by this client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsCertificate :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Types.Certificate)
dlcrfrsCertificate = Lens.field @"certificate"
{-# DEPRECATED dlcrfrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The certificate fingerprint.
--
-- /Note:/ Consider using 'certificateFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsCertificateFingerprint :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Types.CertificateFingerprint)
dlcrfrsCertificateFingerprint = Lens.field @"certificateFingerprint"
{-# DEPRECATED dlcrfrsCertificateFingerprint "Use generic-lens or generic-optics with 'certificateFingerprint' instead." #-}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsClientArn :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Types.ClientArn)
dlcrfrsClientArn = Lens.field @"clientArn"
{-# DEPRECATED dlcrfrsClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

-- | The label of the client.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsLabel :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Types.Label)
dlcrfrsLabel = Lens.field @"label"
{-# DEPRECATED dlcrfrsLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The date and time the client was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsLastModifiedTimestamp :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Types.LastModifiedTimestamp)
dlcrfrsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# DEPRECATED dlcrfrsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrfrsResponseStatus :: Lens.Lens' DescribeLunaClientResponse Core.Int
dlcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
