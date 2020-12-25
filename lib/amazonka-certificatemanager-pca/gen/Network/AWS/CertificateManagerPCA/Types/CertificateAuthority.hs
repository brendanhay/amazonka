{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
  ( CertificateAuthority (..),

    -- * Smart constructor
    mkCertificateAuthority,

    -- * Lenses
    caArn,
    caCertificateAuthorityConfiguration,
    caCreatedAt,
    caFailureReason,
    caLastStateChangeAt,
    caNotAfter,
    caNotBefore,
    caOwnerAccount,
    caRestorableUntil,
    caRevocationConfiguration,
    caSerial,
    caStatus,
    caType,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types.AccountId as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Arn as Types
import qualified Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration as Types
import qualified Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus as Types
import qualified Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType as Types
import qualified Network.AWS.CertificateManagerPCA.Types.FailureReason as Types
import qualified Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration as Types
import qualified Network.AWS.CertificateManagerPCA.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about your private certificate authority (CA). Your private CA can issue and revoke X.509 digital certificates. Digital certificates verify that the entity named in the certificate __Subject__ field owns or controls the public key contained in the __Subject Public Key Info__ field. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action to create your private CA. You must then call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCertificate.html GetCertificateAuthorityCertificate> action to retrieve a private CA certificate signing request (CSR). Sign the CSR with your ACM Private CA-hosted or on-premises root or subordinate CA certificate. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action to import the signed certificate into AWS Certificate Manager (ACM).
--
-- /See:/ 'mkCertificateAuthority' smart constructor.
data CertificateAuthority = CertificateAuthority'
  { -- | Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
    arn :: Core.Maybe Types.Arn,
    -- | Your private CA configuration.
    certificateAuthorityConfiguration :: Core.Maybe Types.CertificateAuthorityConfiguration,
    -- | Date and time at which your private CA was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | Reason the request to create your private CA failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | Date and time at which your private CA was last updated.
    lastStateChangeAt :: Core.Maybe Core.NominalDiffTime,
    -- | Date and time after which your private CA certificate is not valid.
    notAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Date and time before which your private CA certificate is not valid.
    notBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS account ID that owns the certificate authority.
    ownerAccount :: Core.Maybe Types.AccountId,
    -- | The period during which a deleted CA can be restored. For more information, see the @PermanentDeletionTimeInDays@ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest> action.
    restorableUntil :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the certificate revocation list (CRL) created and maintained by your private CA.
    revocationConfiguration :: Core.Maybe Types.RevocationConfiguration,
    -- | Serial number of your private CA.
    serial :: Core.Maybe Types.String,
    -- | Status of your private CA.
    status :: Core.Maybe Types.CertificateAuthorityStatus,
    -- | Type of your private CA.
    type' :: Core.Maybe Types.CertificateAuthorityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CertificateAuthority' value with any optional fields omitted.
mkCertificateAuthority ::
  CertificateAuthority
mkCertificateAuthority =
  CertificateAuthority'
    { arn = Core.Nothing,
      certificateAuthorityConfiguration = Core.Nothing,
      createdAt = Core.Nothing,
      failureReason = Core.Nothing,
      lastStateChangeAt = Core.Nothing,
      notAfter = Core.Nothing,
      notBefore = Core.Nothing,
      ownerAccount = Core.Nothing,
      restorableUntil = Core.Nothing,
      revocationConfiguration = Core.Nothing,
      serial = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing
    }

-- | Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caArn :: Lens.Lens' CertificateAuthority (Core.Maybe Types.Arn)
caArn = Lens.field @"arn"
{-# DEPRECATED caArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Your private CA configuration.
--
-- /Note:/ Consider using 'certificateAuthorityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCertificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Core.Maybe Types.CertificateAuthorityConfiguration)
caCertificateAuthorityConfiguration = Lens.field @"certificateAuthorityConfiguration"
{-# DEPRECATED caCertificateAuthorityConfiguration "Use generic-lens or generic-optics with 'certificateAuthorityConfiguration' instead." #-}

-- | Date and time at which your private CA was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreatedAt :: Lens.Lens' CertificateAuthority (Core.Maybe Core.NominalDiffTime)
caCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED caCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Reason the request to create your private CA failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caFailureReason :: Lens.Lens' CertificateAuthority (Core.Maybe Types.FailureReason)
caFailureReason = Lens.field @"failureReason"
{-# DEPRECATED caFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Date and time at which your private CA was last updated.
--
-- /Note:/ Consider using 'lastStateChangeAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLastStateChangeAt :: Lens.Lens' CertificateAuthority (Core.Maybe Core.NominalDiffTime)
caLastStateChangeAt = Lens.field @"lastStateChangeAt"
{-# DEPRECATED caLastStateChangeAt "Use generic-lens or generic-optics with 'lastStateChangeAt' instead." #-}

-- | Date and time after which your private CA certificate is not valid.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNotAfter :: Lens.Lens' CertificateAuthority (Core.Maybe Core.NominalDiffTime)
caNotAfter = Lens.field @"notAfter"
{-# DEPRECATED caNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | Date and time before which your private CA certificate is not valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNotBefore :: Lens.Lens' CertificateAuthority (Core.Maybe Core.NominalDiffTime)
caNotBefore = Lens.field @"notBefore"
{-# DEPRECATED caNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The AWS account ID that owns the certificate authority.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOwnerAccount :: Lens.Lens' CertificateAuthority (Core.Maybe Types.AccountId)
caOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED caOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The period during which a deleted CA can be restored. For more information, see the @PermanentDeletionTimeInDays@ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest> action.
--
-- /Note:/ Consider using 'restorableUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRestorableUntil :: Lens.Lens' CertificateAuthority (Core.Maybe Core.NominalDiffTime)
caRestorableUntil = Lens.field @"restorableUntil"
{-# DEPRECATED caRestorableUntil "Use generic-lens or generic-optics with 'restorableUntil' instead." #-}

-- | Information about the certificate revocation list (CRL) created and maintained by your private CA.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRevocationConfiguration :: Lens.Lens' CertificateAuthority (Core.Maybe Types.RevocationConfiguration)
caRevocationConfiguration = Lens.field @"revocationConfiguration"
{-# DEPRECATED caRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Serial number of your private CA.
--
-- /Note:/ Consider using 'serial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSerial :: Lens.Lens' CertificateAuthority (Core.Maybe Types.String)
caSerial = Lens.field @"serial"
{-# DEPRECATED caSerial "Use generic-lens or generic-optics with 'serial' instead." #-}

-- | Status of your private CA.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStatus :: Lens.Lens' CertificateAuthority (Core.Maybe Types.CertificateAuthorityStatus)
caStatus = Lens.field @"status"
{-# DEPRECATED caStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Type of your private CA.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CertificateAuthority (Core.Maybe Types.CertificateAuthorityType)
caType = Lens.field @"type'"
{-# DEPRECATED caType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON CertificateAuthority where
  parseJSON =
    Core.withObject "CertificateAuthority" Core.$
      \x ->
        CertificateAuthority'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CertificateAuthorityConfiguration")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "LastStateChangeAt")
          Core.<*> (x Core..:? "NotAfter")
          Core.<*> (x Core..:? "NotBefore")
          Core.<*> (x Core..:? "OwnerAccount")
          Core.<*> (x Core..:? "RestorableUntil")
          Core.<*> (x Core..:? "RevocationConfiguration")
          Core.<*> (x Core..:? "Serial")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Type")
