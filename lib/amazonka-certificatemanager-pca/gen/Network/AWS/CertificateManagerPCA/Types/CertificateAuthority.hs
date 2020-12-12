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
    caStatus,
    caFailureReason,
    caCertificateAuthorityConfiguration,
    caARN,
    caCreatedAt,
    caSerial,
    caNotBefore,
    caRestorableUntil,
    caType,
    caOwnerAccount,
    caRevocationConfiguration,
    caLastStateChangeAt,
    caNotAfter,
  )
where

import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
import Network.AWS.CertificateManagerPCA.Types.FailureReason
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about your private certificate authority (CA). Your private CA can issue and revoke X.509 digital certificates. Digital certificates verify that the entity named in the certificate __Subject__ field owns or controls the public key contained in the __Subject Public Key Info__ field. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action to create your private CA. You must then call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCertificate.html GetCertificateAuthorityCertificate> action to retrieve a private CA certificate signing request (CSR). Sign the CSR with your ACM Private CA-hosted or on-premises root or subordinate CA certificate. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action to import the signed certificate into AWS Certificate Manager (ACM).
--
-- /See:/ 'mkCertificateAuthority' smart constructor.
data CertificateAuthority = CertificateAuthority'
  { status ::
      Lude.Maybe CertificateAuthorityStatus,
    failureReason :: Lude.Maybe FailureReason,
    certificateAuthorityConfiguration ::
      Lude.Maybe CertificateAuthorityConfiguration,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    serial :: Lude.Maybe Lude.Text,
    notBefore :: Lude.Maybe Lude.Timestamp,
    restorableUntil :: Lude.Maybe Lude.Timestamp,
    type' :: Lude.Maybe CertificateAuthorityType,
    ownerAccount :: Lude.Maybe Lude.Text,
    revocationConfiguration ::
      Lude.Maybe RevocationConfiguration,
    lastStateChangeAt :: Lude.Maybe Lude.Timestamp,
    notAfter :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateAuthority' with the minimum fields required to make a request.
--
-- * 'arn' - Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
-- * 'certificateAuthorityConfiguration' - Your private CA configuration.
-- * 'createdAt' - Date and time at which your private CA was created.
-- * 'failureReason' - Reason the request to create your private CA failed.
-- * 'lastStateChangeAt' - Date and time at which your private CA was last updated.
-- * 'notAfter' - Date and time after which your private CA certificate is not valid.
-- * 'notBefore' - Date and time before which your private CA certificate is not valid.
-- * 'ownerAccount' - The AWS account ID that owns the certificate authority.
-- * 'restorableUntil' - The period during which a deleted CA can be restored. For more information, see the @PermanentDeletionTimeInDays@ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest> action.
-- * 'revocationConfiguration' - Information about the certificate revocation list (CRL) created and maintained by your private CA.
-- * 'serial' - Serial number of your private CA.
-- * 'status' - Status of your private CA.
-- * 'type'' - Type of your private CA.
mkCertificateAuthority ::
  CertificateAuthority
mkCertificateAuthority =
  CertificateAuthority'
    { status = Lude.Nothing,
      failureReason = Lude.Nothing,
      certificateAuthorityConfiguration = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      serial = Lude.Nothing,
      notBefore = Lude.Nothing,
      restorableUntil = Lude.Nothing,
      type' = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      revocationConfiguration = Lude.Nothing,
      lastStateChangeAt = Lude.Nothing,
      notAfter = Lude.Nothing
    }

-- | Status of your private CA.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStatus :: Lens.Lens' CertificateAuthority (Lude.Maybe CertificateAuthorityStatus)
caStatus = Lens.lens (status :: CertificateAuthority -> Lude.Maybe CertificateAuthorityStatus) (\s a -> s {status = a} :: CertificateAuthority)
{-# DEPRECATED caStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Reason the request to create your private CA failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caFailureReason :: Lens.Lens' CertificateAuthority (Lude.Maybe FailureReason)
caFailureReason = Lens.lens (failureReason :: CertificateAuthority -> Lude.Maybe FailureReason) (\s a -> s {failureReason = a} :: CertificateAuthority)
{-# DEPRECATED caFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Your private CA configuration.
--
-- /Note:/ Consider using 'certificateAuthorityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCertificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Lude.Maybe CertificateAuthorityConfiguration)
caCertificateAuthorityConfiguration = Lens.lens (certificateAuthorityConfiguration :: CertificateAuthority -> Lude.Maybe CertificateAuthorityConfiguration) (\s a -> s {certificateAuthorityConfiguration = a} :: CertificateAuthority)
{-# DEPRECATED caCertificateAuthorityConfiguration "Use generic-lens or generic-optics with 'certificateAuthorityConfiguration' instead." #-}

-- | Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caARN :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Text)
caARN = Lens.lens (arn :: CertificateAuthority -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CertificateAuthority)
{-# DEPRECATED caARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Date and time at which your private CA was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreatedAt :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Timestamp)
caCreatedAt = Lens.lens (createdAt :: CertificateAuthority -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: CertificateAuthority)
{-# DEPRECATED caCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Serial number of your private CA.
--
-- /Note:/ Consider using 'serial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSerial :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Text)
caSerial = Lens.lens (serial :: CertificateAuthority -> Lude.Maybe Lude.Text) (\s a -> s {serial = a} :: CertificateAuthority)
{-# DEPRECATED caSerial "Use generic-lens or generic-optics with 'serial' instead." #-}

-- | Date and time before which your private CA certificate is not valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNotBefore :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Timestamp)
caNotBefore = Lens.lens (notBefore :: CertificateAuthority -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: CertificateAuthority)
{-# DEPRECATED caNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The period during which a deleted CA can be restored. For more information, see the @PermanentDeletionTimeInDays@ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest> action.
--
-- /Note:/ Consider using 'restorableUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRestorableUntil :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Timestamp)
caRestorableUntil = Lens.lens (restorableUntil :: CertificateAuthority -> Lude.Maybe Lude.Timestamp) (\s a -> s {restorableUntil = a} :: CertificateAuthority)
{-# DEPRECATED caRestorableUntil "Use generic-lens or generic-optics with 'restorableUntil' instead." #-}

-- | Type of your private CA.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CertificateAuthority (Lude.Maybe CertificateAuthorityType)
caType = Lens.lens (type' :: CertificateAuthority -> Lude.Maybe CertificateAuthorityType) (\s a -> s {type' = a} :: CertificateAuthority)
{-# DEPRECATED caType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The AWS account ID that owns the certificate authority.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOwnerAccount :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Text)
caOwnerAccount = Lens.lens (ownerAccount :: CertificateAuthority -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: CertificateAuthority)
{-# DEPRECATED caOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the certificate revocation list (CRL) created and maintained by your private CA.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRevocationConfiguration :: Lens.Lens' CertificateAuthority (Lude.Maybe RevocationConfiguration)
caRevocationConfiguration = Lens.lens (revocationConfiguration :: CertificateAuthority -> Lude.Maybe RevocationConfiguration) (\s a -> s {revocationConfiguration = a} :: CertificateAuthority)
{-# DEPRECATED caRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Date and time at which your private CA was last updated.
--
-- /Note:/ Consider using 'lastStateChangeAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLastStateChangeAt :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Timestamp)
caLastStateChangeAt = Lens.lens (lastStateChangeAt :: CertificateAuthority -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStateChangeAt = a} :: CertificateAuthority)
{-# DEPRECATED caLastStateChangeAt "Use generic-lens or generic-optics with 'lastStateChangeAt' instead." #-}

-- | Date and time after which your private CA certificate is not valid.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNotAfter :: Lens.Lens' CertificateAuthority (Lude.Maybe Lude.Timestamp)
caNotAfter = Lens.lens (notAfter :: CertificateAuthority -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: CertificateAuthority)
{-# DEPRECATED caNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

instance Lude.FromJSON CertificateAuthority where
  parseJSON =
    Lude.withObject
      "CertificateAuthority"
      ( \x ->
          CertificateAuthority'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "CertificateAuthorityConfiguration")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Serial")
            Lude.<*> (x Lude..:? "NotBefore")
            Lude.<*> (x Lude..:? "RestorableUntil")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "OwnerAccount")
            Lude.<*> (x Lude..:? "RevocationConfiguration")
            Lude.<*> (x Lude..:? "LastStateChangeAt")
            Lude.<*> (x Lude..:? "NotAfter")
      )
