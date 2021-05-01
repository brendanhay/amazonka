{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthority where

import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
import Network.AWS.CertificateManagerPCA.Types.FailureReason
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about your private certificate authority (CA). Your
-- private CA can issue and revoke X.509 digital certificates. Digital
-- certificates verify that the entity named in the certificate __Subject__
-- field owns or controls the public key contained in the __Subject Public
-- Key Info__ field. Call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action to create your private CA. You must then call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCertificate.html GetCertificateAuthorityCertificate>
-- action to retrieve a private CA certificate signing request (CSR). Sign
-- the CSR with your ACM Private CA-hosted or on-premises root or
-- subordinate CA certificate. Call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate>
-- action to import the signed certificate into AWS Certificate Manager
-- (ACM).
--
-- /See:/ 'newCertificateAuthority' smart constructor.
data CertificateAuthority = CertificateAuthority'
  { -- | Status of your private CA.
    status :: Prelude.Maybe CertificateAuthorityStatus,
    -- | Date and time before which your private CA certificate is not valid.
    notBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Information about the certificate revocation list (CRL) created and
    -- maintained by your private CA.
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Serial number of your private CA.
    serial :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) for your private certificate authority (CA).
    -- The format is @ 12345678-1234-1234-1234-123456789012 @.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Date and time at which your private CA was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | Your private CA configuration.
    certificateAuthorityConfiguration :: Prelude.Maybe CertificateAuthorityConfiguration,
    -- | Reason the request to create your private CA failed.
    failureReason :: Prelude.Maybe FailureReason,
    -- | Date and time after which your private CA certificate is not valid.
    notAfter :: Prelude.Maybe Prelude.POSIX,
    -- | Date and time at which your private CA was last updated.
    lastStateChangeAt :: Prelude.Maybe Prelude.POSIX,
    -- | Type of your private CA.
    type' :: Prelude.Maybe CertificateAuthorityType,
    -- | The AWS account ID that owns the certificate authority.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The period during which a deleted CA can be restored. For more
    -- information, see the @PermanentDeletionTimeInDays@ parameter of the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
    -- action.
    restorableUntil :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'certificateAuthority_status' - Status of your private CA.
--
-- 'notBefore', 'certificateAuthority_notBefore' - Date and time before which your private CA certificate is not valid.
--
-- 'revocationConfiguration', 'certificateAuthority_revocationConfiguration' - Information about the certificate revocation list (CRL) created and
-- maintained by your private CA.
--
-- 'serial', 'certificateAuthority_serial' - Serial number of your private CA.
--
-- 'arn', 'certificateAuthority_arn' - Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
--
-- 'createdAt', 'certificateAuthority_createdAt' - Date and time at which your private CA was created.
--
-- 'certificateAuthorityConfiguration', 'certificateAuthority_certificateAuthorityConfiguration' - Your private CA configuration.
--
-- 'failureReason', 'certificateAuthority_failureReason' - Reason the request to create your private CA failed.
--
-- 'notAfter', 'certificateAuthority_notAfter' - Date and time after which your private CA certificate is not valid.
--
-- 'lastStateChangeAt', 'certificateAuthority_lastStateChangeAt' - Date and time at which your private CA was last updated.
--
-- 'type'', 'certificateAuthority_type' - Type of your private CA.
--
-- 'ownerAccount', 'certificateAuthority_ownerAccount' - The AWS account ID that owns the certificate authority.
--
-- 'restorableUntil', 'certificateAuthority_restorableUntil' - The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
newCertificateAuthority ::
  CertificateAuthority
newCertificateAuthority =
  CertificateAuthority'
    { status = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      revocationConfiguration = Prelude.Nothing,
      serial = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      certificateAuthorityConfiguration = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      lastStateChangeAt = Prelude.Nothing,
      type' = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      restorableUntil = Prelude.Nothing
    }

-- | Status of your private CA.
certificateAuthority_status :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityStatus)
certificateAuthority_status = Lens.lens (\CertificateAuthority' {status} -> status) (\s@CertificateAuthority' {} a -> s {status = a} :: CertificateAuthority)

-- | Date and time before which your private CA certificate is not valid.
certificateAuthority_notBefore :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notBefore = Lens.lens (\CertificateAuthority' {notBefore} -> notBefore) (\s@CertificateAuthority' {} a -> s {notBefore = a} :: CertificateAuthority) Prelude.. Lens.mapping Prelude._Time

-- | Information about the certificate revocation list (CRL) created and
-- maintained by your private CA.
certificateAuthority_revocationConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe RevocationConfiguration)
certificateAuthority_revocationConfiguration = Lens.lens (\CertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CertificateAuthority' {} a -> s {revocationConfiguration = a} :: CertificateAuthority)

-- | Serial number of your private CA.
certificateAuthority_serial :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_serial = Lens.lens (\CertificateAuthority' {serial} -> serial) (\s@CertificateAuthority' {} a -> s {serial = a} :: CertificateAuthority)

-- | Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
certificateAuthority_arn :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_arn = Lens.lens (\CertificateAuthority' {arn} -> arn) (\s@CertificateAuthority' {} a -> s {arn = a} :: CertificateAuthority)

-- | Date and time at which your private CA was created.
certificateAuthority_createdAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_createdAt = Lens.lens (\CertificateAuthority' {createdAt} -> createdAt) (\s@CertificateAuthority' {} a -> s {createdAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Prelude._Time

-- | Your private CA configuration.
certificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityConfiguration)
certificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CertificateAuthority)

-- | Reason the request to create your private CA failed.
certificateAuthority_failureReason :: Lens.Lens' CertificateAuthority (Prelude.Maybe FailureReason)
certificateAuthority_failureReason = Lens.lens (\CertificateAuthority' {failureReason} -> failureReason) (\s@CertificateAuthority' {} a -> s {failureReason = a} :: CertificateAuthority)

-- | Date and time after which your private CA certificate is not valid.
certificateAuthority_notAfter :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notAfter = Lens.lens (\CertificateAuthority' {notAfter} -> notAfter) (\s@CertificateAuthority' {} a -> s {notAfter = a} :: CertificateAuthority) Prelude.. Lens.mapping Prelude._Time

-- | Date and time at which your private CA was last updated.
certificateAuthority_lastStateChangeAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_lastStateChangeAt = Lens.lens (\CertificateAuthority' {lastStateChangeAt} -> lastStateChangeAt) (\s@CertificateAuthority' {} a -> s {lastStateChangeAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Prelude._Time

-- | Type of your private CA.
certificateAuthority_type :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityType)
certificateAuthority_type = Lens.lens (\CertificateAuthority' {type'} -> type') (\s@CertificateAuthority' {} a -> s {type' = a} :: CertificateAuthority)

-- | The AWS account ID that owns the certificate authority.
certificateAuthority_ownerAccount :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_ownerAccount = Lens.lens (\CertificateAuthority' {ownerAccount} -> ownerAccount) (\s@CertificateAuthority' {} a -> s {ownerAccount = a} :: CertificateAuthority)

-- | The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
certificateAuthority_restorableUntil :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_restorableUntil = Lens.lens (\CertificateAuthority' {restorableUntil} -> restorableUntil) (\s@CertificateAuthority' {} a -> s {restorableUntil = a} :: CertificateAuthority) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON CertificateAuthority where
  parseJSON =
    Prelude.withObject
      "CertificateAuthority"
      ( \x ->
          CertificateAuthority'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "NotBefore")
            Prelude.<*> (x Prelude..:? "RevocationConfiguration")
            Prelude.<*> (x Prelude..:? "Serial")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "CertificateAuthorityConfiguration")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "NotAfter")
            Prelude.<*> (x Prelude..:? "LastStateChangeAt")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "OwnerAccount")
            Prelude.<*> (x Prelude..:? "RestorableUntil")
      )

instance Prelude.Hashable CertificateAuthority

instance Prelude.NFData CertificateAuthority
