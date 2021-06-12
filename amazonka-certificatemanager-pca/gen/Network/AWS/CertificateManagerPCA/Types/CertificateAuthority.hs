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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    status :: Core.Maybe CertificateAuthorityStatus,
    -- | Date and time before which your private CA certificate is not valid.
    notBefore :: Core.Maybe Core.POSIX,
    -- | Information about the certificate revocation list (CRL) created and
    -- maintained by your private CA.
    revocationConfiguration :: Core.Maybe RevocationConfiguration,
    -- | Serial number of your private CA.
    serial :: Core.Maybe Core.Text,
    -- | Amazon Resource Name (ARN) for your private certificate authority (CA).
    -- The format is @ 12345678-1234-1234-1234-123456789012 @.
    arn :: Core.Maybe Core.Text,
    -- | Date and time at which your private CA was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | Your private CA configuration.
    certificateAuthorityConfiguration :: Core.Maybe CertificateAuthorityConfiguration,
    -- | Reason the request to create your private CA failed.
    failureReason :: Core.Maybe FailureReason,
    -- | Date and time after which your private CA certificate is not valid.
    notAfter :: Core.Maybe Core.POSIX,
    -- | Date and time at which your private CA was last updated.
    lastStateChangeAt :: Core.Maybe Core.POSIX,
    -- | Type of your private CA.
    type' :: Core.Maybe CertificateAuthorityType,
    -- | The AWS account ID that owns the certificate authority.
    ownerAccount :: Core.Maybe Core.Text,
    -- | The period during which a deleted CA can be restored. For more
    -- information, see the @PermanentDeletionTimeInDays@ parameter of the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
    -- action.
    restorableUntil :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      notBefore = Core.Nothing,
      revocationConfiguration = Core.Nothing,
      serial = Core.Nothing,
      arn = Core.Nothing,
      createdAt = Core.Nothing,
      certificateAuthorityConfiguration = Core.Nothing,
      failureReason = Core.Nothing,
      notAfter = Core.Nothing,
      lastStateChangeAt = Core.Nothing,
      type' = Core.Nothing,
      ownerAccount = Core.Nothing,
      restorableUntil = Core.Nothing
    }

-- | Status of your private CA.
certificateAuthority_status :: Lens.Lens' CertificateAuthority (Core.Maybe CertificateAuthorityStatus)
certificateAuthority_status = Lens.lens (\CertificateAuthority' {status} -> status) (\s@CertificateAuthority' {} a -> s {status = a} :: CertificateAuthority)

-- | Date and time before which your private CA certificate is not valid.
certificateAuthority_notBefore :: Lens.Lens' CertificateAuthority (Core.Maybe Core.UTCTime)
certificateAuthority_notBefore = Lens.lens (\CertificateAuthority' {notBefore} -> notBefore) (\s@CertificateAuthority' {} a -> s {notBefore = a} :: CertificateAuthority) Core.. Lens.mapping Core._Time

-- | Information about the certificate revocation list (CRL) created and
-- maintained by your private CA.
certificateAuthority_revocationConfiguration :: Lens.Lens' CertificateAuthority (Core.Maybe RevocationConfiguration)
certificateAuthority_revocationConfiguration = Lens.lens (\CertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CertificateAuthority' {} a -> s {revocationConfiguration = a} :: CertificateAuthority)

-- | Serial number of your private CA.
certificateAuthority_serial :: Lens.Lens' CertificateAuthority (Core.Maybe Core.Text)
certificateAuthority_serial = Lens.lens (\CertificateAuthority' {serial} -> serial) (\s@CertificateAuthority' {} a -> s {serial = a} :: CertificateAuthority)

-- | Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
certificateAuthority_arn :: Lens.Lens' CertificateAuthority (Core.Maybe Core.Text)
certificateAuthority_arn = Lens.lens (\CertificateAuthority' {arn} -> arn) (\s@CertificateAuthority' {} a -> s {arn = a} :: CertificateAuthority)

-- | Date and time at which your private CA was created.
certificateAuthority_createdAt :: Lens.Lens' CertificateAuthority (Core.Maybe Core.UTCTime)
certificateAuthority_createdAt = Lens.lens (\CertificateAuthority' {createdAt} -> createdAt) (\s@CertificateAuthority' {} a -> s {createdAt = a} :: CertificateAuthority) Core.. Lens.mapping Core._Time

-- | Your private CA configuration.
certificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Core.Maybe CertificateAuthorityConfiguration)
certificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CertificateAuthority)

-- | Reason the request to create your private CA failed.
certificateAuthority_failureReason :: Lens.Lens' CertificateAuthority (Core.Maybe FailureReason)
certificateAuthority_failureReason = Lens.lens (\CertificateAuthority' {failureReason} -> failureReason) (\s@CertificateAuthority' {} a -> s {failureReason = a} :: CertificateAuthority)

-- | Date and time after which your private CA certificate is not valid.
certificateAuthority_notAfter :: Lens.Lens' CertificateAuthority (Core.Maybe Core.UTCTime)
certificateAuthority_notAfter = Lens.lens (\CertificateAuthority' {notAfter} -> notAfter) (\s@CertificateAuthority' {} a -> s {notAfter = a} :: CertificateAuthority) Core.. Lens.mapping Core._Time

-- | Date and time at which your private CA was last updated.
certificateAuthority_lastStateChangeAt :: Lens.Lens' CertificateAuthority (Core.Maybe Core.UTCTime)
certificateAuthority_lastStateChangeAt = Lens.lens (\CertificateAuthority' {lastStateChangeAt} -> lastStateChangeAt) (\s@CertificateAuthority' {} a -> s {lastStateChangeAt = a} :: CertificateAuthority) Core.. Lens.mapping Core._Time

-- | Type of your private CA.
certificateAuthority_type :: Lens.Lens' CertificateAuthority (Core.Maybe CertificateAuthorityType)
certificateAuthority_type = Lens.lens (\CertificateAuthority' {type'} -> type') (\s@CertificateAuthority' {} a -> s {type' = a} :: CertificateAuthority)

-- | The AWS account ID that owns the certificate authority.
certificateAuthority_ownerAccount :: Lens.Lens' CertificateAuthority (Core.Maybe Core.Text)
certificateAuthority_ownerAccount = Lens.lens (\CertificateAuthority' {ownerAccount} -> ownerAccount) (\s@CertificateAuthority' {} a -> s {ownerAccount = a} :: CertificateAuthority)

-- | The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
certificateAuthority_restorableUntil :: Lens.Lens' CertificateAuthority (Core.Maybe Core.UTCTime)
certificateAuthority_restorableUntil = Lens.lens (\CertificateAuthority' {restorableUntil} -> restorableUntil) (\s@CertificateAuthority' {} a -> s {restorableUntil = a} :: CertificateAuthority) Core.. Lens.mapping Core._Time

instance Core.FromJSON CertificateAuthority where
  parseJSON =
    Core.withObject
      "CertificateAuthority"
      ( \x ->
          CertificateAuthority'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "NotBefore")
            Core.<*> (x Core..:? "RevocationConfiguration")
            Core.<*> (x Core..:? "Serial")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "CertificateAuthorityConfiguration")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "NotAfter")
            Core.<*> (x Core..:? "LastStateChangeAt")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "OwnerAccount")
            Core.<*> (x Core..:? "RestorableUntil")
      )

instance Core.Hashable CertificateAuthority

instance Core.NFData CertificateAuthority
