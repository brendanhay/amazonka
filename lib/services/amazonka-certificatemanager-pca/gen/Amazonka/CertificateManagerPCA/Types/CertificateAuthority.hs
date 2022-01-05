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
-- Module      : Amazonka.CertificateManagerPCA.Types.CertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CertificateAuthority where

import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityType
import Amazonka.CertificateManagerPCA.Types.FailureReason
import Amazonka.CertificateManagerPCA.Types.KeyStorageSecurityStandard
import Amazonka.CertificateManagerPCA.Types.RevocationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
    -- | Reason the request to create your private CA failed.
    failureReason :: Prelude.Maybe FailureReason,
    -- | Your private CA configuration.
    certificateAuthorityConfiguration :: Prelude.Maybe CertificateAuthorityConfiguration,
    -- | Amazon Resource Name (ARN) for your private certificate authority (CA).
    -- The format is @ 12345678-1234-1234-1234-123456789012 @.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Date and time at which your private CA was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Serial number of your private CA.
    serial :: Prelude.Maybe Prelude.Text,
    -- | Defines a cryptographic key management compliance standard used for
    -- handling CA keys.
    --
    -- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
    --
    -- Note: AWS Region ap-northeast-3 supports only
    -- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
    -- and value when creating a CA in that Region. Specifying a different
    -- value (or no value) results in an @InvalidArgsException@ with the
    -- message \"A certificate authority cannot be created in this region with
    -- the specified security standard.\"
    keyStorageSecurityStandard :: Prelude.Maybe KeyStorageSecurityStandard,
    -- | Date and time before which your private CA certificate is not valid.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The period during which a deleted CA can be restored. For more
    -- information, see the @PermanentDeletionTimeInDays@ parameter of the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
    -- action.
    restorableUntil :: Prelude.Maybe Core.POSIX,
    -- | Type of your private CA.
    type' :: Prelude.Maybe CertificateAuthorityType,
    -- | The AWS account ID that owns the certificate authority.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | Information about the Online Certificate Status Protocol (OCSP)
    -- configuration or certificate revocation list (CRL) created and
    -- maintained by your private CA.
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Date and time at which your private CA was last updated.
    lastStateChangeAt :: Prelude.Maybe Core.POSIX,
    -- | Date and time after which your private CA certificate is not valid.
    notAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'failureReason', 'certificateAuthority_failureReason' - Reason the request to create your private CA failed.
--
-- 'certificateAuthorityConfiguration', 'certificateAuthority_certificateAuthorityConfiguration' - Your private CA configuration.
--
-- 'arn', 'certificateAuthority_arn' - Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
--
-- 'createdAt', 'certificateAuthority_createdAt' - Date and time at which your private CA was created.
--
-- 'serial', 'certificateAuthority_serial' - Serial number of your private CA.
--
-- 'keyStorageSecurityStandard', 'certificateAuthority_keyStorageSecurityStandard' - Defines a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Note: AWS Region ap-northeast-3 supports only
-- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
-- and value when creating a CA in that Region. Specifying a different
-- value (or no value) results in an @InvalidArgsException@ with the
-- message \"A certificate authority cannot be created in this region with
-- the specified security standard.\"
--
-- 'notBefore', 'certificateAuthority_notBefore' - Date and time before which your private CA certificate is not valid.
--
-- 'restorableUntil', 'certificateAuthority_restorableUntil' - The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
--
-- 'type'', 'certificateAuthority_type' - Type of your private CA.
--
-- 'ownerAccount', 'certificateAuthority_ownerAccount' - The AWS account ID that owns the certificate authority.
--
-- 'revocationConfiguration', 'certificateAuthority_revocationConfiguration' - Information about the Online Certificate Status Protocol (OCSP)
-- configuration or certificate revocation list (CRL) created and
-- maintained by your private CA.
--
-- 'lastStateChangeAt', 'certificateAuthority_lastStateChangeAt' - Date and time at which your private CA was last updated.
--
-- 'notAfter', 'certificateAuthority_notAfter' - Date and time after which your private CA certificate is not valid.
newCertificateAuthority ::
  CertificateAuthority
newCertificateAuthority =
  CertificateAuthority'
    { status = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      certificateAuthorityConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      serial = Prelude.Nothing,
      keyStorageSecurityStandard = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      restorableUntil = Prelude.Nothing,
      type' = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      revocationConfiguration = Prelude.Nothing,
      lastStateChangeAt = Prelude.Nothing,
      notAfter = Prelude.Nothing
    }

-- | Status of your private CA.
certificateAuthority_status :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityStatus)
certificateAuthority_status = Lens.lens (\CertificateAuthority' {status} -> status) (\s@CertificateAuthority' {} a -> s {status = a} :: CertificateAuthority)

-- | Reason the request to create your private CA failed.
certificateAuthority_failureReason :: Lens.Lens' CertificateAuthority (Prelude.Maybe FailureReason)
certificateAuthority_failureReason = Lens.lens (\CertificateAuthority' {failureReason} -> failureReason) (\s@CertificateAuthority' {} a -> s {failureReason = a} :: CertificateAuthority)

-- | Your private CA configuration.
certificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityConfiguration)
certificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CertificateAuthority)

-- | Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
certificateAuthority_arn :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_arn = Lens.lens (\CertificateAuthority' {arn} -> arn) (\s@CertificateAuthority' {} a -> s {arn = a} :: CertificateAuthority)

-- | Date and time at which your private CA was created.
certificateAuthority_createdAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_createdAt = Lens.lens (\CertificateAuthority' {createdAt} -> createdAt) (\s@CertificateAuthority' {} a -> s {createdAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Serial number of your private CA.
certificateAuthority_serial :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_serial = Lens.lens (\CertificateAuthority' {serial} -> serial) (\s@CertificateAuthority' {} a -> s {serial = a} :: CertificateAuthority)

-- | Defines a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Note: AWS Region ap-northeast-3 supports only
-- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
-- and value when creating a CA in that Region. Specifying a different
-- value (or no value) results in an @InvalidArgsException@ with the
-- message \"A certificate authority cannot be created in this region with
-- the specified security standard.\"
certificateAuthority_keyStorageSecurityStandard :: Lens.Lens' CertificateAuthority (Prelude.Maybe KeyStorageSecurityStandard)
certificateAuthority_keyStorageSecurityStandard = Lens.lens (\CertificateAuthority' {keyStorageSecurityStandard} -> keyStorageSecurityStandard) (\s@CertificateAuthority' {} a -> s {keyStorageSecurityStandard = a} :: CertificateAuthority)

-- | Date and time before which your private CA certificate is not valid.
certificateAuthority_notBefore :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notBefore = Lens.lens (\CertificateAuthority' {notBefore} -> notBefore) (\s@CertificateAuthority' {} a -> s {notBefore = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
certificateAuthority_restorableUntil :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_restorableUntil = Lens.lens (\CertificateAuthority' {restorableUntil} -> restorableUntil) (\s@CertificateAuthority' {} a -> s {restorableUntil = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Type of your private CA.
certificateAuthority_type :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityType)
certificateAuthority_type = Lens.lens (\CertificateAuthority' {type'} -> type') (\s@CertificateAuthority' {} a -> s {type' = a} :: CertificateAuthority)

-- | The AWS account ID that owns the certificate authority.
certificateAuthority_ownerAccount :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_ownerAccount = Lens.lens (\CertificateAuthority' {ownerAccount} -> ownerAccount) (\s@CertificateAuthority' {} a -> s {ownerAccount = a} :: CertificateAuthority)

-- | Information about the Online Certificate Status Protocol (OCSP)
-- configuration or certificate revocation list (CRL) created and
-- maintained by your private CA.
certificateAuthority_revocationConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe RevocationConfiguration)
certificateAuthority_revocationConfiguration = Lens.lens (\CertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CertificateAuthority' {} a -> s {revocationConfiguration = a} :: CertificateAuthority)

-- | Date and time at which your private CA was last updated.
certificateAuthority_lastStateChangeAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_lastStateChangeAt = Lens.lens (\CertificateAuthority' {lastStateChangeAt} -> lastStateChangeAt) (\s@CertificateAuthority' {} a -> s {lastStateChangeAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Date and time after which your private CA certificate is not valid.
certificateAuthority_notAfter :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notAfter = Lens.lens (\CertificateAuthority' {notAfter} -> notAfter) (\s@CertificateAuthority' {} a -> s {notAfter = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CertificateAuthority where
  parseJSON =
    Core.withObject
      "CertificateAuthority"
      ( \x ->
          CertificateAuthority'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "CertificateAuthorityConfiguration")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Serial")
            Prelude.<*> (x Core..:? "KeyStorageSecurityStandard")
            Prelude.<*> (x Core..:? "NotBefore")
            Prelude.<*> (x Core..:? "RestorableUntil")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "OwnerAccount")
            Prelude.<*> (x Core..:? "RevocationConfiguration")
            Prelude.<*> (x Core..:? "LastStateChangeAt")
            Prelude.<*> (x Core..:? "NotAfter")
      )

instance Prelude.Hashable CertificateAuthority where
  hashWithSalt _salt CertificateAuthority' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` certificateAuthorityConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` serial
      `Prelude.hashWithSalt` keyStorageSecurityStandard
      `Prelude.hashWithSalt` notBefore
      `Prelude.hashWithSalt` restorableUntil
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` revocationConfiguration
      `Prelude.hashWithSalt` lastStateChangeAt
      `Prelude.hashWithSalt` notAfter

instance Prelude.NFData CertificateAuthority where
  rnf CertificateAuthority' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf certificateAuthorityConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf keyStorageSecurityStandard
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf restorableUntil
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf revocationConfiguration
      `Prelude.seq` Prelude.rnf lastStateChangeAt
      `Prelude.seq` Prelude.rnf notAfter
