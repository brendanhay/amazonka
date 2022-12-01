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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CertificateAuthority where

import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityType
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityUsageMode
import Amazonka.CertificateManagerPCA.Types.FailureReason
import Amazonka.CertificateManagerPCA.Types.KeyStorageSecurityStandard
import Amazonka.CertificateManagerPCA.Types.RevocationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
-- action to import the signed certificate into Certificate Manager (ACM).
--
-- /See:/ 'newCertificateAuthority' smart constructor.
data CertificateAuthority = CertificateAuthority'
  { -- | Type of your private CA.
    type' :: Prelude.Maybe CertificateAuthorityType,
    -- | Defines a cryptographic key management compliance standard used for
    -- handling CA keys.
    --
    -- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
    --
    -- Note: Amazon Web Services Region ap-northeast-3 supports only
    -- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
    -- and value when creating a CA in that Region. Specifying a different
    -- value (or no value) results in an @InvalidArgsException@ with the
    -- message \"A certificate authority cannot be created in this region with
    -- the specified security standard.\"
    keyStorageSecurityStandard :: Prelude.Maybe KeyStorageSecurityStandard,
    -- | Specifies whether the CA issues general-purpose certificates that
    -- typically require a revocation mechanism, or short-lived certificates
    -- that may optionally omit revocation because they expire quickly.
    -- Short-lived certificate validity is limited to seven days.
    --
    -- The default value is GENERAL_PURPOSE.
    usageMode :: Prelude.Maybe CertificateAuthorityUsageMode,
    -- | Date and time at which your private CA was last updated.
    lastStateChangeAt :: Prelude.Maybe Core.POSIX,
    -- | Serial number of your private CA.
    serial :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) for your private certificate authority (CA).
    -- The format is @ 12345678-1234-1234-1234-123456789012 @.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Status of your private CA.
    status :: Prelude.Maybe CertificateAuthorityStatus,
    -- | Information about the Online Certificate Status Protocol (OCSP)
    -- configuration or certificate revocation list (CRL) created and
    -- maintained by your private CA.
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Your private CA configuration.
    certificateAuthorityConfiguration :: Prelude.Maybe CertificateAuthorityConfiguration,
    -- | Date and time before which your private CA certificate is not valid.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account ID that owns the certificate authority.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | Date and time after which your private CA certificate is not valid.
    notAfter :: Prelude.Maybe Core.POSIX,
    -- | Date and time at which your private CA was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Reason the request to create your private CA failed.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The period during which a deleted CA can be restored. For more
    -- information, see the @PermanentDeletionTimeInDays@ parameter of the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
    -- action.
    restorableUntil :: Prelude.Maybe Core.POSIX
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
-- 'type'', 'certificateAuthority_type' - Type of your private CA.
--
-- 'keyStorageSecurityStandard', 'certificateAuthority_keyStorageSecurityStandard' - Defines a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Note: Amazon Web Services Region ap-northeast-3 supports only
-- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
-- and value when creating a CA in that Region. Specifying a different
-- value (or no value) results in an @InvalidArgsException@ with the
-- message \"A certificate authority cannot be created in this region with
-- the specified security standard.\"
--
-- 'usageMode', 'certificateAuthority_usageMode' - Specifies whether the CA issues general-purpose certificates that
-- typically require a revocation mechanism, or short-lived certificates
-- that may optionally omit revocation because they expire quickly.
-- Short-lived certificate validity is limited to seven days.
--
-- The default value is GENERAL_PURPOSE.
--
-- 'lastStateChangeAt', 'certificateAuthority_lastStateChangeAt' - Date and time at which your private CA was last updated.
--
-- 'serial', 'certificateAuthority_serial' - Serial number of your private CA.
--
-- 'arn', 'certificateAuthority_arn' - Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
--
-- 'status', 'certificateAuthority_status' - Status of your private CA.
--
-- 'revocationConfiguration', 'certificateAuthority_revocationConfiguration' - Information about the Online Certificate Status Protocol (OCSP)
-- configuration or certificate revocation list (CRL) created and
-- maintained by your private CA.
--
-- 'certificateAuthorityConfiguration', 'certificateAuthority_certificateAuthorityConfiguration' - Your private CA configuration.
--
-- 'notBefore', 'certificateAuthority_notBefore' - Date and time before which your private CA certificate is not valid.
--
-- 'ownerAccount', 'certificateAuthority_ownerAccount' - The Amazon Web Services account ID that owns the certificate authority.
--
-- 'notAfter', 'certificateAuthority_notAfter' - Date and time after which your private CA certificate is not valid.
--
-- 'createdAt', 'certificateAuthority_createdAt' - Date and time at which your private CA was created.
--
-- 'failureReason', 'certificateAuthority_failureReason' - Reason the request to create your private CA failed.
--
-- 'restorableUntil', 'certificateAuthority_restorableUntil' - The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
newCertificateAuthority ::
  CertificateAuthority
newCertificateAuthority =
  CertificateAuthority'
    { type' = Prelude.Nothing,
      keyStorageSecurityStandard = Prelude.Nothing,
      usageMode = Prelude.Nothing,
      lastStateChangeAt = Prelude.Nothing,
      serial = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      revocationConfiguration = Prelude.Nothing,
      certificateAuthorityConfiguration = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      restorableUntil = Prelude.Nothing
    }

-- | Type of your private CA.
certificateAuthority_type :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityType)
certificateAuthority_type = Lens.lens (\CertificateAuthority' {type'} -> type') (\s@CertificateAuthority' {} a -> s {type' = a} :: CertificateAuthority)

-- | Defines a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Note: Amazon Web Services Region ap-northeast-3 supports only
-- FIPS_140_2_LEVEL_2_OR_HIGHER. You must explicitly specify this parameter
-- and value when creating a CA in that Region. Specifying a different
-- value (or no value) results in an @InvalidArgsException@ with the
-- message \"A certificate authority cannot be created in this region with
-- the specified security standard.\"
certificateAuthority_keyStorageSecurityStandard :: Lens.Lens' CertificateAuthority (Prelude.Maybe KeyStorageSecurityStandard)
certificateAuthority_keyStorageSecurityStandard = Lens.lens (\CertificateAuthority' {keyStorageSecurityStandard} -> keyStorageSecurityStandard) (\s@CertificateAuthority' {} a -> s {keyStorageSecurityStandard = a} :: CertificateAuthority)

-- | Specifies whether the CA issues general-purpose certificates that
-- typically require a revocation mechanism, or short-lived certificates
-- that may optionally omit revocation because they expire quickly.
-- Short-lived certificate validity is limited to seven days.
--
-- The default value is GENERAL_PURPOSE.
certificateAuthority_usageMode :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityUsageMode)
certificateAuthority_usageMode = Lens.lens (\CertificateAuthority' {usageMode} -> usageMode) (\s@CertificateAuthority' {} a -> s {usageMode = a} :: CertificateAuthority)

-- | Date and time at which your private CA was last updated.
certificateAuthority_lastStateChangeAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_lastStateChangeAt = Lens.lens (\CertificateAuthority' {lastStateChangeAt} -> lastStateChangeAt) (\s@CertificateAuthority' {} a -> s {lastStateChangeAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Serial number of your private CA.
certificateAuthority_serial :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_serial = Lens.lens (\CertificateAuthority' {serial} -> serial) (\s@CertificateAuthority' {} a -> s {serial = a} :: CertificateAuthority)

-- | Amazon Resource Name (ARN) for your private certificate authority (CA).
-- The format is @ 12345678-1234-1234-1234-123456789012 @.
certificateAuthority_arn :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_arn = Lens.lens (\CertificateAuthority' {arn} -> arn) (\s@CertificateAuthority' {} a -> s {arn = a} :: CertificateAuthority)

-- | Status of your private CA.
certificateAuthority_status :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityStatus)
certificateAuthority_status = Lens.lens (\CertificateAuthority' {status} -> status) (\s@CertificateAuthority' {} a -> s {status = a} :: CertificateAuthority)

-- | Information about the Online Certificate Status Protocol (OCSP)
-- configuration or certificate revocation list (CRL) created and
-- maintained by your private CA.
certificateAuthority_revocationConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe RevocationConfiguration)
certificateAuthority_revocationConfiguration = Lens.lens (\CertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CertificateAuthority' {} a -> s {revocationConfiguration = a} :: CertificateAuthority)

-- | Your private CA configuration.
certificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CertificateAuthority (Prelude.Maybe CertificateAuthorityConfiguration)
certificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CertificateAuthority)

-- | Date and time before which your private CA certificate is not valid.
certificateAuthority_notBefore :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notBefore = Lens.lens (\CertificateAuthority' {notBefore} -> notBefore) (\s@CertificateAuthority' {} a -> s {notBefore = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account ID that owns the certificate authority.
certificateAuthority_ownerAccount :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.Text)
certificateAuthority_ownerAccount = Lens.lens (\CertificateAuthority' {ownerAccount} -> ownerAccount) (\s@CertificateAuthority' {} a -> s {ownerAccount = a} :: CertificateAuthority)

-- | Date and time after which your private CA certificate is not valid.
certificateAuthority_notAfter :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_notAfter = Lens.lens (\CertificateAuthority' {notAfter} -> notAfter) (\s@CertificateAuthority' {} a -> s {notAfter = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Date and time at which your private CA was created.
certificateAuthority_createdAt :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_createdAt = Lens.lens (\CertificateAuthority' {createdAt} -> createdAt) (\s@CertificateAuthority' {} a -> s {createdAt = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

-- | Reason the request to create your private CA failed.
certificateAuthority_failureReason :: Lens.Lens' CertificateAuthority (Prelude.Maybe FailureReason)
certificateAuthority_failureReason = Lens.lens (\CertificateAuthority' {failureReason} -> failureReason) (\s@CertificateAuthority' {} a -> s {failureReason = a} :: CertificateAuthority)

-- | The period during which a deleted CA can be restored. For more
-- information, see the @PermanentDeletionTimeInDays@ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthorityRequest.html DeleteCertificateAuthorityRequest>
-- action.
certificateAuthority_restorableUntil :: Lens.Lens' CertificateAuthority (Prelude.Maybe Prelude.UTCTime)
certificateAuthority_restorableUntil = Lens.lens (\CertificateAuthority' {restorableUntil} -> restorableUntil) (\s@CertificateAuthority' {} a -> s {restorableUntil = a} :: CertificateAuthority) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CertificateAuthority where
  parseJSON =
    Core.withObject
      "CertificateAuthority"
      ( \x ->
          CertificateAuthority'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "KeyStorageSecurityStandard")
            Prelude.<*> (x Core..:? "UsageMode")
            Prelude.<*> (x Core..:? "LastStateChangeAt")
            Prelude.<*> (x Core..:? "Serial")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RevocationConfiguration")
            Prelude.<*> (x Core..:? "CertificateAuthorityConfiguration")
            Prelude.<*> (x Core..:? "NotBefore")
            Prelude.<*> (x Core..:? "OwnerAccount")
            Prelude.<*> (x Core..:? "NotAfter")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "RestorableUntil")
      )

instance Prelude.Hashable CertificateAuthority where
  hashWithSalt _salt CertificateAuthority' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` keyStorageSecurityStandard
      `Prelude.hashWithSalt` usageMode
      `Prelude.hashWithSalt` lastStateChangeAt
      `Prelude.hashWithSalt` serial
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` revocationConfiguration
      `Prelude.hashWithSalt` certificateAuthorityConfiguration
      `Prelude.hashWithSalt` notBefore
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` restorableUntil

instance Prelude.NFData CertificateAuthority where
  rnf CertificateAuthority' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf keyStorageSecurityStandard
      `Prelude.seq` Prelude.rnf usageMode
      `Prelude.seq` Prelude.rnf lastStateChangeAt
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf revocationConfiguration
      `Prelude.seq` Prelude.rnf certificateAuthorityConfiguration
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf restorableUntil
