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
-- Module      : Amazonka.AppStream.Types.CertificateBasedAuthProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.CertificateBasedAuthProperties where

import Amazonka.AppStream.Types.CertificateBasedAuthStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The certificate-based authentication properties used to authenticate
-- SAML 2.0 Identity Provider (IdP) user identities to Active Directory
-- domain-joined streaming instances. Fallback is turned on by default when
-- certificate-based authentication is __Enabled__ . Fallback allows users
-- to log in using their AD domain password if certificate-based
-- authentication is unsuccessful, or to unlock a desktop lock screen.
-- __Enabled_no_directory_login_fallback__ enables certificate-based
-- authentication, but does not allow users to log in using their AD domain
-- password. Users will be disconnected to re-authenticate using
-- certificates.
--
-- /See:/ 'newCertificateBasedAuthProperties' smart constructor.
data CertificateBasedAuthProperties = CertificateBasedAuthProperties'
  { -- | The ARN of the AWS Certificate Manager Private CA resource.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the certificate-based authentication properties.
    status :: Prelude.Maybe CertificateBasedAuthStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateBasedAuthProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'certificateBasedAuthProperties_certificateAuthorityArn' - The ARN of the AWS Certificate Manager Private CA resource.
--
-- 'status', 'certificateBasedAuthProperties_status' - The status of the certificate-based authentication properties.
newCertificateBasedAuthProperties ::
  CertificateBasedAuthProperties
newCertificateBasedAuthProperties =
  CertificateBasedAuthProperties'
    { certificateAuthorityArn =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN of the AWS Certificate Manager Private CA resource.
certificateBasedAuthProperties_certificateAuthorityArn :: Lens.Lens' CertificateBasedAuthProperties (Prelude.Maybe Prelude.Text)
certificateBasedAuthProperties_certificateAuthorityArn = Lens.lens (\CertificateBasedAuthProperties' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CertificateBasedAuthProperties' {} a -> s {certificateAuthorityArn = a} :: CertificateBasedAuthProperties)

-- | The status of the certificate-based authentication properties.
certificateBasedAuthProperties_status :: Lens.Lens' CertificateBasedAuthProperties (Prelude.Maybe CertificateBasedAuthStatus)
certificateBasedAuthProperties_status = Lens.lens (\CertificateBasedAuthProperties' {status} -> status) (\s@CertificateBasedAuthProperties' {} a -> s {status = a} :: CertificateBasedAuthProperties)

instance Core.FromJSON CertificateBasedAuthProperties where
  parseJSON =
    Core.withObject
      "CertificateBasedAuthProperties"
      ( \x ->
          CertificateBasedAuthProperties'
            Prelude.<$> (x Core..:? "CertificateAuthorityArn")
            Prelude.<*> (x Core..:? "Status")
      )

instance
  Prelude.Hashable
    CertificateBasedAuthProperties
  where
  hashWithSalt
    _salt
    CertificateBasedAuthProperties' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArn
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    CertificateBasedAuthProperties
  where
  rnf CertificateBasedAuthProperties' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf status

instance Core.ToJSON CertificateBasedAuthProperties where
  toJSON CertificateBasedAuthProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CertificateAuthorityArn" Core..=)
              Prelude.<$> certificateAuthorityArn,
            ("Status" Core..=) Prelude.<$> status
          ]
      )
