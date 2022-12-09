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
-- Module      : Amazonka.CertificateManagerPCA.Types.RevocationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.RevocationConfiguration where

import Amazonka.CertificateManagerPCA.Types.CrlConfiguration
import Amazonka.CertificateManagerPCA.Types.OcspConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Certificate revocation information used by the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- and
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority>
-- actions. Your private certificate authority (CA) can configure Online
-- Certificate Status Protocol (OCSP) support and\/or maintain a
-- certificate revocation list (CRL). OCSP returns validation information
-- about certificates as requested by clients, and a CRL contains an
-- updated list of certificates revoked by your CA. For more information,
-- see
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate>
-- and
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/revocation-setup.html Setting up a certificate revocation method>
-- in the /Private Certificate Authority (PCA) User Guide/.
--
-- /See:/ 'newRevocationConfiguration' smart constructor.
data RevocationConfiguration = RevocationConfiguration'
  { -- | Configuration of the certificate revocation list (CRL), if any,
    -- maintained by your private CA. A CRL is typically updated approximately
    -- 30 minutes after a certificate is revoked. If for any reason a CRL
    -- update fails, ACM Private CA makes further attempts every 15 minutes.
    crlConfiguration :: Prelude.Maybe CrlConfiguration,
    -- | Configuration of Online Certificate Status Protocol (OCSP) support, if
    -- any, maintained by your private CA. When you revoke a certificate, OCSP
    -- responses may take up to 60 minutes to reflect the new status.
    ocspConfiguration :: Prelude.Maybe OcspConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevocationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlConfiguration', 'revocationConfiguration_crlConfiguration' - Configuration of the certificate revocation list (CRL), if any,
-- maintained by your private CA. A CRL is typically updated approximately
-- 30 minutes after a certificate is revoked. If for any reason a CRL
-- update fails, ACM Private CA makes further attempts every 15 minutes.
--
-- 'ocspConfiguration', 'revocationConfiguration_ocspConfiguration' - Configuration of Online Certificate Status Protocol (OCSP) support, if
-- any, maintained by your private CA. When you revoke a certificate, OCSP
-- responses may take up to 60 minutes to reflect the new status.
newRevocationConfiguration ::
  RevocationConfiguration
newRevocationConfiguration =
  RevocationConfiguration'
    { crlConfiguration =
        Prelude.Nothing,
      ocspConfiguration = Prelude.Nothing
    }

-- | Configuration of the certificate revocation list (CRL), if any,
-- maintained by your private CA. A CRL is typically updated approximately
-- 30 minutes after a certificate is revoked. If for any reason a CRL
-- update fails, ACM Private CA makes further attempts every 15 minutes.
revocationConfiguration_crlConfiguration :: Lens.Lens' RevocationConfiguration (Prelude.Maybe CrlConfiguration)
revocationConfiguration_crlConfiguration = Lens.lens (\RevocationConfiguration' {crlConfiguration} -> crlConfiguration) (\s@RevocationConfiguration' {} a -> s {crlConfiguration = a} :: RevocationConfiguration)

-- | Configuration of Online Certificate Status Protocol (OCSP) support, if
-- any, maintained by your private CA. When you revoke a certificate, OCSP
-- responses may take up to 60 minutes to reflect the new status.
revocationConfiguration_ocspConfiguration :: Lens.Lens' RevocationConfiguration (Prelude.Maybe OcspConfiguration)
revocationConfiguration_ocspConfiguration = Lens.lens (\RevocationConfiguration' {ocspConfiguration} -> ocspConfiguration) (\s@RevocationConfiguration' {} a -> s {ocspConfiguration = a} :: RevocationConfiguration)

instance Data.FromJSON RevocationConfiguration where
  parseJSON =
    Data.withObject
      "RevocationConfiguration"
      ( \x ->
          RevocationConfiguration'
            Prelude.<$> (x Data..:? "CrlConfiguration")
            Prelude.<*> (x Data..:? "OcspConfiguration")
      )

instance Prelude.Hashable RevocationConfiguration where
  hashWithSalt _salt RevocationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` crlConfiguration
      `Prelude.hashWithSalt` ocspConfiguration

instance Prelude.NFData RevocationConfiguration where
  rnf RevocationConfiguration' {..} =
    Prelude.rnf crlConfiguration
      `Prelude.seq` Prelude.rnf ocspConfiguration

instance Data.ToJSON RevocationConfiguration where
  toJSON RevocationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrlConfiguration" Data..=)
              Prelude.<$> crlConfiguration,
            ("OcspConfiguration" Data..=)
              Prelude.<$> ocspConfiguration
          ]
      )
