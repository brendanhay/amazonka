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
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration where

import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Certificate revocation information used by the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- and
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority>
-- actions. Your private certificate authority (CA) can create and maintain
-- a certificate revocation list (CRL). A CRL contains information about
-- certificates revoked by your CA. For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate>.
--
-- /See:/ 'newRevocationConfiguration' smart constructor.
data RevocationConfiguration = RevocationConfiguration'
  { -- | Configuration of the certificate revocation list (CRL), if any,
    -- maintained by your private CA.
    crlConfiguration :: Prelude.Maybe CrlConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevocationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlConfiguration', 'revocationConfiguration_crlConfiguration' - Configuration of the certificate revocation list (CRL), if any,
-- maintained by your private CA.
newRevocationConfiguration ::
  RevocationConfiguration
newRevocationConfiguration =
  RevocationConfiguration'
    { crlConfiguration =
        Prelude.Nothing
    }

-- | Configuration of the certificate revocation list (CRL), if any,
-- maintained by your private CA.
revocationConfiguration_crlConfiguration :: Lens.Lens' RevocationConfiguration (Prelude.Maybe CrlConfiguration)
revocationConfiguration_crlConfiguration = Lens.lens (\RevocationConfiguration' {crlConfiguration} -> crlConfiguration) (\s@RevocationConfiguration' {} a -> s {crlConfiguration = a} :: RevocationConfiguration)

instance Prelude.FromJSON RevocationConfiguration where
  parseJSON =
    Prelude.withObject
      "RevocationConfiguration"
      ( \x ->
          RevocationConfiguration'
            Prelude.<$> (x Prelude..:? "CrlConfiguration")
      )

instance Prelude.Hashable RevocationConfiguration

instance Prelude.NFData RevocationConfiguration

instance Prelude.ToJSON RevocationConfiguration where
  toJSON RevocationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CrlConfiguration" Prelude..=)
              Prelude.<$> crlConfiguration
          ]
      )
