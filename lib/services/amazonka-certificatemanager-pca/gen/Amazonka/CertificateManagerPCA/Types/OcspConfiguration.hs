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
-- Module      : Amazonka.CertificateManagerPCA.Types.OcspConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.OcspConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information to enable and configure Online Certificate Status
-- Protocol (OCSP) for validating certificate revocation status.
--
-- When you revoke a certificate, OCSP responses may take up to 60 minutes
-- to reflect the new status.
--
-- /See:/ 'newOcspConfiguration' smart constructor.
data OcspConfiguration = OcspConfiguration'
  { -- | By default, Amazon Web Services Private CA injects an Amazon Web
    -- Services domain into certificates being validated by the Online
    -- Certificate Status Protocol (OCSP). A customer can alternatively use
    -- this object to define a CNAME specifying a customized OCSP domain.
    --
    -- The content of a Canonical Name (CNAME) record must conform to
    -- <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the use
    -- of special characters in URIs. Additionally, the value of the CNAME must
    -- not include a protocol prefix such as \"http:\/\/\" or \"https:\/\/\".
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/ocsp-customize.html Customizing Online Certificate Status Protocol (OCSP)>
    -- in the /Amazon Web Services Private Certificate Authority User Guide/.
    ocspCustomCname :: Prelude.Maybe Prelude.Text,
    -- | Flag enabling use of the Online Certificate Status Protocol (OCSP) for
    -- validating certificate revocation status.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OcspConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ocspCustomCname', 'ocspConfiguration_ocspCustomCname' - By default, Amazon Web Services Private CA injects an Amazon Web
-- Services domain into certificates being validated by the Online
-- Certificate Status Protocol (OCSP). A customer can alternatively use
-- this object to define a CNAME specifying a customized OCSP domain.
--
-- The content of a Canonical Name (CNAME) record must conform to
-- <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the use
-- of special characters in URIs. Additionally, the value of the CNAME must
-- not include a protocol prefix such as \"http:\/\/\" or \"https:\/\/\".
--
-- For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/ocsp-customize.html Customizing Online Certificate Status Protocol (OCSP)>
-- in the /Amazon Web Services Private Certificate Authority User Guide/.
--
-- 'enabled', 'ocspConfiguration_enabled' - Flag enabling use of the Online Certificate Status Protocol (OCSP) for
-- validating certificate revocation status.
newOcspConfiguration ::
  -- | 'enabled'
  Prelude.Bool ->
  OcspConfiguration
newOcspConfiguration pEnabled_ =
  OcspConfiguration'
    { ocspCustomCname =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | By default, Amazon Web Services Private CA injects an Amazon Web
-- Services domain into certificates being validated by the Online
-- Certificate Status Protocol (OCSP). A customer can alternatively use
-- this object to define a CNAME specifying a customized OCSP domain.
--
-- The content of a Canonical Name (CNAME) record must conform to
-- <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the use
-- of special characters in URIs. Additionally, the value of the CNAME must
-- not include a protocol prefix such as \"http:\/\/\" or \"https:\/\/\".
--
-- For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/ocsp-customize.html Customizing Online Certificate Status Protocol (OCSP)>
-- in the /Amazon Web Services Private Certificate Authority User Guide/.
ocspConfiguration_ocspCustomCname :: Lens.Lens' OcspConfiguration (Prelude.Maybe Prelude.Text)
ocspConfiguration_ocspCustomCname = Lens.lens (\OcspConfiguration' {ocspCustomCname} -> ocspCustomCname) (\s@OcspConfiguration' {} a -> s {ocspCustomCname = a} :: OcspConfiguration)

-- | Flag enabling use of the Online Certificate Status Protocol (OCSP) for
-- validating certificate revocation status.
ocspConfiguration_enabled :: Lens.Lens' OcspConfiguration Prelude.Bool
ocspConfiguration_enabled = Lens.lens (\OcspConfiguration' {enabled} -> enabled) (\s@OcspConfiguration' {} a -> s {enabled = a} :: OcspConfiguration)

instance Data.FromJSON OcspConfiguration where
  parseJSON =
    Data.withObject
      "OcspConfiguration"
      ( \x ->
          OcspConfiguration'
            Prelude.<$> (x Data..:? "OcspCustomCname")
            Prelude.<*> (x Data..: "Enabled")
      )

instance Prelude.Hashable OcspConfiguration where
  hashWithSalt _salt OcspConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` ocspCustomCname
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData OcspConfiguration where
  rnf OcspConfiguration' {..} =
    Prelude.rnf ocspCustomCname
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON OcspConfiguration where
  toJSON OcspConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OcspCustomCname" Data..=)
              Prelude.<$> ocspCustomCname,
            Prelude.Just ("Enabled" Data..= enabled)
          ]
      )
