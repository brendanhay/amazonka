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
-- Module      : Amazonka.CertificateManager.Types.ExtendedKeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.ExtendedKeyUsage where

import Amazonka.CertificateManager.Types.ExtendedKeyUsageName
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Extended Key Usage X.509 v3 extension defines one or more purposes
-- for which the public key can be used. This is in addition to or in place
-- of the basic purposes specified by the Key Usage extension.
--
-- /See:/ 'newExtendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { -- | An object identifier (OID) for the extension value. OIDs are strings of
    -- numbers separated by periods. The following OIDs are defined in RFC 3280
    -- and RFC 5280.
    --
    -- -   @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@
    --
    -- -   @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@
    --
    -- -   @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@
    --
    -- -   @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@
    --
    -- -   @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@
    --
    -- -   @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@
    --
    -- -   @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@
    --
    -- -   @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@
    --
    -- -   @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
    oid :: Prelude.Maybe Prelude.Text,
    -- | The name of an Extended Key Usage value.
    name :: Prelude.Maybe ExtendedKeyUsageName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oid', 'extendedKeyUsage_oid' - An object identifier (OID) for the extension value. OIDs are strings of
-- numbers separated by periods. The following OIDs are defined in RFC 3280
-- and RFC 5280.
--
-- -   @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@
--
-- -   @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@
--
-- -   @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@
--
-- -   @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@
--
-- -   @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@
--
-- -   @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@
--
-- -   @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@
--
-- -   @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@
--
-- -   @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
--
-- 'name', 'extendedKeyUsage_name' - The name of an Extended Key Usage value.
newExtendedKeyUsage ::
  ExtendedKeyUsage
newExtendedKeyUsage =
  ExtendedKeyUsage'
    { oid = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An object identifier (OID) for the extension value. OIDs are strings of
-- numbers separated by periods. The following OIDs are defined in RFC 3280
-- and RFC 5280.
--
-- -   @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@
--
-- -   @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@
--
-- -   @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@
--
-- -   @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@
--
-- -   @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@
--
-- -   @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@
--
-- -   @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@
--
-- -   @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@
--
-- -   @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
extendedKeyUsage_oid :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe Prelude.Text)
extendedKeyUsage_oid = Lens.lens (\ExtendedKeyUsage' {oid} -> oid) (\s@ExtendedKeyUsage' {} a -> s {oid = a} :: ExtendedKeyUsage)

-- | The name of an Extended Key Usage value.
extendedKeyUsage_name :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe ExtendedKeyUsageName)
extendedKeyUsage_name = Lens.lens (\ExtendedKeyUsage' {name} -> name) (\s@ExtendedKeyUsage' {} a -> s {name = a} :: ExtendedKeyUsage)

instance Core.FromJSON ExtendedKeyUsage where
  parseJSON =
    Core.withObject
      "ExtendedKeyUsage"
      ( \x ->
          ExtendedKeyUsage'
            Prelude.<$> (x Core..:? "OID") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable ExtendedKeyUsage where
  hashWithSalt salt' ExtendedKeyUsage' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` oid

instance Prelude.NFData ExtendedKeyUsage where
  rnf ExtendedKeyUsage' {..} =
    Prelude.rnf oid `Prelude.seq` Prelude.rnf name
