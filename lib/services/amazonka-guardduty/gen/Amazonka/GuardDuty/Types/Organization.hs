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
-- Module      : Amazonka.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Organization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the ISP organization of the remote IP
-- address.
--
-- /See:/ 'newOrganization' smart constructor.
data Organization = Organization'
  { -- | The ISP information for the internet provider.
    isp :: Prelude.Maybe Prelude.Text,
    -- | The name of the internet provider.
    org :: Prelude.Maybe Prelude.Text,
    -- | The Autonomous System Number (ASN) of the internet provider of the
    -- remote IP address.
    asn :: Prelude.Maybe Prelude.Text,
    -- | The organization that registered this ASN.
    asnOrg :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Organization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isp', 'organization_isp' - The ISP information for the internet provider.
--
-- 'org', 'organization_org' - The name of the internet provider.
--
-- 'asn', 'organization_asn' - The Autonomous System Number (ASN) of the internet provider of the
-- remote IP address.
--
-- 'asnOrg', 'organization_asnOrg' - The organization that registered this ASN.
newOrganization ::
  Organization
newOrganization =
  Organization'
    { isp = Prelude.Nothing,
      org = Prelude.Nothing,
      asn = Prelude.Nothing,
      asnOrg = Prelude.Nothing
    }

-- | The ISP information for the internet provider.
organization_isp :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_isp = Lens.lens (\Organization' {isp} -> isp) (\s@Organization' {} a -> s {isp = a} :: Organization)

-- | The name of the internet provider.
organization_org :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_org = Lens.lens (\Organization' {org} -> org) (\s@Organization' {} a -> s {org = a} :: Organization)

-- | The Autonomous System Number (ASN) of the internet provider of the
-- remote IP address.
organization_asn :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_asn = Lens.lens (\Organization' {asn} -> asn) (\s@Organization' {} a -> s {asn = a} :: Organization)

-- | The organization that registered this ASN.
organization_asnOrg :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_asnOrg = Lens.lens (\Organization' {asnOrg} -> asnOrg) (\s@Organization' {} a -> s {asnOrg = a} :: Organization)

instance Data.FromJSON Organization where
  parseJSON =
    Data.withObject
      "Organization"
      ( \x ->
          Organization'
            Prelude.<$> (x Data..:? "isp")
            Prelude.<*> (x Data..:? "org")
            Prelude.<*> (x Data..:? "asn")
            Prelude.<*> (x Data..:? "asnOrg")
      )

instance Prelude.Hashable Organization where
  hashWithSalt _salt Organization' {..} =
    _salt `Prelude.hashWithSalt` isp
      `Prelude.hashWithSalt` org
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` asnOrg

instance Prelude.NFData Organization where
  rnf Organization' {..} =
    Prelude.rnf isp
      `Prelude.seq` Prelude.rnf org
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf asnOrg
