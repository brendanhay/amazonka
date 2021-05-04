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
-- Module      : Network.AWS.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Organization where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the ISP organization of the remote IP
-- address.
--
-- /See:/ 'newOrganization' smart constructor.
data Organization = Organization'
  { -- | The Autonomous System Number (ASN) of the internet provider of the
    -- remote IP address.
    asn :: Prelude.Maybe Prelude.Text,
    -- | The ISP information for the internet provider.
    isp :: Prelude.Maybe Prelude.Text,
    -- | The organization that registered this ASN.
    asnOrg :: Prelude.Maybe Prelude.Text,
    -- | The name of the internet provider.
    org :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Organization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asn', 'organization_asn' - The Autonomous System Number (ASN) of the internet provider of the
-- remote IP address.
--
-- 'isp', 'organization_isp' - The ISP information for the internet provider.
--
-- 'asnOrg', 'organization_asnOrg' - The organization that registered this ASN.
--
-- 'org', 'organization_org' - The name of the internet provider.
newOrganization ::
  Organization
newOrganization =
  Organization'
    { asn = Prelude.Nothing,
      isp = Prelude.Nothing,
      asnOrg = Prelude.Nothing,
      org = Prelude.Nothing
    }

-- | The Autonomous System Number (ASN) of the internet provider of the
-- remote IP address.
organization_asn :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_asn = Lens.lens (\Organization' {asn} -> asn) (\s@Organization' {} a -> s {asn = a} :: Organization)

-- | The ISP information for the internet provider.
organization_isp :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_isp = Lens.lens (\Organization' {isp} -> isp) (\s@Organization' {} a -> s {isp = a} :: Organization)

-- | The organization that registered this ASN.
organization_asnOrg :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_asnOrg = Lens.lens (\Organization' {asnOrg} -> asnOrg) (\s@Organization' {} a -> s {asnOrg = a} :: Organization)

-- | The name of the internet provider.
organization_org :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_org = Lens.lens (\Organization' {org} -> org) (\s@Organization' {} a -> s {org = a} :: Organization)

instance Prelude.FromJSON Organization where
  parseJSON =
    Prelude.withObject
      "Organization"
      ( \x ->
          Organization'
            Prelude.<$> (x Prelude..:? "asn")
            Prelude.<*> (x Prelude..:? "isp")
            Prelude.<*> (x Prelude..:? "asnOrg")
            Prelude.<*> (x Prelude..:? "org")
      )

instance Prelude.Hashable Organization

instance Prelude.NFData Organization
