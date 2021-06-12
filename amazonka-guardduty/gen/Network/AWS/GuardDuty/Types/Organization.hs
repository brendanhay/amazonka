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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the ISP organization of the remote IP
-- address.
--
-- /See:/ 'newOrganization' smart constructor.
data Organization = Organization'
  { -- | The Autonomous System Number (ASN) of the internet provider of the
    -- remote IP address.
    asn :: Core.Maybe Core.Text,
    -- | The ISP information for the internet provider.
    isp :: Core.Maybe Core.Text,
    -- | The organization that registered this ASN.
    asnOrg :: Core.Maybe Core.Text,
    -- | The name of the internet provider.
    org :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { asn = Core.Nothing,
      isp = Core.Nothing,
      asnOrg = Core.Nothing,
      org = Core.Nothing
    }

-- | The Autonomous System Number (ASN) of the internet provider of the
-- remote IP address.
organization_asn :: Lens.Lens' Organization (Core.Maybe Core.Text)
organization_asn = Lens.lens (\Organization' {asn} -> asn) (\s@Organization' {} a -> s {asn = a} :: Organization)

-- | The ISP information for the internet provider.
organization_isp :: Lens.Lens' Organization (Core.Maybe Core.Text)
organization_isp = Lens.lens (\Organization' {isp} -> isp) (\s@Organization' {} a -> s {isp = a} :: Organization)

-- | The organization that registered this ASN.
organization_asnOrg :: Lens.Lens' Organization (Core.Maybe Core.Text)
organization_asnOrg = Lens.lens (\Organization' {asnOrg} -> asnOrg) (\s@Organization' {} a -> s {asnOrg = a} :: Organization)

-- | The name of the internet provider.
organization_org :: Lens.Lens' Organization (Core.Maybe Core.Text)
organization_org = Lens.lens (\Organization' {org} -> org) (\s@Organization' {} a -> s {org = a} :: Organization)

instance Core.FromJSON Organization where
  parseJSON =
    Core.withObject
      "Organization"
      ( \x ->
          Organization'
            Core.<$> (x Core..:? "asn")
            Core.<*> (x Core..:? "isp")
            Core.<*> (x Core..:? "asnOrg")
            Core.<*> (x Core..:? "org")
      )

instance Core.Hashable Organization

instance Core.NFData Organization
