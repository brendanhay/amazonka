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
-- Module      : Amazonka.SecurityHub.Types.IpOrganizationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.IpOrganizationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an internet provider.
--
-- /See:/ 'newIpOrganizationDetails' smart constructor.
data IpOrganizationDetails = IpOrganizationDetails'
  { -- | The name of the internet provider.
    org :: Prelude.Maybe Prelude.Text,
    -- | The name of the organization that registered the ASN.
    asnOrg :: Prelude.Maybe Prelude.Text,
    -- | The Autonomous System Number (ASN) of the internet provider
    asn :: Prelude.Maybe Prelude.Int,
    -- | The ISP information for the internet provider.
    isp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpOrganizationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'org', 'ipOrganizationDetails_org' - The name of the internet provider.
--
-- 'asnOrg', 'ipOrganizationDetails_asnOrg' - The name of the organization that registered the ASN.
--
-- 'asn', 'ipOrganizationDetails_asn' - The Autonomous System Number (ASN) of the internet provider
--
-- 'isp', 'ipOrganizationDetails_isp' - The ISP information for the internet provider.
newIpOrganizationDetails ::
  IpOrganizationDetails
newIpOrganizationDetails =
  IpOrganizationDetails'
    { org = Prelude.Nothing,
      asnOrg = Prelude.Nothing,
      asn = Prelude.Nothing,
      isp = Prelude.Nothing
    }

-- | The name of the internet provider.
ipOrganizationDetails_org :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_org = Lens.lens (\IpOrganizationDetails' {org} -> org) (\s@IpOrganizationDetails' {} a -> s {org = a} :: IpOrganizationDetails)

-- | The name of the organization that registered the ASN.
ipOrganizationDetails_asnOrg :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_asnOrg = Lens.lens (\IpOrganizationDetails' {asnOrg} -> asnOrg) (\s@IpOrganizationDetails' {} a -> s {asnOrg = a} :: IpOrganizationDetails)

-- | The Autonomous System Number (ASN) of the internet provider
ipOrganizationDetails_asn :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Int)
ipOrganizationDetails_asn = Lens.lens (\IpOrganizationDetails' {asn} -> asn) (\s@IpOrganizationDetails' {} a -> s {asn = a} :: IpOrganizationDetails)

-- | The ISP information for the internet provider.
ipOrganizationDetails_isp :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_isp = Lens.lens (\IpOrganizationDetails' {isp} -> isp) (\s@IpOrganizationDetails' {} a -> s {isp = a} :: IpOrganizationDetails)

instance Core.FromJSON IpOrganizationDetails where
  parseJSON =
    Core.withObject
      "IpOrganizationDetails"
      ( \x ->
          IpOrganizationDetails'
            Prelude.<$> (x Core..:? "Org")
            Prelude.<*> (x Core..:? "AsnOrg")
            Prelude.<*> (x Core..:? "Asn")
            Prelude.<*> (x Core..:? "Isp")
      )

instance Prelude.Hashable IpOrganizationDetails where
  hashWithSalt _salt IpOrganizationDetails' {..} =
    _salt `Prelude.hashWithSalt` org
      `Prelude.hashWithSalt` asnOrg
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` isp

instance Prelude.NFData IpOrganizationDetails where
  rnf IpOrganizationDetails' {..} =
    Prelude.rnf org
      `Prelude.seq` Prelude.rnf asnOrg
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf isp

instance Core.ToJSON IpOrganizationDetails where
  toJSON IpOrganizationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Org" Core..=) Prelude.<$> org,
            ("AsnOrg" Core..=) Prelude.<$> asnOrg,
            ("Asn" Core..=) Prelude.<$> asn,
            ("Isp" Core..=) Prelude.<$> isp
          ]
      )
