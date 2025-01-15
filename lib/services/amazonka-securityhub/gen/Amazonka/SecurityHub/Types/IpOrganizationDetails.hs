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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.IpOrganizationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an internet provider.
--
-- /See:/ 'newIpOrganizationDetails' smart constructor.
data IpOrganizationDetails = IpOrganizationDetails'
  { -- | The Autonomous System Number (ASN) of the internet provider
    asn :: Prelude.Maybe Prelude.Int,
    -- | The name of the organization that registered the ASN.
    asnOrg :: Prelude.Maybe Prelude.Text,
    -- | The ISP information for the internet provider.
    isp :: Prelude.Maybe Prelude.Text,
    -- | The name of the internet provider.
    org :: Prelude.Maybe Prelude.Text
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
-- 'asn', 'ipOrganizationDetails_asn' - The Autonomous System Number (ASN) of the internet provider
--
-- 'asnOrg', 'ipOrganizationDetails_asnOrg' - The name of the organization that registered the ASN.
--
-- 'isp', 'ipOrganizationDetails_isp' - The ISP information for the internet provider.
--
-- 'org', 'ipOrganizationDetails_org' - The name of the internet provider.
newIpOrganizationDetails ::
  IpOrganizationDetails
newIpOrganizationDetails =
  IpOrganizationDetails'
    { asn = Prelude.Nothing,
      asnOrg = Prelude.Nothing,
      isp = Prelude.Nothing,
      org = Prelude.Nothing
    }

-- | The Autonomous System Number (ASN) of the internet provider
ipOrganizationDetails_asn :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Int)
ipOrganizationDetails_asn = Lens.lens (\IpOrganizationDetails' {asn} -> asn) (\s@IpOrganizationDetails' {} a -> s {asn = a} :: IpOrganizationDetails)

-- | The name of the organization that registered the ASN.
ipOrganizationDetails_asnOrg :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_asnOrg = Lens.lens (\IpOrganizationDetails' {asnOrg} -> asnOrg) (\s@IpOrganizationDetails' {} a -> s {asnOrg = a} :: IpOrganizationDetails)

-- | The ISP information for the internet provider.
ipOrganizationDetails_isp :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_isp = Lens.lens (\IpOrganizationDetails' {isp} -> isp) (\s@IpOrganizationDetails' {} a -> s {isp = a} :: IpOrganizationDetails)

-- | The name of the internet provider.
ipOrganizationDetails_org :: Lens.Lens' IpOrganizationDetails (Prelude.Maybe Prelude.Text)
ipOrganizationDetails_org = Lens.lens (\IpOrganizationDetails' {org} -> org) (\s@IpOrganizationDetails' {} a -> s {org = a} :: IpOrganizationDetails)

instance Data.FromJSON IpOrganizationDetails where
  parseJSON =
    Data.withObject
      "IpOrganizationDetails"
      ( \x ->
          IpOrganizationDetails'
            Prelude.<$> (x Data..:? "Asn")
            Prelude.<*> (x Data..:? "AsnOrg")
            Prelude.<*> (x Data..:? "Isp")
            Prelude.<*> (x Data..:? "Org")
      )

instance Prelude.Hashable IpOrganizationDetails where
  hashWithSalt _salt IpOrganizationDetails' {..} =
    _salt
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` asnOrg
      `Prelude.hashWithSalt` isp
      `Prelude.hashWithSalt` org

instance Prelude.NFData IpOrganizationDetails where
  rnf IpOrganizationDetails' {..} =
    Prelude.rnf asn `Prelude.seq`
      Prelude.rnf asnOrg `Prelude.seq`
        Prelude.rnf isp `Prelude.seq`
          Prelude.rnf org

instance Data.ToJSON IpOrganizationDetails where
  toJSON IpOrganizationDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Asn" Data..=) Prelude.<$> asn,
            ("AsnOrg" Data..=) Prelude.<$> asnOrg,
            ("Isp" Data..=) Prelude.<$> isp,
            ("Org" Data..=) Prelude.<$> org
          ]
      )
