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
-- Module      : Amazonka.MacieV2.Types.IpOwner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IpOwner where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the registered owner of an IP address.
--
-- /See:/ 'newIpOwner' smart constructor.
data IpOwner = IpOwner'
  { -- | The name of the internet service provider (ISP) that owned the IP
    -- address.
    isp :: Prelude.Maybe Prelude.Text,
    -- | The name of the organization that owned the IP address.
    org :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system number (ASN) for the autonomous system that
    -- included the IP address.
    asn :: Prelude.Maybe Prelude.Text,
    -- | The organization identifier that\'s associated with the autonomous
    -- system number (ASN) for the autonomous system that included the IP
    -- address.
    asnOrg :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpOwner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isp', 'ipOwner_isp' - The name of the internet service provider (ISP) that owned the IP
-- address.
--
-- 'org', 'ipOwner_org' - The name of the organization that owned the IP address.
--
-- 'asn', 'ipOwner_asn' - The autonomous system number (ASN) for the autonomous system that
-- included the IP address.
--
-- 'asnOrg', 'ipOwner_asnOrg' - The organization identifier that\'s associated with the autonomous
-- system number (ASN) for the autonomous system that included the IP
-- address.
newIpOwner ::
  IpOwner
newIpOwner =
  IpOwner'
    { isp = Prelude.Nothing,
      org = Prelude.Nothing,
      asn = Prelude.Nothing,
      asnOrg = Prelude.Nothing
    }

-- | The name of the internet service provider (ISP) that owned the IP
-- address.
ipOwner_isp :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_isp = Lens.lens (\IpOwner' {isp} -> isp) (\s@IpOwner' {} a -> s {isp = a} :: IpOwner)

-- | The name of the organization that owned the IP address.
ipOwner_org :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_org = Lens.lens (\IpOwner' {org} -> org) (\s@IpOwner' {} a -> s {org = a} :: IpOwner)

-- | The autonomous system number (ASN) for the autonomous system that
-- included the IP address.
ipOwner_asn :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_asn = Lens.lens (\IpOwner' {asn} -> asn) (\s@IpOwner' {} a -> s {asn = a} :: IpOwner)

-- | The organization identifier that\'s associated with the autonomous
-- system number (ASN) for the autonomous system that included the IP
-- address.
ipOwner_asnOrg :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_asnOrg = Lens.lens (\IpOwner' {asnOrg} -> asnOrg) (\s@IpOwner' {} a -> s {asnOrg = a} :: IpOwner)

instance Core.FromJSON IpOwner where
  parseJSON =
    Core.withObject
      "IpOwner"
      ( \x ->
          IpOwner'
            Prelude.<$> (x Core..:? "isp")
            Prelude.<*> (x Core..:? "org")
            Prelude.<*> (x Core..:? "asn")
            Prelude.<*> (x Core..:? "asnOrg")
      )

instance Prelude.Hashable IpOwner where
  hashWithSalt _salt IpOwner' {..} =
    _salt `Prelude.hashWithSalt` isp
      `Prelude.hashWithSalt` org
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` asnOrg

instance Prelude.NFData IpOwner where
  rnf IpOwner' {..} =
    Prelude.rnf isp
      `Prelude.seq` Prelude.rnf org
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf asnOrg
