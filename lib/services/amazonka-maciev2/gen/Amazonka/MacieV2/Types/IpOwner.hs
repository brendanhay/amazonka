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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IpOwner where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the registered owner of an IP address.
--
-- /See:/ 'newIpOwner' smart constructor.
data IpOwner = IpOwner'
  { -- | The autonomous system number (ASN) for the autonomous system that
    -- included the IP address.
    asn :: Prelude.Maybe Prelude.Text,
    -- | The organization identifier that\'s associated with the autonomous
    -- system number (ASN) for the autonomous system that included the IP
    -- address.
    asnOrg :: Prelude.Maybe Prelude.Text,
    -- | The name of the internet service provider (ISP) that owned the IP
    -- address.
    isp :: Prelude.Maybe Prelude.Text,
    -- | The name of the organization that owned the IP address.
    org :: Prelude.Maybe Prelude.Text
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
-- 'asn', 'ipOwner_asn' - The autonomous system number (ASN) for the autonomous system that
-- included the IP address.
--
-- 'asnOrg', 'ipOwner_asnOrg' - The organization identifier that\'s associated with the autonomous
-- system number (ASN) for the autonomous system that included the IP
-- address.
--
-- 'isp', 'ipOwner_isp' - The name of the internet service provider (ISP) that owned the IP
-- address.
--
-- 'org', 'ipOwner_org' - The name of the organization that owned the IP address.
newIpOwner ::
  IpOwner
newIpOwner =
  IpOwner'
    { asn = Prelude.Nothing,
      asnOrg = Prelude.Nothing,
      isp = Prelude.Nothing,
      org = Prelude.Nothing
    }

-- | The autonomous system number (ASN) for the autonomous system that
-- included the IP address.
ipOwner_asn :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_asn = Lens.lens (\IpOwner' {asn} -> asn) (\s@IpOwner' {} a -> s {asn = a} :: IpOwner)

-- | The organization identifier that\'s associated with the autonomous
-- system number (ASN) for the autonomous system that included the IP
-- address.
ipOwner_asnOrg :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_asnOrg = Lens.lens (\IpOwner' {asnOrg} -> asnOrg) (\s@IpOwner' {} a -> s {asnOrg = a} :: IpOwner)

-- | The name of the internet service provider (ISP) that owned the IP
-- address.
ipOwner_isp :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_isp = Lens.lens (\IpOwner' {isp} -> isp) (\s@IpOwner' {} a -> s {isp = a} :: IpOwner)

-- | The name of the organization that owned the IP address.
ipOwner_org :: Lens.Lens' IpOwner (Prelude.Maybe Prelude.Text)
ipOwner_org = Lens.lens (\IpOwner' {org} -> org) (\s@IpOwner' {} a -> s {org = a} :: IpOwner)

instance Data.FromJSON IpOwner where
  parseJSON =
    Data.withObject
      "IpOwner"
      ( \x ->
          IpOwner'
            Prelude.<$> (x Data..:? "asn")
            Prelude.<*> (x Data..:? "asnOrg")
            Prelude.<*> (x Data..:? "isp")
            Prelude.<*> (x Data..:? "org")
      )

instance Prelude.Hashable IpOwner where
  hashWithSalt _salt IpOwner' {..} =
    _salt
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` asnOrg
      `Prelude.hashWithSalt` isp
      `Prelude.hashWithSalt` org

instance Prelude.NFData IpOwner where
  rnf IpOwner' {..} =
    Prelude.rnf asn `Prelude.seq`
      Prelude.rnf asnOrg `Prelude.seq`
        Prelude.rnf isp `Prelude.seq`
          Prelude.rnf org
