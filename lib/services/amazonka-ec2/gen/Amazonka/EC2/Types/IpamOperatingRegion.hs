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
-- Module      : Amazonka.EC2.Types.IpamOperatingRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamOperatingRegion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The operating Regions for an IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- /See:/ 'newIpamOperatingRegion' smart constructor.
data IpamOperatingRegion = IpamOperatingRegion'
  { -- | The name of the operating Region.
    regionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamOperatingRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'ipamOperatingRegion_regionName' - The name of the operating Region.
newIpamOperatingRegion ::
  IpamOperatingRegion
newIpamOperatingRegion =
  IpamOperatingRegion' {regionName = Prelude.Nothing}

-- | The name of the operating Region.
ipamOperatingRegion_regionName :: Lens.Lens' IpamOperatingRegion (Prelude.Maybe Prelude.Text)
ipamOperatingRegion_regionName = Lens.lens (\IpamOperatingRegion' {regionName} -> regionName) (\s@IpamOperatingRegion' {} a -> s {regionName = a} :: IpamOperatingRegion)

instance Data.FromXML IpamOperatingRegion where
  parseXML x =
    IpamOperatingRegion'
      Prelude.<$> (x Data..@? "regionName")

instance Prelude.Hashable IpamOperatingRegion where
  hashWithSalt _salt IpamOperatingRegion' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData IpamOperatingRegion where
  rnf IpamOperatingRegion' {..} = Prelude.rnf regionName
