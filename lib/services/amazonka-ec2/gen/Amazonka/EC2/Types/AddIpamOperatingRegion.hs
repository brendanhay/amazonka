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
-- Module      : Amazonka.EC2.Types.AddIpamOperatingRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddIpamOperatingRegion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Add an operating Region to an IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- /See:/ 'newAddIpamOperatingRegion' smart constructor.
data AddIpamOperatingRegion = AddIpamOperatingRegion'
  { -- | The name of the operating Region.
    regionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddIpamOperatingRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'addIpamOperatingRegion_regionName' - The name of the operating Region.
newAddIpamOperatingRegion ::
  AddIpamOperatingRegion
newAddIpamOperatingRegion =
  AddIpamOperatingRegion'
    { regionName =
        Prelude.Nothing
    }

-- | The name of the operating Region.
addIpamOperatingRegion_regionName :: Lens.Lens' AddIpamOperatingRegion (Prelude.Maybe Prelude.Text)
addIpamOperatingRegion_regionName = Lens.lens (\AddIpamOperatingRegion' {regionName} -> regionName) (\s@AddIpamOperatingRegion' {} a -> s {regionName = a} :: AddIpamOperatingRegion)

instance Prelude.Hashable AddIpamOperatingRegion where
  hashWithSalt _salt AddIpamOperatingRegion' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData AddIpamOperatingRegion where
  rnf AddIpamOperatingRegion' {..} =
    Prelude.rnf regionName

instance Core.ToQuery AddIpamOperatingRegion where
  toQuery AddIpamOperatingRegion' {..} =
    Prelude.mconcat ["RegionName" Core.=: regionName]
