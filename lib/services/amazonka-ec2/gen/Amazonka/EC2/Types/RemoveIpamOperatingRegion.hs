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
-- Module      : Amazonka.EC2.Types.RemoveIpamOperatingRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RemoveIpamOperatingRegion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Remove an operating Region from an IPAM. Operating Regions are Amazon
-- Web Services Regions where the IPAM is allowed to manage IP address
-- CIDRs. IPAM only discovers and monitors resources in the Amazon Web
-- Services Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/
--
-- /See:/ 'newRemoveIpamOperatingRegion' smart constructor.
data RemoveIpamOperatingRegion = RemoveIpamOperatingRegion'
  { -- | The name of the operating Region you want to remove.
    regionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveIpamOperatingRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'removeIpamOperatingRegion_regionName' - The name of the operating Region you want to remove.
newRemoveIpamOperatingRegion ::
  RemoveIpamOperatingRegion
newRemoveIpamOperatingRegion =
  RemoveIpamOperatingRegion'
    { regionName =
        Prelude.Nothing
    }

-- | The name of the operating Region you want to remove.
removeIpamOperatingRegion_regionName :: Lens.Lens' RemoveIpamOperatingRegion (Prelude.Maybe Prelude.Text)
removeIpamOperatingRegion_regionName = Lens.lens (\RemoveIpamOperatingRegion' {regionName} -> regionName) (\s@RemoveIpamOperatingRegion' {} a -> s {regionName = a} :: RemoveIpamOperatingRegion)

instance Prelude.Hashable RemoveIpamOperatingRegion where
  hashWithSalt _salt RemoveIpamOperatingRegion' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData RemoveIpamOperatingRegion where
  rnf RemoveIpamOperatingRegion' {..} =
    Prelude.rnf regionName

instance Core.ToQuery RemoveIpamOperatingRegion where
  toQuery RemoveIpamOperatingRegion' {..} =
    Prelude.mconcat ["RegionName" Core.=: regionName]
