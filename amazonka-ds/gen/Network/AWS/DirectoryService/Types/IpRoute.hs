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
-- Module      : Network.AWS.DirectoryService.Types.IpRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IpRoute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | IP address block. This is often the address block of the DNS server used
-- for your on-premises domain.
--
-- /See:/ 'newIpRoute' smart constructor.
data IpRoute = IpRoute'
  { -- | IP address block using CIDR format, for example 10.0.0.0\/24. This is
    -- often the address block of the DNS server used for your on-premises
    -- domain. For a single IP address use a CIDR address block with \/32. For
    -- example 10.0.0.0\/32.
    cidrIp :: Core.Maybe Core.Text,
    -- | Description of the address block.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IpRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'ipRoute_cidrIp' - IP address block using CIDR format, for example 10.0.0.0\/24. This is
-- often the address block of the DNS server used for your on-premises
-- domain. For a single IP address use a CIDR address block with \/32. For
-- example 10.0.0.0\/32.
--
-- 'description', 'ipRoute_description' - Description of the address block.
newIpRoute ::
  IpRoute
newIpRoute =
  IpRoute'
    { cidrIp = Core.Nothing,
      description = Core.Nothing
    }

-- | IP address block using CIDR format, for example 10.0.0.0\/24. This is
-- often the address block of the DNS server used for your on-premises
-- domain. For a single IP address use a CIDR address block with \/32. For
-- example 10.0.0.0\/32.
ipRoute_cidrIp :: Lens.Lens' IpRoute (Core.Maybe Core.Text)
ipRoute_cidrIp = Lens.lens (\IpRoute' {cidrIp} -> cidrIp) (\s@IpRoute' {} a -> s {cidrIp = a} :: IpRoute)

-- | Description of the address block.
ipRoute_description :: Lens.Lens' IpRoute (Core.Maybe Core.Text)
ipRoute_description = Lens.lens (\IpRoute' {description} -> description) (\s@IpRoute' {} a -> s {description = a} :: IpRoute)

instance Core.Hashable IpRoute

instance Core.NFData IpRoute

instance Core.ToJSON IpRoute where
  toJSON IpRoute' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CidrIp" Core..=) Core.<$> cidrIp,
            ("Description" Core..=) Core.<$> description
          ]
      )
