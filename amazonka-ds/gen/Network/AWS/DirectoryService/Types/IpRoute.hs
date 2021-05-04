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
-- Module      : Network.AWS.DirectoryService.Types.IpRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IpRoute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | IP address block. This is often the address block of the DNS server used
-- for your on-premises domain.
--
-- /See:/ 'newIpRoute' smart constructor.
data IpRoute = IpRoute'
  { -- | IP address block using CIDR format, for example 10.0.0.0\/24. This is
    -- often the address block of the DNS server used for your on-premises
    -- domain. For a single IP address use a CIDR address block with \/32. For
    -- example 10.0.0.0\/32.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | Description of the address block.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { cidrIp = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | IP address block using CIDR format, for example 10.0.0.0\/24. This is
-- often the address block of the DNS server used for your on-premises
-- domain. For a single IP address use a CIDR address block with \/32. For
-- example 10.0.0.0\/32.
ipRoute_cidrIp :: Lens.Lens' IpRoute (Prelude.Maybe Prelude.Text)
ipRoute_cidrIp = Lens.lens (\IpRoute' {cidrIp} -> cidrIp) (\s@IpRoute' {} a -> s {cidrIp = a} :: IpRoute)

-- | Description of the address block.
ipRoute_description :: Lens.Lens' IpRoute (Prelude.Maybe Prelude.Text)
ipRoute_description = Lens.lens (\IpRoute' {description} -> description) (\s@IpRoute' {} a -> s {description = a} :: IpRoute)

instance Prelude.Hashable IpRoute

instance Prelude.NFData IpRoute

instance Prelude.ToJSON IpRoute where
  toJSON IpRoute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CidrIp" Prelude..=) Prelude.<$> cidrIp,
            ("Description" Prelude..=) Prelude.<$> description
          ]
      )
