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
-- Module      : Amazonka.DirectoryService.Types.IpRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.IpRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | IP address block. This is often the address block of the DNS server used
-- for your self-managed domain.
--
-- /See:/ 'newIpRoute' smart constructor.
data IpRoute = IpRoute'
  { -- | IP address block using CIDR format, for example 10.0.0.0\/24. This is
    -- often the address block of the DNS server used for your self-managed
    -- domain. For a single IP address use a CIDR address block with \/32. For
    -- example 10.0.0.0\/32.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | Description of the address block.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'ipRoute_cidrIp' - IP address block using CIDR format, for example 10.0.0.0\/24. This is
-- often the address block of the DNS server used for your self-managed
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
-- often the address block of the DNS server used for your self-managed
-- domain. For a single IP address use a CIDR address block with \/32. For
-- example 10.0.0.0\/32.
ipRoute_cidrIp :: Lens.Lens' IpRoute (Prelude.Maybe Prelude.Text)
ipRoute_cidrIp = Lens.lens (\IpRoute' {cidrIp} -> cidrIp) (\s@IpRoute' {} a -> s {cidrIp = a} :: IpRoute)

-- | Description of the address block.
ipRoute_description :: Lens.Lens' IpRoute (Prelude.Maybe Prelude.Text)
ipRoute_description = Lens.lens (\IpRoute' {description} -> description) (\s@IpRoute' {} a -> s {description = a} :: IpRoute)

instance Prelude.Hashable IpRoute where
  hashWithSalt _salt IpRoute' {..} =
    _salt
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` description

instance Prelude.NFData IpRoute where
  rnf IpRoute' {..} =
    Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf description

instance Data.ToJSON IpRoute where
  toJSON IpRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CidrIp" Data..=) Prelude.<$> cidrIp,
            ("Description" Data..=) Prelude.<$> description
          ]
      )
