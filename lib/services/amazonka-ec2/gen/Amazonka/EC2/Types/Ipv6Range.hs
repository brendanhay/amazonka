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
-- Module      : Amazonka.EC2.Types.Ipv6Range
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | [EC2-VPC only] Describes an IPv6 range.
--
-- /See:/ 'newIpv6Range' smart constructor.
data Ipv6Range = Ipv6Range'
  { -- | The IPv6 CIDR range. You can either specify a CIDR range or a source
    -- security group, not both. To specify a single IPv6 address, use the
    -- \/128 prefix length.
    cidrIpv6 :: Prelude.Maybe Prelude.Text,
    -- | A description for the security group rule that references this IPv6
    -- address range.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIpv6', 'ipv6Range_cidrIpv6' - The IPv6 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv6 address, use the
-- \/128 prefix length.
--
-- 'description', 'ipv6Range_description' - A description for the security group rule that references this IPv6
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
newIpv6Range ::
  Ipv6Range
newIpv6Range =
  Ipv6Range'
    { cidrIpv6 = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The IPv6 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv6 address, use the
-- \/128 prefix length.
ipv6Range_cidrIpv6 :: Lens.Lens' Ipv6Range (Prelude.Maybe Prelude.Text)
ipv6Range_cidrIpv6 = Lens.lens (\Ipv6Range' {cidrIpv6} -> cidrIpv6) (\s@Ipv6Range' {} a -> s {cidrIpv6 = a} :: Ipv6Range)

-- | A description for the security group rule that references this IPv6
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
ipv6Range_description :: Lens.Lens' Ipv6Range (Prelude.Maybe Prelude.Text)
ipv6Range_description = Lens.lens (\Ipv6Range' {description} -> description) (\s@Ipv6Range' {} a -> s {description = a} :: Ipv6Range)

instance Data.FromXML Ipv6Range where
  parseXML x =
    Ipv6Range'
      Prelude.<$> (x Data..@? "cidrIpv6")
      Prelude.<*> (x Data..@? "description")

instance Prelude.Hashable Ipv6Range where
  hashWithSalt _salt Ipv6Range' {..} =
    _salt
      `Prelude.hashWithSalt` cidrIpv6
      `Prelude.hashWithSalt` description

instance Prelude.NFData Ipv6Range where
  rnf Ipv6Range' {..} =
    Prelude.rnf cidrIpv6
      `Prelude.seq` Prelude.rnf description

instance Data.ToQuery Ipv6Range where
  toQuery Ipv6Range' {..} =
    Prelude.mconcat
      [ "CidrIpv6" Data.=: cidrIpv6,
        "Description" Data.=: description
      ]
