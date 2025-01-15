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
-- Module      : Amazonka.EC2.Types.IpRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv4 range.
--
-- /See:/ 'newIpRange' smart constructor.
data IpRange = IpRange'
  { -- | A description for the security group rule that references this IPv4
    -- address range.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
    description :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR range. You can either specify a CIDR range or a source
    -- security group, not both. To specify a single IPv4 address, use the \/32
    -- prefix length.
    cidrIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'ipRange_description' - A description for the security group rule that references this IPv4
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
--
-- 'cidrIp', 'ipRange_cidrIp' - The IPv4 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv4 address, use the \/32
-- prefix length.
newIpRange ::
  -- | 'cidrIp'
  Prelude.Text ->
  IpRange
newIpRange pCidrIp_ =
  IpRange'
    { description = Prelude.Nothing,
      cidrIp = pCidrIp_
    }

-- | A description for the security group rule that references this IPv4
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
ipRange_description :: Lens.Lens' IpRange (Prelude.Maybe Prelude.Text)
ipRange_description = Lens.lens (\IpRange' {description} -> description) (\s@IpRange' {} a -> s {description = a} :: IpRange)

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv4 address, use the \/32
-- prefix length.
ipRange_cidrIp :: Lens.Lens' IpRange Prelude.Text
ipRange_cidrIp = Lens.lens (\IpRange' {cidrIp} -> cidrIp) (\s@IpRange' {} a -> s {cidrIp = a} :: IpRange)

instance Data.FromXML IpRange where
  parseXML x =
    IpRange'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@ "cidrIp")

instance Prelude.Hashable IpRange where
  hashWithSalt _salt IpRange' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` cidrIp

instance Prelude.NFData IpRange where
  rnf IpRange' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf cidrIp

instance Data.ToQuery IpRange where
  toQuery IpRange' {..} =
    Prelude.mconcat
      [ "Description" Data.=: description,
        "CidrIp" Data.=: cidrIp
      ]
