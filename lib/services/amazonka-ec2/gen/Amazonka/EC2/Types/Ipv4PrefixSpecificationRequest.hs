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
-- Module      : Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the IPv4 prefix option for a network interface.
--
-- /See:/ 'newIpv4PrefixSpecificationRequest' smart constructor.
data Ipv4PrefixSpecificationRequest = Ipv4PrefixSpecificationRequest'
  { -- | The IPv4 prefix. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ipv4Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv4PrefixSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefix', 'ipv4PrefixSpecificationRequest_ipv4Prefix' - The IPv4 prefix. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newIpv4PrefixSpecificationRequest ::
  Ipv4PrefixSpecificationRequest
newIpv4PrefixSpecificationRequest =
  Ipv4PrefixSpecificationRequest'
    { ipv4Prefix =
        Prelude.Nothing
    }

-- | The IPv4 prefix. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
ipv4PrefixSpecificationRequest_ipv4Prefix :: Lens.Lens' Ipv4PrefixSpecificationRequest (Prelude.Maybe Prelude.Text)
ipv4PrefixSpecificationRequest_ipv4Prefix = Lens.lens (\Ipv4PrefixSpecificationRequest' {ipv4Prefix} -> ipv4Prefix) (\s@Ipv4PrefixSpecificationRequest' {} a -> s {ipv4Prefix = a} :: Ipv4PrefixSpecificationRequest)

instance Data.FromXML Ipv4PrefixSpecificationRequest where
  parseXML x =
    Ipv4PrefixSpecificationRequest'
      Prelude.<$> (x Data..@? "Ipv4Prefix")

instance
  Prelude.Hashable
    Ipv4PrefixSpecificationRequest
  where
  hashWithSalt
    _salt
    Ipv4PrefixSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` ipv4Prefix

instance
  Prelude.NFData
    Ipv4PrefixSpecificationRequest
  where
  rnf Ipv4PrefixSpecificationRequest' {..} =
    Prelude.rnf ipv4Prefix

instance Data.ToQuery Ipv4PrefixSpecificationRequest where
  toQuery Ipv4PrefixSpecificationRequest' {..} =
    Prelude.mconcat ["Ipv4Prefix" Data.=: ipv4Prefix]
