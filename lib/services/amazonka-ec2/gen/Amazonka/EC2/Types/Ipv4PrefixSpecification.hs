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
-- Module      : Amazonka.EC2.Types.Ipv4PrefixSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv4PrefixSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv4 prefix.
--
-- /See:/ 'newIpv4PrefixSpecification' smart constructor.
data Ipv4PrefixSpecification = Ipv4PrefixSpecification'
  { -- | The IPv4 prefix. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ipv4Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv4PrefixSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefix', 'ipv4PrefixSpecification_ipv4Prefix' - The IPv4 prefix. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newIpv4PrefixSpecification ::
  Ipv4PrefixSpecification
newIpv4PrefixSpecification =
  Ipv4PrefixSpecification'
    { ipv4Prefix =
        Prelude.Nothing
    }

-- | The IPv4 prefix. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
ipv4PrefixSpecification_ipv4Prefix :: Lens.Lens' Ipv4PrefixSpecification (Prelude.Maybe Prelude.Text)
ipv4PrefixSpecification_ipv4Prefix = Lens.lens (\Ipv4PrefixSpecification' {ipv4Prefix} -> ipv4Prefix) (\s@Ipv4PrefixSpecification' {} a -> s {ipv4Prefix = a} :: Ipv4PrefixSpecification)

instance Data.FromXML Ipv4PrefixSpecification where
  parseXML x =
    Ipv4PrefixSpecification'
      Prelude.<$> (x Data..@? "ipv4Prefix")

instance Prelude.Hashable Ipv4PrefixSpecification where
  hashWithSalt _salt Ipv4PrefixSpecification' {..} =
    _salt `Prelude.hashWithSalt` ipv4Prefix

instance Prelude.NFData Ipv4PrefixSpecification where
  rnf Ipv4PrefixSpecification' {..} =
    Prelude.rnf ipv4Prefix
