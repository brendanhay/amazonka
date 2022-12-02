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
-- Module      : Amazonka.EC2.Types.AnalysisSecurityGroupRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AnalysisSecurityGroupRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a security group rule.
--
-- /See:/ 'newAnalysisSecurityGroupRule' smart constructor.
data AnalysisSecurityGroupRule = AnalysisSecurityGroupRule'
  { -- | The port range.
    portRange :: Prelude.Maybe PortRange,
    -- | The IPv4 address range, in CIDR notation.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The prefix list ID.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The security group ID.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The protocol name.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The direction. The following are the possible values:
    --
    -- -   egress
    --
    -- -   ingress
    direction :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSecurityGroupRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portRange', 'analysisSecurityGroupRule_portRange' - The port range.
--
-- 'cidr', 'analysisSecurityGroupRule_cidr' - The IPv4 address range, in CIDR notation.
--
-- 'prefixListId', 'analysisSecurityGroupRule_prefixListId' - The prefix list ID.
--
-- 'securityGroupId', 'analysisSecurityGroupRule_securityGroupId' - The security group ID.
--
-- 'protocol', 'analysisSecurityGroupRule_protocol' - The protocol name.
--
-- 'direction', 'analysisSecurityGroupRule_direction' - The direction. The following are the possible values:
--
-- -   egress
--
-- -   ingress
newAnalysisSecurityGroupRule ::
  AnalysisSecurityGroupRule
newAnalysisSecurityGroupRule =
  AnalysisSecurityGroupRule'
    { portRange =
        Prelude.Nothing,
      cidr = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      securityGroupId = Prelude.Nothing,
      protocol = Prelude.Nothing,
      direction = Prelude.Nothing
    }

-- | The port range.
analysisSecurityGroupRule_portRange :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe PortRange)
analysisSecurityGroupRule_portRange = Lens.lens (\AnalysisSecurityGroupRule' {portRange} -> portRange) (\s@AnalysisSecurityGroupRule' {} a -> s {portRange = a} :: AnalysisSecurityGroupRule)

-- | The IPv4 address range, in CIDR notation.
analysisSecurityGroupRule_cidr :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_cidr = Lens.lens (\AnalysisSecurityGroupRule' {cidr} -> cidr) (\s@AnalysisSecurityGroupRule' {} a -> s {cidr = a} :: AnalysisSecurityGroupRule)

-- | The prefix list ID.
analysisSecurityGroupRule_prefixListId :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_prefixListId = Lens.lens (\AnalysisSecurityGroupRule' {prefixListId} -> prefixListId) (\s@AnalysisSecurityGroupRule' {} a -> s {prefixListId = a} :: AnalysisSecurityGroupRule)

-- | The security group ID.
analysisSecurityGroupRule_securityGroupId :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_securityGroupId = Lens.lens (\AnalysisSecurityGroupRule' {securityGroupId} -> securityGroupId) (\s@AnalysisSecurityGroupRule' {} a -> s {securityGroupId = a} :: AnalysisSecurityGroupRule)

-- | The protocol name.
analysisSecurityGroupRule_protocol :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_protocol = Lens.lens (\AnalysisSecurityGroupRule' {protocol} -> protocol) (\s@AnalysisSecurityGroupRule' {} a -> s {protocol = a} :: AnalysisSecurityGroupRule)

-- | The direction. The following are the possible values:
--
-- -   egress
--
-- -   ingress
analysisSecurityGroupRule_direction :: Lens.Lens' AnalysisSecurityGroupRule (Prelude.Maybe Prelude.Text)
analysisSecurityGroupRule_direction = Lens.lens (\AnalysisSecurityGroupRule' {direction} -> direction) (\s@AnalysisSecurityGroupRule' {} a -> s {direction = a} :: AnalysisSecurityGroupRule)

instance Data.FromXML AnalysisSecurityGroupRule where
  parseXML x =
    AnalysisSecurityGroupRule'
      Prelude.<$> (x Data..@? "portRange")
      Prelude.<*> (x Data..@? "cidr")
      Prelude.<*> (x Data..@? "prefixListId")
      Prelude.<*> (x Data..@? "securityGroupId")
      Prelude.<*> (x Data..@? "protocol")
      Prelude.<*> (x Data..@? "direction")

instance Prelude.Hashable AnalysisSecurityGroupRule where
  hashWithSalt _salt AnalysisSecurityGroupRule' {..} =
    _salt `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` direction

instance Prelude.NFData AnalysisSecurityGroupRule where
  rnf AnalysisSecurityGroupRule' {..} =
    Prelude.rnf portRange
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf direction
