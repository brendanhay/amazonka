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
-- Module      : Amazonka.WorkSpaces.Types.IpRuleItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.IpRuleItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a rule for an IP access control group.
--
-- /See:/ 'newIpRuleItem' smart constructor.
data IpRuleItem = IpRuleItem'
  { -- | The description.
    ruleDesc :: Prelude.Maybe Prelude.Text,
    -- | The IP address range, in CIDR notation.
    ipRule :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRuleItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleDesc', 'ipRuleItem_ruleDesc' - The description.
--
-- 'ipRule', 'ipRuleItem_ipRule' - The IP address range, in CIDR notation.
newIpRuleItem ::
  IpRuleItem
newIpRuleItem =
  IpRuleItem'
    { ruleDesc = Prelude.Nothing,
      ipRule = Prelude.Nothing
    }

-- | The description.
ipRuleItem_ruleDesc :: Lens.Lens' IpRuleItem (Prelude.Maybe Prelude.Text)
ipRuleItem_ruleDesc = Lens.lens (\IpRuleItem' {ruleDesc} -> ruleDesc) (\s@IpRuleItem' {} a -> s {ruleDesc = a} :: IpRuleItem)

-- | The IP address range, in CIDR notation.
ipRuleItem_ipRule :: Lens.Lens' IpRuleItem (Prelude.Maybe Prelude.Text)
ipRuleItem_ipRule = Lens.lens (\IpRuleItem' {ipRule} -> ipRule) (\s@IpRuleItem' {} a -> s {ipRule = a} :: IpRuleItem)

instance Core.FromJSON IpRuleItem where
  parseJSON =
    Core.withObject
      "IpRuleItem"
      ( \x ->
          IpRuleItem'
            Prelude.<$> (x Core..:? "ruleDesc")
            Prelude.<*> (x Core..:? "ipRule")
      )

instance Prelude.Hashable IpRuleItem where
  hashWithSalt salt' IpRuleItem' {..} =
    salt' `Prelude.hashWithSalt` ipRule
      `Prelude.hashWithSalt` ruleDesc

instance Prelude.NFData IpRuleItem where
  rnf IpRuleItem' {..} =
    Prelude.rnf ruleDesc
      `Prelude.seq` Prelude.rnf ipRule

instance Core.ToJSON IpRuleItem where
  toJSON IpRuleItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ruleDesc" Core..=) Prelude.<$> ruleDesc,
            ("ipRule" Core..=) Prelude.<$> ipRule
          ]
      )
