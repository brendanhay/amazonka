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
-- Module      : Network.AWS.WorkSpaces.Types.IpRuleItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.IpRuleItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a rule for an IP access control group.
--
-- /See:/ 'newIpRuleItem' smart constructor.
data IpRuleItem = IpRuleItem'
  { -- | The description.
    ruleDesc :: Core.Maybe Core.Text,
    -- | The IP address range, in CIDR notation.
    ipRule :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ruleDesc = Core.Nothing,
      ipRule = Core.Nothing
    }

-- | The description.
ipRuleItem_ruleDesc :: Lens.Lens' IpRuleItem (Core.Maybe Core.Text)
ipRuleItem_ruleDesc = Lens.lens (\IpRuleItem' {ruleDesc} -> ruleDesc) (\s@IpRuleItem' {} a -> s {ruleDesc = a} :: IpRuleItem)

-- | The IP address range, in CIDR notation.
ipRuleItem_ipRule :: Lens.Lens' IpRuleItem (Core.Maybe Core.Text)
ipRuleItem_ipRule = Lens.lens (\IpRuleItem' {ipRule} -> ipRule) (\s@IpRuleItem' {} a -> s {ipRule = a} :: IpRuleItem)

instance Core.FromJSON IpRuleItem where
  parseJSON =
    Core.withObject
      "IpRuleItem"
      ( \x ->
          IpRuleItem'
            Core.<$> (x Core..:? "ruleDesc")
            Core.<*> (x Core..:? "ipRule")
      )

instance Core.Hashable IpRuleItem

instance Core.NFData IpRuleItem

instance Core.ToJSON IpRuleItem where
  toJSON IpRuleItem' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ruleDesc" Core..=) Core.<$> ruleDesc,
            ("ipRule" Core..=) Core.<$> ipRule
          ]
      )
