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
-- Module      : Network.AWS.WorkSpaces.Types.IpRuleItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.IpRuleItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a rule for an IP access control group.
--
-- /See:/ 'newIpRuleItem' smart constructor.
data IpRuleItem = IpRuleItem'
  { -- | The description.
    ruleDesc :: Prelude.Maybe Prelude.Text,
    -- | The IP address range, in CIDR notation.
    ipRule :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON IpRuleItem where
  parseJSON =
    Prelude.withObject
      "IpRuleItem"
      ( \x ->
          IpRuleItem'
            Prelude.<$> (x Prelude..:? "ruleDesc")
            Prelude.<*> (x Prelude..:? "ipRule")
      )

instance Prelude.Hashable IpRuleItem

instance Prelude.NFData IpRuleItem

instance Prelude.ToJSON IpRuleItem where
  toJSON IpRuleItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ruleDesc" Prelude..=) Prelude.<$> ruleDesc,
            ("ipRule" Prelude..=) Prelude.<$> ipRule
          ]
      )
