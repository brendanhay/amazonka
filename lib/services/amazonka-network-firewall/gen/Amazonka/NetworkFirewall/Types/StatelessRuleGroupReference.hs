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
-- Module      : Amazonka.NetworkFirewall.Types.StatelessRuleGroupReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatelessRuleGroupReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifier for a single stateless rule group, used in a firewall policy
-- to refer to the rule group.
--
-- /See:/ 'newStatelessRuleGroupReference' smart constructor.
data StatelessRuleGroupReference = StatelessRuleGroupReference'
  { -- | The Amazon Resource Name (ARN) of the stateless rule group.
    resourceArn :: Prelude.Text,
    -- | An integer setting that indicates the order in which to run the
    -- stateless rule groups in a single FirewallPolicy. Network Firewall
    -- applies each stateless rule group to a packet starting with the group
    -- that has the lowest priority setting. You must ensure that the priority
    -- settings are unique within each policy.
    priority :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessRuleGroupReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'statelessRuleGroupReference_resourceArn' - The Amazon Resource Name (ARN) of the stateless rule group.
--
-- 'priority', 'statelessRuleGroupReference_priority' - An integer setting that indicates the order in which to run the
-- stateless rule groups in a single FirewallPolicy. Network Firewall
-- applies each stateless rule group to a packet starting with the group
-- that has the lowest priority setting. You must ensure that the priority
-- settings are unique within each policy.
newStatelessRuleGroupReference ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  StatelessRuleGroupReference
newStatelessRuleGroupReference
  pResourceArn_
  pPriority_ =
    StatelessRuleGroupReference'
      { resourceArn =
          pResourceArn_,
        priority = pPriority_
      }

-- | The Amazon Resource Name (ARN) of the stateless rule group.
statelessRuleGroupReference_resourceArn :: Lens.Lens' StatelessRuleGroupReference Prelude.Text
statelessRuleGroupReference_resourceArn = Lens.lens (\StatelessRuleGroupReference' {resourceArn} -> resourceArn) (\s@StatelessRuleGroupReference' {} a -> s {resourceArn = a} :: StatelessRuleGroupReference)

-- | An integer setting that indicates the order in which to run the
-- stateless rule groups in a single FirewallPolicy. Network Firewall
-- applies each stateless rule group to a packet starting with the group
-- that has the lowest priority setting. You must ensure that the priority
-- settings are unique within each policy.
statelessRuleGroupReference_priority :: Lens.Lens' StatelessRuleGroupReference Prelude.Natural
statelessRuleGroupReference_priority = Lens.lens (\StatelessRuleGroupReference' {priority} -> priority) (\s@StatelessRuleGroupReference' {} a -> s {priority = a} :: StatelessRuleGroupReference)

instance Data.FromJSON StatelessRuleGroupReference where
  parseJSON =
    Data.withObject
      "StatelessRuleGroupReference"
      ( \x ->
          StatelessRuleGroupReference'
            Prelude.<$> (x Data..: "ResourceArn")
            Prelude.<*> (x Data..: "Priority")
      )

instance Prelude.Hashable StatelessRuleGroupReference where
  hashWithSalt _salt StatelessRuleGroupReference' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` priority

instance Prelude.NFData StatelessRuleGroupReference where
  rnf StatelessRuleGroupReference' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf priority

instance Data.ToJSON StatelessRuleGroupReference where
  toJSON StatelessRuleGroupReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Priority" Data..= priority)
          ]
      )
