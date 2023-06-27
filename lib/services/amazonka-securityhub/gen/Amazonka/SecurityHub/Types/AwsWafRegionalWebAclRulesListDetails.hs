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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListActionDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListOverrideActionDetails

-- | A combination of @ByteMatchSet@, @IPSet@, and\/or @SqlInjectionMatchSet@
-- objects that identify the web requests that you want to allow, block, or
-- count.
--
-- /See:/ 'newAwsWafRegionalWebAclRulesListDetails' smart constructor.
data AwsWafRegionalWebAclRulesListDetails = AwsWafRegionalWebAclRulesListDetails'
  { -- | The action that WAF takes when a web request matches all conditions in
    -- the rule, such as allow, block, or count the request.
    action :: Prelude.Maybe AwsWafRegionalWebAclRulesListActionDetails,
    -- | Overrides the rule evaluation result in the rule group.
    overrideAction :: Prelude.Maybe AwsWafRegionalWebAclRulesListOverrideActionDetails,
    -- | The order in which WAF evaluates the rules in a web ACL.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The ID of an WAF Regional rule to associate with a web ACL.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | For actions that are associated with a rule, the action that WAF takes
    -- when a web request matches all conditions in a rule.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalWebAclRulesListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'awsWafRegionalWebAclRulesListDetails_action' - The action that WAF takes when a web request matches all conditions in
-- the rule, such as allow, block, or count the request.
--
-- 'overrideAction', 'awsWafRegionalWebAclRulesListDetails_overrideAction' - Overrides the rule evaluation result in the rule group.
--
-- 'priority', 'awsWafRegionalWebAclRulesListDetails_priority' - The order in which WAF evaluates the rules in a web ACL.
--
-- 'ruleId', 'awsWafRegionalWebAclRulesListDetails_ruleId' - The ID of an WAF Regional rule to associate with a web ACL.
--
-- 'type'', 'awsWafRegionalWebAclRulesListDetails_type' - For actions that are associated with a rule, the action that WAF takes
-- when a web request matches all conditions in a rule.
newAwsWafRegionalWebAclRulesListDetails ::
  AwsWafRegionalWebAclRulesListDetails
newAwsWafRegionalWebAclRulesListDetails =
  AwsWafRegionalWebAclRulesListDetails'
    { action =
        Prelude.Nothing,
      overrideAction = Prelude.Nothing,
      priority = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The action that WAF takes when a web request matches all conditions in
-- the rule, such as allow, block, or count the request.
awsWafRegionalWebAclRulesListDetails_action :: Lens.Lens' AwsWafRegionalWebAclRulesListDetails (Prelude.Maybe AwsWafRegionalWebAclRulesListActionDetails)
awsWafRegionalWebAclRulesListDetails_action = Lens.lens (\AwsWafRegionalWebAclRulesListDetails' {action} -> action) (\s@AwsWafRegionalWebAclRulesListDetails' {} a -> s {action = a} :: AwsWafRegionalWebAclRulesListDetails)

-- | Overrides the rule evaluation result in the rule group.
awsWafRegionalWebAclRulesListDetails_overrideAction :: Lens.Lens' AwsWafRegionalWebAclRulesListDetails (Prelude.Maybe AwsWafRegionalWebAclRulesListOverrideActionDetails)
awsWafRegionalWebAclRulesListDetails_overrideAction = Lens.lens (\AwsWafRegionalWebAclRulesListDetails' {overrideAction} -> overrideAction) (\s@AwsWafRegionalWebAclRulesListDetails' {} a -> s {overrideAction = a} :: AwsWafRegionalWebAclRulesListDetails)

-- | The order in which WAF evaluates the rules in a web ACL.
awsWafRegionalWebAclRulesListDetails_priority :: Lens.Lens' AwsWafRegionalWebAclRulesListDetails (Prelude.Maybe Prelude.Int)
awsWafRegionalWebAclRulesListDetails_priority = Lens.lens (\AwsWafRegionalWebAclRulesListDetails' {priority} -> priority) (\s@AwsWafRegionalWebAclRulesListDetails' {} a -> s {priority = a} :: AwsWafRegionalWebAclRulesListDetails)

-- | The ID of an WAF Regional rule to associate with a web ACL.
awsWafRegionalWebAclRulesListDetails_ruleId :: Lens.Lens' AwsWafRegionalWebAclRulesListDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclRulesListDetails_ruleId = Lens.lens (\AwsWafRegionalWebAclRulesListDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalWebAclRulesListDetails' {} a -> s {ruleId = a} :: AwsWafRegionalWebAclRulesListDetails)

-- | For actions that are associated with a rule, the action that WAF takes
-- when a web request matches all conditions in a rule.
awsWafRegionalWebAclRulesListDetails_type :: Lens.Lens' AwsWafRegionalWebAclRulesListDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclRulesListDetails_type = Lens.lens (\AwsWafRegionalWebAclRulesListDetails' {type'} -> type') (\s@AwsWafRegionalWebAclRulesListDetails' {} a -> s {type' = a} :: AwsWafRegionalWebAclRulesListDetails)

instance
  Data.FromJSON
    AwsWafRegionalWebAclRulesListDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalWebAclRulesListDetails"
      ( \x ->
          AwsWafRegionalWebAclRulesListDetails'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "OverrideAction")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "RuleId")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalWebAclRulesListDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalWebAclRulesListDetails' {..} =
      _salt
        `Prelude.hashWithSalt` action
        `Prelude.hashWithSalt` overrideAction
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` ruleId
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalWebAclRulesListDetails
  where
  rnf AwsWafRegionalWebAclRulesListDetails' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf overrideAction
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRegionalWebAclRulesListDetails
  where
  toJSON AwsWafRegionalWebAclRulesListDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("OverrideAction" Data..=)
              Prelude.<$> overrideAction,
            ("Priority" Data..=) Prelude.<$> priority,
            ("RuleId" Data..=) Prelude.<$> ruleId,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
