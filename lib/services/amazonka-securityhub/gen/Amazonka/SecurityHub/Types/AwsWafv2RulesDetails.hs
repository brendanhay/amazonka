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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2RulesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2RulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails

-- | Provides details about rules in a rule group. A rule identifies web
-- requests that you want to allow, block, or count. Each rule includes one
-- top-level Statement that AWS WAF uses to identify matching web requests,
-- and parameters that govern how AWS WAF handles them.
--
-- /See:/ 'newAwsWafv2RulesDetails' smart constructor.
data AwsWafv2RulesDetails = AwsWafv2RulesDetails'
  { -- | The action that WAF should take on a web request when it matches the
    -- rule statement. Settings at the web ACL level can override the rule
    -- action setting.
    action :: Prelude.Maybe AwsWafv2RulesActionDetails,
    -- | The name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The action to use in the place of the action that results from the rule
    -- group evaluation.
    overrideAction :: Prelude.Maybe Prelude.Text,
    -- | If you define more than one Rule in a WebACL, WAF evaluates each request
    -- against the Rules in order based on the value of @Priority@. WAF
    -- processes rules with lower priority first. The priorities don\'t need to
    -- be consecutive, but they must all be different.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: Prelude.Maybe AwsWafv2VisibilityConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2RulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'awsWafv2RulesDetails_action' - The action that WAF should take on a web request when it matches the
-- rule statement. Settings at the web ACL level can override the rule
-- action setting.
--
-- 'name', 'awsWafv2RulesDetails_name' - The name of the rule.
--
-- 'overrideAction', 'awsWafv2RulesDetails_overrideAction' - The action to use in the place of the action that results from the rule
-- group evaluation.
--
-- 'priority', 'awsWafv2RulesDetails_priority' - If you define more than one Rule in a WebACL, WAF evaluates each request
-- against the Rules in order based on the value of @Priority@. WAF
-- processes rules with lower priority first. The priorities don\'t need to
-- be consecutive, but they must all be different.
--
-- 'visibilityConfig', 'awsWafv2RulesDetails_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newAwsWafv2RulesDetails ::
  AwsWafv2RulesDetails
newAwsWafv2RulesDetails =
  AwsWafv2RulesDetails'
    { action = Prelude.Nothing,
      name = Prelude.Nothing,
      overrideAction = Prelude.Nothing,
      priority = Prelude.Nothing,
      visibilityConfig = Prelude.Nothing
    }

-- | The action that WAF should take on a web request when it matches the
-- rule statement. Settings at the web ACL level can override the rule
-- action setting.
awsWafv2RulesDetails_action :: Lens.Lens' AwsWafv2RulesDetails (Prelude.Maybe AwsWafv2RulesActionDetails)
awsWafv2RulesDetails_action = Lens.lens (\AwsWafv2RulesDetails' {action} -> action) (\s@AwsWafv2RulesDetails' {} a -> s {action = a} :: AwsWafv2RulesDetails)

-- | The name of the rule.
awsWafv2RulesDetails_name :: Lens.Lens' AwsWafv2RulesDetails (Prelude.Maybe Prelude.Text)
awsWafv2RulesDetails_name = Lens.lens (\AwsWafv2RulesDetails' {name} -> name) (\s@AwsWafv2RulesDetails' {} a -> s {name = a} :: AwsWafv2RulesDetails)

-- | The action to use in the place of the action that results from the rule
-- group evaluation.
awsWafv2RulesDetails_overrideAction :: Lens.Lens' AwsWafv2RulesDetails (Prelude.Maybe Prelude.Text)
awsWafv2RulesDetails_overrideAction = Lens.lens (\AwsWafv2RulesDetails' {overrideAction} -> overrideAction) (\s@AwsWafv2RulesDetails' {} a -> s {overrideAction = a} :: AwsWafv2RulesDetails)

-- | If you define more than one Rule in a WebACL, WAF evaluates each request
-- against the Rules in order based on the value of @Priority@. WAF
-- processes rules with lower priority first. The priorities don\'t need to
-- be consecutive, but they must all be different.
awsWafv2RulesDetails_priority :: Lens.Lens' AwsWafv2RulesDetails (Prelude.Maybe Prelude.Int)
awsWafv2RulesDetails_priority = Lens.lens (\AwsWafv2RulesDetails' {priority} -> priority) (\s@AwsWafv2RulesDetails' {} a -> s {priority = a} :: AwsWafv2RulesDetails)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
awsWafv2RulesDetails_visibilityConfig :: Lens.Lens' AwsWafv2RulesDetails (Prelude.Maybe AwsWafv2VisibilityConfigDetails)
awsWafv2RulesDetails_visibilityConfig = Lens.lens (\AwsWafv2RulesDetails' {visibilityConfig} -> visibilityConfig) (\s@AwsWafv2RulesDetails' {} a -> s {visibilityConfig = a} :: AwsWafv2RulesDetails)

instance Data.FromJSON AwsWafv2RulesDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2RulesDetails"
      ( \x ->
          AwsWafv2RulesDetails'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OverrideAction")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "VisibilityConfig")
      )

instance Prelude.Hashable AwsWafv2RulesDetails where
  hashWithSalt _salt AwsWafv2RulesDetails' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overrideAction
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData AwsWafv2RulesDetails where
  rnf AwsWafv2RulesDetails' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overrideAction
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToJSON AwsWafv2RulesDetails where
  toJSON AwsWafv2RulesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Name" Data..=) Prelude.<$> name,
            ("OverrideAction" Data..=)
              Prelude.<$> overrideAction,
            ("Priority" Data..=) Prelude.<$> priority,
            ("VisibilityConfig" Data..=)
              Prelude.<$> visibilityConfig
          ]
      )
