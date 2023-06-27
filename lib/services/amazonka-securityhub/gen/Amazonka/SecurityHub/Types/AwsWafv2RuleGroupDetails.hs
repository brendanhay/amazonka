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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2RuleGroupDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2RuleGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2RulesDetails
import Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails

-- | Details about an WAFv2 rule group.
--
-- /See:/ 'newAwsWafv2RuleGroupDetails' smart constructor.
data AwsWafv2RuleGroupDetails = AwsWafv2RuleGroupDetails'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The web ACL capacity units (WCUs) required for this rule group.
    capacity :: Prelude.Maybe Prelude.Integer,
    -- | A description of the rule group that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the rule group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [AwsWafv2RulesDetails],
    -- | Specifies whether the rule group is for an Amazon CloudFront
    -- distribution or for a regional application. A regional application can
    -- be an Application Load Balancer (ALB), an Amazon API Gateway REST API,
    -- an AppSync GraphQL API, or an Amazon Cognito user pool.
    scope :: Prelude.Maybe Prelude.Text,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: Prelude.Maybe AwsWafv2VisibilityConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2RuleGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsWafv2RuleGroupDetails_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'capacity', 'awsWafv2RuleGroupDetails_capacity' - The web ACL capacity units (WCUs) required for this rule group.
--
-- 'description', 'awsWafv2RuleGroupDetails_description' - A description of the rule group that helps with identification.
--
-- 'id', 'awsWafv2RuleGroupDetails_id' - A unique identifier for the rule group.
--
-- 'name', 'awsWafv2RuleGroupDetails_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'rules', 'awsWafv2RuleGroupDetails_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'scope', 'awsWafv2RuleGroupDetails_scope' - Specifies whether the rule group is for an Amazon CloudFront
-- distribution or for a regional application. A regional application can
-- be an Application Load Balancer (ALB), an Amazon API Gateway REST API,
-- an AppSync GraphQL API, or an Amazon Cognito user pool.
--
-- 'visibilityConfig', 'awsWafv2RuleGroupDetails_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newAwsWafv2RuleGroupDetails ::
  AwsWafv2RuleGroupDetails
newAwsWafv2RuleGroupDetails =
  AwsWafv2RuleGroupDetails'
    { arn = Prelude.Nothing,
      capacity = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      rules = Prelude.Nothing,
      scope = Prelude.Nothing,
      visibilityConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
awsWafv2RuleGroupDetails_arn :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafv2RuleGroupDetails_arn = Lens.lens (\AwsWafv2RuleGroupDetails' {arn} -> arn) (\s@AwsWafv2RuleGroupDetails' {} a -> s {arn = a} :: AwsWafv2RuleGroupDetails)

-- | The web ACL capacity units (WCUs) required for this rule group.
awsWafv2RuleGroupDetails_capacity :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Integer)
awsWafv2RuleGroupDetails_capacity = Lens.lens (\AwsWafv2RuleGroupDetails' {capacity} -> capacity) (\s@AwsWafv2RuleGroupDetails' {} a -> s {capacity = a} :: AwsWafv2RuleGroupDetails)

-- | A description of the rule group that helps with identification.
awsWafv2RuleGroupDetails_description :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafv2RuleGroupDetails_description = Lens.lens (\AwsWafv2RuleGroupDetails' {description} -> description) (\s@AwsWafv2RuleGroupDetails' {} a -> s {description = a} :: AwsWafv2RuleGroupDetails)

-- | A unique identifier for the rule group.
awsWafv2RuleGroupDetails_id :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafv2RuleGroupDetails_id = Lens.lens (\AwsWafv2RuleGroupDetails' {id} -> id) (\s@AwsWafv2RuleGroupDetails' {} a -> s {id = a} :: AwsWafv2RuleGroupDetails)

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
awsWafv2RuleGroupDetails_name :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafv2RuleGroupDetails_name = Lens.lens (\AwsWafv2RuleGroupDetails' {name} -> name) (\s@AwsWafv2RuleGroupDetails' {} a -> s {name = a} :: AwsWafv2RuleGroupDetails)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
awsWafv2RuleGroupDetails_rules :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe [AwsWafv2RulesDetails])
awsWafv2RuleGroupDetails_rules = Lens.lens (\AwsWafv2RuleGroupDetails' {rules} -> rules) (\s@AwsWafv2RuleGroupDetails' {} a -> s {rules = a} :: AwsWafv2RuleGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the rule group is for an Amazon CloudFront
-- distribution or for a regional application. A regional application can
-- be an Application Load Balancer (ALB), an Amazon API Gateway REST API,
-- an AppSync GraphQL API, or an Amazon Cognito user pool.
awsWafv2RuleGroupDetails_scope :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafv2RuleGroupDetails_scope = Lens.lens (\AwsWafv2RuleGroupDetails' {scope} -> scope) (\s@AwsWafv2RuleGroupDetails' {} a -> s {scope = a} :: AwsWafv2RuleGroupDetails)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
awsWafv2RuleGroupDetails_visibilityConfig :: Lens.Lens' AwsWafv2RuleGroupDetails (Prelude.Maybe AwsWafv2VisibilityConfigDetails)
awsWafv2RuleGroupDetails_visibilityConfig = Lens.lens (\AwsWafv2RuleGroupDetails' {visibilityConfig} -> visibilityConfig) (\s@AwsWafv2RuleGroupDetails' {} a -> s {visibilityConfig = a} :: AwsWafv2RuleGroupDetails)

instance Data.FromJSON AwsWafv2RuleGroupDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2RuleGroupDetails"
      ( \x ->
          AwsWafv2RuleGroupDetails'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Scope")
            Prelude.<*> (x Data..:? "VisibilityConfig")
      )

instance Prelude.Hashable AwsWafv2RuleGroupDetails where
  hashWithSalt _salt AwsWafv2RuleGroupDetails' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData AwsWafv2RuleGroupDetails where
  rnf AwsWafv2RuleGroupDetails' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToJSON AwsWafv2RuleGroupDetails where
  toJSON AwsWafv2RuleGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("Capacity" Data..=) Prelude.<$> capacity,
            ("Description" Data..=) Prelude.<$> description,
            ("Id" Data..=) Prelude.<$> id,
            ("Name" Data..=) Prelude.<$> name,
            ("Rules" Data..=) Prelude.<$> rules,
            ("Scope" Data..=) Prelude.<$> scope,
            ("VisibilityConfig" Data..=)
              Prelude.<$> visibilityConfig
          ]
      )
