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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2WebAclDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2WebAclDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2RulesDetails
import Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigDetails

-- | Details about an WAFv2 web Access Control List (ACL).
--
-- /See:/ 'newAwsWafv2WebAclDetails' smart constructor.
data AwsWafv2WebAclDetails = AwsWafv2WebAclDetails'
  { -- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
    -- with the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The web ACL capacity units (WCUs) currently being used by this web ACL.
    capacity :: Prelude.Maybe Prelude.Integer,
    -- | Specifies how WAF should handle CAPTCHA evaluations for rules that
    -- don\'t have their own @CaptchaConfig@ settings.
    captchaConfig :: Prelude.Maybe AwsWafv2WebAclCaptchaConfigDetails,
    -- | The action to perform if none of the Rules contained in the web ACL
    -- match.
    defaultAction :: Prelude.Maybe AwsWafv2WebAclActionDetails,
    -- | A description of the web ACL that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the web ACL.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this web ACL is managed by Firewall Manager.
    managedbyFirewallManager :: Prelude.Maybe Prelude.Bool,
    -- | The name of the web ACL.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [AwsWafv2RulesDetails],
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: Prelude.Maybe AwsWafv2VisibilityConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2WebAclDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsWafv2WebAclDetails_arn' - The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
--
-- 'capacity', 'awsWafv2WebAclDetails_capacity' - The web ACL capacity units (WCUs) currently being used by this web ACL.
--
-- 'captchaConfig', 'awsWafv2WebAclDetails_captchaConfig' - Specifies how WAF should handle CAPTCHA evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings.
--
-- 'defaultAction', 'awsWafv2WebAclDetails_defaultAction' - The action to perform if none of the Rules contained in the web ACL
-- match.
--
-- 'description', 'awsWafv2WebAclDetails_description' - A description of the web ACL that helps with identification.
--
-- 'id', 'awsWafv2WebAclDetails_id' - A unique identifier for the web ACL.
--
-- 'managedbyFirewallManager', 'awsWafv2WebAclDetails_managedbyFirewallManager' - Indicates whether this web ACL is managed by Firewall Manager.
--
-- 'name', 'awsWafv2WebAclDetails_name' - The name of the web ACL.
--
-- 'rules', 'awsWafv2WebAclDetails_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'visibilityConfig', 'awsWafv2WebAclDetails_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newAwsWafv2WebAclDetails ::
  AwsWafv2WebAclDetails
newAwsWafv2WebAclDetails =
  AwsWafv2WebAclDetails'
    { arn = Prelude.Nothing,
      capacity = Prelude.Nothing,
      captchaConfig = Prelude.Nothing,
      defaultAction = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      managedbyFirewallManager = Prelude.Nothing,
      name = Prelude.Nothing,
      rules = Prelude.Nothing,
      visibilityConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
awsWafv2WebAclDetails_arn :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Text)
awsWafv2WebAclDetails_arn = Lens.lens (\AwsWafv2WebAclDetails' {arn} -> arn) (\s@AwsWafv2WebAclDetails' {} a -> s {arn = a} :: AwsWafv2WebAclDetails)

-- | The web ACL capacity units (WCUs) currently being used by this web ACL.
awsWafv2WebAclDetails_capacity :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Integer)
awsWafv2WebAclDetails_capacity = Lens.lens (\AwsWafv2WebAclDetails' {capacity} -> capacity) (\s@AwsWafv2WebAclDetails' {} a -> s {capacity = a} :: AwsWafv2WebAclDetails)

-- | Specifies how WAF should handle CAPTCHA evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings.
awsWafv2WebAclDetails_captchaConfig :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe AwsWafv2WebAclCaptchaConfigDetails)
awsWafv2WebAclDetails_captchaConfig = Lens.lens (\AwsWafv2WebAclDetails' {captchaConfig} -> captchaConfig) (\s@AwsWafv2WebAclDetails' {} a -> s {captchaConfig = a} :: AwsWafv2WebAclDetails)

-- | The action to perform if none of the Rules contained in the web ACL
-- match.
awsWafv2WebAclDetails_defaultAction :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe AwsWafv2WebAclActionDetails)
awsWafv2WebAclDetails_defaultAction = Lens.lens (\AwsWafv2WebAclDetails' {defaultAction} -> defaultAction) (\s@AwsWafv2WebAclDetails' {} a -> s {defaultAction = a} :: AwsWafv2WebAclDetails)

-- | A description of the web ACL that helps with identification.
awsWafv2WebAclDetails_description :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Text)
awsWafv2WebAclDetails_description = Lens.lens (\AwsWafv2WebAclDetails' {description} -> description) (\s@AwsWafv2WebAclDetails' {} a -> s {description = a} :: AwsWafv2WebAclDetails)

-- | A unique identifier for the web ACL.
awsWafv2WebAclDetails_id :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Text)
awsWafv2WebAclDetails_id = Lens.lens (\AwsWafv2WebAclDetails' {id} -> id) (\s@AwsWafv2WebAclDetails' {} a -> s {id = a} :: AwsWafv2WebAclDetails)

-- | Indicates whether this web ACL is managed by Firewall Manager.
awsWafv2WebAclDetails_managedbyFirewallManager :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Bool)
awsWafv2WebAclDetails_managedbyFirewallManager = Lens.lens (\AwsWafv2WebAclDetails' {managedbyFirewallManager} -> managedbyFirewallManager) (\s@AwsWafv2WebAclDetails' {} a -> s {managedbyFirewallManager = a} :: AwsWafv2WebAclDetails)

-- | The name of the web ACL.
awsWafv2WebAclDetails_name :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe Prelude.Text)
awsWafv2WebAclDetails_name = Lens.lens (\AwsWafv2WebAclDetails' {name} -> name) (\s@AwsWafv2WebAclDetails' {} a -> s {name = a} :: AwsWafv2WebAclDetails)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
awsWafv2WebAclDetails_rules :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe [AwsWafv2RulesDetails])
awsWafv2WebAclDetails_rules = Lens.lens (\AwsWafv2WebAclDetails' {rules} -> rules) (\s@AwsWafv2WebAclDetails' {} a -> s {rules = a} :: AwsWafv2WebAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
awsWafv2WebAclDetails_visibilityConfig :: Lens.Lens' AwsWafv2WebAclDetails (Prelude.Maybe AwsWafv2VisibilityConfigDetails)
awsWafv2WebAclDetails_visibilityConfig = Lens.lens (\AwsWafv2WebAclDetails' {visibilityConfig} -> visibilityConfig) (\s@AwsWafv2WebAclDetails' {} a -> s {visibilityConfig = a} :: AwsWafv2WebAclDetails)

instance Data.FromJSON AwsWafv2WebAclDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2WebAclDetails"
      ( \x ->
          AwsWafv2WebAclDetails'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "CaptchaConfig")
            Prelude.<*> (x Data..:? "DefaultAction")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "ManagedbyFirewallManager")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VisibilityConfig")
      )

instance Prelude.Hashable AwsWafv2WebAclDetails where
  hashWithSalt _salt AwsWafv2WebAclDetails' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` captchaConfig
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` managedbyFirewallManager
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData AwsWafv2WebAclDetails where
  rnf AwsWafv2WebAclDetails' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf captchaConfig
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf managedbyFirewallManager
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToJSON AwsWafv2WebAclDetails where
  toJSON AwsWafv2WebAclDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("Capacity" Data..=) Prelude.<$> capacity,
            ("CaptchaConfig" Data..=) Prelude.<$> captchaConfig,
            ("DefaultAction" Data..=) Prelude.<$> defaultAction,
            ("Description" Data..=) Prelude.<$> description,
            ("Id" Data..=) Prelude.<$> id,
            ("ManagedbyFirewallManager" Data..=)
              Prelude.<$> managedbyFirewallManager,
            ("Name" Data..=) Prelude.<$> name,
            ("Rules" Data..=) Prelude.<$> rules,
            ("VisibilityConfig" Data..=)
              Prelude.<$> visibilityConfig
          ]
      )
