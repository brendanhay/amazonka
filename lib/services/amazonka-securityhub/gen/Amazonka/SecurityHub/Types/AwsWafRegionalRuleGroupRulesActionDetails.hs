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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the action that WAF should take on a web request when it
-- matches the criteria defined in the rule.
--
-- /See:/ 'newAwsWafRegionalRuleGroupRulesActionDetails' smart constructor.
data AwsWafRegionalRuleGroupRulesActionDetails = AwsWafRegionalRuleGroupRulesActionDetails'
  { -- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
    -- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
    -- objects that you want to add to a rule and, for each object, indicates
    -- whether you want to negate the settings.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRuleGroupRulesActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsWafRegionalRuleGroupRulesActionDetails_type' - Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
newAwsWafRegionalRuleGroupRulesActionDetails ::
  AwsWafRegionalRuleGroupRulesActionDetails
newAwsWafRegionalRuleGroupRulesActionDetails =
  AwsWafRegionalRuleGroupRulesActionDetails'
    { type' =
        Prelude.Nothing
    }

-- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
awsWafRegionalRuleGroupRulesActionDetails_type :: Lens.Lens' AwsWafRegionalRuleGroupRulesActionDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleGroupRulesActionDetails_type = Lens.lens (\AwsWafRegionalRuleGroupRulesActionDetails' {type'} -> type') (\s@AwsWafRegionalRuleGroupRulesActionDetails' {} a -> s {type' = a} :: AwsWafRegionalRuleGroupRulesActionDetails)

instance
  Core.FromJSON
    AwsWafRegionalRuleGroupRulesActionDetails
  where
  parseJSON =
    Core.withObject
      "AwsWafRegionalRuleGroupRulesActionDetails"
      ( \x ->
          AwsWafRegionalRuleGroupRulesActionDetails'
            Prelude.<$> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRuleGroupRulesActionDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalRuleGroupRulesActionDetails' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalRuleGroupRulesActionDetails
  where
  rnf AwsWafRegionalRuleGroupRulesActionDetails' {..} =
    Prelude.rnf type'

instance
  Core.ToJSON
    AwsWafRegionalRuleGroupRulesActionDetails
  where
  toJSON AwsWafRegionalRuleGroupRulesActionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Type" Core..=) Prelude.<$> type']
      )
