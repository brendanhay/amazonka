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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesActionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about what action WAF should take on a web request
-- when it matches the criteria defined in the rule.
--
-- /See:/ 'newAwsWafRuleGroupRulesActionDetails' smart constructor.
data AwsWafRuleGroupRulesActionDetails = AwsWafRuleGroupRulesActionDetails'
  { -- | The action that WAF should take on a web request when it matches the
    -- rule\'s statement.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRuleGroupRulesActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsWafRuleGroupRulesActionDetails_type' - The action that WAF should take on a web request when it matches the
-- rule\'s statement.
newAwsWafRuleGroupRulesActionDetails ::
  AwsWafRuleGroupRulesActionDetails
newAwsWafRuleGroupRulesActionDetails =
  AwsWafRuleGroupRulesActionDetails'
    { type' =
        Prelude.Nothing
    }

-- | The action that WAF should take on a web request when it matches the
-- rule\'s statement.
awsWafRuleGroupRulesActionDetails_type :: Lens.Lens' AwsWafRuleGroupRulesActionDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupRulesActionDetails_type = Lens.lens (\AwsWafRuleGroupRulesActionDetails' {type'} -> type') (\s@AwsWafRuleGroupRulesActionDetails' {} a -> s {type' = a} :: AwsWafRuleGroupRulesActionDetails)

instance
  Data.FromJSON
    AwsWafRuleGroupRulesActionDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRuleGroupRulesActionDetails"
      ( \x ->
          AwsWafRuleGroupRulesActionDetails'
            Prelude.<$> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRuleGroupRulesActionDetails
  where
  hashWithSalt
    _salt
    AwsWafRuleGroupRulesActionDetails' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRuleGroupRulesActionDetails
  where
  rnf AwsWafRuleGroupRulesActionDetails' {..} =
    Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRuleGroupRulesActionDetails
  where
  toJSON AwsWafRuleGroupRulesActionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Type" Data..=) Prelude.<$> type']
      )
