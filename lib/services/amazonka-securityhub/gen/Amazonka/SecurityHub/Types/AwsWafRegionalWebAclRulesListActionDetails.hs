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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListActionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The action that WAF takes when a web request matches all conditions in
-- the rule, such as allow, block, or count the request.
--
-- /See:/ 'newAwsWafRegionalWebAclRulesListActionDetails' smart constructor.
data AwsWafRegionalWebAclRulesListActionDetails = AwsWafRegionalWebAclRulesListActionDetails'
  { -- | For actions that are associated with a rule, the action that WAF takes
    -- when a web request matches all conditions in a rule.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalWebAclRulesListActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsWafRegionalWebAclRulesListActionDetails_type' - For actions that are associated with a rule, the action that WAF takes
-- when a web request matches all conditions in a rule.
newAwsWafRegionalWebAclRulesListActionDetails ::
  AwsWafRegionalWebAclRulesListActionDetails
newAwsWafRegionalWebAclRulesListActionDetails =
  AwsWafRegionalWebAclRulesListActionDetails'
    { type' =
        Prelude.Nothing
    }

-- | For actions that are associated with a rule, the action that WAF takes
-- when a web request matches all conditions in a rule.
awsWafRegionalWebAclRulesListActionDetails_type :: Lens.Lens' AwsWafRegionalWebAclRulesListActionDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclRulesListActionDetails_type = Lens.lens (\AwsWafRegionalWebAclRulesListActionDetails' {type'} -> type') (\s@AwsWafRegionalWebAclRulesListActionDetails' {} a -> s {type' = a} :: AwsWafRegionalWebAclRulesListActionDetails)

instance
  Core.FromJSON
    AwsWafRegionalWebAclRulesListActionDetails
  where
  parseJSON =
    Core.withObject
      "AwsWafRegionalWebAclRulesListActionDetails"
      ( \x ->
          AwsWafRegionalWebAclRulesListActionDetails'
            Prelude.<$> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalWebAclRulesListActionDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalWebAclRulesListActionDetails' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalWebAclRulesListActionDetails
  where
  rnf AwsWafRegionalWebAclRulesListActionDetails' {..} =
    Prelude.rnf type'

instance
  Core.ToJSON
    AwsWafRegionalWebAclRulesListActionDetails
  where
  toJSON
    AwsWafRegionalWebAclRulesListActionDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Type" Core..=) Prelude.<$> type']
        )
