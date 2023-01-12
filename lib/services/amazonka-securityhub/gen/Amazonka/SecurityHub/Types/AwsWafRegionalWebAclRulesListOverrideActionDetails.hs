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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListOverrideActionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListOverrideActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the action to use in the place of the action that
-- results from the rule group evaluation.
--
-- /See:/ 'newAwsWafRegionalWebAclRulesListOverrideActionDetails' smart constructor.
data AwsWafRegionalWebAclRulesListOverrideActionDetails = AwsWafRegionalWebAclRulesListOverrideActionDetails'
  { -- | Overrides the rule evaluation result in the rule group.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalWebAclRulesListOverrideActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsWafRegionalWebAclRulesListOverrideActionDetails_type' - Overrides the rule evaluation result in the rule group.
newAwsWafRegionalWebAclRulesListOverrideActionDetails ::
  AwsWafRegionalWebAclRulesListOverrideActionDetails
newAwsWafRegionalWebAclRulesListOverrideActionDetails =
  AwsWafRegionalWebAclRulesListOverrideActionDetails'
    { type' =
        Prelude.Nothing
    }

-- | Overrides the rule evaluation result in the rule group.
awsWafRegionalWebAclRulesListOverrideActionDetails_type :: Lens.Lens' AwsWafRegionalWebAclRulesListOverrideActionDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclRulesListOverrideActionDetails_type = Lens.lens (\AwsWafRegionalWebAclRulesListOverrideActionDetails' {type'} -> type') (\s@AwsWafRegionalWebAclRulesListOverrideActionDetails' {} a -> s {type' = a} :: AwsWafRegionalWebAclRulesListOverrideActionDetails)

instance
  Data.FromJSON
    AwsWafRegionalWebAclRulesListOverrideActionDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalWebAclRulesListOverrideActionDetails"
      ( \x ->
          AwsWafRegionalWebAclRulesListOverrideActionDetails'
            Prelude.<$> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalWebAclRulesListOverrideActionDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalWebAclRulesListOverrideActionDetails' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalWebAclRulesListOverrideActionDetails
  where
  rnf
    AwsWafRegionalWebAclRulesListOverrideActionDetails' {..} =
      Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRegionalWebAclRulesListOverrideActionDetails
  where
  toJSON
    AwsWafRegionalWebAclRulesListOverrideActionDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Type" Data..=) Prelude.<$> type']
        )
