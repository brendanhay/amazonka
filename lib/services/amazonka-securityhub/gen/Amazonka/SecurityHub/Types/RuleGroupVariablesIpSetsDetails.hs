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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupVariablesIpSetsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupVariablesIpSetsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of IP addresses and address ranges, in CIDR notation.
--
-- /See:/ 'newRuleGroupVariablesIpSetsDetails' smart constructor.
data RuleGroupVariablesIpSetsDetails = RuleGroupVariablesIpSetsDetails'
  { -- | The list of IP addresses and ranges.
    definition :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupVariablesIpSetsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'ruleGroupVariablesIpSetsDetails_definition' - The list of IP addresses and ranges.
newRuleGroupVariablesIpSetsDetails ::
  RuleGroupVariablesIpSetsDetails
newRuleGroupVariablesIpSetsDetails =
  RuleGroupVariablesIpSetsDetails'
    { definition =
        Prelude.Nothing
    }

-- | The list of IP addresses and ranges.
ruleGroupVariablesIpSetsDetails_definition :: Lens.Lens' RuleGroupVariablesIpSetsDetails (Prelude.Maybe [Prelude.Text])
ruleGroupVariablesIpSetsDetails_definition = Lens.lens (\RuleGroupVariablesIpSetsDetails' {definition} -> definition) (\s@RuleGroupVariablesIpSetsDetails' {} a -> s {definition = a} :: RuleGroupVariablesIpSetsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RuleGroupVariablesIpSetsDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupVariablesIpSetsDetails"
      ( \x ->
          RuleGroupVariablesIpSetsDetails'
            Prelude.<$> (x Data..:? "Definition" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RuleGroupVariablesIpSetsDetails
  where
  hashWithSalt
    _salt
    RuleGroupVariablesIpSetsDetails' {..} =
      _salt `Prelude.hashWithSalt` definition

instance
  Prelude.NFData
    RuleGroupVariablesIpSetsDetails
  where
  rnf RuleGroupVariablesIpSetsDetails' {..} =
    Prelude.rnf definition

instance Data.ToJSON RuleGroupVariablesIpSetsDetails where
  toJSON RuleGroupVariablesIpSetsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Definition" Data..=) Prelude.<$> definition]
      )
