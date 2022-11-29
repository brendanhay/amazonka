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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupVariablesPortSetsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupVariablesPortSetsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of port ranges.
--
-- /See:/ 'newRuleGroupVariablesPortSetsDetails' smart constructor.
data RuleGroupVariablesPortSetsDetails = RuleGroupVariablesPortSetsDetails'
  { -- | The list of port ranges.
    definition :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupVariablesPortSetsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'ruleGroupVariablesPortSetsDetails_definition' - The list of port ranges.
newRuleGroupVariablesPortSetsDetails ::
  RuleGroupVariablesPortSetsDetails
newRuleGroupVariablesPortSetsDetails =
  RuleGroupVariablesPortSetsDetails'
    { definition =
        Prelude.Nothing
    }

-- | The list of port ranges.
ruleGroupVariablesPortSetsDetails_definition :: Lens.Lens' RuleGroupVariablesPortSetsDetails (Prelude.Maybe [Prelude.Text])
ruleGroupVariablesPortSetsDetails_definition = Lens.lens (\RuleGroupVariablesPortSetsDetails' {definition} -> definition) (\s@RuleGroupVariablesPortSetsDetails' {} a -> s {definition = a} :: RuleGroupVariablesPortSetsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    RuleGroupVariablesPortSetsDetails
  where
  parseJSON =
    Core.withObject
      "RuleGroupVariablesPortSetsDetails"
      ( \x ->
          RuleGroupVariablesPortSetsDetails'
            Prelude.<$> (x Core..:? "Definition" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RuleGroupVariablesPortSetsDetails
  where
  hashWithSalt
    _salt
    RuleGroupVariablesPortSetsDetails' {..} =
      _salt `Prelude.hashWithSalt` definition

instance
  Prelude.NFData
    RuleGroupVariablesPortSetsDetails
  where
  rnf RuleGroupVariablesPortSetsDetails' {..} =
    Prelude.rnf definition

instance
  Core.ToJSON
    RuleGroupVariablesPortSetsDetails
  where
  toJSON RuleGroupVariablesPortSetsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Definition" Core..=) Prelude.<$> definition]
      )
