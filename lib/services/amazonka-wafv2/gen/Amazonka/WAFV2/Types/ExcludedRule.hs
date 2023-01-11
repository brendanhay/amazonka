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
-- Module      : Amazonka.WAFV2.Types.ExcludedRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ExcludedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a single rule in a rule group whose action you want to
-- override to @Count@.
--
-- Instead of this option, use @RuleActionOverrides@. It accepts any valid
-- action setting, including @Count@.
--
-- /See:/ 'newExcludedRule' smart constructor.
data ExcludedRule = ExcludedRule'
  { -- | The name of the rule whose action you want to override to @Count@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExcludedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'excludedRule_name' - The name of the rule whose action you want to override to @Count@.
newExcludedRule ::
  -- | 'name'
  Prelude.Text ->
  ExcludedRule
newExcludedRule pName_ = ExcludedRule' {name = pName_}

-- | The name of the rule whose action you want to override to @Count@.
excludedRule_name :: Lens.Lens' ExcludedRule Prelude.Text
excludedRule_name = Lens.lens (\ExcludedRule' {name} -> name) (\s@ExcludedRule' {} a -> s {name = a} :: ExcludedRule)

instance Data.FromJSON ExcludedRule where
  parseJSON =
    Data.withObject
      "ExcludedRule"
      (\x -> ExcludedRule' Prelude.<$> (x Data..: "Name"))

instance Prelude.Hashable ExcludedRule where
  hashWithSalt _salt ExcludedRule' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ExcludedRule where
  rnf ExcludedRule' {..} = Prelude.rnf name

instance Data.ToJSON ExcludedRule where
  toJSON ExcludedRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
