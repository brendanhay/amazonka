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
-- Module      : Amazonka.AccessAnalyzer.Types.InlineArchiveRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.InlineArchiveRule where

import Amazonka.AccessAnalyzer.Types.Criterion
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An criterion statement in an archive rule. Each archive rule may have
-- multiple criteria.
--
-- /See:/ 'newInlineArchiveRule' smart constructor.
data InlineArchiveRule = InlineArchiveRule'
  { -- | The condition and values for a criterion.
    filter' :: Prelude.HashMap Prelude.Text Criterion,
    -- | The name of the rule.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InlineArchiveRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'inlineArchiveRule_filter' - The condition and values for a criterion.
--
-- 'ruleName', 'inlineArchiveRule_ruleName' - The name of the rule.
newInlineArchiveRule ::
  -- | 'ruleName'
  Prelude.Text ->
  InlineArchiveRule
newInlineArchiveRule pRuleName_ =
  InlineArchiveRule'
    { filter' = Prelude.mempty,
      ruleName = pRuleName_
    }

-- | The condition and values for a criterion.
inlineArchiveRule_filter :: Lens.Lens' InlineArchiveRule (Prelude.HashMap Prelude.Text Criterion)
inlineArchiveRule_filter = Lens.lens (\InlineArchiveRule' {filter'} -> filter') (\s@InlineArchiveRule' {} a -> s {filter' = a} :: InlineArchiveRule) Prelude.. Lens.coerced

-- | The name of the rule.
inlineArchiveRule_ruleName :: Lens.Lens' InlineArchiveRule Prelude.Text
inlineArchiveRule_ruleName = Lens.lens (\InlineArchiveRule' {ruleName} -> ruleName) (\s@InlineArchiveRule' {} a -> s {ruleName = a} :: InlineArchiveRule)

instance Prelude.Hashable InlineArchiveRule

instance Prelude.NFData InlineArchiveRule

instance Core.ToJSON InlineArchiveRule where
  toJSON InlineArchiveRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filter" Core..= filter'),
            Prelude.Just ("ruleName" Core..= ruleName)
          ]
      )
