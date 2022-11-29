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
-- Module      : Amazonka.QuickSight.Types.FilterOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A transform operation that filters rows based on a condition.
--
-- /See:/ 'newFilterOperation' smart constructor.
data FilterOperation = FilterOperation'
  { -- | An expression that must evaluate to a Boolean value. Rows for which the
    -- expression evaluates to true are kept in the dataset.
    conditionExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionExpression', 'filterOperation_conditionExpression' - An expression that must evaluate to a Boolean value. Rows for which the
-- expression evaluates to true are kept in the dataset.
newFilterOperation ::
  -- | 'conditionExpression'
  Prelude.Text ->
  FilterOperation
newFilterOperation pConditionExpression_ =
  FilterOperation'
    { conditionExpression =
        pConditionExpression_
    }

-- | An expression that must evaluate to a Boolean value. Rows for which the
-- expression evaluates to true are kept in the dataset.
filterOperation_conditionExpression :: Lens.Lens' FilterOperation Prelude.Text
filterOperation_conditionExpression = Lens.lens (\FilterOperation' {conditionExpression} -> conditionExpression) (\s@FilterOperation' {} a -> s {conditionExpression = a} :: FilterOperation)

instance Core.FromJSON FilterOperation where
  parseJSON =
    Core.withObject
      "FilterOperation"
      ( \x ->
          FilterOperation'
            Prelude.<$> (x Core..: "ConditionExpression")
      )

instance Prelude.Hashable FilterOperation where
  hashWithSalt _salt FilterOperation' {..} =
    _salt `Prelude.hashWithSalt` conditionExpression

instance Prelude.NFData FilterOperation where
  rnf FilterOperation' {..} =
    Prelude.rnf conditionExpression

instance Core.ToJSON FilterOperation where
  toJSON FilterOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConditionExpression" Core..= conditionExpression)
          ]
      )
