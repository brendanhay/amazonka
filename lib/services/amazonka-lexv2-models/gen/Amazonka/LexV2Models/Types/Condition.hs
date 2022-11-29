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
-- Module      : Amazonka.LexV2Models.Types.Condition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides an expression that evaluates to true or false.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The expression string that is evaluated.
    expressionString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionString', 'condition_expressionString' - The expression string that is evaluated.
newCondition ::
  -- | 'expressionString'
  Prelude.Text ->
  Condition
newCondition pExpressionString_ =
  Condition' {expressionString = pExpressionString_}

-- | The expression string that is evaluated.
condition_expressionString :: Lens.Lens' Condition Prelude.Text
condition_expressionString = Lens.lens (\Condition' {expressionString} -> expressionString) (\s@Condition' {} a -> s {expressionString = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..: "expressionString")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` expressionString

instance Prelude.NFData Condition where
  rnf Condition' {..} = Prelude.rnf expressionString

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("expressionString" Core..= expressionString)
          ]
      )
