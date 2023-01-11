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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingIconSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingIconSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingIconSetType

-- | Formatting configuration for icon set.
--
-- /See:/ 'newConditionalFormattingIconSet' smart constructor.
data ConditionalFormattingIconSet = ConditionalFormattingIconSet'
  { -- | Determines the icon set type.
    iconSetType :: Prelude.Maybe ConditionalFormattingIconSetType,
    -- | The expression that determines the formatting configuration for the icon
    -- set.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingIconSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iconSetType', 'conditionalFormattingIconSet_iconSetType' - Determines the icon set type.
--
-- 'expression', 'conditionalFormattingIconSet_expression' - The expression that determines the formatting configuration for the icon
-- set.
newConditionalFormattingIconSet ::
  -- | 'expression'
  Prelude.Text ->
  ConditionalFormattingIconSet
newConditionalFormattingIconSet pExpression_ =
  ConditionalFormattingIconSet'
    { iconSetType =
        Prelude.Nothing,
      expression =
        Data._Sensitive Lens.# pExpression_
    }

-- | Determines the icon set type.
conditionalFormattingIconSet_iconSetType :: Lens.Lens' ConditionalFormattingIconSet (Prelude.Maybe ConditionalFormattingIconSetType)
conditionalFormattingIconSet_iconSetType = Lens.lens (\ConditionalFormattingIconSet' {iconSetType} -> iconSetType) (\s@ConditionalFormattingIconSet' {} a -> s {iconSetType = a} :: ConditionalFormattingIconSet)

-- | The expression that determines the formatting configuration for the icon
-- set.
conditionalFormattingIconSet_expression :: Lens.Lens' ConditionalFormattingIconSet Prelude.Text
conditionalFormattingIconSet_expression = Lens.lens (\ConditionalFormattingIconSet' {expression} -> expression) (\s@ConditionalFormattingIconSet' {} a -> s {expression = a} :: ConditionalFormattingIconSet) Prelude.. Data._Sensitive

instance Data.FromJSON ConditionalFormattingIconSet where
  parseJSON =
    Data.withObject
      "ConditionalFormattingIconSet"
      ( \x ->
          ConditionalFormattingIconSet'
            Prelude.<$> (x Data..:? "IconSetType")
            Prelude.<*> (x Data..: "Expression")
      )

instance
  Prelude.Hashable
    ConditionalFormattingIconSet
  where
  hashWithSalt _salt ConditionalFormattingIconSet' {..} =
    _salt `Prelude.hashWithSalt` iconSetType
      `Prelude.hashWithSalt` expression

instance Prelude.NFData ConditionalFormattingIconSet where
  rnf ConditionalFormattingIconSet' {..} =
    Prelude.rnf iconSetType
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON ConditionalFormattingIconSet where
  toJSON ConditionalFormattingIconSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IconSetType" Data..=) Prelude.<$> iconSetType,
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
