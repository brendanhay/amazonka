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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingGradientColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingGradientColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GradientColor

-- | Formatting configuration for gradient color.
--
-- /See:/ 'newConditionalFormattingGradientColor' smart constructor.
data ConditionalFormattingGradientColor = ConditionalFormattingGradientColor'
  { -- | The expression that determines the formatting configuration for gradient
    -- color.
    expression :: Data.Sensitive Prelude.Text,
    -- | Determines the color.
    color :: GradientColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingGradientColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'conditionalFormattingGradientColor_expression' - The expression that determines the formatting configuration for gradient
-- color.
--
-- 'color', 'conditionalFormattingGradientColor_color' - Determines the color.
newConditionalFormattingGradientColor ::
  -- | 'expression'
  Prelude.Text ->
  -- | 'color'
  GradientColor ->
  ConditionalFormattingGradientColor
newConditionalFormattingGradientColor
  pExpression_
  pColor_ =
    ConditionalFormattingGradientColor'
      { expression =
          Data._Sensitive Lens.# pExpression_,
        color = pColor_
      }

-- | The expression that determines the formatting configuration for gradient
-- color.
conditionalFormattingGradientColor_expression :: Lens.Lens' ConditionalFormattingGradientColor Prelude.Text
conditionalFormattingGradientColor_expression = Lens.lens (\ConditionalFormattingGradientColor' {expression} -> expression) (\s@ConditionalFormattingGradientColor' {} a -> s {expression = a} :: ConditionalFormattingGradientColor) Prelude.. Data._Sensitive

-- | Determines the color.
conditionalFormattingGradientColor_color :: Lens.Lens' ConditionalFormattingGradientColor GradientColor
conditionalFormattingGradientColor_color = Lens.lens (\ConditionalFormattingGradientColor' {color} -> color) (\s@ConditionalFormattingGradientColor' {} a -> s {color = a} :: ConditionalFormattingGradientColor)

instance
  Data.FromJSON
    ConditionalFormattingGradientColor
  where
  parseJSON =
    Data.withObject
      "ConditionalFormattingGradientColor"
      ( \x ->
          ConditionalFormattingGradientColor'
            Prelude.<$> (x Data..: "Expression")
            Prelude.<*> (x Data..: "Color")
      )

instance
  Prelude.Hashable
    ConditionalFormattingGradientColor
  where
  hashWithSalt
    _salt
    ConditionalFormattingGradientColor' {..} =
      _salt
        `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` color

instance
  Prelude.NFData
    ConditionalFormattingGradientColor
  where
  rnf ConditionalFormattingGradientColor' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf color

instance
  Data.ToJSON
    ConditionalFormattingGradientColor
  where
  toJSON ConditionalFormattingGradientColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Expression" Data..= expression),
            Prelude.Just ("Color" Data..= color)
          ]
      )
