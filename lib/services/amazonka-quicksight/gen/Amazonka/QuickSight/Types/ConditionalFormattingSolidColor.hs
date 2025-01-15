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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingSolidColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingSolidColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Formatting configuration for solid color.
--
-- /See:/ 'newConditionalFormattingSolidColor' smart constructor.
data ConditionalFormattingSolidColor = ConditionalFormattingSolidColor'
  { -- | Determines the color.
    color :: Prelude.Maybe Prelude.Text,
    -- | The expression that determines the formatting configuration for solid
    -- color.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingSolidColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'conditionalFormattingSolidColor_color' - Determines the color.
--
-- 'expression', 'conditionalFormattingSolidColor_expression' - The expression that determines the formatting configuration for solid
-- color.
newConditionalFormattingSolidColor ::
  -- | 'expression'
  Prelude.Text ->
  ConditionalFormattingSolidColor
newConditionalFormattingSolidColor pExpression_ =
  ConditionalFormattingSolidColor'
    { color =
        Prelude.Nothing,
      expression =
        Data._Sensitive Lens.# pExpression_
    }

-- | Determines the color.
conditionalFormattingSolidColor_color :: Lens.Lens' ConditionalFormattingSolidColor (Prelude.Maybe Prelude.Text)
conditionalFormattingSolidColor_color = Lens.lens (\ConditionalFormattingSolidColor' {color} -> color) (\s@ConditionalFormattingSolidColor' {} a -> s {color = a} :: ConditionalFormattingSolidColor)

-- | The expression that determines the formatting configuration for solid
-- color.
conditionalFormattingSolidColor_expression :: Lens.Lens' ConditionalFormattingSolidColor Prelude.Text
conditionalFormattingSolidColor_expression = Lens.lens (\ConditionalFormattingSolidColor' {expression} -> expression) (\s@ConditionalFormattingSolidColor' {} a -> s {expression = a} :: ConditionalFormattingSolidColor) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    ConditionalFormattingSolidColor
  where
  parseJSON =
    Data.withObject
      "ConditionalFormattingSolidColor"
      ( \x ->
          ConditionalFormattingSolidColor'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..: "Expression")
      )

instance
  Prelude.Hashable
    ConditionalFormattingSolidColor
  where
  hashWithSalt
    _salt
    ConditionalFormattingSolidColor' {..} =
      _salt
        `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` expression

instance
  Prelude.NFData
    ConditionalFormattingSolidColor
  where
  rnf ConditionalFormattingSolidColor' {..} =
    Prelude.rnf color `Prelude.seq`
      Prelude.rnf expression

instance Data.ToJSON ConditionalFormattingSolidColor where
  toJSON ConditionalFormattingSolidColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
