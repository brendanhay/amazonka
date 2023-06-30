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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingCustomIconCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingCustomIconCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingCustomIconOptions
import Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayConfiguration

-- | Determines the custom condition for an icon set.
--
-- /See:/ 'newConditionalFormattingCustomIconCondition' smart constructor.
data ConditionalFormattingCustomIconCondition = ConditionalFormattingCustomIconCondition'
  { -- | Determines the color of the icon.
    color :: Prelude.Maybe Prelude.Text,
    -- | Determines the icon display configuration.
    displayConfiguration :: Prelude.Maybe ConditionalFormattingIconDisplayConfiguration,
    -- | The expression that determines the condition of the icon set.
    expression :: Data.Sensitive Prelude.Text,
    -- | Custom icon options for an icon set.
    iconOptions :: ConditionalFormattingCustomIconOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingCustomIconCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'conditionalFormattingCustomIconCondition_color' - Determines the color of the icon.
--
-- 'displayConfiguration', 'conditionalFormattingCustomIconCondition_displayConfiguration' - Determines the icon display configuration.
--
-- 'expression', 'conditionalFormattingCustomIconCondition_expression' - The expression that determines the condition of the icon set.
--
-- 'iconOptions', 'conditionalFormattingCustomIconCondition_iconOptions' - Custom icon options for an icon set.
newConditionalFormattingCustomIconCondition ::
  -- | 'expression'
  Prelude.Text ->
  -- | 'iconOptions'
  ConditionalFormattingCustomIconOptions ->
  ConditionalFormattingCustomIconCondition
newConditionalFormattingCustomIconCondition
  pExpression_
  pIconOptions_ =
    ConditionalFormattingCustomIconCondition'
      { color =
          Prelude.Nothing,
        displayConfiguration =
          Prelude.Nothing,
        expression =
          Data._Sensitive
            Lens.# pExpression_,
        iconOptions = pIconOptions_
      }

-- | Determines the color of the icon.
conditionalFormattingCustomIconCondition_color :: Lens.Lens' ConditionalFormattingCustomIconCondition (Prelude.Maybe Prelude.Text)
conditionalFormattingCustomIconCondition_color = Lens.lens (\ConditionalFormattingCustomIconCondition' {color} -> color) (\s@ConditionalFormattingCustomIconCondition' {} a -> s {color = a} :: ConditionalFormattingCustomIconCondition)

-- | Determines the icon display configuration.
conditionalFormattingCustomIconCondition_displayConfiguration :: Lens.Lens' ConditionalFormattingCustomIconCondition (Prelude.Maybe ConditionalFormattingIconDisplayConfiguration)
conditionalFormattingCustomIconCondition_displayConfiguration = Lens.lens (\ConditionalFormattingCustomIconCondition' {displayConfiguration} -> displayConfiguration) (\s@ConditionalFormattingCustomIconCondition' {} a -> s {displayConfiguration = a} :: ConditionalFormattingCustomIconCondition)

-- | The expression that determines the condition of the icon set.
conditionalFormattingCustomIconCondition_expression :: Lens.Lens' ConditionalFormattingCustomIconCondition Prelude.Text
conditionalFormattingCustomIconCondition_expression = Lens.lens (\ConditionalFormattingCustomIconCondition' {expression} -> expression) (\s@ConditionalFormattingCustomIconCondition' {} a -> s {expression = a} :: ConditionalFormattingCustomIconCondition) Prelude.. Data._Sensitive

-- | Custom icon options for an icon set.
conditionalFormattingCustomIconCondition_iconOptions :: Lens.Lens' ConditionalFormattingCustomIconCondition ConditionalFormattingCustomIconOptions
conditionalFormattingCustomIconCondition_iconOptions = Lens.lens (\ConditionalFormattingCustomIconCondition' {iconOptions} -> iconOptions) (\s@ConditionalFormattingCustomIconCondition' {} a -> s {iconOptions = a} :: ConditionalFormattingCustomIconCondition)

instance
  Data.FromJSON
    ConditionalFormattingCustomIconCondition
  where
  parseJSON =
    Data.withObject
      "ConditionalFormattingCustomIconCondition"
      ( \x ->
          ConditionalFormattingCustomIconCondition'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "DisplayConfiguration")
            Prelude.<*> (x Data..: "Expression")
            Prelude.<*> (x Data..: "IconOptions")
      )

instance
  Prelude.Hashable
    ConditionalFormattingCustomIconCondition
  where
  hashWithSalt
    _salt
    ConditionalFormattingCustomIconCondition' {..} =
      _salt
        `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` displayConfiguration
        `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` iconOptions

instance
  Prelude.NFData
    ConditionalFormattingCustomIconCondition
  where
  rnf ConditionalFormattingCustomIconCondition' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf displayConfiguration
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf iconOptions

instance
  Data.ToJSON
    ConditionalFormattingCustomIconCondition
  where
  toJSON ConditionalFormattingCustomIconCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("DisplayConfiguration" Data..=)
              Prelude.<$> displayConfiguration,
            Prelude.Just ("Expression" Data..= expression),
            Prelude.Just ("IconOptions" Data..= iconOptions)
          ]
      )
