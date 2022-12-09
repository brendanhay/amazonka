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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingColor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingGradientColor
import Amazonka.QuickSight.Types.ConditionalFormattingSolidColor

-- | The formatting configuration for the color.
--
-- /See:/ 'newConditionalFormattingColor' smart constructor.
data ConditionalFormattingColor = ConditionalFormattingColor'
  { -- | Formatting configuration for gradient color.
    gradient :: Prelude.Maybe ConditionalFormattingGradientColor,
    -- | Formatting configuration for solid color.
    solid :: Prelude.Maybe ConditionalFormattingSolidColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gradient', 'conditionalFormattingColor_gradient' - Formatting configuration for gradient color.
--
-- 'solid', 'conditionalFormattingColor_solid' - Formatting configuration for solid color.
newConditionalFormattingColor ::
  ConditionalFormattingColor
newConditionalFormattingColor =
  ConditionalFormattingColor'
    { gradient =
        Prelude.Nothing,
      solid = Prelude.Nothing
    }

-- | Formatting configuration for gradient color.
conditionalFormattingColor_gradient :: Lens.Lens' ConditionalFormattingColor (Prelude.Maybe ConditionalFormattingGradientColor)
conditionalFormattingColor_gradient = Lens.lens (\ConditionalFormattingColor' {gradient} -> gradient) (\s@ConditionalFormattingColor' {} a -> s {gradient = a} :: ConditionalFormattingColor)

-- | Formatting configuration for solid color.
conditionalFormattingColor_solid :: Lens.Lens' ConditionalFormattingColor (Prelude.Maybe ConditionalFormattingSolidColor)
conditionalFormattingColor_solid = Lens.lens (\ConditionalFormattingColor' {solid} -> solid) (\s@ConditionalFormattingColor' {} a -> s {solid = a} :: ConditionalFormattingColor)

instance Data.FromJSON ConditionalFormattingColor where
  parseJSON =
    Data.withObject
      "ConditionalFormattingColor"
      ( \x ->
          ConditionalFormattingColor'
            Prelude.<$> (x Data..:? "Gradient")
            Prelude.<*> (x Data..:? "Solid")
      )

instance Prelude.Hashable ConditionalFormattingColor where
  hashWithSalt _salt ConditionalFormattingColor' {..} =
    _salt `Prelude.hashWithSalt` gradient
      `Prelude.hashWithSalt` solid

instance Prelude.NFData ConditionalFormattingColor where
  rnf ConditionalFormattingColor' {..} =
    Prelude.rnf gradient
      `Prelude.seq` Prelude.rnf solid

instance Data.ToJSON ConditionalFormattingColor where
  toJSON ConditionalFormattingColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Gradient" Data..=) Prelude.<$> gradient,
            ("Solid" Data..=) Prelude.<$> solid
          ]
      )
