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
-- Module      : Amazonka.QuickSight.Types.ShapeConditionalFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ShapeConditionalFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor

-- | The shape conditional formatting of a filled map visual.
--
-- /See:/ 'newShapeConditionalFormat' smart constructor.
data ShapeConditionalFormat = ShapeConditionalFormat'
  { -- | The conditional formatting for the shape background color of a filled
    -- map visual.
    backgroundColor :: ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShapeConditionalFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'shapeConditionalFormat_backgroundColor' - The conditional formatting for the shape background color of a filled
-- map visual.
newShapeConditionalFormat ::
  -- | 'backgroundColor'
  ConditionalFormattingColor ->
  ShapeConditionalFormat
newShapeConditionalFormat pBackgroundColor_ =
  ShapeConditionalFormat'
    { backgroundColor =
        pBackgroundColor_
    }

-- | The conditional formatting for the shape background color of a filled
-- map visual.
shapeConditionalFormat_backgroundColor :: Lens.Lens' ShapeConditionalFormat ConditionalFormattingColor
shapeConditionalFormat_backgroundColor = Lens.lens (\ShapeConditionalFormat' {backgroundColor} -> backgroundColor) (\s@ShapeConditionalFormat' {} a -> s {backgroundColor = a} :: ShapeConditionalFormat)

instance Data.FromJSON ShapeConditionalFormat where
  parseJSON =
    Data.withObject
      "ShapeConditionalFormat"
      ( \x ->
          ShapeConditionalFormat'
            Prelude.<$> (x Data..: "BackgroundColor")
      )

instance Prelude.Hashable ShapeConditionalFormat where
  hashWithSalt _salt ShapeConditionalFormat' {..} =
    _salt `Prelude.hashWithSalt` backgroundColor

instance Prelude.NFData ShapeConditionalFormat where
  rnf ShapeConditionalFormat' {..} =
    Prelude.rnf backgroundColor

instance Data.ToJSON ShapeConditionalFormat where
  toJSON ShapeConditionalFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BackgroundColor" Data..= backgroundColor)
          ]
      )
