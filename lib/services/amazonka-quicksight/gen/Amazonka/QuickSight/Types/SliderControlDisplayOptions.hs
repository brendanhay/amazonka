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
-- Module      : Amazonka.QuickSight.Types.SliderControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SliderControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions

-- | The display options of a control.
--
-- /See:/ 'newSliderControlDisplayOptions' smart constructor.
data SliderControlDisplayOptions = SliderControlDisplayOptions'
  { -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SliderControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'titleOptions', 'sliderControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newSliderControlDisplayOptions ::
  SliderControlDisplayOptions
newSliderControlDisplayOptions =
  SliderControlDisplayOptions'
    { titleOptions =
        Prelude.Nothing
    }

-- | The options to configure the title visibility, name, and font size.
sliderControlDisplayOptions_titleOptions :: Lens.Lens' SliderControlDisplayOptions (Prelude.Maybe LabelOptions)
sliderControlDisplayOptions_titleOptions = Lens.lens (\SliderControlDisplayOptions' {titleOptions} -> titleOptions) (\s@SliderControlDisplayOptions' {} a -> s {titleOptions = a} :: SliderControlDisplayOptions)

instance Data.FromJSON SliderControlDisplayOptions where
  parseJSON =
    Data.withObject
      "SliderControlDisplayOptions"
      ( \x ->
          SliderControlDisplayOptions'
            Prelude.<$> (x Data..:? "TitleOptions")
      )

instance Prelude.Hashable SliderControlDisplayOptions where
  hashWithSalt _salt SliderControlDisplayOptions' {..} =
    _salt `Prelude.hashWithSalt` titleOptions

instance Prelude.NFData SliderControlDisplayOptions where
  rnf SliderControlDisplayOptions' {..} =
    Prelude.rnf titleOptions

instance Data.ToJSON SliderControlDisplayOptions where
  toJSON SliderControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TitleOptions" Data..=) Prelude.<$> titleOptions]
      )
