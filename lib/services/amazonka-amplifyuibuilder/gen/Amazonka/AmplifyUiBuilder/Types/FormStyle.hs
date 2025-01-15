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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormStyle where

import Amazonka.AmplifyUiBuilder.Types.FormStyleConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for the form\'s style.
--
-- /See:/ 'newFormStyle' smart constructor.
data FormStyle = FormStyle'
  { -- | The spacing for the horizontal gap.
    horizontalGap :: Prelude.Maybe FormStyleConfig,
    -- | The size of the outer padding for the form.
    outerPadding :: Prelude.Maybe FormStyleConfig,
    -- | The spacing for the vertical gap.
    verticalGap :: Prelude.Maybe FormStyleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'horizontalGap', 'formStyle_horizontalGap' - The spacing for the horizontal gap.
--
-- 'outerPadding', 'formStyle_outerPadding' - The size of the outer padding for the form.
--
-- 'verticalGap', 'formStyle_verticalGap' - The spacing for the vertical gap.
newFormStyle ::
  FormStyle
newFormStyle =
  FormStyle'
    { horizontalGap = Prelude.Nothing,
      outerPadding = Prelude.Nothing,
      verticalGap = Prelude.Nothing
    }

-- | The spacing for the horizontal gap.
formStyle_horizontalGap :: Lens.Lens' FormStyle (Prelude.Maybe FormStyleConfig)
formStyle_horizontalGap = Lens.lens (\FormStyle' {horizontalGap} -> horizontalGap) (\s@FormStyle' {} a -> s {horizontalGap = a} :: FormStyle)

-- | The size of the outer padding for the form.
formStyle_outerPadding :: Lens.Lens' FormStyle (Prelude.Maybe FormStyleConfig)
formStyle_outerPadding = Lens.lens (\FormStyle' {outerPadding} -> outerPadding) (\s@FormStyle' {} a -> s {outerPadding = a} :: FormStyle)

-- | The spacing for the vertical gap.
formStyle_verticalGap :: Lens.Lens' FormStyle (Prelude.Maybe FormStyleConfig)
formStyle_verticalGap = Lens.lens (\FormStyle' {verticalGap} -> verticalGap) (\s@FormStyle' {} a -> s {verticalGap = a} :: FormStyle)

instance Data.FromJSON FormStyle where
  parseJSON =
    Data.withObject
      "FormStyle"
      ( \x ->
          FormStyle'
            Prelude.<$> (x Data..:? "horizontalGap")
            Prelude.<*> (x Data..:? "outerPadding")
            Prelude.<*> (x Data..:? "verticalGap")
      )

instance Prelude.Hashable FormStyle where
  hashWithSalt _salt FormStyle' {..} =
    _salt
      `Prelude.hashWithSalt` horizontalGap
      `Prelude.hashWithSalt` outerPadding
      `Prelude.hashWithSalt` verticalGap

instance Prelude.NFData FormStyle where
  rnf FormStyle' {..} =
    Prelude.rnf horizontalGap `Prelude.seq`
      Prelude.rnf outerPadding `Prelude.seq`
        Prelude.rnf verticalGap

instance Data.ToJSON FormStyle where
  toJSON FormStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("horizontalGap" Data..=) Prelude.<$> horizontalGap,
            ("outerPadding" Data..=) Prelude.<$> outerPadding,
            ("verticalGap" Data..=) Prelude.<$> verticalGap
          ]
      )
