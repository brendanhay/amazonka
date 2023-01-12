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
-- Module      : Amazonka.QuickSight.Types.Font
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Font where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines the font settings.
--
-- /See:/ 'newFont' smart constructor.
data Font = Font'
  { -- | Determines the font family settings.
    fontFamily :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Font' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fontFamily', 'font_fontFamily' - Determines the font family settings.
newFont ::
  Font
newFont = Font' {fontFamily = Prelude.Nothing}

-- | Determines the font family settings.
font_fontFamily :: Lens.Lens' Font (Prelude.Maybe Prelude.Text)
font_fontFamily = Lens.lens (\Font' {fontFamily} -> fontFamily) (\s@Font' {} a -> s {fontFamily = a} :: Font)

instance Data.FromJSON Font where
  parseJSON =
    Data.withObject
      "Font"
      (\x -> Font' Prelude.<$> (x Data..:? "FontFamily"))

instance Prelude.Hashable Font where
  hashWithSalt _salt Font' {..} =
    _salt `Prelude.hashWithSalt` fontFamily

instance Prelude.NFData Font where
  rnf Font' {..} = Prelude.rnf fontFamily

instance Data.ToJSON Font where
  toJSON Font' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FontFamily" Data..=) Prelude.<$> fontFamily]
      )
