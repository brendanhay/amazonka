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
-- Module      : Amazonka.QuickSight.Types.SectionStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Spacing

-- | The options that style a section.
--
-- /See:/ 'newSectionStyle' smart constructor.
data SectionStyle = SectionStyle'
  { -- | The height of a section.
    --
    -- Heights can only be defined for header and footer sections. The default
    -- height margin is 0.5 inches.
    height :: Prelude.Maybe Prelude.Text,
    -- | The spacing between section content and its top, bottom, left, and right
    -- edges.
    --
    -- There is no padding by default.
    padding :: Prelude.Maybe Spacing
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'sectionStyle_height' - The height of a section.
--
-- Heights can only be defined for header and footer sections. The default
-- height margin is 0.5 inches.
--
-- 'padding', 'sectionStyle_padding' - The spacing between section content and its top, bottom, left, and right
-- edges.
--
-- There is no padding by default.
newSectionStyle ::
  SectionStyle
newSectionStyle =
  SectionStyle'
    { height = Prelude.Nothing,
      padding = Prelude.Nothing
    }

-- | The height of a section.
--
-- Heights can only be defined for header and footer sections. The default
-- height margin is 0.5 inches.
sectionStyle_height :: Lens.Lens' SectionStyle (Prelude.Maybe Prelude.Text)
sectionStyle_height = Lens.lens (\SectionStyle' {height} -> height) (\s@SectionStyle' {} a -> s {height = a} :: SectionStyle)

-- | The spacing between section content and its top, bottom, left, and right
-- edges.
--
-- There is no padding by default.
sectionStyle_padding :: Lens.Lens' SectionStyle (Prelude.Maybe Spacing)
sectionStyle_padding = Lens.lens (\SectionStyle' {padding} -> padding) (\s@SectionStyle' {} a -> s {padding = a} :: SectionStyle)

instance Data.FromJSON SectionStyle where
  parseJSON =
    Data.withObject
      "SectionStyle"
      ( \x ->
          SectionStyle'
            Prelude.<$> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "Padding")
      )

instance Prelude.Hashable SectionStyle where
  hashWithSalt _salt SectionStyle' {..} =
    _salt
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` padding

instance Prelude.NFData SectionStyle where
  rnf SectionStyle' {..} =
    Prelude.rnf height
      `Prelude.seq` Prelude.rnf padding

instance Data.ToJSON SectionStyle where
  toJSON SectionStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Height" Data..=) Prelude.<$> height,
            ("Padding" Data..=) Prelude.<$> padding
          ]
      )
