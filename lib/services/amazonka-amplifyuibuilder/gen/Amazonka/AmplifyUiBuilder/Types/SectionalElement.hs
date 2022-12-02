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
-- Module      : Amazonka.AmplifyUiBuilder.Types.SectionalElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.SectionalElement where

import Amazonka.AmplifyUiBuilder.Types.FieldPosition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores the configuration information for a visual helper element for a
-- form. A sectional element can be a header, a text block, or a divider.
-- These elements are static and not associated with any data.
--
-- /See:/ 'newSectionalElement' smart constructor.
data SectionalElement = SectionalElement'
  { -- | Specifies the orientation for a @Divider@ sectional element. Valid
    -- values are @horizontal@ or @vertical@.
    orientation :: Prelude.Maybe Prelude.Text,
    -- | Specifies the size of the font for a @Heading@ sectional element. Valid
    -- values are @1 | 2 | 3 | 4 | 5 | 6@.
    level :: Prelude.Maybe Prelude.Int,
    -- | Specifies the position of the text in a field for a @Text@ sectional
    -- element.
    position :: Prelude.Maybe FieldPosition,
    -- | The text for a @Text@ sectional element.
    text :: Prelude.Maybe Prelude.Text,
    -- | The type of sectional element. Valid values are @Heading@, @Text@, and
    -- @Divider@.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionalElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orientation', 'sectionalElement_orientation' - Specifies the orientation for a @Divider@ sectional element. Valid
-- values are @horizontal@ or @vertical@.
--
-- 'level', 'sectionalElement_level' - Specifies the size of the font for a @Heading@ sectional element. Valid
-- values are @1 | 2 | 3 | 4 | 5 | 6@.
--
-- 'position', 'sectionalElement_position' - Specifies the position of the text in a field for a @Text@ sectional
-- element.
--
-- 'text', 'sectionalElement_text' - The text for a @Text@ sectional element.
--
-- 'type'', 'sectionalElement_type' - The type of sectional element. Valid values are @Heading@, @Text@, and
-- @Divider@.
newSectionalElement ::
  -- | 'type''
  Prelude.Text ->
  SectionalElement
newSectionalElement pType_ =
  SectionalElement'
    { orientation = Prelude.Nothing,
      level = Prelude.Nothing,
      position = Prelude.Nothing,
      text = Prelude.Nothing,
      type' = pType_
    }

-- | Specifies the orientation for a @Divider@ sectional element. Valid
-- values are @horizontal@ or @vertical@.
sectionalElement_orientation :: Lens.Lens' SectionalElement (Prelude.Maybe Prelude.Text)
sectionalElement_orientation = Lens.lens (\SectionalElement' {orientation} -> orientation) (\s@SectionalElement' {} a -> s {orientation = a} :: SectionalElement)

-- | Specifies the size of the font for a @Heading@ sectional element. Valid
-- values are @1 | 2 | 3 | 4 | 5 | 6@.
sectionalElement_level :: Lens.Lens' SectionalElement (Prelude.Maybe Prelude.Int)
sectionalElement_level = Lens.lens (\SectionalElement' {level} -> level) (\s@SectionalElement' {} a -> s {level = a} :: SectionalElement)

-- | Specifies the position of the text in a field for a @Text@ sectional
-- element.
sectionalElement_position :: Lens.Lens' SectionalElement (Prelude.Maybe FieldPosition)
sectionalElement_position = Lens.lens (\SectionalElement' {position} -> position) (\s@SectionalElement' {} a -> s {position = a} :: SectionalElement)

-- | The text for a @Text@ sectional element.
sectionalElement_text :: Lens.Lens' SectionalElement (Prelude.Maybe Prelude.Text)
sectionalElement_text = Lens.lens (\SectionalElement' {text} -> text) (\s@SectionalElement' {} a -> s {text = a} :: SectionalElement)

-- | The type of sectional element. Valid values are @Heading@, @Text@, and
-- @Divider@.
sectionalElement_type :: Lens.Lens' SectionalElement Prelude.Text
sectionalElement_type = Lens.lens (\SectionalElement' {type'} -> type') (\s@SectionalElement' {} a -> s {type' = a} :: SectionalElement)

instance Data.FromJSON SectionalElement where
  parseJSON =
    Data.withObject
      "SectionalElement"
      ( \x ->
          SectionalElement'
            Prelude.<$> (x Data..:? "orientation")
            Prelude.<*> (x Data..:? "level")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "text")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable SectionalElement where
  hashWithSalt _salt SectionalElement' {..} =
    _salt `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SectionalElement where
  rnf SectionalElement' {..} =
    Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON SectionalElement where
  toJSON SectionalElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("orientation" Data..=) Prelude.<$> orientation,
            ("level" Data..=) Prelude.<$> level,
            ("position" Data..=) Prelude.<$> position,
            ("text" Data..=) Prelude.<$> text,
            Prelude.Just ("type" Data..= type')
          ]
      )
