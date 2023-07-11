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
-- Module      : Amazonka.GamesParks.Types.Section
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.Section where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.Document
import qualified Amazonka.Prelude as Prelude

-- | The configuration section.
--
-- /See:/ 'newSection' smart constructor.
data Section = Section'
  { -- | The content of a configuration section.
    attributes :: Prelude.Maybe Document,
    -- | The name of the section.
    name :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the section contents.
    size :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Section' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'section_attributes' - The content of a configuration section.
--
-- 'name', 'section_name' - The name of the section.
--
-- 'size', 'section_size' - The size, in bytes, of the section contents.
newSection ::
  Section
newSection =
  Section'
    { attributes = Prelude.Nothing,
      name = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The content of a configuration section.
section_attributes :: Lens.Lens' Section (Prelude.Maybe Document)
section_attributes = Lens.lens (\Section' {attributes} -> attributes) (\s@Section' {} a -> s {attributes = a} :: Section)

-- | The name of the section.
section_name :: Lens.Lens' Section (Prelude.Maybe Prelude.Text)
section_name = Lens.lens (\Section' {name} -> name) (\s@Section' {} a -> s {name = a} :: Section)

-- | The size, in bytes, of the section contents.
section_size :: Lens.Lens' Section (Prelude.Maybe Prelude.Natural)
section_size = Lens.lens (\Section' {size} -> size) (\s@Section' {} a -> s {size = a} :: Section)

instance Data.FromJSON Section where
  parseJSON =
    Data.withObject
      "Section"
      ( \x ->
          Section'
            Prelude.<$> (x Data..:? "Attributes")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Size")
      )

instance Prelude.Hashable Section where
  hashWithSalt _salt Section' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` size

instance Prelude.NFData Section where
  rnf Section' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf size
