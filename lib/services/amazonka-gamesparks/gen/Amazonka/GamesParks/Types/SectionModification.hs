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
-- Module      : Amazonka.GamesParks.Types.SectionModification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.SectionModification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.Document
import Amazonka.GamesParks.Types.Operation
import qualified Amazonka.Prelude as Prelude

-- | A single modification to the configuration section.
--
-- /See:/ 'newSectionModification' smart constructor.
data SectionModification = SectionModification'
  { -- | For add and replace operations, this is the value that will be used.
    --
    -- This field should be omitted for delete operations.
    value :: Prelude.Maybe Document,
    -- | The operation to be performed on a configuration section.
    --
    -- Content can be added, deleted, or replaced within a section.
    operation :: Operation,
    -- | The path within the section content to be modified.
    path :: Prelude.Text,
    -- | The name of the section to be modified.
    section :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'sectionModification_value' - For add and replace operations, this is the value that will be used.
--
-- This field should be omitted for delete operations.
--
-- 'operation', 'sectionModification_operation' - The operation to be performed on a configuration section.
--
-- Content can be added, deleted, or replaced within a section.
--
-- 'path', 'sectionModification_path' - The path within the section content to be modified.
--
-- 'section', 'sectionModification_section' - The name of the section to be modified.
newSectionModification ::
  -- | 'operation'
  Operation ->
  -- | 'path'
  Prelude.Text ->
  -- | 'section'
  Prelude.Text ->
  SectionModification
newSectionModification pOperation_ pPath_ pSection_ =
  SectionModification'
    { value = Prelude.Nothing,
      operation = pOperation_,
      path = pPath_,
      section = pSection_
    }

-- | For add and replace operations, this is the value that will be used.
--
-- This field should be omitted for delete operations.
sectionModification_value :: Lens.Lens' SectionModification (Prelude.Maybe Document)
sectionModification_value = Lens.lens (\SectionModification' {value} -> value) (\s@SectionModification' {} a -> s {value = a} :: SectionModification)

-- | The operation to be performed on a configuration section.
--
-- Content can be added, deleted, or replaced within a section.
sectionModification_operation :: Lens.Lens' SectionModification Operation
sectionModification_operation = Lens.lens (\SectionModification' {operation} -> operation) (\s@SectionModification' {} a -> s {operation = a} :: SectionModification)

-- | The path within the section content to be modified.
sectionModification_path :: Lens.Lens' SectionModification Prelude.Text
sectionModification_path = Lens.lens (\SectionModification' {path} -> path) (\s@SectionModification' {} a -> s {path = a} :: SectionModification)

-- | The name of the section to be modified.
sectionModification_section :: Lens.Lens' SectionModification Prelude.Text
sectionModification_section = Lens.lens (\SectionModification' {section} -> section) (\s@SectionModification' {} a -> s {section = a} :: SectionModification)

instance Prelude.Hashable SectionModification where
  hashWithSalt _salt SectionModification' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` section

instance Prelude.NFData SectionModification where
  rnf SectionModification' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf section

instance Data.ToJSON SectionModification where
  toJSON SectionModification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just ("Path" Data..= path),
            Prelude.Just ("Section" Data..= section)
          ]
      )
