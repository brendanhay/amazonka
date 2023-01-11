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
-- Module      : Amazonka.ConnectCases.Types.Section
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.Section where

import Amazonka.ConnectCases.Types.FieldGroup
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This represents a sections within a panel or tab of the page layout.
--
-- /See:/ 'newSection' smart constructor.
data Section = Section'
  { -- | Consists of a group of fields and associated properties.
    fieldGroup :: Prelude.Maybe FieldGroup
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
-- 'fieldGroup', 'section_fieldGroup' - Consists of a group of fields and associated properties.
newSection ::
  Section
newSection = Section' {fieldGroup = Prelude.Nothing}

-- | Consists of a group of fields and associated properties.
section_fieldGroup :: Lens.Lens' Section (Prelude.Maybe FieldGroup)
section_fieldGroup = Lens.lens (\Section' {fieldGroup} -> fieldGroup) (\s@Section' {} a -> s {fieldGroup = a} :: Section)

instance Data.FromJSON Section where
  parseJSON =
    Data.withObject
      "Section"
      ( \x ->
          Section' Prelude.<$> (x Data..:? "fieldGroup")
      )

instance Prelude.Hashable Section where
  hashWithSalt _salt Section' {..} =
    _salt `Prelude.hashWithSalt` fieldGroup

instance Prelude.NFData Section where
  rnf Section' {..} = Prelude.rnf fieldGroup

instance Data.ToJSON Section where
  toJSON Section' {..} =
    Data.object
      ( Prelude.catMaybes
          [("fieldGroup" Data..=) Prelude.<$> fieldGroup]
      )
