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
-- Module      : Amazonka.ConnectCases.Types.LayoutSections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.LayoutSections where

import Amazonka.ConnectCases.Types.Section
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ordered list containing different kinds of sections that can be added. A
-- LayoutSections object can only contain one section.
--
-- /See:/ 'newLayoutSections' smart constructor.
data LayoutSections = LayoutSections'
  { sections :: Prelude.Maybe [Section]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayoutSections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sections', 'layoutSections_sections' - Undocumented member.
newLayoutSections ::
  LayoutSections
newLayoutSections =
  LayoutSections' {sections = Prelude.Nothing}

-- | Undocumented member.
layoutSections_sections :: Lens.Lens' LayoutSections (Prelude.Maybe [Section])
layoutSections_sections = Lens.lens (\LayoutSections' {sections} -> sections) (\s@LayoutSections' {} a -> s {sections = a} :: LayoutSections) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LayoutSections where
  parseJSON =
    Data.withObject
      "LayoutSections"
      ( \x ->
          LayoutSections'
            Prelude.<$> (x Data..:? "sections" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LayoutSections where
  hashWithSalt _salt LayoutSections' {..} =
    _salt `Prelude.hashWithSalt` sections

instance Prelude.NFData LayoutSections where
  rnf LayoutSections' {..} = Prelude.rnf sections

instance Data.ToJSON LayoutSections where
  toJSON LayoutSections' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sections" Data..=) Prelude.<$> sections]
      )
