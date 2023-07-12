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
-- Module      : Amazonka.QuickSight.Types.SectionPageBreakConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionPageBreakConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionAfterPageBreak

-- | The configuration of a page break for a section.
--
-- /See:/ 'newSectionPageBreakConfiguration' smart constructor.
data SectionPageBreakConfiguration = SectionPageBreakConfiguration'
  { -- | The configuration of a page break after a section.
    after :: Prelude.Maybe SectionAfterPageBreak
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionPageBreakConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'after', 'sectionPageBreakConfiguration_after' - The configuration of a page break after a section.
newSectionPageBreakConfiguration ::
  SectionPageBreakConfiguration
newSectionPageBreakConfiguration =
  SectionPageBreakConfiguration'
    { after =
        Prelude.Nothing
    }

-- | The configuration of a page break after a section.
sectionPageBreakConfiguration_after :: Lens.Lens' SectionPageBreakConfiguration (Prelude.Maybe SectionAfterPageBreak)
sectionPageBreakConfiguration_after = Lens.lens (\SectionPageBreakConfiguration' {after} -> after) (\s@SectionPageBreakConfiguration' {} a -> s {after = a} :: SectionPageBreakConfiguration)

instance Data.FromJSON SectionPageBreakConfiguration where
  parseJSON =
    Data.withObject
      "SectionPageBreakConfiguration"
      ( \x ->
          SectionPageBreakConfiguration'
            Prelude.<$> (x Data..:? "After")
      )

instance
  Prelude.Hashable
    SectionPageBreakConfiguration
  where
  hashWithSalt _salt SectionPageBreakConfiguration' {..} =
    _salt `Prelude.hashWithSalt` after

instance Prelude.NFData SectionPageBreakConfiguration where
  rnf SectionPageBreakConfiguration' {..} =
    Prelude.rnf after

instance Data.ToJSON SectionPageBreakConfiguration where
  toJSON SectionPageBreakConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("After" Data..=) Prelude.<$> after]
      )
