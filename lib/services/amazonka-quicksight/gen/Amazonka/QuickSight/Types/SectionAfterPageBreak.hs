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
-- Module      : Amazonka.QuickSight.Types.SectionAfterPageBreak
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionAfterPageBreak where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionPageBreakStatus

-- | The configuration of a page break after a section.
--
-- /See:/ 'newSectionAfterPageBreak' smart constructor.
data SectionAfterPageBreak = SectionAfterPageBreak'
  { -- | The option that enables or disables a page break at the end of a
    -- section.
    status :: Prelude.Maybe SectionPageBreakStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionAfterPageBreak' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'sectionAfterPageBreak_status' - The option that enables or disables a page break at the end of a
-- section.
newSectionAfterPageBreak ::
  SectionAfterPageBreak
newSectionAfterPageBreak =
  SectionAfterPageBreak' {status = Prelude.Nothing}

-- | The option that enables or disables a page break at the end of a
-- section.
sectionAfterPageBreak_status :: Lens.Lens' SectionAfterPageBreak (Prelude.Maybe SectionPageBreakStatus)
sectionAfterPageBreak_status = Lens.lens (\SectionAfterPageBreak' {status} -> status) (\s@SectionAfterPageBreak' {} a -> s {status = a} :: SectionAfterPageBreak)

instance Data.FromJSON SectionAfterPageBreak where
  parseJSON =
    Data.withObject
      "SectionAfterPageBreak"
      ( \x ->
          SectionAfterPageBreak'
            Prelude.<$> (x Data..:? "Status")
      )

instance Prelude.Hashable SectionAfterPageBreak where
  hashWithSalt _salt SectionAfterPageBreak' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData SectionAfterPageBreak where
  rnf SectionAfterPageBreak' {..} = Prelude.rnf status

instance Data.ToJSON SectionAfterPageBreak where
  toJSON SectionAfterPageBreak' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Status" Data..=) Prelude.<$> status]
      )
