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
-- Module      : Amazonka.QuickSight.Types.SectionBasedLayoutCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionBasedLayoutCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionBasedLayoutPaperCanvasSizeOptions

-- | The options for the canvas of a section-based layout.
--
-- /See:/ 'newSectionBasedLayoutCanvasSizeOptions' smart constructor.
data SectionBasedLayoutCanvasSizeOptions = SectionBasedLayoutCanvasSizeOptions'
  { -- | The options for a paper canvas of a section-based layout.
    paperCanvasSizeOptions :: Prelude.Maybe SectionBasedLayoutPaperCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionBasedLayoutCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paperCanvasSizeOptions', 'sectionBasedLayoutCanvasSizeOptions_paperCanvasSizeOptions' - The options for a paper canvas of a section-based layout.
newSectionBasedLayoutCanvasSizeOptions ::
  SectionBasedLayoutCanvasSizeOptions
newSectionBasedLayoutCanvasSizeOptions =
  SectionBasedLayoutCanvasSizeOptions'
    { paperCanvasSizeOptions =
        Prelude.Nothing
    }

-- | The options for a paper canvas of a section-based layout.
sectionBasedLayoutCanvasSizeOptions_paperCanvasSizeOptions :: Lens.Lens' SectionBasedLayoutCanvasSizeOptions (Prelude.Maybe SectionBasedLayoutPaperCanvasSizeOptions)
sectionBasedLayoutCanvasSizeOptions_paperCanvasSizeOptions = Lens.lens (\SectionBasedLayoutCanvasSizeOptions' {paperCanvasSizeOptions} -> paperCanvasSizeOptions) (\s@SectionBasedLayoutCanvasSizeOptions' {} a -> s {paperCanvasSizeOptions = a} :: SectionBasedLayoutCanvasSizeOptions)

instance
  Data.FromJSON
    SectionBasedLayoutCanvasSizeOptions
  where
  parseJSON =
    Data.withObject
      "SectionBasedLayoutCanvasSizeOptions"
      ( \x ->
          SectionBasedLayoutCanvasSizeOptions'
            Prelude.<$> (x Data..:? "PaperCanvasSizeOptions")
      )

instance
  Prelude.Hashable
    SectionBasedLayoutCanvasSizeOptions
  where
  hashWithSalt
    _salt
    SectionBasedLayoutCanvasSizeOptions' {..} =
      _salt `Prelude.hashWithSalt` paperCanvasSizeOptions

instance
  Prelude.NFData
    SectionBasedLayoutCanvasSizeOptions
  where
  rnf SectionBasedLayoutCanvasSizeOptions' {..} =
    Prelude.rnf paperCanvasSizeOptions

instance
  Data.ToJSON
    SectionBasedLayoutCanvasSizeOptions
  where
  toJSON SectionBasedLayoutCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PaperCanvasSizeOptions" Data..=)
              Prelude.<$> paperCanvasSizeOptions
          ]
      )
