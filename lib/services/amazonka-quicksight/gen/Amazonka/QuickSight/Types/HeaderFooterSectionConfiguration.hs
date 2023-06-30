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
-- Module      : Amazonka.QuickSight.Types.HeaderFooterSectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeaderFooterSectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionLayoutConfiguration
import Amazonka.QuickSight.Types.SectionStyle

-- | The configuration of a header or footer section.
--
-- /See:/ 'newHeaderFooterSectionConfiguration' smart constructor.
data HeaderFooterSectionConfiguration = HeaderFooterSectionConfiguration'
  { -- | The style options of a header or footer section.
    style :: Prelude.Maybe SectionStyle,
    -- | The unique identifier of the header or footer section.
    sectionId :: Prelude.Text,
    -- | The layout configuration of the header or footer section.
    layout :: SectionLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderFooterSectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'style', 'headerFooterSectionConfiguration_style' - The style options of a header or footer section.
--
-- 'sectionId', 'headerFooterSectionConfiguration_sectionId' - The unique identifier of the header or footer section.
--
-- 'layout', 'headerFooterSectionConfiguration_layout' - The layout configuration of the header or footer section.
newHeaderFooterSectionConfiguration ::
  -- | 'sectionId'
  Prelude.Text ->
  -- | 'layout'
  SectionLayoutConfiguration ->
  HeaderFooterSectionConfiguration
newHeaderFooterSectionConfiguration
  pSectionId_
  pLayout_ =
    HeaderFooterSectionConfiguration'
      { style =
          Prelude.Nothing,
        sectionId = pSectionId_,
        layout = pLayout_
      }

-- | The style options of a header or footer section.
headerFooterSectionConfiguration_style :: Lens.Lens' HeaderFooterSectionConfiguration (Prelude.Maybe SectionStyle)
headerFooterSectionConfiguration_style = Lens.lens (\HeaderFooterSectionConfiguration' {style} -> style) (\s@HeaderFooterSectionConfiguration' {} a -> s {style = a} :: HeaderFooterSectionConfiguration)

-- | The unique identifier of the header or footer section.
headerFooterSectionConfiguration_sectionId :: Lens.Lens' HeaderFooterSectionConfiguration Prelude.Text
headerFooterSectionConfiguration_sectionId = Lens.lens (\HeaderFooterSectionConfiguration' {sectionId} -> sectionId) (\s@HeaderFooterSectionConfiguration' {} a -> s {sectionId = a} :: HeaderFooterSectionConfiguration)

-- | The layout configuration of the header or footer section.
headerFooterSectionConfiguration_layout :: Lens.Lens' HeaderFooterSectionConfiguration SectionLayoutConfiguration
headerFooterSectionConfiguration_layout = Lens.lens (\HeaderFooterSectionConfiguration' {layout} -> layout) (\s@HeaderFooterSectionConfiguration' {} a -> s {layout = a} :: HeaderFooterSectionConfiguration)

instance
  Data.FromJSON
    HeaderFooterSectionConfiguration
  where
  parseJSON =
    Data.withObject
      "HeaderFooterSectionConfiguration"
      ( \x ->
          HeaderFooterSectionConfiguration'
            Prelude.<$> (x Data..:? "Style")
            Prelude.<*> (x Data..: "SectionId")
            Prelude.<*> (x Data..: "Layout")
      )

instance
  Prelude.Hashable
    HeaderFooterSectionConfiguration
  where
  hashWithSalt
    _salt
    HeaderFooterSectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` style
        `Prelude.hashWithSalt` sectionId
        `Prelude.hashWithSalt` layout

instance
  Prelude.NFData
    HeaderFooterSectionConfiguration
  where
  rnf HeaderFooterSectionConfiguration' {..} =
    Prelude.rnf style
      `Prelude.seq` Prelude.rnf sectionId
      `Prelude.seq` Prelude.rnf layout

instance Data.ToJSON HeaderFooterSectionConfiguration where
  toJSON HeaderFooterSectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Style" Data..=) Prelude.<$> style,
            Prelude.Just ("SectionId" Data..= sectionId),
            Prelude.Just ("Layout" Data..= layout)
          ]
      )
