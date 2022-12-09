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
-- Module      : Amazonka.QuickSight.Types.SectionBasedLayoutConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionBasedLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BodySectionConfiguration
import Amazonka.QuickSight.Types.HeaderFooterSectionConfiguration
import Amazonka.QuickSight.Types.SectionBasedLayoutCanvasSizeOptions

-- | The configuration for a section-based layout.
--
-- /See:/ 'newSectionBasedLayoutConfiguration' smart constructor.
data SectionBasedLayoutConfiguration = SectionBasedLayoutConfiguration'
  { -- | A list of header section configurations.
    headerSections :: [HeaderFooterSectionConfiguration],
    -- | A list of body section configurations.
    bodySections :: [BodySectionConfiguration],
    -- | A list of footer section configurations.
    footerSections :: [HeaderFooterSectionConfiguration],
    -- | The options for the canvas of a section-based layout.
    canvasSizeOptions :: SectionBasedLayoutCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionBasedLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerSections', 'sectionBasedLayoutConfiguration_headerSections' - A list of header section configurations.
--
-- 'bodySections', 'sectionBasedLayoutConfiguration_bodySections' - A list of body section configurations.
--
-- 'footerSections', 'sectionBasedLayoutConfiguration_footerSections' - A list of footer section configurations.
--
-- 'canvasSizeOptions', 'sectionBasedLayoutConfiguration_canvasSizeOptions' - The options for the canvas of a section-based layout.
newSectionBasedLayoutConfiguration ::
  -- | 'canvasSizeOptions'
  SectionBasedLayoutCanvasSizeOptions ->
  SectionBasedLayoutConfiguration
newSectionBasedLayoutConfiguration
  pCanvasSizeOptions_ =
    SectionBasedLayoutConfiguration'
      { headerSections =
          Prelude.mempty,
        bodySections = Prelude.mempty,
        footerSections = Prelude.mempty,
        canvasSizeOptions = pCanvasSizeOptions_
      }

-- | A list of header section configurations.
sectionBasedLayoutConfiguration_headerSections :: Lens.Lens' SectionBasedLayoutConfiguration [HeaderFooterSectionConfiguration]
sectionBasedLayoutConfiguration_headerSections = Lens.lens (\SectionBasedLayoutConfiguration' {headerSections} -> headerSections) (\s@SectionBasedLayoutConfiguration' {} a -> s {headerSections = a} :: SectionBasedLayoutConfiguration) Prelude.. Lens.coerced

-- | A list of body section configurations.
sectionBasedLayoutConfiguration_bodySections :: Lens.Lens' SectionBasedLayoutConfiguration [BodySectionConfiguration]
sectionBasedLayoutConfiguration_bodySections = Lens.lens (\SectionBasedLayoutConfiguration' {bodySections} -> bodySections) (\s@SectionBasedLayoutConfiguration' {} a -> s {bodySections = a} :: SectionBasedLayoutConfiguration) Prelude.. Lens.coerced

-- | A list of footer section configurations.
sectionBasedLayoutConfiguration_footerSections :: Lens.Lens' SectionBasedLayoutConfiguration [HeaderFooterSectionConfiguration]
sectionBasedLayoutConfiguration_footerSections = Lens.lens (\SectionBasedLayoutConfiguration' {footerSections} -> footerSections) (\s@SectionBasedLayoutConfiguration' {} a -> s {footerSections = a} :: SectionBasedLayoutConfiguration) Prelude.. Lens.coerced

-- | The options for the canvas of a section-based layout.
sectionBasedLayoutConfiguration_canvasSizeOptions :: Lens.Lens' SectionBasedLayoutConfiguration SectionBasedLayoutCanvasSizeOptions
sectionBasedLayoutConfiguration_canvasSizeOptions = Lens.lens (\SectionBasedLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@SectionBasedLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: SectionBasedLayoutConfiguration)

instance
  Data.FromJSON
    SectionBasedLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "SectionBasedLayoutConfiguration"
      ( \x ->
          SectionBasedLayoutConfiguration'
            Prelude.<$> (x Data..:? "HeaderSections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BodySections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FooterSections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "CanvasSizeOptions")
      )

instance
  Prelude.Hashable
    SectionBasedLayoutConfiguration
  where
  hashWithSalt
    _salt
    SectionBasedLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` headerSections
        `Prelude.hashWithSalt` bodySections
        `Prelude.hashWithSalt` footerSections
        `Prelude.hashWithSalt` canvasSizeOptions

instance
  Prelude.NFData
    SectionBasedLayoutConfiguration
  where
  rnf SectionBasedLayoutConfiguration' {..} =
    Prelude.rnf headerSections
      `Prelude.seq` Prelude.rnf bodySections
      `Prelude.seq` Prelude.rnf footerSections
      `Prelude.seq` Prelude.rnf canvasSizeOptions

instance Data.ToJSON SectionBasedLayoutConfiguration where
  toJSON SectionBasedLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HeaderSections" Data..= headerSections),
            Prelude.Just ("BodySections" Data..= bodySections),
            Prelude.Just
              ("FooterSections" Data..= footerSections),
            Prelude.Just
              ("CanvasSizeOptions" Data..= canvasSizeOptions)
          ]
      )
