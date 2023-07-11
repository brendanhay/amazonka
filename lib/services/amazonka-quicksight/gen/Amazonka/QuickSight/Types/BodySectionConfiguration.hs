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
-- Module      : Amazonka.QuickSight.Types.BodySectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BodySectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BodySectionContent
import Amazonka.QuickSight.Types.SectionPageBreakConfiguration
import Amazonka.QuickSight.Types.SectionStyle

-- | The configuration of a body section.
--
-- /See:/ 'newBodySectionConfiguration' smart constructor.
data BodySectionConfiguration = BodySectionConfiguration'
  { -- | The configuration of a page break for a section.
    pageBreakConfiguration :: Prelude.Maybe SectionPageBreakConfiguration,
    -- | The style options of a body section.
    style :: Prelude.Maybe SectionStyle,
    -- | The unique identifier of a body section.
    sectionId :: Prelude.Text,
    -- | The configuration of content in a body section.
    content :: BodySectionContent
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BodySectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageBreakConfiguration', 'bodySectionConfiguration_pageBreakConfiguration' - The configuration of a page break for a section.
--
-- 'style', 'bodySectionConfiguration_style' - The style options of a body section.
--
-- 'sectionId', 'bodySectionConfiguration_sectionId' - The unique identifier of a body section.
--
-- 'content', 'bodySectionConfiguration_content' - The configuration of content in a body section.
newBodySectionConfiguration ::
  -- | 'sectionId'
  Prelude.Text ->
  -- | 'content'
  BodySectionContent ->
  BodySectionConfiguration
newBodySectionConfiguration pSectionId_ pContent_ =
  BodySectionConfiguration'
    { pageBreakConfiguration =
        Prelude.Nothing,
      style = Prelude.Nothing,
      sectionId = pSectionId_,
      content = pContent_
    }

-- | The configuration of a page break for a section.
bodySectionConfiguration_pageBreakConfiguration :: Lens.Lens' BodySectionConfiguration (Prelude.Maybe SectionPageBreakConfiguration)
bodySectionConfiguration_pageBreakConfiguration = Lens.lens (\BodySectionConfiguration' {pageBreakConfiguration} -> pageBreakConfiguration) (\s@BodySectionConfiguration' {} a -> s {pageBreakConfiguration = a} :: BodySectionConfiguration)

-- | The style options of a body section.
bodySectionConfiguration_style :: Lens.Lens' BodySectionConfiguration (Prelude.Maybe SectionStyle)
bodySectionConfiguration_style = Lens.lens (\BodySectionConfiguration' {style} -> style) (\s@BodySectionConfiguration' {} a -> s {style = a} :: BodySectionConfiguration)

-- | The unique identifier of a body section.
bodySectionConfiguration_sectionId :: Lens.Lens' BodySectionConfiguration Prelude.Text
bodySectionConfiguration_sectionId = Lens.lens (\BodySectionConfiguration' {sectionId} -> sectionId) (\s@BodySectionConfiguration' {} a -> s {sectionId = a} :: BodySectionConfiguration)

-- | The configuration of content in a body section.
bodySectionConfiguration_content :: Lens.Lens' BodySectionConfiguration BodySectionContent
bodySectionConfiguration_content = Lens.lens (\BodySectionConfiguration' {content} -> content) (\s@BodySectionConfiguration' {} a -> s {content = a} :: BodySectionConfiguration)

instance Data.FromJSON BodySectionConfiguration where
  parseJSON =
    Data.withObject
      "BodySectionConfiguration"
      ( \x ->
          BodySectionConfiguration'
            Prelude.<$> (x Data..:? "PageBreakConfiguration")
            Prelude.<*> (x Data..:? "Style")
            Prelude.<*> (x Data..: "SectionId")
            Prelude.<*> (x Data..: "Content")
      )

instance Prelude.Hashable BodySectionConfiguration where
  hashWithSalt _salt BodySectionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` pageBreakConfiguration
      `Prelude.hashWithSalt` style
      `Prelude.hashWithSalt` sectionId
      `Prelude.hashWithSalt` content

instance Prelude.NFData BodySectionConfiguration where
  rnf BodySectionConfiguration' {..} =
    Prelude.rnf pageBreakConfiguration
      `Prelude.seq` Prelude.rnf style
      `Prelude.seq` Prelude.rnf sectionId
      `Prelude.seq` Prelude.rnf content

instance Data.ToJSON BodySectionConfiguration where
  toJSON BodySectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PageBreakConfiguration" Data..=)
              Prelude.<$> pageBreakConfiguration,
            ("Style" Data..=) Prelude.<$> style,
            Prelude.Just ("SectionId" Data..= sectionId),
            Prelude.Just ("Content" Data..= content)
          ]
      )
