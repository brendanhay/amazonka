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
-- Module      : Amazonka.QuickSight.Types.BodySectionContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BodySectionContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionLayoutConfiguration

-- | The configuration of content in a body section.
--
-- /See:/ 'newBodySectionContent' smart constructor.
data BodySectionContent = BodySectionContent'
  { -- | The layout configuration of a body section.
    layout :: Prelude.Maybe SectionLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BodySectionContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layout', 'bodySectionContent_layout' - The layout configuration of a body section.
newBodySectionContent ::
  BodySectionContent
newBodySectionContent =
  BodySectionContent' {layout = Prelude.Nothing}

-- | The layout configuration of a body section.
bodySectionContent_layout :: Lens.Lens' BodySectionContent (Prelude.Maybe SectionLayoutConfiguration)
bodySectionContent_layout = Lens.lens (\BodySectionContent' {layout} -> layout) (\s@BodySectionContent' {} a -> s {layout = a} :: BodySectionContent)

instance Data.FromJSON BodySectionContent where
  parseJSON =
    Data.withObject
      "BodySectionContent"
      ( \x ->
          BodySectionContent'
            Prelude.<$> (x Data..:? "Layout")
      )

instance Prelude.Hashable BodySectionContent where
  hashWithSalt _salt BodySectionContent' {..} =
    _salt `Prelude.hashWithSalt` layout

instance Prelude.NFData BodySectionContent where
  rnf BodySectionContent' {..} = Prelude.rnf layout

instance Data.ToJSON BodySectionContent where
  toJSON BodySectionContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Layout" Data..=) Prelude.<$> layout]
      )
