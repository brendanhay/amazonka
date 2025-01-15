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
-- Module      : Amazonka.Rekognition.Types.GeneralLabelsSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.GeneralLabelsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains filters for the object labels returned by DetectLabels. Filters
-- can be inclusive, exclusive, or a combination of both and can be applied
-- to individual l abels or entire label categories.
--
-- /See:/ 'newGeneralLabelsSettings' smart constructor.
data GeneralLabelsSettings = GeneralLabelsSettings'
  { -- | The label categories that should be excluded from the return from
    -- DetectLabels.
    labelCategoryExclusionFilters :: Prelude.Maybe [Prelude.Text],
    -- | The label categories that should be included in the return from
    -- DetectLabels.
    labelCategoryInclusionFilters :: Prelude.Maybe [Prelude.Text],
    -- | The labels that should be excluded from the return from DetectLabels.
    labelExclusionFilters :: Prelude.Maybe [Prelude.Text],
    -- | The labels that should be included in the return from DetectLabels.
    labelInclusionFilters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneralLabelsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelCategoryExclusionFilters', 'generalLabelsSettings_labelCategoryExclusionFilters' - The label categories that should be excluded from the return from
-- DetectLabels.
--
-- 'labelCategoryInclusionFilters', 'generalLabelsSettings_labelCategoryInclusionFilters' - The label categories that should be included in the return from
-- DetectLabels.
--
-- 'labelExclusionFilters', 'generalLabelsSettings_labelExclusionFilters' - The labels that should be excluded from the return from DetectLabels.
--
-- 'labelInclusionFilters', 'generalLabelsSettings_labelInclusionFilters' - The labels that should be included in the return from DetectLabels.
newGeneralLabelsSettings ::
  GeneralLabelsSettings
newGeneralLabelsSettings =
  GeneralLabelsSettings'
    { labelCategoryExclusionFilters =
        Prelude.Nothing,
      labelCategoryInclusionFilters = Prelude.Nothing,
      labelExclusionFilters = Prelude.Nothing,
      labelInclusionFilters = Prelude.Nothing
    }

-- | The label categories that should be excluded from the return from
-- DetectLabels.
generalLabelsSettings_labelCategoryExclusionFilters :: Lens.Lens' GeneralLabelsSettings (Prelude.Maybe [Prelude.Text])
generalLabelsSettings_labelCategoryExclusionFilters = Lens.lens (\GeneralLabelsSettings' {labelCategoryExclusionFilters} -> labelCategoryExclusionFilters) (\s@GeneralLabelsSettings' {} a -> s {labelCategoryExclusionFilters = a} :: GeneralLabelsSettings) Prelude.. Lens.mapping Lens.coerced

-- | The label categories that should be included in the return from
-- DetectLabels.
generalLabelsSettings_labelCategoryInclusionFilters :: Lens.Lens' GeneralLabelsSettings (Prelude.Maybe [Prelude.Text])
generalLabelsSettings_labelCategoryInclusionFilters = Lens.lens (\GeneralLabelsSettings' {labelCategoryInclusionFilters} -> labelCategoryInclusionFilters) (\s@GeneralLabelsSettings' {} a -> s {labelCategoryInclusionFilters = a} :: GeneralLabelsSettings) Prelude.. Lens.mapping Lens.coerced

-- | The labels that should be excluded from the return from DetectLabels.
generalLabelsSettings_labelExclusionFilters :: Lens.Lens' GeneralLabelsSettings (Prelude.Maybe [Prelude.Text])
generalLabelsSettings_labelExclusionFilters = Lens.lens (\GeneralLabelsSettings' {labelExclusionFilters} -> labelExclusionFilters) (\s@GeneralLabelsSettings' {} a -> s {labelExclusionFilters = a} :: GeneralLabelsSettings) Prelude.. Lens.mapping Lens.coerced

-- | The labels that should be included in the return from DetectLabels.
generalLabelsSettings_labelInclusionFilters :: Lens.Lens' GeneralLabelsSettings (Prelude.Maybe [Prelude.Text])
generalLabelsSettings_labelInclusionFilters = Lens.lens (\GeneralLabelsSettings' {labelInclusionFilters} -> labelInclusionFilters) (\s@GeneralLabelsSettings' {} a -> s {labelInclusionFilters = a} :: GeneralLabelsSettings) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable GeneralLabelsSettings where
  hashWithSalt _salt GeneralLabelsSettings' {..} =
    _salt
      `Prelude.hashWithSalt` labelCategoryExclusionFilters
      `Prelude.hashWithSalt` labelCategoryInclusionFilters
      `Prelude.hashWithSalt` labelExclusionFilters
      `Prelude.hashWithSalt` labelInclusionFilters

instance Prelude.NFData GeneralLabelsSettings where
  rnf GeneralLabelsSettings' {..} =
    Prelude.rnf labelCategoryExclusionFilters `Prelude.seq`
      Prelude.rnf labelCategoryInclusionFilters `Prelude.seq`
        Prelude.rnf labelExclusionFilters `Prelude.seq`
          Prelude.rnf labelInclusionFilters

instance Data.ToJSON GeneralLabelsSettings where
  toJSON GeneralLabelsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelCategoryExclusionFilters" Data..=)
              Prelude.<$> labelCategoryExclusionFilters,
            ("LabelCategoryInclusionFilters" Data..=)
              Prelude.<$> labelCategoryInclusionFilters,
            ("LabelExclusionFilters" Data..=)
              Prelude.<$> labelExclusionFilters,
            ("LabelInclusionFilters" Data..=)
              Prelude.<$> labelInclusionFilters
          ]
      )
