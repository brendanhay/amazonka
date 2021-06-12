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
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTemplateFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.DurationRange
import qualified Network.AWS.Lens as Lens

-- | Used as the request parameter in the ListAssessmentTemplates action.
--
-- /See:/ 'newAssessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { -- | For a record to match a filter, the values that are specified for this
    -- data type property must be contained in the list of values of the
    -- __rulesPackageArns__ property of the AssessmentTemplate data type.
    rulesPackageArns :: Core.Maybe [Core.Text],
    -- | For a record to match a filter, the value specified for this data type
    -- property must inclusively match any value between the specified minimum
    -- and maximum values of the __durationInSeconds__ property of the
    -- AssessmentTemplate data type.
    durationRange :: Core.Maybe DurationRange,
    -- | For a record to match a filter, an explicit value or a string that
    -- contains a wildcard that is specified for this data type property must
    -- match the value of the __assessmentTemplateName__ property of the
    -- AssessmentTemplate data type.
    namePattern :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssessmentTemplateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rulesPackageArns', 'assessmentTemplateFilter_rulesPackageArns' - For a record to match a filter, the values that are specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackageArns__ property of the AssessmentTemplate data type.
--
-- 'durationRange', 'assessmentTemplateFilter_durationRange' - For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __durationInSeconds__ property of the
-- AssessmentTemplate data type.
--
-- 'namePattern', 'assessmentTemplateFilter_namePattern' - For a record to match a filter, an explicit value or a string that
-- contains a wildcard that is specified for this data type property must
-- match the value of the __assessmentTemplateName__ property of the
-- AssessmentTemplate data type.
newAssessmentTemplateFilter ::
  AssessmentTemplateFilter
newAssessmentTemplateFilter =
  AssessmentTemplateFilter'
    { rulesPackageArns =
        Core.Nothing,
      durationRange = Core.Nothing,
      namePattern = Core.Nothing
    }

-- | For a record to match a filter, the values that are specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackageArns__ property of the AssessmentTemplate data type.
assessmentTemplateFilter_rulesPackageArns :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe [Core.Text])
assessmentTemplateFilter_rulesPackageArns = Lens.lens (\AssessmentTemplateFilter' {rulesPackageArns} -> rulesPackageArns) (\s@AssessmentTemplateFilter' {} a -> s {rulesPackageArns = a} :: AssessmentTemplateFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __durationInSeconds__ property of the
-- AssessmentTemplate data type.
assessmentTemplateFilter_durationRange :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe DurationRange)
assessmentTemplateFilter_durationRange = Lens.lens (\AssessmentTemplateFilter' {durationRange} -> durationRange) (\s@AssessmentTemplateFilter' {} a -> s {durationRange = a} :: AssessmentTemplateFilter)

-- | For a record to match a filter, an explicit value or a string that
-- contains a wildcard that is specified for this data type property must
-- match the value of the __assessmentTemplateName__ property of the
-- AssessmentTemplate data type.
assessmentTemplateFilter_namePattern :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe Core.Text)
assessmentTemplateFilter_namePattern = Lens.lens (\AssessmentTemplateFilter' {namePattern} -> namePattern) (\s@AssessmentTemplateFilter' {} a -> s {namePattern = a} :: AssessmentTemplateFilter)

instance Core.Hashable AssessmentTemplateFilter

instance Core.NFData AssessmentTemplateFilter

instance Core.ToJSON AssessmentTemplateFilter where
  toJSON AssessmentTemplateFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rulesPackageArns" Core..=)
              Core.<$> rulesPackageArns,
            ("durationRange" Core..=) Core.<$> durationRange,
            ("namePattern" Core..=) Core.<$> namePattern
          ]
      )
