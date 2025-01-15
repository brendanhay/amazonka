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
-- Module      : Amazonka.Inspector.Types.AssessmentTemplateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentTemplateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.DurationRange
import qualified Amazonka.Prelude as Prelude

-- | Used as the request parameter in the ListAssessmentTemplates action.
--
-- /See:/ 'newAssessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { -- | For a record to match a filter, the value specified for this data type
    -- property must inclusively match any value between the specified minimum
    -- and maximum values of the __durationInSeconds__ property of the
    -- AssessmentTemplate data type.
    durationRange :: Prelude.Maybe DurationRange,
    -- | For a record to match a filter, an explicit value or a string that
    -- contains a wildcard that is specified for this data type property must
    -- match the value of the __assessmentTemplateName__ property of the
    -- AssessmentTemplate data type.
    namePattern :: Prelude.Maybe Prelude.Text,
    -- | For a record to match a filter, the values that are specified for this
    -- data type property must be contained in the list of values of the
    -- __rulesPackageArns__ property of the AssessmentTemplate data type.
    rulesPackageArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentTemplateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'rulesPackageArns', 'assessmentTemplateFilter_rulesPackageArns' - For a record to match a filter, the values that are specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackageArns__ property of the AssessmentTemplate data type.
newAssessmentTemplateFilter ::
  AssessmentTemplateFilter
newAssessmentTemplateFilter =
  AssessmentTemplateFilter'
    { durationRange =
        Prelude.Nothing,
      namePattern = Prelude.Nothing,
      rulesPackageArns = Prelude.Nothing
    }

-- | For a record to match a filter, the value specified for this data type
-- property must inclusively match any value between the specified minimum
-- and maximum values of the __durationInSeconds__ property of the
-- AssessmentTemplate data type.
assessmentTemplateFilter_durationRange :: Lens.Lens' AssessmentTemplateFilter (Prelude.Maybe DurationRange)
assessmentTemplateFilter_durationRange = Lens.lens (\AssessmentTemplateFilter' {durationRange} -> durationRange) (\s@AssessmentTemplateFilter' {} a -> s {durationRange = a} :: AssessmentTemplateFilter)

-- | For a record to match a filter, an explicit value or a string that
-- contains a wildcard that is specified for this data type property must
-- match the value of the __assessmentTemplateName__ property of the
-- AssessmentTemplate data type.
assessmentTemplateFilter_namePattern :: Lens.Lens' AssessmentTemplateFilter (Prelude.Maybe Prelude.Text)
assessmentTemplateFilter_namePattern = Lens.lens (\AssessmentTemplateFilter' {namePattern} -> namePattern) (\s@AssessmentTemplateFilter' {} a -> s {namePattern = a} :: AssessmentTemplateFilter)

-- | For a record to match a filter, the values that are specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackageArns__ property of the AssessmentTemplate data type.
assessmentTemplateFilter_rulesPackageArns :: Lens.Lens' AssessmentTemplateFilter (Prelude.Maybe [Prelude.Text])
assessmentTemplateFilter_rulesPackageArns = Lens.lens (\AssessmentTemplateFilter' {rulesPackageArns} -> rulesPackageArns) (\s@AssessmentTemplateFilter' {} a -> s {rulesPackageArns = a} :: AssessmentTemplateFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AssessmentTemplateFilter where
  hashWithSalt _salt AssessmentTemplateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` durationRange
      `Prelude.hashWithSalt` namePattern
      `Prelude.hashWithSalt` rulesPackageArns

instance Prelude.NFData AssessmentTemplateFilter where
  rnf AssessmentTemplateFilter' {..} =
    Prelude.rnf durationRange `Prelude.seq`
      Prelude.rnf namePattern `Prelude.seq`
        Prelude.rnf rulesPackageArns

instance Data.ToJSON AssessmentTemplateFilter where
  toJSON AssessmentTemplateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("durationRange" Data..=) Prelude.<$> durationRange,
            ("namePattern" Data..=) Prelude.<$> namePattern,
            ("rulesPackageArns" Data..=)
              Prelude.<$> rulesPackageArns
          ]
      )
