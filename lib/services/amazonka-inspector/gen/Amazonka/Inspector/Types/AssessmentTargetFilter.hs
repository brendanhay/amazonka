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
-- Module      : Amazonka.Inspector.Types.AssessmentTargetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentTargetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used as the request parameter in the ListAssessmentTargets action.
--
-- /See:/ 'newAssessmentTargetFilter' smart constructor.
data AssessmentTargetFilter = AssessmentTargetFilter'
  { -- | For a record to match a filter, an explicit value or a string that
    -- contains a wildcard that is specified for this data type property must
    -- match the value of the __assessmentTargetName__ property of the
    -- AssessmentTarget data type.
    assessmentTargetNamePattern :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentTargetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTargetNamePattern', 'assessmentTargetFilter_assessmentTargetNamePattern' - For a record to match a filter, an explicit value or a string that
-- contains a wildcard that is specified for this data type property must
-- match the value of the __assessmentTargetName__ property of the
-- AssessmentTarget data type.
newAssessmentTargetFilter ::
  AssessmentTargetFilter
newAssessmentTargetFilter =
  AssessmentTargetFilter'
    { assessmentTargetNamePattern =
        Prelude.Nothing
    }

-- | For a record to match a filter, an explicit value or a string that
-- contains a wildcard that is specified for this data type property must
-- match the value of the __assessmentTargetName__ property of the
-- AssessmentTarget data type.
assessmentTargetFilter_assessmentTargetNamePattern :: Lens.Lens' AssessmentTargetFilter (Prelude.Maybe Prelude.Text)
assessmentTargetFilter_assessmentTargetNamePattern = Lens.lens (\AssessmentTargetFilter' {assessmentTargetNamePattern} -> assessmentTargetNamePattern) (\s@AssessmentTargetFilter' {} a -> s {assessmentTargetNamePattern = a} :: AssessmentTargetFilter)

instance Prelude.Hashable AssessmentTargetFilter where
  hashWithSalt _salt AssessmentTargetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentTargetNamePattern

instance Prelude.NFData AssessmentTargetFilter where
  rnf AssessmentTargetFilter' {..} =
    Prelude.rnf assessmentTargetNamePattern

instance Data.ToJSON AssessmentTargetFilter where
  toJSON AssessmentTargetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assessmentTargetNamePattern" Data..=)
              Prelude.<$> assessmentTargetNamePattern
          ]
      )
