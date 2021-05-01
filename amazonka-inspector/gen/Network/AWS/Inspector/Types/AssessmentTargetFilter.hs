{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Inspector.Types.AssessmentTargetFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTargetFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable AssessmentTargetFilter

instance Prelude.NFData AssessmentTargetFilter

instance Prelude.ToJSON AssessmentTargetFilter where
  toJSON AssessmentTargetFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("assessmentTargetNamePattern" Prelude..=)
              Prelude.<$> assessmentTargetNamePattern
          ]
      )
