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
-- Module      : Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet where

import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @controlSet@ entity that represents a collection of controls in Audit
-- Manager. This does not contain the control set ID.
--
-- /See:/ 'newUpdateAssessmentFrameworkControlSet' smart constructor.
data UpdateAssessmentFrameworkControlSet = UpdateAssessmentFrameworkControlSet'
  { -- | The unique identifier for the control set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of controls contained within the control set.
    controls :: Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl),
    -- | The name of the control set.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentFrameworkControlSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateAssessmentFrameworkControlSet_id' - The unique identifier for the control set.
--
-- 'controls', 'updateAssessmentFrameworkControlSet_controls' - The list of controls contained within the control set.
--
-- 'name', 'updateAssessmentFrameworkControlSet_name' - The name of the control set.
newUpdateAssessmentFrameworkControlSet ::
  -- | 'name'
  Prelude.Text ->
  UpdateAssessmentFrameworkControlSet
newUpdateAssessmentFrameworkControlSet pName_ =
  UpdateAssessmentFrameworkControlSet'
    { id =
        Prelude.Nothing,
      controls = Prelude.Nothing,
      name = pName_
    }

-- | The unique identifier for the control set.
updateAssessmentFrameworkControlSet_id :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.Maybe Prelude.Text)
updateAssessmentFrameworkControlSet_id = Lens.lens (\UpdateAssessmentFrameworkControlSet' {id} -> id) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {id = a} :: UpdateAssessmentFrameworkControlSet)

-- | The list of controls contained within the control set.
updateAssessmentFrameworkControlSet_controls :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl))
updateAssessmentFrameworkControlSet_controls = Lens.lens (\UpdateAssessmentFrameworkControlSet' {controls} -> controls) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {controls = a} :: UpdateAssessmentFrameworkControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the control set.
updateAssessmentFrameworkControlSet_name :: Lens.Lens' UpdateAssessmentFrameworkControlSet Prelude.Text
updateAssessmentFrameworkControlSet_name = Lens.lens (\UpdateAssessmentFrameworkControlSet' {name} -> name) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {name = a} :: UpdateAssessmentFrameworkControlSet)

instance
  Prelude.Hashable
    UpdateAssessmentFrameworkControlSet
  where
  hashWithSalt
    _salt
    UpdateAssessmentFrameworkControlSet' {..} =
      _salt `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` controls
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    UpdateAssessmentFrameworkControlSet
  where
  rnf UpdateAssessmentFrameworkControlSet' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf controls
      `Prelude.seq` Prelude.rnf name

instance
  Core.ToJSON
    UpdateAssessmentFrameworkControlSet
  where
  toJSON UpdateAssessmentFrameworkControlSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("id" Core..=) Prelude.<$> id,
            ("controls" Core..=) Prelude.<$> controls,
            Prelude.Just ("name" Core..= name)
          ]
      )
