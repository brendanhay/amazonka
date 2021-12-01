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
  { -- | The list of controls contained within the control set.
    controls :: Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl),
    -- | The unique identifier for the control set.
    id :: Prelude.Maybe Prelude.Text,
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
-- 'controls', 'updateAssessmentFrameworkControlSet_controls' - The list of controls contained within the control set.
--
-- 'id', 'updateAssessmentFrameworkControlSet_id' - The unique identifier for the control set.
--
-- 'name', 'updateAssessmentFrameworkControlSet_name' - The name of the control set.
newUpdateAssessmentFrameworkControlSet ::
  -- | 'name'
  Prelude.Text ->
  UpdateAssessmentFrameworkControlSet
newUpdateAssessmentFrameworkControlSet pName_ =
  UpdateAssessmentFrameworkControlSet'
    { controls =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = pName_
    }

-- | The list of controls contained within the control set.
updateAssessmentFrameworkControlSet_controls :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl))
updateAssessmentFrameworkControlSet_controls = Lens.lens (\UpdateAssessmentFrameworkControlSet' {controls} -> controls) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {controls = a} :: UpdateAssessmentFrameworkControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the control set.
updateAssessmentFrameworkControlSet_id :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.Maybe Prelude.Text)
updateAssessmentFrameworkControlSet_id = Lens.lens (\UpdateAssessmentFrameworkControlSet' {id} -> id) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {id = a} :: UpdateAssessmentFrameworkControlSet)

-- | The name of the control set.
updateAssessmentFrameworkControlSet_name :: Lens.Lens' UpdateAssessmentFrameworkControlSet Prelude.Text
updateAssessmentFrameworkControlSet_name = Lens.lens (\UpdateAssessmentFrameworkControlSet' {name} -> name) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {name = a} :: UpdateAssessmentFrameworkControlSet)

instance
  Prelude.Hashable
    UpdateAssessmentFrameworkControlSet
  where
  hashWithSalt
    salt'
    UpdateAssessmentFrameworkControlSet' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` controls

instance
  Prelude.NFData
    UpdateAssessmentFrameworkControlSet
  where
  rnf UpdateAssessmentFrameworkControlSet' {..} =
    Prelude.rnf controls `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance
  Core.ToJSON
    UpdateAssessmentFrameworkControlSet
  where
  toJSON UpdateAssessmentFrameworkControlSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("controls" Core..=) Prelude.<$> controls,
            ("id" Core..=) Prelude.<$> id,
            Prelude.Just ("name" Core..= name)
          ]
      )
