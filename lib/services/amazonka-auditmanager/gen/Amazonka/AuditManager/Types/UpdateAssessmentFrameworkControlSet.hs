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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet where

import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @controlSet@ entity that represents a collection of controls in Audit
-- Manager. This doesn\'t contain the control set ID.
--
-- /See:/ 'newUpdateAssessmentFrameworkControlSet' smart constructor.
data UpdateAssessmentFrameworkControlSet = UpdateAssessmentFrameworkControlSet'
  { -- | The unique identifier for the control set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the control set.
    name :: Prelude.Text,
    -- | The list of controls that are contained within the control set.
    controls :: Prelude.NonEmpty CreateAssessmentFrameworkControl
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
-- 'name', 'updateAssessmentFrameworkControlSet_name' - The name of the control set.
--
-- 'controls', 'updateAssessmentFrameworkControlSet_controls' - The list of controls that are contained within the control set.
newUpdateAssessmentFrameworkControlSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'controls'
  Prelude.NonEmpty CreateAssessmentFrameworkControl ->
  UpdateAssessmentFrameworkControlSet
newUpdateAssessmentFrameworkControlSet
  pName_
  pControls_ =
    UpdateAssessmentFrameworkControlSet'
      { id =
          Prelude.Nothing,
        name = pName_,
        controls =
          Lens.coerced Lens.# pControls_
      }

-- | The unique identifier for the control set.
updateAssessmentFrameworkControlSet_id :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.Maybe Prelude.Text)
updateAssessmentFrameworkControlSet_id = Lens.lens (\UpdateAssessmentFrameworkControlSet' {id} -> id) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {id = a} :: UpdateAssessmentFrameworkControlSet)

-- | The name of the control set.
updateAssessmentFrameworkControlSet_name :: Lens.Lens' UpdateAssessmentFrameworkControlSet Prelude.Text
updateAssessmentFrameworkControlSet_name = Lens.lens (\UpdateAssessmentFrameworkControlSet' {name} -> name) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {name = a} :: UpdateAssessmentFrameworkControlSet)

-- | The list of controls that are contained within the control set.
updateAssessmentFrameworkControlSet_controls :: Lens.Lens' UpdateAssessmentFrameworkControlSet (Prelude.NonEmpty CreateAssessmentFrameworkControl)
updateAssessmentFrameworkControlSet_controls = Lens.lens (\UpdateAssessmentFrameworkControlSet' {controls} -> controls) (\s@UpdateAssessmentFrameworkControlSet' {} a -> s {controls = a} :: UpdateAssessmentFrameworkControlSet) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    UpdateAssessmentFrameworkControlSet
  where
  hashWithSalt
    _salt
    UpdateAssessmentFrameworkControlSet' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` controls

instance
  Prelude.NFData
    UpdateAssessmentFrameworkControlSet
  where
  rnf UpdateAssessmentFrameworkControlSet' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf controls

instance
  Data.ToJSON
    UpdateAssessmentFrameworkControlSet
  where
  toJSON UpdateAssessmentFrameworkControlSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id" Data..=) Prelude.<$> id,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("controls" Data..= controls)
          ]
      )
