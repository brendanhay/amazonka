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
-- Module      : Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The control entity attributes that uniquely identify an existing control
-- to be added to a framework in Audit Manager.
--
-- /See:/ 'newCreateAssessmentFrameworkControl' smart constructor.
data CreateAssessmentFrameworkControl = CreateAssessmentFrameworkControl'
  { -- | The unique identifier of the control.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentFrameworkControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createAssessmentFrameworkControl_id' - The unique identifier of the control.
newCreateAssessmentFrameworkControl ::
  -- | 'id'
  Prelude.Text ->
  CreateAssessmentFrameworkControl
newCreateAssessmentFrameworkControl pId_ =
  CreateAssessmentFrameworkControl' {id = pId_}

-- | The unique identifier of the control.
createAssessmentFrameworkControl_id :: Lens.Lens' CreateAssessmentFrameworkControl Prelude.Text
createAssessmentFrameworkControl_id = Lens.lens (\CreateAssessmentFrameworkControl' {id} -> id) (\s@CreateAssessmentFrameworkControl' {} a -> s {id = a} :: CreateAssessmentFrameworkControl)

instance
  Prelude.Hashable
    CreateAssessmentFrameworkControl
  where
  hashWithSalt
    _salt
    CreateAssessmentFrameworkControl' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    CreateAssessmentFrameworkControl
  where
  rnf CreateAssessmentFrameworkControl' {..} =
    Prelude.rnf id

instance Data.ToJSON CreateAssessmentFrameworkControl where
  toJSON CreateAssessmentFrameworkControl' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])
