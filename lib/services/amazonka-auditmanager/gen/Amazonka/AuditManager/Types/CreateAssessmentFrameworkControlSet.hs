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
-- Module      : Amazonka.AuditManager.Types.CreateAssessmentFrameworkControlSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.CreateAssessmentFrameworkControlSet where

import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @controlSet@ entity that represents a collection of controls in Audit
-- Manager. This doesn\'t contain the control set ID.
--
-- /See:/ 'newCreateAssessmentFrameworkControlSet' smart constructor.
data CreateAssessmentFrameworkControlSet = CreateAssessmentFrameworkControlSet'
  { -- | The list of controls within the control set. This doesn\'t contain the
    -- control set ID.
    controls :: Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl),
    -- | The name of the control set.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentFrameworkControlSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controls', 'createAssessmentFrameworkControlSet_controls' - The list of controls within the control set. This doesn\'t contain the
-- control set ID.
--
-- 'name', 'createAssessmentFrameworkControlSet_name' - The name of the control set.
newCreateAssessmentFrameworkControlSet ::
  -- | 'name'
  Prelude.Text ->
  CreateAssessmentFrameworkControlSet
newCreateAssessmentFrameworkControlSet pName_ =
  CreateAssessmentFrameworkControlSet'
    { controls =
        Prelude.Nothing,
      name = pName_
    }

-- | The list of controls within the control set. This doesn\'t contain the
-- control set ID.
createAssessmentFrameworkControlSet_controls :: Lens.Lens' CreateAssessmentFrameworkControlSet (Prelude.Maybe (Prelude.NonEmpty CreateAssessmentFrameworkControl))
createAssessmentFrameworkControlSet_controls = Lens.lens (\CreateAssessmentFrameworkControlSet' {controls} -> controls) (\s@CreateAssessmentFrameworkControlSet' {} a -> s {controls = a} :: CreateAssessmentFrameworkControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the control set.
createAssessmentFrameworkControlSet_name :: Lens.Lens' CreateAssessmentFrameworkControlSet Prelude.Text
createAssessmentFrameworkControlSet_name = Lens.lens (\CreateAssessmentFrameworkControlSet' {name} -> name) (\s@CreateAssessmentFrameworkControlSet' {} a -> s {name = a} :: CreateAssessmentFrameworkControlSet)

instance
  Prelude.Hashable
    CreateAssessmentFrameworkControlSet
  where
  hashWithSalt
    _salt
    CreateAssessmentFrameworkControlSet' {..} =
      _salt
        `Prelude.hashWithSalt` controls
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CreateAssessmentFrameworkControlSet
  where
  rnf CreateAssessmentFrameworkControlSet' {..} =
    Prelude.rnf controls `Prelude.seq` Prelude.rnf name

instance
  Data.ToJSON
    CreateAssessmentFrameworkControlSet
  where
  toJSON CreateAssessmentFrameworkControlSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("controls" Data..=) Prelude.<$> controls,
            Prelude.Just ("name" Data..= name)
          ]
      )
