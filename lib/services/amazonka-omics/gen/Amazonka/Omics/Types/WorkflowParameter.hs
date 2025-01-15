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
-- Module      : Amazonka.Omics.Types.WorkflowParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.WorkflowParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A workflow parameter.
--
-- /See:/ 'newWorkflowParameter' smart constructor.
data WorkflowParameter = WorkflowParameter'
  { -- | The parameter\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the parameter is optional.
    optional :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'workflowParameter_description' - The parameter\'s description.
--
-- 'optional', 'workflowParameter_optional' - Whether the parameter is optional.
newWorkflowParameter ::
  WorkflowParameter
newWorkflowParameter =
  WorkflowParameter'
    { description = Prelude.Nothing,
      optional = Prelude.Nothing
    }

-- | The parameter\'s description.
workflowParameter_description :: Lens.Lens' WorkflowParameter (Prelude.Maybe Prelude.Text)
workflowParameter_description = Lens.lens (\WorkflowParameter' {description} -> description) (\s@WorkflowParameter' {} a -> s {description = a} :: WorkflowParameter)

-- | Whether the parameter is optional.
workflowParameter_optional :: Lens.Lens' WorkflowParameter (Prelude.Maybe Prelude.Bool)
workflowParameter_optional = Lens.lens (\WorkflowParameter' {optional} -> optional) (\s@WorkflowParameter' {} a -> s {optional = a} :: WorkflowParameter)

instance Data.FromJSON WorkflowParameter where
  parseJSON =
    Data.withObject
      "WorkflowParameter"
      ( \x ->
          WorkflowParameter'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "optional")
      )

instance Prelude.Hashable WorkflowParameter where
  hashWithSalt _salt WorkflowParameter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` optional

instance Prelude.NFData WorkflowParameter where
  rnf WorkflowParameter' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf optional

instance Data.ToJSON WorkflowParameter where
  toJSON WorkflowParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("optional" Data..=) Prelude.<$> optional
          ]
      )
