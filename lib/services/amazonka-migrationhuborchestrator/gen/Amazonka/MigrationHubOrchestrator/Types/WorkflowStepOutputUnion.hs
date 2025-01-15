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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutputUnion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutputUnion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure to hold multiple values of an output.
--
-- /See:/ 'newWorkflowStepOutputUnion' smart constructor.
data WorkflowStepOutputUnion = WorkflowStepOutputUnion'
  { -- | The integer value.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | The list of string value.
    listOfStringValue :: Prelude.Maybe [Prelude.Text],
    -- | The string value.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepOutputUnion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'workflowStepOutputUnion_integerValue' - The integer value.
--
-- 'listOfStringValue', 'workflowStepOutputUnion_listOfStringValue' - The list of string value.
--
-- 'stringValue', 'workflowStepOutputUnion_stringValue' - The string value.
newWorkflowStepOutputUnion ::
  WorkflowStepOutputUnion
newWorkflowStepOutputUnion =
  WorkflowStepOutputUnion'
    { integerValue =
        Prelude.Nothing,
      listOfStringValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The integer value.
workflowStepOutputUnion_integerValue :: Lens.Lens' WorkflowStepOutputUnion (Prelude.Maybe Prelude.Int)
workflowStepOutputUnion_integerValue = Lens.lens (\WorkflowStepOutputUnion' {integerValue} -> integerValue) (\s@WorkflowStepOutputUnion' {} a -> s {integerValue = a} :: WorkflowStepOutputUnion)

-- | The list of string value.
workflowStepOutputUnion_listOfStringValue :: Lens.Lens' WorkflowStepOutputUnion (Prelude.Maybe [Prelude.Text])
workflowStepOutputUnion_listOfStringValue = Lens.lens (\WorkflowStepOutputUnion' {listOfStringValue} -> listOfStringValue) (\s@WorkflowStepOutputUnion' {} a -> s {listOfStringValue = a} :: WorkflowStepOutputUnion) Prelude.. Lens.mapping Lens.coerced

-- | The string value.
workflowStepOutputUnion_stringValue :: Lens.Lens' WorkflowStepOutputUnion (Prelude.Maybe Prelude.Text)
workflowStepOutputUnion_stringValue = Lens.lens (\WorkflowStepOutputUnion' {stringValue} -> stringValue) (\s@WorkflowStepOutputUnion' {} a -> s {stringValue = a} :: WorkflowStepOutputUnion)

instance Data.FromJSON WorkflowStepOutputUnion where
  parseJSON =
    Data.withObject
      "WorkflowStepOutputUnion"
      ( \x ->
          WorkflowStepOutputUnion'
            Prelude.<$> (x Data..:? "integerValue")
            Prelude.<*> ( x
                            Data..:? "listOfStringValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable WorkflowStepOutputUnion where
  hashWithSalt _salt WorkflowStepOutputUnion' {..} =
    _salt
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` listOfStringValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData WorkflowStepOutputUnion where
  rnf WorkflowStepOutputUnion' {..} =
    Prelude.rnf integerValue `Prelude.seq`
      Prelude.rnf listOfStringValue `Prelude.seq`
        Prelude.rnf stringValue

instance Data.ToJSON WorkflowStepOutputUnion where
  toJSON WorkflowStepOutputUnion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("integerValue" Data..=) Prelude.<$> integerValue,
            ("listOfStringValue" Data..=)
              Prelude.<$> listOfStringValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
