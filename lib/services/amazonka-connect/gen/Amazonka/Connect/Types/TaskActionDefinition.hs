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
-- Module      : Amazonka.Connect.Types.TaskActionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskActionDefinition where

import Amazonka.Connect.Types.Reference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the task action.
--
-- /See:/ 'newTaskActionDefinition' smart constructor.
data TaskActionDefinition = TaskActionDefinition'
  { -- | The description. Supports variable injection. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
    -- in the /Amazon Connect Administrators Guide/.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the reference when the @referenceType@ is @URL@.
    -- Otherwise, null. (Supports variable injection in the @Value@ field.)
    references :: Prelude.Maybe (Prelude.HashMap Prelude.Text Reference),
    -- | The name. Supports variable injection. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
    -- in the /Amazon Connect Administrators Guide/.
    name :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'taskActionDefinition_description' - The description. Supports variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
--
-- 'references', 'taskActionDefinition_references' - Information about the reference when the @referenceType@ is @URL@.
-- Otherwise, null. (Supports variable injection in the @Value@ field.)
--
-- 'name', 'taskActionDefinition_name' - The name. Supports variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
--
-- 'contactFlowId', 'taskActionDefinition_contactFlowId' - The identifier of the flow.
newTaskActionDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  TaskActionDefinition
newTaskActionDefinition pName_ pContactFlowId_ =
  TaskActionDefinition'
    { description =
        Prelude.Nothing,
      references = Prelude.Nothing,
      name = pName_,
      contactFlowId = pContactFlowId_
    }

-- | The description. Supports variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
taskActionDefinition_description :: Lens.Lens' TaskActionDefinition (Prelude.Maybe Prelude.Text)
taskActionDefinition_description = Lens.lens (\TaskActionDefinition' {description} -> description) (\s@TaskActionDefinition' {} a -> s {description = a} :: TaskActionDefinition)

-- | Information about the reference when the @referenceType@ is @URL@.
-- Otherwise, null. (Supports variable injection in the @Value@ field.)
taskActionDefinition_references :: Lens.Lens' TaskActionDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Reference))
taskActionDefinition_references = Lens.lens (\TaskActionDefinition' {references} -> references) (\s@TaskActionDefinition' {} a -> s {references = a} :: TaskActionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name. Supports variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
taskActionDefinition_name :: Lens.Lens' TaskActionDefinition Prelude.Text
taskActionDefinition_name = Lens.lens (\TaskActionDefinition' {name} -> name) (\s@TaskActionDefinition' {} a -> s {name = a} :: TaskActionDefinition)

-- | The identifier of the flow.
taskActionDefinition_contactFlowId :: Lens.Lens' TaskActionDefinition Prelude.Text
taskActionDefinition_contactFlowId = Lens.lens (\TaskActionDefinition' {contactFlowId} -> contactFlowId) (\s@TaskActionDefinition' {} a -> s {contactFlowId = a} :: TaskActionDefinition)

instance Data.FromJSON TaskActionDefinition where
  parseJSON =
    Data.withObject
      "TaskActionDefinition"
      ( \x ->
          TaskActionDefinition'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "References" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ContactFlowId")
      )

instance Prelude.Hashable TaskActionDefinition where
  hashWithSalt _salt TaskActionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` references
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` contactFlowId

instance Prelude.NFData TaskActionDefinition where
  rnf TaskActionDefinition' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf references
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf contactFlowId

instance Data.ToJSON TaskActionDefinition where
  toJSON TaskActionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("References" Data..=) Prelude.<$> references,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ContactFlowId" Data..= contactFlowId)
          ]
      )
