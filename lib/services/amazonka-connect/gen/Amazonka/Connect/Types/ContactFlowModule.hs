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
-- Module      : Amazonka.Connect.Types.ContactFlowModule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlowModule where

import Amazonka.Connect.Types.ContactFlowModuleState
import Amazonka.Connect.Types.ContactFlowModuleStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a flow module.
--
-- /See:/ 'newContactFlowModule' smart constructor.
data ContactFlowModule = ContactFlowModule'
  { -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The content of the flow module.
    content :: Prelude.Maybe Prelude.Text,
    -- | The description of the flow module.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow module.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow module.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of flow module.
    state :: Prelude.Maybe ContactFlowModuleState,
    -- | The status of the flow module.
    status :: Prelude.Maybe ContactFlowModuleStatus,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactFlowModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'contactFlowModule_arn' - The Amazon Resource Name (ARN).
--
-- 'content', 'contactFlowModule_content' - The content of the flow module.
--
-- 'description', 'contactFlowModule_description' - The description of the flow module.
--
-- 'id', 'contactFlowModule_id' - The identifier of the flow module.
--
-- 'name', 'contactFlowModule_name' - The name of the flow module.
--
-- 'state', 'contactFlowModule_state' - The type of flow module.
--
-- 'status', 'contactFlowModule_status' - The status of the flow module.
--
-- 'tags', 'contactFlowModule_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newContactFlowModule ::
  ContactFlowModule
newContactFlowModule =
  ContactFlowModule'
    { arn = Prelude.Nothing,
      content = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN).
contactFlowModule_arn :: Lens.Lens' ContactFlowModule (Prelude.Maybe Prelude.Text)
contactFlowModule_arn = Lens.lens (\ContactFlowModule' {arn} -> arn) (\s@ContactFlowModule' {} a -> s {arn = a} :: ContactFlowModule)

-- | The content of the flow module.
contactFlowModule_content :: Lens.Lens' ContactFlowModule (Prelude.Maybe Prelude.Text)
contactFlowModule_content = Lens.lens (\ContactFlowModule' {content} -> content) (\s@ContactFlowModule' {} a -> s {content = a} :: ContactFlowModule)

-- | The description of the flow module.
contactFlowModule_description :: Lens.Lens' ContactFlowModule (Prelude.Maybe Prelude.Text)
contactFlowModule_description = Lens.lens (\ContactFlowModule' {description} -> description) (\s@ContactFlowModule' {} a -> s {description = a} :: ContactFlowModule)

-- | The identifier of the flow module.
contactFlowModule_id :: Lens.Lens' ContactFlowModule (Prelude.Maybe Prelude.Text)
contactFlowModule_id = Lens.lens (\ContactFlowModule' {id} -> id) (\s@ContactFlowModule' {} a -> s {id = a} :: ContactFlowModule)

-- | The name of the flow module.
contactFlowModule_name :: Lens.Lens' ContactFlowModule (Prelude.Maybe Prelude.Text)
contactFlowModule_name = Lens.lens (\ContactFlowModule' {name} -> name) (\s@ContactFlowModule' {} a -> s {name = a} :: ContactFlowModule)

-- | The type of flow module.
contactFlowModule_state :: Lens.Lens' ContactFlowModule (Prelude.Maybe ContactFlowModuleState)
contactFlowModule_state = Lens.lens (\ContactFlowModule' {state} -> state) (\s@ContactFlowModule' {} a -> s {state = a} :: ContactFlowModule)

-- | The status of the flow module.
contactFlowModule_status :: Lens.Lens' ContactFlowModule (Prelude.Maybe ContactFlowModuleStatus)
contactFlowModule_status = Lens.lens (\ContactFlowModule' {status} -> status) (\s@ContactFlowModule' {} a -> s {status = a} :: ContactFlowModule)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
contactFlowModule_tags :: Lens.Lens' ContactFlowModule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contactFlowModule_tags = Lens.lens (\ContactFlowModule' {tags} -> tags) (\s@ContactFlowModule' {} a -> s {tags = a} :: ContactFlowModule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContactFlowModule where
  parseJSON =
    Data.withObject
      "ContactFlowModule"
      ( \x ->
          ContactFlowModule'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContactFlowModule where
  hashWithSalt _salt ContactFlowModule' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ContactFlowModule where
  rnf ContactFlowModule' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
