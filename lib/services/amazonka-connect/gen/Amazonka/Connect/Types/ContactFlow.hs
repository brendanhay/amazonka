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
-- Module      : Amazonka.Connect.Types.ContactFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlow where

import Amazonka.Connect.Types.ContactFlowState
import Amazonka.Connect.Types.ContactFlowType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a flow.
--
-- /See:/ 'newContactFlow' smart constructor.
data ContactFlow = ContactFlow'
  { -- | The Amazon Resource Name (ARN) of the flow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The content of the flow.
    content :: Prelude.Maybe Prelude.Text,
    -- | The description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of flow.
    state :: Prelude.Maybe ContactFlowState,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the flow. For descriptions of the available types, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
    -- in the /Amazon Connect Administrator Guide/.
    type' :: Prelude.Maybe ContactFlowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'contactFlow_arn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'content', 'contactFlow_content' - The content of the flow.
--
-- 'description', 'contactFlow_description' - The description of the flow.
--
-- 'id', 'contactFlow_id' - The identifier of the flow.
--
-- 'name', 'contactFlow_name' - The name of the flow.
--
-- 'state', 'contactFlow_state' - The type of flow.
--
-- 'tags', 'contactFlow_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'type'', 'contactFlow_type' - The type of the flow. For descriptions of the available types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
-- in the /Amazon Connect Administrator Guide/.
newContactFlow ::
  ContactFlow
newContactFlow =
  ContactFlow'
    { arn = Prelude.Nothing,
      content = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the flow.
contactFlow_arn :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_arn = Lens.lens (\ContactFlow' {arn} -> arn) (\s@ContactFlow' {} a -> s {arn = a} :: ContactFlow)

-- | The content of the flow.
contactFlow_content :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_content = Lens.lens (\ContactFlow' {content} -> content) (\s@ContactFlow' {} a -> s {content = a} :: ContactFlow)

-- | The description of the flow.
contactFlow_description :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_description = Lens.lens (\ContactFlow' {description} -> description) (\s@ContactFlow' {} a -> s {description = a} :: ContactFlow)

-- | The identifier of the flow.
contactFlow_id :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_id = Lens.lens (\ContactFlow' {id} -> id) (\s@ContactFlow' {} a -> s {id = a} :: ContactFlow)

-- | The name of the flow.
contactFlow_name :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_name = Lens.lens (\ContactFlow' {name} -> name) (\s@ContactFlow' {} a -> s {name = a} :: ContactFlow)

-- | The type of flow.
contactFlow_state :: Lens.Lens' ContactFlow (Prelude.Maybe ContactFlowState)
contactFlow_state = Lens.lens (\ContactFlow' {state} -> state) (\s@ContactFlow' {} a -> s {state = a} :: ContactFlow)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
contactFlow_tags :: Lens.Lens' ContactFlow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contactFlow_tags = Lens.lens (\ContactFlow' {tags} -> tags) (\s@ContactFlow' {} a -> s {tags = a} :: ContactFlow) Prelude.. Lens.mapping Lens.coerced

-- | The type of the flow. For descriptions of the available types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a flow type>
-- in the /Amazon Connect Administrator Guide/.
contactFlow_type :: Lens.Lens' ContactFlow (Prelude.Maybe ContactFlowType)
contactFlow_type = Lens.lens (\ContactFlow' {type'} -> type') (\s@ContactFlow' {} a -> s {type' = a} :: ContactFlow)

instance Data.FromJSON ContactFlow where
  parseJSON =
    Data.withObject
      "ContactFlow"
      ( \x ->
          ContactFlow'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ContactFlow where
  hashWithSalt _salt ContactFlow' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ContactFlow where
  rnf ContactFlow' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
