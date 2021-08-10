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
-- Module      : Network.AWS.Connect.Types.ContactFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlow where

import Network.AWS.Connect.Types.ContactFlowType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a contact flow.
--
-- /See:/ 'newContactFlow' smart constructor.
data ContactFlow = ContactFlow'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the contact flow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The content of the contact flow.
    content :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the contact flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the contact flow. For descriptions of the available types,
    -- see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
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
-- 'arn', 'contactFlow_arn' - The Amazon Resource Name (ARN) of the contact flow.
--
-- 'id', 'contactFlow_id' - The identifier of the contact flow.
--
-- 'name', 'contactFlow_name' - The name of the contact flow.
--
-- 'content', 'contactFlow_content' - The content of the contact flow.
--
-- 'tags', 'contactFlow_tags' - One or more tags.
--
-- 'description', 'contactFlow_description' - The description of the contact flow.
--
-- 'type'', 'contactFlow_type' - The type of the contact flow. For descriptions of the available types,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
-- in the /Amazon Connect Administrator Guide/.
newContactFlow ::
  ContactFlow
newContactFlow =
  ContactFlow'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      content = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
contactFlow_arn :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_arn = Lens.lens (\ContactFlow' {arn} -> arn) (\s@ContactFlow' {} a -> s {arn = a} :: ContactFlow)

-- | The identifier of the contact flow.
contactFlow_id :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_id = Lens.lens (\ContactFlow' {id} -> id) (\s@ContactFlow' {} a -> s {id = a} :: ContactFlow)

-- | The name of the contact flow.
contactFlow_name :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_name = Lens.lens (\ContactFlow' {name} -> name) (\s@ContactFlow' {} a -> s {name = a} :: ContactFlow)

-- | The content of the contact flow.
contactFlow_content :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_content = Lens.lens (\ContactFlow' {content} -> content) (\s@ContactFlow' {} a -> s {content = a} :: ContactFlow)

-- | One or more tags.
contactFlow_tags :: Lens.Lens' ContactFlow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contactFlow_tags = Lens.lens (\ContactFlow' {tags} -> tags) (\s@ContactFlow' {} a -> s {tags = a} :: ContactFlow) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the contact flow.
contactFlow_description :: Lens.Lens' ContactFlow (Prelude.Maybe Prelude.Text)
contactFlow_description = Lens.lens (\ContactFlow' {description} -> description) (\s@ContactFlow' {} a -> s {description = a} :: ContactFlow)

-- | The type of the contact flow. For descriptions of the available types,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type>
-- in the /Amazon Connect Administrator Guide/.
contactFlow_type :: Lens.Lens' ContactFlow (Prelude.Maybe ContactFlowType)
contactFlow_type = Lens.lens (\ContactFlow' {type'} -> type') (\s@ContactFlow' {} a -> s {type' = a} :: ContactFlow)

instance Core.FromJSON ContactFlow where
  parseJSON =
    Core.withObject
      "ContactFlow"
      ( \x ->
          ContactFlow'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Content")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable ContactFlow

instance Prelude.NFData ContactFlow
