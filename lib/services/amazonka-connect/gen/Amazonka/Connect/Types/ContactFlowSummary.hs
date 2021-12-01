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
-- Module      : Amazonka.Connect.Types.ContactFlowSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlowSummary where

import Amazonka.Connect.Types.ContactFlowType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a contact flow.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
--
-- /See:/ 'newContactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of contact flow.
    contactFlowType :: Prelude.Maybe ContactFlowType,
    -- | The identifier of the contact flow.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactFlowSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'contactFlowSummary_arn' - The Amazon Resource Name (ARN) of the contact flow.
--
-- 'name', 'contactFlowSummary_name' - The name of the contact flow.
--
-- 'contactFlowType', 'contactFlowSummary_contactFlowType' - The type of contact flow.
--
-- 'id', 'contactFlowSummary_id' - The identifier of the contact flow.
newContactFlowSummary ::
  ContactFlowSummary
newContactFlowSummary =
  ContactFlowSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      contactFlowType = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
contactFlowSummary_arn :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_arn = Lens.lens (\ContactFlowSummary' {arn} -> arn) (\s@ContactFlowSummary' {} a -> s {arn = a} :: ContactFlowSummary)

-- | The name of the contact flow.
contactFlowSummary_name :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_name = Lens.lens (\ContactFlowSummary' {name} -> name) (\s@ContactFlowSummary' {} a -> s {name = a} :: ContactFlowSummary)

-- | The type of contact flow.
contactFlowSummary_contactFlowType :: Lens.Lens' ContactFlowSummary (Prelude.Maybe ContactFlowType)
contactFlowSummary_contactFlowType = Lens.lens (\ContactFlowSummary' {contactFlowType} -> contactFlowType) (\s@ContactFlowSummary' {} a -> s {contactFlowType = a} :: ContactFlowSummary)

-- | The identifier of the contact flow.
contactFlowSummary_id :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_id = Lens.lens (\ContactFlowSummary' {id} -> id) (\s@ContactFlowSummary' {} a -> s {id = a} :: ContactFlowSummary)

instance Core.FromJSON ContactFlowSummary where
  parseJSON =
    Core.withObject
      "ContactFlowSummary"
      ( \x ->
          ContactFlowSummary'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ContactFlowType")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ContactFlowSummary where
  hashWithSalt salt' ContactFlowSummary' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` contactFlowType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ContactFlowSummary where
  rnf ContactFlowSummary' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf contactFlowType
      `Prelude.seq` Prelude.rnf name
