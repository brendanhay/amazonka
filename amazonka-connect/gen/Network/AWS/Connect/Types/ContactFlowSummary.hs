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
-- Module      : Network.AWS.Connect.Types.ContactFlowSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowSummary where

import Network.AWS.Connect.Types.ContactFlowType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a contact flow.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
--
-- /See:/ 'newContactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the contact flow.
    id :: Core.Maybe Core.Text,
    -- | The type of contact flow.
    contactFlowType :: Core.Maybe ContactFlowType,
    -- | The name of the contact flow.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'id', 'contactFlowSummary_id' - The identifier of the contact flow.
--
-- 'contactFlowType', 'contactFlowSummary_contactFlowType' - The type of contact flow.
--
-- 'name', 'contactFlowSummary_name' - The name of the contact flow.
newContactFlowSummary ::
  ContactFlowSummary
newContactFlowSummary =
  ContactFlowSummary'
    { arn = Core.Nothing,
      id = Core.Nothing,
      contactFlowType = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
contactFlowSummary_arn :: Lens.Lens' ContactFlowSummary (Core.Maybe Core.Text)
contactFlowSummary_arn = Lens.lens (\ContactFlowSummary' {arn} -> arn) (\s@ContactFlowSummary' {} a -> s {arn = a} :: ContactFlowSummary)

-- | The identifier of the contact flow.
contactFlowSummary_id :: Lens.Lens' ContactFlowSummary (Core.Maybe Core.Text)
contactFlowSummary_id = Lens.lens (\ContactFlowSummary' {id} -> id) (\s@ContactFlowSummary' {} a -> s {id = a} :: ContactFlowSummary)

-- | The type of contact flow.
contactFlowSummary_contactFlowType :: Lens.Lens' ContactFlowSummary (Core.Maybe ContactFlowType)
contactFlowSummary_contactFlowType = Lens.lens (\ContactFlowSummary' {contactFlowType} -> contactFlowType) (\s@ContactFlowSummary' {} a -> s {contactFlowType = a} :: ContactFlowSummary)

-- | The name of the contact flow.
contactFlowSummary_name :: Lens.Lens' ContactFlowSummary (Core.Maybe Core.Text)
contactFlowSummary_name = Lens.lens (\ContactFlowSummary' {name} -> name) (\s@ContactFlowSummary' {} a -> s {name = a} :: ContactFlowSummary)

instance Core.FromJSON ContactFlowSummary where
  parseJSON =
    Core.withObject
      "ContactFlowSummary"
      ( \x ->
          ContactFlowSummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "ContactFlowType")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable ContactFlowSummary

instance Core.NFData ContactFlowSummary
