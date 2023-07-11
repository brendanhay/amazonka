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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlowSummary where

import Amazonka.Connect.Types.ContactFlowState
import Amazonka.Connect.Types.ContactFlowType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a flow.
--
-- You can also create and update flows using the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/flow-language.html Amazon Connect Flow language>.
--
-- /See:/ 'newContactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { -- | The Amazon Resource Name (ARN) of the flow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of flow.
    contactFlowState :: Prelude.Maybe ContactFlowState,
    -- | The type of flow.
    contactFlowType :: Prelude.Maybe ContactFlowType,
    -- | The identifier of the flow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow.
    name :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'contactFlowSummary_arn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'contactFlowState', 'contactFlowSummary_contactFlowState' - The type of flow.
--
-- 'contactFlowType', 'contactFlowSummary_contactFlowType' - The type of flow.
--
-- 'id', 'contactFlowSummary_id' - The identifier of the flow.
--
-- 'name', 'contactFlowSummary_name' - The name of the flow.
newContactFlowSummary ::
  ContactFlowSummary
newContactFlowSummary =
  ContactFlowSummary'
    { arn = Prelude.Nothing,
      contactFlowState = Prelude.Nothing,
      contactFlowType = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the flow.
contactFlowSummary_arn :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_arn = Lens.lens (\ContactFlowSummary' {arn} -> arn) (\s@ContactFlowSummary' {} a -> s {arn = a} :: ContactFlowSummary)

-- | The type of flow.
contactFlowSummary_contactFlowState :: Lens.Lens' ContactFlowSummary (Prelude.Maybe ContactFlowState)
contactFlowSummary_contactFlowState = Lens.lens (\ContactFlowSummary' {contactFlowState} -> contactFlowState) (\s@ContactFlowSummary' {} a -> s {contactFlowState = a} :: ContactFlowSummary)

-- | The type of flow.
contactFlowSummary_contactFlowType :: Lens.Lens' ContactFlowSummary (Prelude.Maybe ContactFlowType)
contactFlowSummary_contactFlowType = Lens.lens (\ContactFlowSummary' {contactFlowType} -> contactFlowType) (\s@ContactFlowSummary' {} a -> s {contactFlowType = a} :: ContactFlowSummary)

-- | The identifier of the flow.
contactFlowSummary_id :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_id = Lens.lens (\ContactFlowSummary' {id} -> id) (\s@ContactFlowSummary' {} a -> s {id = a} :: ContactFlowSummary)

-- | The name of the flow.
contactFlowSummary_name :: Lens.Lens' ContactFlowSummary (Prelude.Maybe Prelude.Text)
contactFlowSummary_name = Lens.lens (\ContactFlowSummary' {name} -> name) (\s@ContactFlowSummary' {} a -> s {name = a} :: ContactFlowSummary)

instance Data.FromJSON ContactFlowSummary where
  parseJSON =
    Data.withObject
      "ContactFlowSummary"
      ( \x ->
          ContactFlowSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ContactFlowState")
            Prelude.<*> (x Data..:? "ContactFlowType")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ContactFlowSummary where
  hashWithSalt _salt ContactFlowSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` contactFlowState
      `Prelude.hashWithSalt` contactFlowType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ContactFlowSummary where
  rnf ContactFlowSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf contactFlowState
      `Prelude.seq` Prelude.rnf contactFlowType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
