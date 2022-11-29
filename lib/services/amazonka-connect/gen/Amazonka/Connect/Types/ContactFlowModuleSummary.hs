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
-- Module      : Amazonka.Connect.Types.ContactFlowModuleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlowModuleSummary where

import Amazonka.Connect.Types.ContactFlowModuleState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a flow.
--
-- /See:/ 'newContactFlowModuleSummary' smart constructor.
data ContactFlowModuleSummary = ContactFlowModuleSummary'
  { -- | The name of the flow module.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow module.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of flow module.
    state :: Prelude.Maybe ContactFlowModuleState,
    -- | The identifier of the flow module.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactFlowModuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'contactFlowModuleSummary_name' - The name of the flow module.
--
-- 'arn', 'contactFlowModuleSummary_arn' - The Amazon Resource Name (ARN) of the flow module.
--
-- 'state', 'contactFlowModuleSummary_state' - The type of flow module.
--
-- 'id', 'contactFlowModuleSummary_id' - The identifier of the flow module.
newContactFlowModuleSummary ::
  ContactFlowModuleSummary
newContactFlowModuleSummary =
  ContactFlowModuleSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The name of the flow module.
contactFlowModuleSummary_name :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_name = Lens.lens (\ContactFlowModuleSummary' {name} -> name) (\s@ContactFlowModuleSummary' {} a -> s {name = a} :: ContactFlowModuleSummary)

-- | The Amazon Resource Name (ARN) of the flow module.
contactFlowModuleSummary_arn :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_arn = Lens.lens (\ContactFlowModuleSummary' {arn} -> arn) (\s@ContactFlowModuleSummary' {} a -> s {arn = a} :: ContactFlowModuleSummary)

-- | The type of flow module.
contactFlowModuleSummary_state :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe ContactFlowModuleState)
contactFlowModuleSummary_state = Lens.lens (\ContactFlowModuleSummary' {state} -> state) (\s@ContactFlowModuleSummary' {} a -> s {state = a} :: ContactFlowModuleSummary)

-- | The identifier of the flow module.
contactFlowModuleSummary_id :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_id = Lens.lens (\ContactFlowModuleSummary' {id} -> id) (\s@ContactFlowModuleSummary' {} a -> s {id = a} :: ContactFlowModuleSummary)

instance Core.FromJSON ContactFlowModuleSummary where
  parseJSON =
    Core.withObject
      "ContactFlowModuleSummary"
      ( \x ->
          ContactFlowModuleSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ContactFlowModuleSummary where
  hashWithSalt _salt ContactFlowModuleSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id

instance Prelude.NFData ContactFlowModuleSummary where
  rnf ContactFlowModuleSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
