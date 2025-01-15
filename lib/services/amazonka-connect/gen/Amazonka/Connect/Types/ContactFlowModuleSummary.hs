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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFlowModuleSummary where

import Amazonka.Connect.Types.ContactFlowModuleState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a flow.
--
-- /See:/ 'newContactFlowModuleSummary' smart constructor.
data ContactFlowModuleSummary = ContactFlowModuleSummary'
  { -- | The Amazon Resource Name (ARN) of the flow module.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the flow module.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow module.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of flow module.
    state :: Prelude.Maybe ContactFlowModuleState
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
-- 'arn', 'contactFlowModuleSummary_arn' - The Amazon Resource Name (ARN) of the flow module.
--
-- 'id', 'contactFlowModuleSummary_id' - The identifier of the flow module.
--
-- 'name', 'contactFlowModuleSummary_name' - The name of the flow module.
--
-- 'state', 'contactFlowModuleSummary_state' - The type of flow module.
newContactFlowModuleSummary ::
  ContactFlowModuleSummary
newContactFlowModuleSummary =
  ContactFlowModuleSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the flow module.
contactFlowModuleSummary_arn :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_arn = Lens.lens (\ContactFlowModuleSummary' {arn} -> arn) (\s@ContactFlowModuleSummary' {} a -> s {arn = a} :: ContactFlowModuleSummary)

-- | The identifier of the flow module.
contactFlowModuleSummary_id :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_id = Lens.lens (\ContactFlowModuleSummary' {id} -> id) (\s@ContactFlowModuleSummary' {} a -> s {id = a} :: ContactFlowModuleSummary)

-- | The name of the flow module.
contactFlowModuleSummary_name :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe Prelude.Text)
contactFlowModuleSummary_name = Lens.lens (\ContactFlowModuleSummary' {name} -> name) (\s@ContactFlowModuleSummary' {} a -> s {name = a} :: ContactFlowModuleSummary)

-- | The type of flow module.
contactFlowModuleSummary_state :: Lens.Lens' ContactFlowModuleSummary (Prelude.Maybe ContactFlowModuleState)
contactFlowModuleSummary_state = Lens.lens (\ContactFlowModuleSummary' {state} -> state) (\s@ContactFlowModuleSummary' {} a -> s {state = a} :: ContactFlowModuleSummary)

instance Data.FromJSON ContactFlowModuleSummary where
  parseJSON =
    Data.withObject
      "ContactFlowModuleSummary"
      ( \x ->
          ContactFlowModuleSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ContactFlowModuleSummary where
  hashWithSalt _salt ContactFlowModuleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData ContactFlowModuleSummary where
  rnf ContactFlowModuleSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf state
