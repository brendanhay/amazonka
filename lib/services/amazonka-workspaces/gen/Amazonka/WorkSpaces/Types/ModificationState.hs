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
-- Module      : Amazonka.WorkSpaces.Types.ModificationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ModificationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.ModificationResourceEnum
import Amazonka.WorkSpaces.Types.ModificationStateEnum

-- | Describes a WorkSpace modification.
--
-- /See:/ 'newModificationState' smart constructor.
data ModificationState = ModificationState'
  { -- | The resource.
    resource :: Prelude.Maybe ModificationResourceEnum,
    -- | The modification state.
    state :: Prelude.Maybe ModificationStateEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModificationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'modificationState_resource' - The resource.
--
-- 'state', 'modificationState_state' - The modification state.
newModificationState ::
  ModificationState
newModificationState =
  ModificationState'
    { resource = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The resource.
modificationState_resource :: Lens.Lens' ModificationState (Prelude.Maybe ModificationResourceEnum)
modificationState_resource = Lens.lens (\ModificationState' {resource} -> resource) (\s@ModificationState' {} a -> s {resource = a} :: ModificationState)

-- | The modification state.
modificationState_state :: Lens.Lens' ModificationState (Prelude.Maybe ModificationStateEnum)
modificationState_state = Lens.lens (\ModificationState' {state} -> state) (\s@ModificationState' {} a -> s {state = a} :: ModificationState)

instance Data.FromJSON ModificationState where
  parseJSON =
    Data.withObject
      "ModificationState"
      ( \x ->
          ModificationState'
            Prelude.<$> (x Data..:? "Resource")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ModificationState where
  hashWithSalt _salt ModificationState' {..} =
    _salt
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` state

instance Prelude.NFData ModificationState where
  rnf ModificationState' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf state
