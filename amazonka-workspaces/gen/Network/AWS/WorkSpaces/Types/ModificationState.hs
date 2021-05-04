{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.Types.ModificationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationState where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationStateEnum

-- | Describes a WorkSpace modification.
--
-- /See:/ 'newModificationState' smart constructor.
data ModificationState = ModificationState'
  { -- | The modification state.
    state :: Prelude.Maybe ModificationStateEnum,
    -- | The resource.
    resource :: Prelude.Maybe ModificationResourceEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModificationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'modificationState_state' - The modification state.
--
-- 'resource', 'modificationState_resource' - The resource.
newModificationState ::
  ModificationState
newModificationState =
  ModificationState'
    { state = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | The modification state.
modificationState_state :: Lens.Lens' ModificationState (Prelude.Maybe ModificationStateEnum)
modificationState_state = Lens.lens (\ModificationState' {state} -> state) (\s@ModificationState' {} a -> s {state = a} :: ModificationState)

-- | The resource.
modificationState_resource :: Lens.Lens' ModificationState (Prelude.Maybe ModificationResourceEnum)
modificationState_resource = Lens.lens (\ModificationState' {resource} -> resource) (\s@ModificationState' {} a -> s {resource = a} :: ModificationState)

instance Prelude.FromJSON ModificationState where
  parseJSON =
    Prelude.withObject
      "ModificationState"
      ( \x ->
          ModificationState'
            Prelude.<$> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Resource")
      )

instance Prelude.Hashable ModificationState

instance Prelude.NFData ModificationState
