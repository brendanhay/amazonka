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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationStateEnum

-- | Describes a WorkSpace modification.
--
-- /See:/ 'newModificationState' smart constructor.
data ModificationState = ModificationState'
  { -- | The modification state.
    state :: Core.Maybe ModificationStateEnum,
    -- | The resource.
    resource :: Core.Maybe ModificationResourceEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { state = Core.Nothing,
      resource = Core.Nothing
    }

-- | The modification state.
modificationState_state :: Lens.Lens' ModificationState (Core.Maybe ModificationStateEnum)
modificationState_state = Lens.lens (\ModificationState' {state} -> state) (\s@ModificationState' {} a -> s {state = a} :: ModificationState)

-- | The resource.
modificationState_resource :: Lens.Lens' ModificationState (Core.Maybe ModificationResourceEnum)
modificationState_resource = Lens.lens (\ModificationState' {resource} -> resource) (\s@ModificationState' {} a -> s {resource = a} :: ModificationState)

instance Core.FromJSON ModificationState where
  parseJSON =
    Core.withObject
      "ModificationState"
      ( \x ->
          ModificationState'
            Core.<$> (x Core..:? "State")
            Core.<*> (x Core..:? "Resource")
      )

instance Core.Hashable ModificationState

instance Core.NFData ModificationState
