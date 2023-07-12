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
-- Module      : Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycleState
import qualified Amazonka.Prelude as Prelude

-- | The request to change the source server migration lifecycle state.
--
-- /See:/ 'newChangeServerLifeCycleStateSourceServerLifecycle' smart constructor.
data ChangeServerLifeCycleStateSourceServerLifecycle = ChangeServerLifeCycleStateSourceServerLifecycle'
  { -- | The request to change the source server migration lifecycle state.
    state :: ChangeServerLifeCycleStateSourceServerLifecycleState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeServerLifeCycleStateSourceServerLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'changeServerLifeCycleStateSourceServerLifecycle_state' - The request to change the source server migration lifecycle state.
newChangeServerLifeCycleStateSourceServerLifecycle ::
  -- | 'state'
  ChangeServerLifeCycleStateSourceServerLifecycleState ->
  ChangeServerLifeCycleStateSourceServerLifecycle
newChangeServerLifeCycleStateSourceServerLifecycle
  pState_ =
    ChangeServerLifeCycleStateSourceServerLifecycle'
      { state =
          pState_
      }

-- | The request to change the source server migration lifecycle state.
changeServerLifeCycleStateSourceServerLifecycle_state :: Lens.Lens' ChangeServerLifeCycleStateSourceServerLifecycle ChangeServerLifeCycleStateSourceServerLifecycleState
changeServerLifeCycleStateSourceServerLifecycle_state = Lens.lens (\ChangeServerLifeCycleStateSourceServerLifecycle' {state} -> state) (\s@ChangeServerLifeCycleStateSourceServerLifecycle' {} a -> s {state = a} :: ChangeServerLifeCycleStateSourceServerLifecycle)

instance
  Prelude.Hashable
    ChangeServerLifeCycleStateSourceServerLifecycle
  where
  hashWithSalt
    _salt
    ChangeServerLifeCycleStateSourceServerLifecycle' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    ChangeServerLifeCycleStateSourceServerLifecycle
  where
  rnf
    ChangeServerLifeCycleStateSourceServerLifecycle' {..} =
      Prelude.rnf state

instance
  Data.ToJSON
    ChangeServerLifeCycleStateSourceServerLifecycle
  where
  toJSON
    ChangeServerLifeCycleStateSourceServerLifecycle' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("state" Data..= state)]
        )
