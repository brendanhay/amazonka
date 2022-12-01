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
-- Module      : Amazonka.ImageBuilder.Types.ComponentState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ComponentState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types.ComponentStatus
import qualified Amazonka.Prelude as Prelude

-- | A group of fields that describe the current status of components that
-- are no longer active.
--
-- /See:/ 'newComponentState' smart constructor.
data ComponentState = ComponentState'
  { -- | The current state of the component.
    status :: Prelude.Maybe ComponentStatus,
    -- | Describes how or why the component changed state.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'componentState_status' - The current state of the component.
--
-- 'reason', 'componentState_reason' - Describes how or why the component changed state.
newComponentState ::
  ComponentState
newComponentState =
  ComponentState'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The current state of the component.
componentState_status :: Lens.Lens' ComponentState (Prelude.Maybe ComponentStatus)
componentState_status = Lens.lens (\ComponentState' {status} -> status) (\s@ComponentState' {} a -> s {status = a} :: ComponentState)

-- | Describes how or why the component changed state.
componentState_reason :: Lens.Lens' ComponentState (Prelude.Maybe Prelude.Text)
componentState_reason = Lens.lens (\ComponentState' {reason} -> reason) (\s@ComponentState' {} a -> s {reason = a} :: ComponentState)

instance Core.FromJSON ComponentState where
  parseJSON =
    Core.withObject
      "ComponentState"
      ( \x ->
          ComponentState'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "reason")
      )

instance Prelude.Hashable ComponentState where
  hashWithSalt _salt ComponentState' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ComponentState where
  rnf ComponentState' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf reason
