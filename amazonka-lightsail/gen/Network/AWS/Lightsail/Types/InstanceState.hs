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
-- Module      : Network.AWS.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the virtual private server (or /instance/) status.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The status code for the instance.
    code :: Core.Maybe Core.Int,
    -- | The state of the instance (e.g., @running@ or @pending@).
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'instanceState_code' - The status code for the instance.
--
-- 'name', 'instanceState_name' - The state of the instance (e.g., @running@ or @pending@).
newInstanceState ::
  InstanceState
newInstanceState =
  InstanceState'
    { code = Core.Nothing,
      name = Core.Nothing
    }

-- | The status code for the instance.
instanceState_code :: Lens.Lens' InstanceState (Core.Maybe Core.Int)
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

-- | The state of the instance (e.g., @running@ or @pending@).
instanceState_name :: Lens.Lens' InstanceState (Core.Maybe Core.Text)
instanceState_name = Lens.lens (\InstanceState' {name} -> name) (\s@InstanceState' {} a -> s {name = a} :: InstanceState)

instance Core.FromJSON InstanceState where
  parseJSON =
    Core.withObject
      "InstanceState"
      ( \x ->
          InstanceState'
            Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "name")
      )

instance Core.Hashable InstanceState

instance Core.NFData InstanceState
