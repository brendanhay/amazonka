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
-- Module      : Amazonka.EC2.Types.InstanceStateChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStateChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceState
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance state change.
--
-- /See:/ 'newInstanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { -- | The current state of the instance.
    currentState :: Prelude.Maybe InstanceState,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The previous state of the instance.
    previousState :: Prelude.Maybe InstanceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentState', 'instanceStateChange_currentState' - The current state of the instance.
--
-- 'instanceId', 'instanceStateChange_instanceId' - The ID of the instance.
--
-- 'previousState', 'instanceStateChange_previousState' - The previous state of the instance.
newInstanceStateChange ::
  InstanceStateChange
newInstanceStateChange =
  InstanceStateChange'
    { currentState =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      previousState = Prelude.Nothing
    }

-- | The current state of the instance.
instanceStateChange_currentState :: Lens.Lens' InstanceStateChange (Prelude.Maybe InstanceState)
instanceStateChange_currentState = Lens.lens (\InstanceStateChange' {currentState} -> currentState) (\s@InstanceStateChange' {} a -> s {currentState = a} :: InstanceStateChange)

-- | The ID of the instance.
instanceStateChange_instanceId :: Lens.Lens' InstanceStateChange (Prelude.Maybe Prelude.Text)
instanceStateChange_instanceId = Lens.lens (\InstanceStateChange' {instanceId} -> instanceId) (\s@InstanceStateChange' {} a -> s {instanceId = a} :: InstanceStateChange)

-- | The previous state of the instance.
instanceStateChange_previousState :: Lens.Lens' InstanceStateChange (Prelude.Maybe InstanceState)
instanceStateChange_previousState = Lens.lens (\InstanceStateChange' {previousState} -> previousState) (\s@InstanceStateChange' {} a -> s {previousState = a} :: InstanceStateChange)

instance Data.FromXML InstanceStateChange where
  parseXML x =
    InstanceStateChange'
      Prelude.<$> (x Data..@? "currentState")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "previousState")

instance Prelude.Hashable InstanceStateChange where
  hashWithSalt _salt InstanceStateChange' {..} =
    _salt
      `Prelude.hashWithSalt` currentState
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` previousState

instance Prelude.NFData InstanceStateChange where
  rnf InstanceStateChange' {..} =
    Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf previousState
