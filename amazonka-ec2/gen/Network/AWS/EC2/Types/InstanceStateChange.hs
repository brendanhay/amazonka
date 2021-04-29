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
-- Module      : Network.AWS.EC2.Types.InstanceStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateChange where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance state change.
--
-- /See:/ 'newInstanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the instance.
    currentState :: Prelude.Maybe InstanceState,
    -- | The previous state of the instance.
    previousState :: Prelude.Maybe InstanceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceStateChange_instanceId' - The ID of the instance.
--
-- 'currentState', 'instanceStateChange_currentState' - The current state of the instance.
--
-- 'previousState', 'instanceStateChange_previousState' - The previous state of the instance.
newInstanceStateChange ::
  InstanceStateChange
newInstanceStateChange =
  InstanceStateChange'
    { instanceId = Prelude.Nothing,
      currentState = Prelude.Nothing,
      previousState = Prelude.Nothing
    }

-- | The ID of the instance.
instanceStateChange_instanceId :: Lens.Lens' InstanceStateChange (Prelude.Maybe Prelude.Text)
instanceStateChange_instanceId = Lens.lens (\InstanceStateChange' {instanceId} -> instanceId) (\s@InstanceStateChange' {} a -> s {instanceId = a} :: InstanceStateChange)

-- | The current state of the instance.
instanceStateChange_currentState :: Lens.Lens' InstanceStateChange (Prelude.Maybe InstanceState)
instanceStateChange_currentState = Lens.lens (\InstanceStateChange' {currentState} -> currentState) (\s@InstanceStateChange' {} a -> s {currentState = a} :: InstanceStateChange)

-- | The previous state of the instance.
instanceStateChange_previousState :: Lens.Lens' InstanceStateChange (Prelude.Maybe InstanceState)
instanceStateChange_previousState = Lens.lens (\InstanceStateChange' {previousState} -> previousState) (\s@InstanceStateChange' {} a -> s {previousState = a} :: InstanceStateChange)

instance Prelude.FromXML InstanceStateChange where
  parseXML x =
    InstanceStateChange'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "currentState")
      Prelude.<*> (x Prelude..@? "previousState")

instance Prelude.Hashable InstanceStateChange

instance Prelude.NFData InstanceStateChange
