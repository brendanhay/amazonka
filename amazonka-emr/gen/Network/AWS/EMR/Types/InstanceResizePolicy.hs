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
-- Module      : Network.AWS.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceResizePolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
--
-- /See:/ 'newInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { -- | Decommissioning timeout override for the specific list of instances to
    -- be terminated.
    instanceTerminationTimeout :: Prelude.Maybe Prelude.Int,
    -- | Specific list of instances to be terminated when shrinking an instance
    -- group.
    instancesToTerminate :: Prelude.Maybe [Prelude.Text],
    -- | Specific list of instances to be protected when shrinking an instance
    -- group.
    instancesToProtect :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceResizePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTerminationTimeout', 'instanceResizePolicy_instanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to
-- be terminated.
--
-- 'instancesToTerminate', 'instanceResizePolicy_instancesToTerminate' - Specific list of instances to be terminated when shrinking an instance
-- group.
--
-- 'instancesToProtect', 'instanceResizePolicy_instancesToProtect' - Specific list of instances to be protected when shrinking an instance
-- group.
newInstanceResizePolicy ::
  InstanceResizePolicy
newInstanceResizePolicy =
  InstanceResizePolicy'
    { instanceTerminationTimeout =
        Prelude.Nothing,
      instancesToTerminate = Prelude.Nothing,
      instancesToProtect = Prelude.Nothing
    }

-- | Decommissioning timeout override for the specific list of instances to
-- be terminated.
instanceResizePolicy_instanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe Prelude.Int)
instanceResizePolicy_instanceTerminationTimeout = Lens.lens (\InstanceResizePolicy' {instanceTerminationTimeout} -> instanceTerminationTimeout) (\s@InstanceResizePolicy' {} a -> s {instanceTerminationTimeout = a} :: InstanceResizePolicy)

-- | Specific list of instances to be terminated when shrinking an instance
-- group.
instanceResizePolicy_instancesToTerminate :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToTerminate = Lens.lens (\InstanceResizePolicy' {instancesToTerminate} -> instancesToTerminate) (\s@InstanceResizePolicy' {} a -> s {instancesToTerminate = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Prelude._Coerce

-- | Specific list of instances to be protected when shrinking an instance
-- group.
instanceResizePolicy_instancesToProtect :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToProtect = Lens.lens (\InstanceResizePolicy' {instancesToProtect} -> instancesToProtect) (\s@InstanceResizePolicy' {} a -> s {instancesToProtect = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON InstanceResizePolicy where
  parseJSON =
    Prelude.withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            Prelude.<$> (x Prelude..:? "InstanceTerminationTimeout")
            Prelude.<*> ( x Prelude..:? "InstancesToTerminate"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "InstancesToProtect"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceResizePolicy

instance Prelude.NFData InstanceResizePolicy

instance Prelude.ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceTerminationTimeout" Prelude..=)
              Prelude.<$> instanceTerminationTimeout,
            ("InstancesToTerminate" Prelude..=)
              Prelude.<$> instancesToTerminate,
            ("InstancesToProtect" Prelude..=)
              Prelude.<$> instancesToProtect
          ]
      )
