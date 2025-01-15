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
-- Module      : Amazonka.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceResizePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
--
-- /See:/ 'newInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { -- | Decommissioning timeout override for the specific list of instances to
    -- be terminated.
    instanceTerminationTimeout :: Prelude.Maybe Prelude.Int,
    -- | Specific list of instances to be protected when shrinking an instance
    -- group.
    instancesToProtect :: Prelude.Maybe [Prelude.Text],
    -- | Specific list of instances to be terminated when shrinking an instance
    -- group.
    instancesToTerminate :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'instancesToProtect', 'instanceResizePolicy_instancesToProtect' - Specific list of instances to be protected when shrinking an instance
-- group.
--
-- 'instancesToTerminate', 'instanceResizePolicy_instancesToTerminate' - Specific list of instances to be terminated when shrinking an instance
-- group.
newInstanceResizePolicy ::
  InstanceResizePolicy
newInstanceResizePolicy =
  InstanceResizePolicy'
    { instanceTerminationTimeout =
        Prelude.Nothing,
      instancesToProtect = Prelude.Nothing,
      instancesToTerminate = Prelude.Nothing
    }

-- | Decommissioning timeout override for the specific list of instances to
-- be terminated.
instanceResizePolicy_instanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe Prelude.Int)
instanceResizePolicy_instanceTerminationTimeout = Lens.lens (\InstanceResizePolicy' {instanceTerminationTimeout} -> instanceTerminationTimeout) (\s@InstanceResizePolicy' {} a -> s {instanceTerminationTimeout = a} :: InstanceResizePolicy)

-- | Specific list of instances to be protected when shrinking an instance
-- group.
instanceResizePolicy_instancesToProtect :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToProtect = Lens.lens (\InstanceResizePolicy' {instancesToProtect} -> instancesToProtect) (\s@InstanceResizePolicy' {} a -> s {instancesToProtect = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specific list of instances to be terminated when shrinking an instance
-- group.
instanceResizePolicy_instancesToTerminate :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToTerminate = Lens.lens (\InstanceResizePolicy' {instancesToTerminate} -> instancesToTerminate) (\s@InstanceResizePolicy' {} a -> s {instancesToTerminate = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceResizePolicy where
  parseJSON =
    Data.withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            Prelude.<$> (x Data..:? "InstanceTerminationTimeout")
            Prelude.<*> ( x
                            Data..:? "InstancesToProtect"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "InstancesToTerminate"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceResizePolicy where
  hashWithSalt _salt InstanceResizePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` instanceTerminationTimeout
      `Prelude.hashWithSalt` instancesToProtect
      `Prelude.hashWithSalt` instancesToTerminate

instance Prelude.NFData InstanceResizePolicy where
  rnf InstanceResizePolicy' {..} =
    Prelude.rnf instanceTerminationTimeout `Prelude.seq`
      Prelude.rnf instancesToProtect `Prelude.seq`
        Prelude.rnf instancesToTerminate

instance Data.ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceTerminationTimeout" Data..=)
              Prelude.<$> instanceTerminationTimeout,
            ("InstancesToProtect" Data..=)
              Prelude.<$> instancesToProtect,
            ("InstancesToTerminate" Data..=)
              Prelude.<$> instancesToTerminate
          ]
      )
