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
-- Module      : Amazonka.KafkaConnect.Types.CapacityUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CapacityUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.AutoScalingUpdate
import Amazonka.KafkaConnect.Types.ProvisionedCapacityUpdate
import qualified Amazonka.Prelude as Prelude

-- | The target capacity for the connector. The capacity can be auto scaled
-- or provisioned.
--
-- /See:/ 'newCapacityUpdate' smart constructor.
data CapacityUpdate = CapacityUpdate'
  { -- | The target settings for provisioned capacity.
    provisionedCapacity :: Prelude.Maybe ProvisionedCapacityUpdate,
    -- | The target auto scaling setting.
    autoScaling :: Prelude.Maybe AutoScalingUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedCapacity', 'capacityUpdate_provisionedCapacity' - The target settings for provisioned capacity.
--
-- 'autoScaling', 'capacityUpdate_autoScaling' - The target auto scaling setting.
newCapacityUpdate ::
  CapacityUpdate
newCapacityUpdate =
  CapacityUpdate'
    { provisionedCapacity =
        Prelude.Nothing,
      autoScaling = Prelude.Nothing
    }

-- | The target settings for provisioned capacity.
capacityUpdate_provisionedCapacity :: Lens.Lens' CapacityUpdate (Prelude.Maybe ProvisionedCapacityUpdate)
capacityUpdate_provisionedCapacity = Lens.lens (\CapacityUpdate' {provisionedCapacity} -> provisionedCapacity) (\s@CapacityUpdate' {} a -> s {provisionedCapacity = a} :: CapacityUpdate)

-- | The target auto scaling setting.
capacityUpdate_autoScaling :: Lens.Lens' CapacityUpdate (Prelude.Maybe AutoScalingUpdate)
capacityUpdate_autoScaling = Lens.lens (\CapacityUpdate' {autoScaling} -> autoScaling) (\s@CapacityUpdate' {} a -> s {autoScaling = a} :: CapacityUpdate)

instance Prelude.Hashable CapacityUpdate where
  hashWithSalt _salt CapacityUpdate' {..} =
    _salt `Prelude.hashWithSalt` provisionedCapacity
      `Prelude.hashWithSalt` autoScaling

instance Prelude.NFData CapacityUpdate where
  rnf CapacityUpdate' {..} =
    Prelude.rnf provisionedCapacity
      `Prelude.seq` Prelude.rnf autoScaling

instance Core.ToJSON CapacityUpdate where
  toJSON CapacityUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("provisionedCapacity" Core..=)
              Prelude.<$> provisionedCapacity,
            ("autoScaling" Core..=) Prelude.<$> autoScaling
          ]
      )
