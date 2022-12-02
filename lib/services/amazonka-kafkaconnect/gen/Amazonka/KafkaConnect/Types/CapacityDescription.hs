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
-- Module      : Amazonka.KafkaConnect.Types.CapacityDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CapacityDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.AutoScalingDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription
import qualified Amazonka.Prelude as Prelude

-- | A description of the connector\'s capacity.
--
-- /See:/ 'newCapacityDescription' smart constructor.
data CapacityDescription = CapacityDescription'
  { -- | Describes a connector\'s provisioned capacity.
    provisionedCapacity :: Prelude.Maybe ProvisionedCapacityDescription,
    -- | Describes the connector\'s auto scaling capacity.
    autoScaling :: Prelude.Maybe AutoScalingDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedCapacity', 'capacityDescription_provisionedCapacity' - Describes a connector\'s provisioned capacity.
--
-- 'autoScaling', 'capacityDescription_autoScaling' - Describes the connector\'s auto scaling capacity.
newCapacityDescription ::
  CapacityDescription
newCapacityDescription =
  CapacityDescription'
    { provisionedCapacity =
        Prelude.Nothing,
      autoScaling = Prelude.Nothing
    }

-- | Describes a connector\'s provisioned capacity.
capacityDescription_provisionedCapacity :: Lens.Lens' CapacityDescription (Prelude.Maybe ProvisionedCapacityDescription)
capacityDescription_provisionedCapacity = Lens.lens (\CapacityDescription' {provisionedCapacity} -> provisionedCapacity) (\s@CapacityDescription' {} a -> s {provisionedCapacity = a} :: CapacityDescription)

-- | Describes the connector\'s auto scaling capacity.
capacityDescription_autoScaling :: Lens.Lens' CapacityDescription (Prelude.Maybe AutoScalingDescription)
capacityDescription_autoScaling = Lens.lens (\CapacityDescription' {autoScaling} -> autoScaling) (\s@CapacityDescription' {} a -> s {autoScaling = a} :: CapacityDescription)

instance Data.FromJSON CapacityDescription where
  parseJSON =
    Data.withObject
      "CapacityDescription"
      ( \x ->
          CapacityDescription'
            Prelude.<$> (x Data..:? "provisionedCapacity")
            Prelude.<*> (x Data..:? "autoScaling")
      )

instance Prelude.Hashable CapacityDescription where
  hashWithSalt _salt CapacityDescription' {..} =
    _salt `Prelude.hashWithSalt` provisionedCapacity
      `Prelude.hashWithSalt` autoScaling

instance Prelude.NFData CapacityDescription where
  rnf CapacityDescription' {..} =
    Prelude.rnf provisionedCapacity
      `Prelude.seq` Prelude.rnf autoScaling
