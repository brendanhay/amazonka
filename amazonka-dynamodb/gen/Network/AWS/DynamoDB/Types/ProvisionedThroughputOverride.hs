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
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Replica-specific provisioned throughput settings. If not specified, uses
-- the source table\'s provisioned throughput settings.
--
-- /See:/ 'newProvisionedThroughputOverride' smart constructor.
data ProvisionedThroughputOverride = ProvisionedThroughputOverride'
  { -- | Replica-specific read capacity units. If not specified, uses the source
    -- table\'s read capacity settings.
    readCapacityUnits :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedThroughputOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'provisionedThroughputOverride_readCapacityUnits' - Replica-specific read capacity units. If not specified, uses the source
-- table\'s read capacity settings.
newProvisionedThroughputOverride ::
  ProvisionedThroughputOverride
newProvisionedThroughputOverride =
  ProvisionedThroughputOverride'
    { readCapacityUnits =
        Core.Nothing
    }

-- | Replica-specific read capacity units. If not specified, uses the source
-- table\'s read capacity settings.
provisionedThroughputOverride_readCapacityUnits :: Lens.Lens' ProvisionedThroughputOverride (Core.Maybe Core.Natural)
provisionedThroughputOverride_readCapacityUnits = Lens.lens (\ProvisionedThroughputOverride' {readCapacityUnits} -> readCapacityUnits) (\s@ProvisionedThroughputOverride' {} a -> s {readCapacityUnits = a} :: ProvisionedThroughputOverride)

instance Core.FromJSON ProvisionedThroughputOverride where
  parseJSON =
    Core.withObject
      "ProvisionedThroughputOverride"
      ( \x ->
          ProvisionedThroughputOverride'
            Core.<$> (x Core..:? "ReadCapacityUnits")
      )

instance Core.Hashable ProvisionedThroughputOverride

instance Core.NFData ProvisionedThroughputOverride

instance Core.ToJSON ProvisionedThroughputOverride where
  toJSON ProvisionedThroughputOverride' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReadCapacityUnits" Core..=)
              Core.<$> readCapacityUnits
          ]
      )
