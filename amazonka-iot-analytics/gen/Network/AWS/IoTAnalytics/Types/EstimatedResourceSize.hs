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
-- Module      : Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.EstimatedResourceSize where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The estimated size of the resource.
--
-- /See:/ 'newEstimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { -- | The estimated size of the resource, in bytes.
    estimatedSizeInBytes :: Core.Maybe Core.Double,
    -- | The time when the estimate of the size of the resource was made.
    estimatedOn :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EstimatedResourceSize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedSizeInBytes', 'estimatedResourceSize_estimatedSizeInBytes' - The estimated size of the resource, in bytes.
--
-- 'estimatedOn', 'estimatedResourceSize_estimatedOn' - The time when the estimate of the size of the resource was made.
newEstimatedResourceSize ::
  EstimatedResourceSize
newEstimatedResourceSize =
  EstimatedResourceSize'
    { estimatedSizeInBytes =
        Core.Nothing,
      estimatedOn = Core.Nothing
    }

-- | The estimated size of the resource, in bytes.
estimatedResourceSize_estimatedSizeInBytes :: Lens.Lens' EstimatedResourceSize (Core.Maybe Core.Double)
estimatedResourceSize_estimatedSizeInBytes = Lens.lens (\EstimatedResourceSize' {estimatedSizeInBytes} -> estimatedSizeInBytes) (\s@EstimatedResourceSize' {} a -> s {estimatedSizeInBytes = a} :: EstimatedResourceSize)

-- | The time when the estimate of the size of the resource was made.
estimatedResourceSize_estimatedOn :: Lens.Lens' EstimatedResourceSize (Core.Maybe Core.UTCTime)
estimatedResourceSize_estimatedOn = Lens.lens (\EstimatedResourceSize' {estimatedOn} -> estimatedOn) (\s@EstimatedResourceSize' {} a -> s {estimatedOn = a} :: EstimatedResourceSize) Core.. Lens.mapping Core._Time

instance Core.FromJSON EstimatedResourceSize where
  parseJSON =
    Core.withObject
      "EstimatedResourceSize"
      ( \x ->
          EstimatedResourceSize'
            Core.<$> (x Core..:? "estimatedSizeInBytes")
            Core.<*> (x Core..:? "estimatedOn")
      )

instance Core.Hashable EstimatedResourceSize

instance Core.NFData EstimatedResourceSize
