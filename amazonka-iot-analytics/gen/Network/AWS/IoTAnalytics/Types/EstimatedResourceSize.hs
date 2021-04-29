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
-- Module      : Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.EstimatedResourceSize where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The estimated size of the resource.
--
-- /See:/ 'newEstimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { -- | The estimated size of the resource, in bytes.
    estimatedSizeInBytes :: Prelude.Maybe Prelude.Double,
    -- | The time when the estimate of the size of the resource was made.
    estimatedOn :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      estimatedOn = Prelude.Nothing
    }

-- | The estimated size of the resource, in bytes.
estimatedResourceSize_estimatedSizeInBytes :: Lens.Lens' EstimatedResourceSize (Prelude.Maybe Prelude.Double)
estimatedResourceSize_estimatedSizeInBytes = Lens.lens (\EstimatedResourceSize' {estimatedSizeInBytes} -> estimatedSizeInBytes) (\s@EstimatedResourceSize' {} a -> s {estimatedSizeInBytes = a} :: EstimatedResourceSize)

-- | The time when the estimate of the size of the resource was made.
estimatedResourceSize_estimatedOn :: Lens.Lens' EstimatedResourceSize (Prelude.Maybe Prelude.UTCTime)
estimatedResourceSize_estimatedOn = Lens.lens (\EstimatedResourceSize' {estimatedOn} -> estimatedOn) (\s@EstimatedResourceSize' {} a -> s {estimatedOn = a} :: EstimatedResourceSize) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON EstimatedResourceSize where
  parseJSON =
    Prelude.withObject
      "EstimatedResourceSize"
      ( \x ->
          EstimatedResourceSize'
            Prelude.<$> (x Prelude..:? "estimatedSizeInBytes")
            Prelude.<*> (x Prelude..:? "estimatedOn")
      )

instance Prelude.Hashable EstimatedResourceSize

instance Prelude.NFData EstimatedResourceSize
