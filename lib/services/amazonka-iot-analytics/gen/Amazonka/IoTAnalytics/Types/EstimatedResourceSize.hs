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
-- Module      : Amazonka.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.EstimatedResourceSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The estimated size of the resource.
--
-- /See:/ 'newEstimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { -- | The estimated size of the resource, in bytes.
    estimatedSizeInBytes :: Prelude.Maybe Prelude.Double,
    -- | The time when the estimate of the size of the resource was made.
    estimatedOn :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
estimatedResourceSize_estimatedOn = Lens.lens (\EstimatedResourceSize' {estimatedOn} -> estimatedOn) (\s@EstimatedResourceSize' {} a -> s {estimatedOn = a} :: EstimatedResourceSize) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EstimatedResourceSize where
  parseJSON =
    Data.withObject
      "EstimatedResourceSize"
      ( \x ->
          EstimatedResourceSize'
            Prelude.<$> (x Data..:? "estimatedSizeInBytes")
            Prelude.<*> (x Data..:? "estimatedOn")
      )

instance Prelude.Hashable EstimatedResourceSize where
  hashWithSalt _salt EstimatedResourceSize' {..} =
    _salt `Prelude.hashWithSalt` estimatedSizeInBytes
      `Prelude.hashWithSalt` estimatedOn

instance Prelude.NFData EstimatedResourceSize where
  rnf EstimatedResourceSize' {..} =
    Prelude.rnf estimatedSizeInBytes
      `Prelude.seq` Prelude.rnf estimatedOn
