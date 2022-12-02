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
-- Module      : Amazonka.IoTWireless.Types.Accuracy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Accuracy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The accuracy of the estimated position in meters. An empty value
-- indicates that no position data is available. A value of ‘0.0’ value
-- indicates that position data is available. This data corresponds to the
-- position information that you specified instead of the position computed
-- by solver.
--
-- /See:/ 'newAccuracy' smart constructor.
data Accuracy = Accuracy'
  { -- | The vertical accuracy of the estimated position in meters.
    verticalAccuracy :: Prelude.Maybe Prelude.Double,
    -- | The horizontal accuracy of the estimated position in meters.
    horizontalAccuracy :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Accuracy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verticalAccuracy', 'accuracy_verticalAccuracy' - The vertical accuracy of the estimated position in meters.
--
-- 'horizontalAccuracy', 'accuracy_horizontalAccuracy' - The horizontal accuracy of the estimated position in meters.
newAccuracy ::
  Accuracy
newAccuracy =
  Accuracy'
    { verticalAccuracy = Prelude.Nothing,
      horizontalAccuracy = Prelude.Nothing
    }

-- | The vertical accuracy of the estimated position in meters.
accuracy_verticalAccuracy :: Lens.Lens' Accuracy (Prelude.Maybe Prelude.Double)
accuracy_verticalAccuracy = Lens.lens (\Accuracy' {verticalAccuracy} -> verticalAccuracy) (\s@Accuracy' {} a -> s {verticalAccuracy = a} :: Accuracy)

-- | The horizontal accuracy of the estimated position in meters.
accuracy_horizontalAccuracy :: Lens.Lens' Accuracy (Prelude.Maybe Prelude.Double)
accuracy_horizontalAccuracy = Lens.lens (\Accuracy' {horizontalAccuracy} -> horizontalAccuracy) (\s@Accuracy' {} a -> s {horizontalAccuracy = a} :: Accuracy)

instance Data.FromJSON Accuracy where
  parseJSON =
    Data.withObject
      "Accuracy"
      ( \x ->
          Accuracy'
            Prelude.<$> (x Data..:? "VerticalAccuracy")
            Prelude.<*> (x Data..:? "HorizontalAccuracy")
      )

instance Prelude.Hashable Accuracy where
  hashWithSalt _salt Accuracy' {..} =
    _salt `Prelude.hashWithSalt` verticalAccuracy
      `Prelude.hashWithSalt` horizontalAccuracy

instance Prelude.NFData Accuracy where
  rnf Accuracy' {..} =
    Prelude.rnf verticalAccuracy
      `Prelude.seq` Prelude.rnf horizontalAccuracy
