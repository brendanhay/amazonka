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
-- Module      : Amazonka.QuickSight.Types.VisualAxisSortOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualAxisSortOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The axis sort options for a visual.
--
-- /See:/ 'newVisualAxisSortOption' smart constructor.
data VisualAxisSortOption = VisualAxisSortOption'
  { -- | The availaiblity status of a visual\'s axis sort options.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualAxisSortOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'visualAxisSortOption_availabilityStatus' - The availaiblity status of a visual\'s axis sort options.
newVisualAxisSortOption ::
  VisualAxisSortOption
newVisualAxisSortOption =
  VisualAxisSortOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The availaiblity status of a visual\'s axis sort options.
visualAxisSortOption_availabilityStatus :: Lens.Lens' VisualAxisSortOption (Prelude.Maybe DashboardBehavior)
visualAxisSortOption_availabilityStatus = Lens.lens (\VisualAxisSortOption' {availabilityStatus} -> availabilityStatus) (\s@VisualAxisSortOption' {} a -> s {availabilityStatus = a} :: VisualAxisSortOption)

instance Data.FromJSON VisualAxisSortOption where
  parseJSON =
    Data.withObject
      "VisualAxisSortOption"
      ( \x ->
          VisualAxisSortOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance Prelude.Hashable VisualAxisSortOption where
  hashWithSalt _salt VisualAxisSortOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData VisualAxisSortOption where
  rnf VisualAxisSortOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON VisualAxisSortOption where
  toJSON VisualAxisSortOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
