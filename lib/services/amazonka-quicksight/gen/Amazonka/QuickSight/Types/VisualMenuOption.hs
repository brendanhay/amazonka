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
-- Module      : Amazonka.QuickSight.Types.VisualMenuOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualMenuOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The menu options for a visual.
--
-- /See:/ 'newVisualMenuOption' smart constructor.
data VisualMenuOption = VisualMenuOption'
  { -- | The availaiblity status of a visual\'s menu options.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualMenuOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'visualMenuOption_availabilityStatus' - The availaiblity status of a visual\'s menu options.
newVisualMenuOption ::
  VisualMenuOption
newVisualMenuOption =
  VisualMenuOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The availaiblity status of a visual\'s menu options.
visualMenuOption_availabilityStatus :: Lens.Lens' VisualMenuOption (Prelude.Maybe DashboardBehavior)
visualMenuOption_availabilityStatus = Lens.lens (\VisualMenuOption' {availabilityStatus} -> availabilityStatus) (\s@VisualMenuOption' {} a -> s {availabilityStatus = a} :: VisualMenuOption)

instance Data.FromJSON VisualMenuOption where
  parseJSON =
    Data.withObject
      "VisualMenuOption"
      ( \x ->
          VisualMenuOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance Prelude.Hashable VisualMenuOption where
  hashWithSalt _salt VisualMenuOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData VisualMenuOption where
  rnf VisualMenuOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON VisualMenuOption where
  toJSON VisualMenuOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
