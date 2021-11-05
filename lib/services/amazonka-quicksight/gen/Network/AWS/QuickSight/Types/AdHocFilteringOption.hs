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
-- Module      : Network.AWS.QuickSight.Types.AdHocFilteringOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.AdHocFilteringOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.DashboardBehavior

-- | Ad hoc (one-time) filtering option.
--
-- /See:/ 'newAdHocFilteringOption' smart constructor.
data AdHocFilteringOption = AdHocFilteringOption'
  { -- | Availability status.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdHocFilteringOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'adHocFilteringOption_availabilityStatus' - Availability status.
newAdHocFilteringOption ::
  AdHocFilteringOption
newAdHocFilteringOption =
  AdHocFilteringOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | Availability status.
adHocFilteringOption_availabilityStatus :: Lens.Lens' AdHocFilteringOption (Prelude.Maybe DashboardBehavior)
adHocFilteringOption_availabilityStatus = Lens.lens (\AdHocFilteringOption' {availabilityStatus} -> availabilityStatus) (\s@AdHocFilteringOption' {} a -> s {availabilityStatus = a} :: AdHocFilteringOption)

instance Prelude.Hashable AdHocFilteringOption

instance Prelude.NFData AdHocFilteringOption

instance Core.ToJSON AdHocFilteringOption where
  toJSON AdHocFilteringOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Core..=)
              Prelude.<$> availabilityStatus
          ]
      )
