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
-- Module      : Network.AWS.CostExplorer.Types.ReservationCoverageGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationCoverageGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.Coverage
import qualified Network.AWS.Lens as Lens

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'newReservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { -- | The attributes for this group of reservations.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | How much instance usage this group of reservations covered.
    coverage :: Core.Maybe Coverage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservationCoverageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'reservationCoverageGroup_attributes' - The attributes for this group of reservations.
--
-- 'coverage', 'reservationCoverageGroup_coverage' - How much instance usage this group of reservations covered.
newReservationCoverageGroup ::
  ReservationCoverageGroup
newReservationCoverageGroup =
  ReservationCoverageGroup'
    { attributes =
        Core.Nothing,
      coverage = Core.Nothing
    }

-- | The attributes for this group of reservations.
reservationCoverageGroup_attributes :: Lens.Lens' ReservationCoverageGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
reservationCoverageGroup_attributes = Lens.lens (\ReservationCoverageGroup' {attributes} -> attributes) (\s@ReservationCoverageGroup' {} a -> s {attributes = a} :: ReservationCoverageGroup) Core.. Lens.mapping Lens._Coerce

-- | How much instance usage this group of reservations covered.
reservationCoverageGroup_coverage :: Lens.Lens' ReservationCoverageGroup (Core.Maybe Coverage)
reservationCoverageGroup_coverage = Lens.lens (\ReservationCoverageGroup' {coverage} -> coverage) (\s@ReservationCoverageGroup' {} a -> s {coverage = a} :: ReservationCoverageGroup)

instance Core.FromJSON ReservationCoverageGroup where
  parseJSON =
    Core.withObject
      "ReservationCoverageGroup"
      ( \x ->
          ReservationCoverageGroup'
            Core.<$> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Coverage")
      )

instance Core.Hashable ReservationCoverageGroup

instance Core.NFData ReservationCoverageGroup
