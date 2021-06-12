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
-- Module      : Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationUtilizationGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.ReservationAggregates
import qualified Network.AWS.Lens as Lens

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'newReservationUtilizationGroup' smart constructor.
data ReservationUtilizationGroup = ReservationUtilizationGroup'
  { -- | The key for a specific reservation attribute.
    key :: Core.Maybe Core.Text,
    -- | How much you used this group of reservations.
    utilization :: Core.Maybe ReservationAggregates,
    -- | The attributes for this group of reservations.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The value of a specific reservation attribute.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservationUtilizationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'reservationUtilizationGroup_key' - The key for a specific reservation attribute.
--
-- 'utilization', 'reservationUtilizationGroup_utilization' - How much you used this group of reservations.
--
-- 'attributes', 'reservationUtilizationGroup_attributes' - The attributes for this group of reservations.
--
-- 'value', 'reservationUtilizationGroup_value' - The value of a specific reservation attribute.
newReservationUtilizationGroup ::
  ReservationUtilizationGroup
newReservationUtilizationGroup =
  ReservationUtilizationGroup'
    { key = Core.Nothing,
      utilization = Core.Nothing,
      attributes = Core.Nothing,
      value = Core.Nothing
    }

-- | The key for a specific reservation attribute.
reservationUtilizationGroup_key :: Lens.Lens' ReservationUtilizationGroup (Core.Maybe Core.Text)
reservationUtilizationGroup_key = Lens.lens (\ReservationUtilizationGroup' {key} -> key) (\s@ReservationUtilizationGroup' {} a -> s {key = a} :: ReservationUtilizationGroup)

-- | How much you used this group of reservations.
reservationUtilizationGroup_utilization :: Lens.Lens' ReservationUtilizationGroup (Core.Maybe ReservationAggregates)
reservationUtilizationGroup_utilization = Lens.lens (\ReservationUtilizationGroup' {utilization} -> utilization) (\s@ReservationUtilizationGroup' {} a -> s {utilization = a} :: ReservationUtilizationGroup)

-- | The attributes for this group of reservations.
reservationUtilizationGroup_attributes :: Lens.Lens' ReservationUtilizationGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
reservationUtilizationGroup_attributes = Lens.lens (\ReservationUtilizationGroup' {attributes} -> attributes) (\s@ReservationUtilizationGroup' {} a -> s {attributes = a} :: ReservationUtilizationGroup) Core.. Lens.mapping Lens._Coerce

-- | The value of a specific reservation attribute.
reservationUtilizationGroup_value :: Lens.Lens' ReservationUtilizationGroup (Core.Maybe Core.Text)
reservationUtilizationGroup_value = Lens.lens (\ReservationUtilizationGroup' {value} -> value) (\s@ReservationUtilizationGroup' {} a -> s {value = a} :: ReservationUtilizationGroup)

instance Core.FromJSON ReservationUtilizationGroup where
  parseJSON =
    Core.withObject
      "ReservationUtilizationGroup"
      ( \x ->
          ReservationUtilizationGroup'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Utilization")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable ReservationUtilizationGroup

instance Core.NFData ReservationUtilizationGroup
