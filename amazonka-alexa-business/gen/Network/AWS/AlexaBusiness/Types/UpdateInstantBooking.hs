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
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateInstantBooking where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Updates settings for the instant booking feature that are applied to a
-- room profile. If instant booking is enabled, Alexa automatically
-- reserves a room if it is free when a user joins a meeting with Alexa.
--
-- /See:/ 'newUpdateInstantBooking' smart constructor.
data UpdateInstantBooking = UpdateInstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines
    -- how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Core.Maybe Core.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstantBooking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'updateInstantBooking_durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
--
-- 'enabled', 'updateInstantBooking_enabled' - Whether instant booking is enabled or not.
newUpdateInstantBooking ::
  UpdateInstantBooking
newUpdateInstantBooking =
  UpdateInstantBooking'
    { durationInMinutes =
        Core.Nothing,
      enabled = Core.Nothing
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
updateInstantBooking_durationInMinutes :: Lens.Lens' UpdateInstantBooking (Core.Maybe Core.Int)
updateInstantBooking_durationInMinutes = Lens.lens (\UpdateInstantBooking' {durationInMinutes} -> durationInMinutes) (\s@UpdateInstantBooking' {} a -> s {durationInMinutes = a} :: UpdateInstantBooking)

-- | Whether instant booking is enabled or not.
updateInstantBooking_enabled :: Lens.Lens' UpdateInstantBooking (Core.Maybe Core.Bool)
updateInstantBooking_enabled = Lens.lens (\UpdateInstantBooking' {enabled} -> enabled) (\s@UpdateInstantBooking' {} a -> s {enabled = a} :: UpdateInstantBooking)

instance Core.Hashable UpdateInstantBooking

instance Core.NFData UpdateInstantBooking

instance Core.ToJSON UpdateInstantBooking where
  toJSON UpdateInstantBooking' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DurationInMinutes" Core..=)
              Core.<$> durationInMinutes,
            ("Enabled" Core..=) Core.<$> enabled
          ]
      )
