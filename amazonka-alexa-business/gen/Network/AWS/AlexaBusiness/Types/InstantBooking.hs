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
-- Module      : Network.AWS.AlexaBusiness.Types.InstantBooking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.InstantBooking where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for the instant booking feature that are applied to a room
-- profile. When users start their meeting with Alexa, Alexa automatically
-- books the room for the configured duration if the room is available.
--
-- /See:/ 'newInstantBooking' smart constructor.
data InstantBooking = InstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines
    -- how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Core.Maybe Core.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstantBooking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'instantBooking_durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
--
-- 'enabled', 'instantBooking_enabled' - Whether instant booking is enabled or not.
newInstantBooking ::
  InstantBooking
newInstantBooking =
  InstantBooking'
    { durationInMinutes = Core.Nothing,
      enabled = Core.Nothing
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
instantBooking_durationInMinutes :: Lens.Lens' InstantBooking (Core.Maybe Core.Int)
instantBooking_durationInMinutes = Lens.lens (\InstantBooking' {durationInMinutes} -> durationInMinutes) (\s@InstantBooking' {} a -> s {durationInMinutes = a} :: InstantBooking)

-- | Whether instant booking is enabled or not.
instantBooking_enabled :: Lens.Lens' InstantBooking (Core.Maybe Core.Bool)
instantBooking_enabled = Lens.lens (\InstantBooking' {enabled} -> enabled) (\s@InstantBooking' {} a -> s {enabled = a} :: InstantBooking)

instance Core.FromJSON InstantBooking where
  parseJSON =
    Core.withObject
      "InstantBooking"
      ( \x ->
          InstantBooking'
            Core.<$> (x Core..:? "DurationInMinutes")
            Core.<*> (x Core..:? "Enabled")
      )

instance Core.Hashable InstantBooking

instance Core.NFData InstantBooking
