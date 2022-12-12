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
-- Module      : Amazonka.AlexaBusiness.Types.InstantBooking
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.InstantBooking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the instant booking feature that are applied to a room
-- profile. When users start their meeting with Alexa, Alexa automatically
-- books the room for the configured duration if the room is available.
--
-- /See:/ 'newInstantBooking' smart constructor.
data InstantBooking = InstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines
    -- how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { durationInMinutes =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
instantBooking_durationInMinutes :: Lens.Lens' InstantBooking (Prelude.Maybe Prelude.Int)
instantBooking_durationInMinutes = Lens.lens (\InstantBooking' {durationInMinutes} -> durationInMinutes) (\s@InstantBooking' {} a -> s {durationInMinutes = a} :: InstantBooking)

-- | Whether instant booking is enabled or not.
instantBooking_enabled :: Lens.Lens' InstantBooking (Prelude.Maybe Prelude.Bool)
instantBooking_enabled = Lens.lens (\InstantBooking' {enabled} -> enabled) (\s@InstantBooking' {} a -> s {enabled = a} :: InstantBooking)

instance Data.FromJSON InstantBooking where
  parseJSON =
    Data.withObject
      "InstantBooking"
      ( \x ->
          InstantBooking'
            Prelude.<$> (x Data..:? "DurationInMinutes")
            Prelude.<*> (x Data..:? "Enabled")
      )

instance Prelude.Hashable InstantBooking where
  hashWithSalt _salt InstantBooking' {..} =
    _salt `Prelude.hashWithSalt` durationInMinutes
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData InstantBooking where
  rnf InstantBooking' {..} =
    Prelude.rnf durationInMinutes
      `Prelude.seq` Prelude.rnf enabled
