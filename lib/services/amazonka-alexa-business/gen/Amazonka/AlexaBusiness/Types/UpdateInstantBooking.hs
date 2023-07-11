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
-- Module      : Amazonka.AlexaBusiness.Types.UpdateInstantBooking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.UpdateInstantBooking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates settings for the instant booking feature that are applied to a
-- room profile. If instant booking is enabled, Alexa automatically
-- reserves a room if it is free when a user joins a meeting with Alexa.
--
-- /See:/ 'newUpdateInstantBooking' smart constructor.
data UpdateInstantBooking = UpdateInstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines
    -- how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
updateInstantBooking_durationInMinutes :: Lens.Lens' UpdateInstantBooking (Prelude.Maybe Prelude.Int)
updateInstantBooking_durationInMinutes = Lens.lens (\UpdateInstantBooking' {durationInMinutes} -> durationInMinutes) (\s@UpdateInstantBooking' {} a -> s {durationInMinutes = a} :: UpdateInstantBooking)

-- | Whether instant booking is enabled or not.
updateInstantBooking_enabled :: Lens.Lens' UpdateInstantBooking (Prelude.Maybe Prelude.Bool)
updateInstantBooking_enabled = Lens.lens (\UpdateInstantBooking' {enabled} -> enabled) (\s@UpdateInstantBooking' {} a -> s {enabled = a} :: UpdateInstantBooking)

instance Prelude.Hashable UpdateInstantBooking where
  hashWithSalt _salt UpdateInstantBooking' {..} =
    _salt
      `Prelude.hashWithSalt` durationInMinutes
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData UpdateInstantBooking where
  rnf UpdateInstantBooking' {..} =
    Prelude.rnf durationInMinutes
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON UpdateInstantBooking where
  toJSON UpdateInstantBooking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInMinutes" Data..=)
              Prelude.<$> durationInMinutes,
            ("Enabled" Data..=) Prelude.<$> enabled
          ]
      )
