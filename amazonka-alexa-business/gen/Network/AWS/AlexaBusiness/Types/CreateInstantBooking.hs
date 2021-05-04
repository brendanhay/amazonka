{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.Types.CreateInstantBooking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateInstantBooking where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates settings for the instant booking feature that are applied to a
-- room profile. When users start their meeting with Alexa, Alexa
-- automatically books the room for the configured duration if the room is
-- available.
--
-- /See:/ 'newCreateInstantBooking' smart constructor.
data CreateInstantBooking = CreateInstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines
    -- how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Prelude.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateInstantBooking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'createInstantBooking_durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
--
-- 'enabled', 'createInstantBooking_enabled' - Whether instant booking is enabled or not.
newCreateInstantBooking ::
  -- | 'durationInMinutes'
  Prelude.Int ->
  -- | 'enabled'
  Prelude.Bool ->
  CreateInstantBooking
newCreateInstantBooking pDurationInMinutes_ pEnabled_ =
  CreateInstantBooking'
    { durationInMinutes =
        pDurationInMinutes_,
      enabled = pEnabled_
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines
-- how long to book an available room when a meeting is started with Alexa.
createInstantBooking_durationInMinutes :: Lens.Lens' CreateInstantBooking Prelude.Int
createInstantBooking_durationInMinutes = Lens.lens (\CreateInstantBooking' {durationInMinutes} -> durationInMinutes) (\s@CreateInstantBooking' {} a -> s {durationInMinutes = a} :: CreateInstantBooking)

-- | Whether instant booking is enabled or not.
createInstantBooking_enabled :: Lens.Lens' CreateInstantBooking Prelude.Bool
createInstantBooking_enabled = Lens.lens (\CreateInstantBooking' {enabled} -> enabled) (\s@CreateInstantBooking' {} a -> s {enabled = a} :: CreateInstantBooking)

instance Prelude.Hashable CreateInstantBooking

instance Prelude.NFData CreateInstantBooking

instance Prelude.ToJSON CreateInstantBooking where
  toJSON CreateInstantBooking' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DurationInMinutes" Prelude..= durationInMinutes),
            Prelude.Just ("Enabled" Prelude..= enabled)
          ]
      )
