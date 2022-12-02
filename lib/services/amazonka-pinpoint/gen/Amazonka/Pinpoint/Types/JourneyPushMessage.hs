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
-- Module      : Amazonka.Pinpoint.Types.JourneyPushMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyPushMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the message configuration for a push notification that\'s sent
-- to participants in a journey.
--
-- /See:/ 'newJourneyPushMessage' smart constructor.
data JourneyPushMessage = JourneyPushMessage'
  { -- | The number of seconds that the push notification service should keep the
    -- message, if the service is unable to deliver the notification the first
    -- time. This value is converted to an expiration value when it\'s sent to
    -- a push-notification service. If this value is 0, the service treats the
    -- notification as if it expires immediately and the service doesn\'t store
    -- or try to deliver the notification again.
    --
    -- This value doesn\'t apply to messages that are sent through the Amazon
    -- Device Messaging (ADM) service.
    timeToLive :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyPushMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLive', 'journeyPushMessage_timeToLive' - The number of seconds that the push notification service should keep the
-- message, if the service is unable to deliver the notification the first
-- time. This value is converted to an expiration value when it\'s sent to
-- a push-notification service. If this value is 0, the service treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- This value doesn\'t apply to messages that are sent through the Amazon
-- Device Messaging (ADM) service.
newJourneyPushMessage ::
  JourneyPushMessage
newJourneyPushMessage =
  JourneyPushMessage' {timeToLive = Prelude.Nothing}

-- | The number of seconds that the push notification service should keep the
-- message, if the service is unable to deliver the notification the first
-- time. This value is converted to an expiration value when it\'s sent to
-- a push-notification service. If this value is 0, the service treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- This value doesn\'t apply to messages that are sent through the Amazon
-- Device Messaging (ADM) service.
journeyPushMessage_timeToLive :: Lens.Lens' JourneyPushMessage (Prelude.Maybe Prelude.Text)
journeyPushMessage_timeToLive = Lens.lens (\JourneyPushMessage' {timeToLive} -> timeToLive) (\s@JourneyPushMessage' {} a -> s {timeToLive = a} :: JourneyPushMessage)

instance Data.FromJSON JourneyPushMessage where
  parseJSON =
    Data.withObject
      "JourneyPushMessage"
      ( \x ->
          JourneyPushMessage'
            Prelude.<$> (x Data..:? "TimeToLive")
      )

instance Prelude.Hashable JourneyPushMessage where
  hashWithSalt _salt JourneyPushMessage' {..} =
    _salt `Prelude.hashWithSalt` timeToLive

instance Prelude.NFData JourneyPushMessage where
  rnf JourneyPushMessage' {..} = Prelude.rnf timeToLive

instance Data.ToJSON JourneyPushMessage where
  toJSON JourneyPushMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TimeToLive" Data..=) Prelude.<$> timeToLive]
      )
