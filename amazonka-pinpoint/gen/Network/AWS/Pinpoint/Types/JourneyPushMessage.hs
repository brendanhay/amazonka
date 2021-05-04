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
-- Module      : Network.AWS.Pinpoint.Types.JourneyPushMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyPushMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON JourneyPushMessage where
  parseJSON =
    Prelude.withObject
      "JourneyPushMessage"
      ( \x ->
          JourneyPushMessage'
            Prelude.<$> (x Prelude..:? "TimeToLive")
      )

instance Prelude.Hashable JourneyPushMessage

instance Prelude.NFData JourneyPushMessage

instance Prelude.ToJSON JourneyPushMessage where
  toJSON JourneyPushMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("TimeToLive" Prelude..=) Prelude.<$> timeToLive]
      )
