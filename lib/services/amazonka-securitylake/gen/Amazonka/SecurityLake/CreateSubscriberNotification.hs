{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityLake.CreateSubscriberNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the subscriber when new data is written to the data lake for
-- the sources that the subscriber consumes in Security Lake. You can
-- create only one subscriber notification per subscriber.
module Amazonka.SecurityLake.CreateSubscriberNotification
  ( -- * Creating a Request
    CreateSubscriberNotification (..),
    newCreateSubscriberNotification,

    -- * Request Lenses
    createSubscriberNotification_configuration,
    createSubscriberNotification_subscriberId,

    -- * Destructuring the Response
    CreateSubscriberNotificationResponse (..),
    newCreateSubscriberNotificationResponse,

    -- * Response Lenses
    createSubscriberNotificationResponse_subscriberEndpoint,
    createSubscriberNotificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateSubscriberNotification' smart constructor.
data CreateSubscriberNotification = CreateSubscriberNotification'
  { -- | Specify the configuration using which you want to create the subscriber
    -- notification.
    configuration :: NotificationConfiguration,
    -- | The subscriber ID for the notification subscription.
    subscriberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriberNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'createSubscriberNotification_configuration' - Specify the configuration using which you want to create the subscriber
-- notification.
--
-- 'subscriberId', 'createSubscriberNotification_subscriberId' - The subscriber ID for the notification subscription.
newCreateSubscriberNotification ::
  -- | 'configuration'
  NotificationConfiguration ->
  -- | 'subscriberId'
  Prelude.Text ->
  CreateSubscriberNotification
newCreateSubscriberNotification
  pConfiguration_
  pSubscriberId_ =
    CreateSubscriberNotification'
      { configuration =
          pConfiguration_,
        subscriberId = pSubscriberId_
      }

-- | Specify the configuration using which you want to create the subscriber
-- notification.
createSubscriberNotification_configuration :: Lens.Lens' CreateSubscriberNotification NotificationConfiguration
createSubscriberNotification_configuration = Lens.lens (\CreateSubscriberNotification' {configuration} -> configuration) (\s@CreateSubscriberNotification' {} a -> s {configuration = a} :: CreateSubscriberNotification)

-- | The subscriber ID for the notification subscription.
createSubscriberNotification_subscriberId :: Lens.Lens' CreateSubscriberNotification Prelude.Text
createSubscriberNotification_subscriberId = Lens.lens (\CreateSubscriberNotification' {subscriberId} -> subscriberId) (\s@CreateSubscriberNotification' {} a -> s {subscriberId = a} :: CreateSubscriberNotification)

instance Core.AWSRequest CreateSubscriberNotification where
  type
    AWSResponse CreateSubscriberNotification =
      CreateSubscriberNotificationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriberNotificationResponse'
            Prelude.<$> (x Data..?> "subscriberEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriberNotification
  where
  hashWithSalt _salt CreateSubscriberNotification' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` subscriberId

instance Prelude.NFData CreateSubscriberNotification where
  rnf CreateSubscriberNotification' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf subscriberId

instance Data.ToHeaders CreateSubscriberNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSubscriberNotification where
  toJSON CreateSubscriberNotification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath CreateSubscriberNotification where
  toPath CreateSubscriberNotification' {..} =
    Prelude.mconcat
      [ "/v1/subscribers/",
        Data.toBS subscriberId,
        "/notification"
      ]

instance Data.ToQuery CreateSubscriberNotification where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriberNotificationResponse' smart constructor.
data CreateSubscriberNotificationResponse = CreateSubscriberNotificationResponse'
  { -- | The subscriber endpoint to which exception messages are posted.
    subscriberEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriberNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriberEndpoint', 'createSubscriberNotificationResponse_subscriberEndpoint' - The subscriber endpoint to which exception messages are posted.
--
-- 'httpStatus', 'createSubscriberNotificationResponse_httpStatus' - The response's http status code.
newCreateSubscriberNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriberNotificationResponse
newCreateSubscriberNotificationResponse pHttpStatus_ =
  CreateSubscriberNotificationResponse'
    { subscriberEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The subscriber endpoint to which exception messages are posted.
createSubscriberNotificationResponse_subscriberEndpoint :: Lens.Lens' CreateSubscriberNotificationResponse (Prelude.Maybe Prelude.Text)
createSubscriberNotificationResponse_subscriberEndpoint = Lens.lens (\CreateSubscriberNotificationResponse' {subscriberEndpoint} -> subscriberEndpoint) (\s@CreateSubscriberNotificationResponse' {} a -> s {subscriberEndpoint = a} :: CreateSubscriberNotificationResponse)

-- | The response's http status code.
createSubscriberNotificationResponse_httpStatus :: Lens.Lens' CreateSubscriberNotificationResponse Prelude.Int
createSubscriberNotificationResponse_httpStatus = Lens.lens (\CreateSubscriberNotificationResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriberNotificationResponse' {} a -> s {httpStatus = a} :: CreateSubscriberNotificationResponse)

instance
  Prelude.NFData
    CreateSubscriberNotificationResponse
  where
  rnf CreateSubscriberNotificationResponse' {..} =
    Prelude.rnf subscriberEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
