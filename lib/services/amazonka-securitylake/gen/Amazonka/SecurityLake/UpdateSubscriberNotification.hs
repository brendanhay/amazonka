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
-- Module      : Amazonka.SecurityLake.UpdateSubscriberNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing notification method for the subscription (SQS or
-- HTTPs endpoint) or switches the notification subscription endpoint for a
-- subscriber.
module Amazonka.SecurityLake.UpdateSubscriberNotification
  ( -- * Creating a Request
    UpdateSubscriberNotification (..),
    newUpdateSubscriberNotification,

    -- * Request Lenses
    updateSubscriberNotification_configuration,
    updateSubscriberNotification_subscriberId,

    -- * Destructuring the Response
    UpdateSubscriberNotificationResponse (..),
    newUpdateSubscriberNotificationResponse,

    -- * Response Lenses
    updateSubscriberNotificationResponse_subscriberEndpoint,
    updateSubscriberNotificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateSubscriberNotification' smart constructor.
data UpdateSubscriberNotification = UpdateSubscriberNotification'
  { -- | The configuration for subscriber notification.
    configuration :: NotificationConfiguration,
    -- | The subscription ID for which the subscription notification is
    -- specified.
    subscriberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriberNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateSubscriberNotification_configuration' - The configuration for subscriber notification.
--
-- 'subscriberId', 'updateSubscriberNotification_subscriberId' - The subscription ID for which the subscription notification is
-- specified.
newUpdateSubscriberNotification ::
  -- | 'configuration'
  NotificationConfiguration ->
  -- | 'subscriberId'
  Prelude.Text ->
  UpdateSubscriberNotification
newUpdateSubscriberNotification
  pConfiguration_
  pSubscriberId_ =
    UpdateSubscriberNotification'
      { configuration =
          pConfiguration_,
        subscriberId = pSubscriberId_
      }

-- | The configuration for subscriber notification.
updateSubscriberNotification_configuration :: Lens.Lens' UpdateSubscriberNotification NotificationConfiguration
updateSubscriberNotification_configuration = Lens.lens (\UpdateSubscriberNotification' {configuration} -> configuration) (\s@UpdateSubscriberNotification' {} a -> s {configuration = a} :: UpdateSubscriberNotification)

-- | The subscription ID for which the subscription notification is
-- specified.
updateSubscriberNotification_subscriberId :: Lens.Lens' UpdateSubscriberNotification Prelude.Text
updateSubscriberNotification_subscriberId = Lens.lens (\UpdateSubscriberNotification' {subscriberId} -> subscriberId) (\s@UpdateSubscriberNotification' {} a -> s {subscriberId = a} :: UpdateSubscriberNotification)

instance Core.AWSRequest UpdateSubscriberNotification where
  type
    AWSResponse UpdateSubscriberNotification =
      UpdateSubscriberNotificationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubscriberNotificationResponse'
            Prelude.<$> (x Data..?> "subscriberEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubscriberNotification
  where
  hashWithSalt _salt UpdateSubscriberNotification' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` subscriberId

instance Prelude.NFData UpdateSubscriberNotification where
  rnf UpdateSubscriberNotification' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf subscriberId

instance Data.ToHeaders UpdateSubscriberNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSubscriberNotification where
  toJSON UpdateSubscriberNotification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath UpdateSubscriberNotification where
  toPath UpdateSubscriberNotification' {..} =
    Prelude.mconcat
      [ "/v1/subscribers/",
        Data.toBS subscriberId,
        "/notification"
      ]

instance Data.ToQuery UpdateSubscriberNotification where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriberNotificationResponse' smart constructor.
data UpdateSubscriberNotificationResponse = UpdateSubscriberNotificationResponse'
  { -- | The subscriber endpoint to which exception messages are posted.
    subscriberEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriberNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriberEndpoint', 'updateSubscriberNotificationResponse_subscriberEndpoint' - The subscriber endpoint to which exception messages are posted.
--
-- 'httpStatus', 'updateSubscriberNotificationResponse_httpStatus' - The response's http status code.
newUpdateSubscriberNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubscriberNotificationResponse
newUpdateSubscriberNotificationResponse pHttpStatus_ =
  UpdateSubscriberNotificationResponse'
    { subscriberEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The subscriber endpoint to which exception messages are posted.
updateSubscriberNotificationResponse_subscriberEndpoint :: Lens.Lens' UpdateSubscriberNotificationResponse (Prelude.Maybe Prelude.Text)
updateSubscriberNotificationResponse_subscriberEndpoint = Lens.lens (\UpdateSubscriberNotificationResponse' {subscriberEndpoint} -> subscriberEndpoint) (\s@UpdateSubscriberNotificationResponse' {} a -> s {subscriberEndpoint = a} :: UpdateSubscriberNotificationResponse)

-- | The response's http status code.
updateSubscriberNotificationResponse_httpStatus :: Lens.Lens' UpdateSubscriberNotificationResponse Prelude.Int
updateSubscriberNotificationResponse_httpStatus = Lens.lens (\UpdateSubscriberNotificationResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriberNotificationResponse' {} a -> s {httpStatus = a} :: UpdateSubscriberNotificationResponse)

instance
  Prelude.NFData
    UpdateSubscriberNotificationResponse
  where
  rnf UpdateSubscriberNotificationResponse' {..} =
    Prelude.rnf subscriberEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
