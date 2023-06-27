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
-- Module      : Amazonka.SecurityLake.GetSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the subscription information for the specified subscription
-- ID. You can get information about a specific subscriber.
module Amazonka.SecurityLake.GetSubscriber
  ( -- * Creating a Request
    GetSubscriber (..),
    newGetSubscriber,

    -- * Request Lenses
    getSubscriber_subscriberId,

    -- * Destructuring the Response
    GetSubscriberResponse (..),
    newGetSubscriberResponse,

    -- * Response Lenses
    getSubscriberResponse_subscriber,
    getSubscriberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetSubscriber' smart constructor.
data GetSubscriber = GetSubscriber'
  { -- | A value created by Amazon Security Lake that uniquely identifies your
    -- @GetSubscriber@ API request.
    subscriberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriberId', 'getSubscriber_subscriberId' - A value created by Amazon Security Lake that uniquely identifies your
-- @GetSubscriber@ API request.
newGetSubscriber ::
  -- | 'subscriberId'
  Prelude.Text ->
  GetSubscriber
newGetSubscriber pSubscriberId_ =
  GetSubscriber' {subscriberId = pSubscriberId_}

-- | A value created by Amazon Security Lake that uniquely identifies your
-- @GetSubscriber@ API request.
getSubscriber_subscriberId :: Lens.Lens' GetSubscriber Prelude.Text
getSubscriber_subscriberId = Lens.lens (\GetSubscriber' {subscriberId} -> subscriberId) (\s@GetSubscriber' {} a -> s {subscriberId = a} :: GetSubscriber)

instance Core.AWSRequest GetSubscriber where
  type
    AWSResponse GetSubscriber =
      GetSubscriberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriberResponse'
            Prelude.<$> (x Data..?> "subscriber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSubscriber where
  hashWithSalt _salt GetSubscriber' {..} =
    _salt `Prelude.hashWithSalt` subscriberId

instance Prelude.NFData GetSubscriber where
  rnf GetSubscriber' {..} = Prelude.rnf subscriberId

instance Data.ToHeaders GetSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSubscriber where
  toPath GetSubscriber' {..} =
    Prelude.mconcat
      ["/v1/subscribers/", Data.toBS subscriberId]

instance Data.ToQuery GetSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSubscriberResponse' smart constructor.
data GetSubscriberResponse = GetSubscriberResponse'
  { -- | The subscriber information for the specified subscriber ID.
    subscriber :: Prelude.Maybe SubscriberResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriber', 'getSubscriberResponse_subscriber' - The subscriber information for the specified subscriber ID.
--
-- 'httpStatus', 'getSubscriberResponse_httpStatus' - The response's http status code.
newGetSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSubscriberResponse
newGetSubscriberResponse pHttpStatus_ =
  GetSubscriberResponse'
    { subscriber =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The subscriber information for the specified subscriber ID.
getSubscriberResponse_subscriber :: Lens.Lens' GetSubscriberResponse (Prelude.Maybe SubscriberResource)
getSubscriberResponse_subscriber = Lens.lens (\GetSubscriberResponse' {subscriber} -> subscriber) (\s@GetSubscriberResponse' {} a -> s {subscriber = a} :: GetSubscriberResponse)

-- | The response's http status code.
getSubscriberResponse_httpStatus :: Lens.Lens' GetSubscriberResponse Prelude.Int
getSubscriberResponse_httpStatus = Lens.lens (\GetSubscriberResponse' {httpStatus} -> httpStatus) (\s@GetSubscriberResponse' {} a -> s {httpStatus = a} :: GetSubscriberResponse)

instance Prelude.NFData GetSubscriberResponse where
  rnf GetSubscriberResponse' {..} =
    Prelude.rnf subscriber
      `Prelude.seq` Prelude.rnf httpStatus
