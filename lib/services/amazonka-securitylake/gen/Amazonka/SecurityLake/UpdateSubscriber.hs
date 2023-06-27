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
-- Module      : Amazonka.SecurityLake.UpdateSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing subscription for the given Amazon Security Lake
-- account ID. You can update a subscriber by changing the sources that the
-- subscriber consumes data from.
module Amazonka.SecurityLake.UpdateSubscriber
  ( -- * Creating a Request
    UpdateSubscriber (..),
    newUpdateSubscriber,

    -- * Request Lenses
    updateSubscriber_sources,
    updateSubscriber_subscriberDescription,
    updateSubscriber_subscriberIdentity,
    updateSubscriber_subscriberName,
    updateSubscriber_subscriberId,

    -- * Destructuring the Response
    UpdateSubscriberResponse (..),
    newUpdateSubscriberResponse,

    -- * Response Lenses
    updateSubscriberResponse_subscriber,
    updateSubscriberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateSubscriber' smart constructor.
data UpdateSubscriber = UpdateSubscriber'
  { -- | The supported Amazon Web Services from which logs and events are
    -- collected. For the list of supported Amazon Web Services, see the
    -- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
    sources :: Prelude.Maybe [LogSourceResource],
    -- | The description of the Security Lake account subscriber.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The AWS identity used to access your data.
    subscriberIdentity :: Prelude.Maybe AwsIdentity,
    -- | The name of the Security Lake account subscriber.
    subscriberName :: Prelude.Maybe Prelude.Text,
    -- | A value created by Security Lake that uniquely identifies your
    -- subscription.
    subscriberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'updateSubscriber_sources' - The supported Amazon Web Services from which logs and events are
-- collected. For the list of supported Amazon Web Services, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
--
-- 'subscriberDescription', 'updateSubscriber_subscriberDescription' - The description of the Security Lake account subscriber.
--
-- 'subscriberIdentity', 'updateSubscriber_subscriberIdentity' - The AWS identity used to access your data.
--
-- 'subscriberName', 'updateSubscriber_subscriberName' - The name of the Security Lake account subscriber.
--
-- 'subscriberId', 'updateSubscriber_subscriberId' - A value created by Security Lake that uniquely identifies your
-- subscription.
newUpdateSubscriber ::
  -- | 'subscriberId'
  Prelude.Text ->
  UpdateSubscriber
newUpdateSubscriber pSubscriberId_ =
  UpdateSubscriber'
    { sources = Prelude.Nothing,
      subscriberDescription = Prelude.Nothing,
      subscriberIdentity = Prelude.Nothing,
      subscriberName = Prelude.Nothing,
      subscriberId = pSubscriberId_
    }

-- | The supported Amazon Web Services from which logs and events are
-- collected. For the list of supported Amazon Web Services, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
updateSubscriber_sources :: Lens.Lens' UpdateSubscriber (Prelude.Maybe [LogSourceResource])
updateSubscriber_sources = Lens.lens (\UpdateSubscriber' {sources} -> sources) (\s@UpdateSubscriber' {} a -> s {sources = a} :: UpdateSubscriber) Prelude.. Lens.mapping Lens.coerced

-- | The description of the Security Lake account subscriber.
updateSubscriber_subscriberDescription :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberDescription = Lens.lens (\UpdateSubscriber' {subscriberDescription} -> subscriberDescription) (\s@UpdateSubscriber' {} a -> s {subscriberDescription = a} :: UpdateSubscriber)

-- | The AWS identity used to access your data.
updateSubscriber_subscriberIdentity :: Lens.Lens' UpdateSubscriber (Prelude.Maybe AwsIdentity)
updateSubscriber_subscriberIdentity = Lens.lens (\UpdateSubscriber' {subscriberIdentity} -> subscriberIdentity) (\s@UpdateSubscriber' {} a -> s {subscriberIdentity = a} :: UpdateSubscriber)

-- | The name of the Security Lake account subscriber.
updateSubscriber_subscriberName :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberName = Lens.lens (\UpdateSubscriber' {subscriberName} -> subscriberName) (\s@UpdateSubscriber' {} a -> s {subscriberName = a} :: UpdateSubscriber)

-- | A value created by Security Lake that uniquely identifies your
-- subscription.
updateSubscriber_subscriberId :: Lens.Lens' UpdateSubscriber Prelude.Text
updateSubscriber_subscriberId = Lens.lens (\UpdateSubscriber' {subscriberId} -> subscriberId) (\s@UpdateSubscriber' {} a -> s {subscriberId = a} :: UpdateSubscriber)

instance Core.AWSRequest UpdateSubscriber where
  type
    AWSResponse UpdateSubscriber =
      UpdateSubscriberResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubscriberResponse'
            Prelude.<$> (x Data..?> "subscriber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSubscriber where
  hashWithSalt _salt UpdateSubscriber' {..} =
    _salt
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` subscriberIdentity
      `Prelude.hashWithSalt` subscriberName
      `Prelude.hashWithSalt` subscriberId

instance Prelude.NFData UpdateSubscriber where
  rnf UpdateSubscriber' {..} =
    Prelude.rnf sources
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf subscriberIdentity
      `Prelude.seq` Prelude.rnf subscriberName
      `Prelude.seq` Prelude.rnf subscriberId

instance Data.ToHeaders UpdateSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSubscriber where
  toJSON UpdateSubscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sources" Data..=) Prelude.<$> sources,
            ("subscriberDescription" Data..=)
              Prelude.<$> subscriberDescription,
            ("subscriberIdentity" Data..=)
              Prelude.<$> subscriberIdentity,
            ("subscriberName" Data..=)
              Prelude.<$> subscriberName
          ]
      )

instance Data.ToPath UpdateSubscriber where
  toPath UpdateSubscriber' {..} =
    Prelude.mconcat
      ["/v1/subscribers/", Data.toBS subscriberId]

instance Data.ToQuery UpdateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriberResponse' smart constructor.
data UpdateSubscriberResponse = UpdateSubscriberResponse'
  { -- | The updated subscriber information.
    subscriber :: Prelude.Maybe SubscriberResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriber', 'updateSubscriberResponse_subscriber' - The updated subscriber information.
--
-- 'httpStatus', 'updateSubscriberResponse_httpStatus' - The response's http status code.
newUpdateSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubscriberResponse
newUpdateSubscriberResponse pHttpStatus_ =
  UpdateSubscriberResponse'
    { subscriber =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated subscriber information.
updateSubscriberResponse_subscriber :: Lens.Lens' UpdateSubscriberResponse (Prelude.Maybe SubscriberResource)
updateSubscriberResponse_subscriber = Lens.lens (\UpdateSubscriberResponse' {subscriber} -> subscriber) (\s@UpdateSubscriberResponse' {} a -> s {subscriber = a} :: UpdateSubscriberResponse)

-- | The response's http status code.
updateSubscriberResponse_httpStatus :: Lens.Lens' UpdateSubscriberResponse Prelude.Int
updateSubscriberResponse_httpStatus = Lens.lens (\UpdateSubscriberResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriberResponse' {} a -> s {httpStatus = a} :: UpdateSubscriberResponse)

instance Prelude.NFData UpdateSubscriberResponse where
  rnf UpdateSubscriberResponse' {..} =
    Prelude.rnf subscriber
      `Prelude.seq` Prelude.rnf httpStatus
