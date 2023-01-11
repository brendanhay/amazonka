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
    updateSubscriber_externalId,
    updateSubscriber_subscriberDescription,
    updateSubscriber_subscriberName,
    updateSubscriber_id,
    updateSubscriber_sourceTypes,

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
  { -- | The external ID of the Security Lake account.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The description of the Security Lake account subscriber.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the Security Lake account subscriber.
    subscriberName :: Prelude.Maybe Prelude.Text,
    -- | A value created by Security Lake that uniquely identifies your
    -- subscription.
    id :: Prelude.Text,
    -- | The supported Amazon Web Services from which logs and events are
    -- collected. For the list of supported Amazon Web Services, see the
    -- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
    sourceTypes :: [SourceType]
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
-- 'externalId', 'updateSubscriber_externalId' - The external ID of the Security Lake account.
--
-- 'subscriberDescription', 'updateSubscriber_subscriberDescription' - The description of the Security Lake account subscriber.
--
-- 'subscriberName', 'updateSubscriber_subscriberName' - The name of the Security Lake account subscriber.
--
-- 'id', 'updateSubscriber_id' - A value created by Security Lake that uniquely identifies your
-- subscription.
--
-- 'sourceTypes', 'updateSubscriber_sourceTypes' - The supported Amazon Web Services from which logs and events are
-- collected. For the list of supported Amazon Web Services, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
newUpdateSubscriber ::
  -- | 'id'
  Prelude.Text ->
  UpdateSubscriber
newUpdateSubscriber pId_ =
  UpdateSubscriber'
    { externalId = Prelude.Nothing,
      subscriberDescription = Prelude.Nothing,
      subscriberName = Prelude.Nothing,
      id = pId_,
      sourceTypes = Prelude.mempty
    }

-- | The external ID of the Security Lake account.
updateSubscriber_externalId :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_externalId = Lens.lens (\UpdateSubscriber' {externalId} -> externalId) (\s@UpdateSubscriber' {} a -> s {externalId = a} :: UpdateSubscriber)

-- | The description of the Security Lake account subscriber.
updateSubscriber_subscriberDescription :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberDescription = Lens.lens (\UpdateSubscriber' {subscriberDescription} -> subscriberDescription) (\s@UpdateSubscriber' {} a -> s {subscriberDescription = a} :: UpdateSubscriber)

-- | The name of the Security Lake account subscriber.
updateSubscriber_subscriberName :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberName = Lens.lens (\UpdateSubscriber' {subscriberName} -> subscriberName) (\s@UpdateSubscriber' {} a -> s {subscriberName = a} :: UpdateSubscriber)

-- | A value created by Security Lake that uniquely identifies your
-- subscription.
updateSubscriber_id :: Lens.Lens' UpdateSubscriber Prelude.Text
updateSubscriber_id = Lens.lens (\UpdateSubscriber' {id} -> id) (\s@UpdateSubscriber' {} a -> s {id = a} :: UpdateSubscriber)

-- | The supported Amazon Web Services from which logs and events are
-- collected. For the list of supported Amazon Web Services, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
updateSubscriber_sourceTypes :: Lens.Lens' UpdateSubscriber [SourceType]
updateSubscriber_sourceTypes = Lens.lens (\UpdateSubscriber' {sourceTypes} -> sourceTypes) (\s@UpdateSubscriber' {} a -> s {sourceTypes = a} :: UpdateSubscriber) Prelude.. Lens.coerced

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
    _salt `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` subscriberName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sourceTypes

instance Prelude.NFData UpdateSubscriber where
  rnf UpdateSubscriber' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf subscriberName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sourceTypes

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
          [ ("externalId" Data..=) Prelude.<$> externalId,
            ("subscriberDescription" Data..=)
              Prelude.<$> subscriberDescription,
            ("subscriberName" Data..=)
              Prelude.<$> subscriberName,
            Prelude.Just ("sourceTypes" Data..= sourceTypes)
          ]
      )

instance Data.ToPath UpdateSubscriber where
  toPath UpdateSubscriber' {..} =
    Prelude.mconcat ["/v1/subscribers/", Data.toBS id]

instance Data.ToQuery UpdateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriberResponse' smart constructor.
data UpdateSubscriberResponse = UpdateSubscriberResponse'
  { -- | The account of the subscriber.
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
-- 'subscriber', 'updateSubscriberResponse_subscriber' - The account of the subscriber.
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

-- | The account of the subscriber.
updateSubscriberResponse_subscriber :: Lens.Lens' UpdateSubscriberResponse (Prelude.Maybe SubscriberResource)
updateSubscriberResponse_subscriber = Lens.lens (\UpdateSubscriberResponse' {subscriber} -> subscriber) (\s@UpdateSubscriberResponse' {} a -> s {subscriber = a} :: UpdateSubscriberResponse)

-- | The response's http status code.
updateSubscriberResponse_httpStatus :: Lens.Lens' UpdateSubscriberResponse Prelude.Int
updateSubscriberResponse_httpStatus = Lens.lens (\UpdateSubscriberResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriberResponse' {} a -> s {httpStatus = a} :: UpdateSubscriberResponse)

instance Prelude.NFData UpdateSubscriberResponse where
  rnf UpdateSubscriberResponse' {..} =
    Prelude.rnf subscriber
      `Prelude.seq` Prelude.rnf httpStatus
