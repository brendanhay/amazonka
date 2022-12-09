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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the subscription permission for the given Security Lake account
-- ID.
module Amazonka.SecurityLake.UpdateSubscriber
  ( -- * Creating a Request
    UpdateSubscriber (..),
    newUpdateSubscriber,

    -- * Request Lenses
    updateSubscriber_externalId,
    updateSubscriber_sourceTypes,
    updateSubscriber_subscriberDescription,
    updateSubscriber_subscriberName,
    updateSubscriber_id,

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
  { -- | External ID of the Security Lake account.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The supported Amazon Web Services services from which logs and events
    -- are collected. Amazon Security Lake supports logs and events collection
    -- for the following natively-supported Amazon Web Services services. For
    -- more information, see the Amazon Security Lake User Guide.
    sourceTypes :: Prelude.Maybe [SourceType],
    -- | Description of the Security Lake account subscriber.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | Name of the Security Lake account subscriber.
    subscriberName :: Prelude.Maybe Prelude.Text,
    -- | A value created by Security Lake that uniquely identifies your
    -- @UpdateSubscriber@ API request.
    id :: Prelude.Text
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
-- 'externalId', 'updateSubscriber_externalId' - External ID of the Security Lake account.
--
-- 'sourceTypes', 'updateSubscriber_sourceTypes' - The supported Amazon Web Services services from which logs and events
-- are collected. Amazon Security Lake supports logs and events collection
-- for the following natively-supported Amazon Web Services services. For
-- more information, see the Amazon Security Lake User Guide.
--
-- 'subscriberDescription', 'updateSubscriber_subscriberDescription' - Description of the Security Lake account subscriber.
--
-- 'subscriberName', 'updateSubscriber_subscriberName' - Name of the Security Lake account subscriber.
--
-- 'id', 'updateSubscriber_id' - A value created by Security Lake that uniquely identifies your
-- @UpdateSubscriber@ API request.
newUpdateSubscriber ::
  -- | 'id'
  Prelude.Text ->
  UpdateSubscriber
newUpdateSubscriber pId_ =
  UpdateSubscriber'
    { externalId = Prelude.Nothing,
      sourceTypes = Prelude.Nothing,
      subscriberDescription = Prelude.Nothing,
      subscriberName = Prelude.Nothing,
      id = pId_
    }

-- | External ID of the Security Lake account.
updateSubscriber_externalId :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_externalId = Lens.lens (\UpdateSubscriber' {externalId} -> externalId) (\s@UpdateSubscriber' {} a -> s {externalId = a} :: UpdateSubscriber)

-- | The supported Amazon Web Services services from which logs and events
-- are collected. Amazon Security Lake supports logs and events collection
-- for the following natively-supported Amazon Web Services services. For
-- more information, see the Amazon Security Lake User Guide.
updateSubscriber_sourceTypes :: Lens.Lens' UpdateSubscriber (Prelude.Maybe [SourceType])
updateSubscriber_sourceTypes = Lens.lens (\UpdateSubscriber' {sourceTypes} -> sourceTypes) (\s@UpdateSubscriber' {} a -> s {sourceTypes = a} :: UpdateSubscriber) Prelude.. Lens.mapping Lens.coerced

-- | Description of the Security Lake account subscriber.
updateSubscriber_subscriberDescription :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberDescription = Lens.lens (\UpdateSubscriber' {subscriberDescription} -> subscriberDescription) (\s@UpdateSubscriber' {} a -> s {subscriberDescription = a} :: UpdateSubscriber)

-- | Name of the Security Lake account subscriber.
updateSubscriber_subscriberName :: Lens.Lens' UpdateSubscriber (Prelude.Maybe Prelude.Text)
updateSubscriber_subscriberName = Lens.lens (\UpdateSubscriber' {subscriberName} -> subscriberName) (\s@UpdateSubscriber' {} a -> s {subscriberName = a} :: UpdateSubscriber)

-- | A value created by Security Lake that uniquely identifies your
-- @UpdateSubscriber@ API request.
updateSubscriber_id :: Lens.Lens' UpdateSubscriber Prelude.Text
updateSubscriber_id = Lens.lens (\UpdateSubscriber' {id} -> id) (\s@UpdateSubscriber' {} a -> s {id = a} :: UpdateSubscriber)

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
      `Prelude.hashWithSalt` sourceTypes
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` subscriberName
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateSubscriber where
  rnf UpdateSubscriber' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf sourceTypes
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf subscriberName
      `Prelude.seq` Prelude.rnf id

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
            ("sourceTypes" Data..=) Prelude.<$> sourceTypes,
            ("subscriberDescription" Data..=)
              Prelude.<$> subscriberDescription,
            ("subscriberName" Data..=)
              Prelude.<$> subscriberName
          ]
      )

instance Data.ToPath UpdateSubscriber where
  toPath UpdateSubscriber' {..} =
    Prelude.mconcat ["/v1/subscribers/", Data.toBS id]

instance Data.ToQuery UpdateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriberResponse' smart constructor.
data UpdateSubscriberResponse = UpdateSubscriberResponse'
  { -- | The account subscriber in Amazon Security Lake.
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
-- 'subscriber', 'updateSubscriberResponse_subscriber' - The account subscriber in Amazon Security Lake.
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

-- | The account subscriber in Amazon Security Lake.
updateSubscriberResponse_subscriber :: Lens.Lens' UpdateSubscriberResponse (Prelude.Maybe SubscriberResource)
updateSubscriberResponse_subscriber = Lens.lens (\UpdateSubscriberResponse' {subscriber} -> subscriber) (\s@UpdateSubscriberResponse' {} a -> s {subscriber = a} :: UpdateSubscriberResponse)

-- | The response's http status code.
updateSubscriberResponse_httpStatus :: Lens.Lens' UpdateSubscriberResponse Prelude.Int
updateSubscriberResponse_httpStatus = Lens.lens (\UpdateSubscriberResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriberResponse' {} a -> s {httpStatus = a} :: UpdateSubscriberResponse)

instance Prelude.NFData UpdateSubscriberResponse where
  rnf UpdateSubscriberResponse' {..} =
    Prelude.rnf subscriber
      `Prelude.seq` Prelude.rnf httpStatus
