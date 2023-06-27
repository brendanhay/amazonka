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
-- Module      : Amazonka.SecurityLake.CreateSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription permission for accounts that are already enabled
-- in Amazon Security Lake. You can create a subscriber with access to data
-- in the current Amazon Web Services Region.
module Amazonka.SecurityLake.CreateSubscriber
  ( -- * Creating a Request
    CreateSubscriber (..),
    newCreateSubscriber,

    -- * Request Lenses
    createSubscriber_accessTypes,
    createSubscriber_subscriberDescription,
    createSubscriber_sources,
    createSubscriber_subscriberIdentity,
    createSubscriber_subscriberName,

    -- * Destructuring the Response
    CreateSubscriberResponse (..),
    newCreateSubscriberResponse,

    -- * Response Lenses
    createSubscriberResponse_subscriber,
    createSubscriberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { -- | The Amazon S3 or Lake Formation access type.
    accessTypes :: Prelude.Maybe [AccessType],
    -- | The description for your subscriber account in Security Lake.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The supported Amazon Web Services from which logs and events are
    -- collected. Security Lake supports log and event collection for natively
    -- supported Amazon Web Services.
    sources :: [LogSourceResource],
    -- | The AWS identity used to access your data.
    subscriberIdentity :: AwsIdentity,
    -- | The name of your Security Lake subscriber account.
    subscriberName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessTypes', 'createSubscriber_accessTypes' - The Amazon S3 or Lake Formation access type.
--
-- 'subscriberDescription', 'createSubscriber_subscriberDescription' - The description for your subscriber account in Security Lake.
--
-- 'sources', 'createSubscriber_sources' - The supported Amazon Web Services from which logs and events are
-- collected. Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
--
-- 'subscriberIdentity', 'createSubscriber_subscriberIdentity' - The AWS identity used to access your data.
--
-- 'subscriberName', 'createSubscriber_subscriberName' - The name of your Security Lake subscriber account.
newCreateSubscriber ::
  -- | 'subscriberIdentity'
  AwsIdentity ->
  -- | 'subscriberName'
  Prelude.Text ->
  CreateSubscriber
newCreateSubscriber
  pSubscriberIdentity_
  pSubscriberName_ =
    CreateSubscriber'
      { accessTypes = Prelude.Nothing,
        subscriberDescription = Prelude.Nothing,
        sources = Prelude.mempty,
        subscriberIdentity = pSubscriberIdentity_,
        subscriberName = pSubscriberName_
      }

-- | The Amazon S3 or Lake Formation access type.
createSubscriber_accessTypes :: Lens.Lens' CreateSubscriber (Prelude.Maybe [AccessType])
createSubscriber_accessTypes = Lens.lens (\CreateSubscriber' {accessTypes} -> accessTypes) (\s@CreateSubscriber' {} a -> s {accessTypes = a} :: CreateSubscriber) Prelude.. Lens.mapping Lens.coerced

-- | The description for your subscriber account in Security Lake.
createSubscriber_subscriberDescription :: Lens.Lens' CreateSubscriber (Prelude.Maybe Prelude.Text)
createSubscriber_subscriberDescription = Lens.lens (\CreateSubscriber' {subscriberDescription} -> subscriberDescription) (\s@CreateSubscriber' {} a -> s {subscriberDescription = a} :: CreateSubscriber)

-- | The supported Amazon Web Services from which logs and events are
-- collected. Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
createSubscriber_sources :: Lens.Lens' CreateSubscriber [LogSourceResource]
createSubscriber_sources = Lens.lens (\CreateSubscriber' {sources} -> sources) (\s@CreateSubscriber' {} a -> s {sources = a} :: CreateSubscriber) Prelude.. Lens.coerced

-- | The AWS identity used to access your data.
createSubscriber_subscriberIdentity :: Lens.Lens' CreateSubscriber AwsIdentity
createSubscriber_subscriberIdentity = Lens.lens (\CreateSubscriber' {subscriberIdentity} -> subscriberIdentity) (\s@CreateSubscriber' {} a -> s {subscriberIdentity = a} :: CreateSubscriber)

-- | The name of your Security Lake subscriber account.
createSubscriber_subscriberName :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_subscriberName = Lens.lens (\CreateSubscriber' {subscriberName} -> subscriberName) (\s@CreateSubscriber' {} a -> s {subscriberName = a} :: CreateSubscriber)

instance Core.AWSRequest CreateSubscriber where
  type
    AWSResponse CreateSubscriber =
      CreateSubscriberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriberResponse'
            Prelude.<$> (x Data..?> "subscriber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubscriber where
  hashWithSalt _salt CreateSubscriber' {..} =
    _salt
      `Prelude.hashWithSalt` accessTypes
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` subscriberIdentity
      `Prelude.hashWithSalt` subscriberName

instance Prelude.NFData CreateSubscriber where
  rnf CreateSubscriber' {..} =
    Prelude.rnf accessTypes
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf subscriberIdentity
      `Prelude.seq` Prelude.rnf subscriberName

instance Data.ToHeaders CreateSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSubscriber where
  toJSON CreateSubscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessTypes" Data..=) Prelude.<$> accessTypes,
            ("subscriberDescription" Data..=)
              Prelude.<$> subscriberDescription,
            Prelude.Just ("sources" Data..= sources),
            Prelude.Just
              ("subscriberIdentity" Data..= subscriberIdentity),
            Prelude.Just
              ("subscriberName" Data..= subscriberName)
          ]
      )

instance Data.ToPath CreateSubscriber where
  toPath = Prelude.const "/v1/subscribers"

instance Data.ToQuery CreateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriberResponse' smart constructor.
data CreateSubscriberResponse = CreateSubscriberResponse'
  { -- | Retrieve information about the subscriber created using the
    -- @CreateSubscriber@ API.
    subscriber :: Prelude.Maybe SubscriberResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriber', 'createSubscriberResponse_subscriber' - Retrieve information about the subscriber created using the
-- @CreateSubscriber@ API.
--
-- 'httpStatus', 'createSubscriberResponse_httpStatus' - The response's http status code.
newCreateSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriberResponse
newCreateSubscriberResponse pHttpStatus_ =
  CreateSubscriberResponse'
    { subscriber =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Retrieve information about the subscriber created using the
-- @CreateSubscriber@ API.
createSubscriberResponse_subscriber :: Lens.Lens' CreateSubscriberResponse (Prelude.Maybe SubscriberResource)
createSubscriberResponse_subscriber = Lens.lens (\CreateSubscriberResponse' {subscriber} -> subscriber) (\s@CreateSubscriberResponse' {} a -> s {subscriber = a} :: CreateSubscriberResponse)

-- | The response's http status code.
createSubscriberResponse_httpStatus :: Lens.Lens' CreateSubscriberResponse Prelude.Int
createSubscriberResponse_httpStatus = Lens.lens (\CreateSubscriberResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriberResponse' {} a -> s {httpStatus = a} :: CreateSubscriberResponse)

instance Prelude.NFData CreateSubscriberResponse where
  rnf CreateSubscriberResponse' {..} =
    Prelude.rnf subscriber
      `Prelude.seq` Prelude.rnf httpStatus
