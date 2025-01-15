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
-- Module      : Amazonka.Greengrass.CreateSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a subscription definition which has already been
-- defined.
module Amazonka.Greengrass.CreateSubscriptionDefinitionVersion
  ( -- * Creating a Request
    CreateSubscriptionDefinitionVersion (..),
    newCreateSubscriptionDefinitionVersion,

    -- * Request Lenses
    createSubscriptionDefinitionVersion_amznClientToken,
    createSubscriptionDefinitionVersion_subscriptions,
    createSubscriptionDefinitionVersion_subscriptionDefinitionId,

    -- * Destructuring the Response
    CreateSubscriptionDefinitionVersionResponse (..),
    newCreateSubscriptionDefinitionVersionResponse,

    -- * Response Lenses
    createSubscriptionDefinitionVersionResponse_arn,
    createSubscriptionDefinitionVersionResponse_creationTimestamp,
    createSubscriptionDefinitionVersionResponse_id,
    createSubscriptionDefinitionVersionResponse_version,
    createSubscriptionDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createSubscriptionDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'subscriptions', 'createSubscriptionDefinitionVersion_subscriptions' - A list of subscriptions.
--
-- 'subscriptionDefinitionId', 'createSubscriptionDefinitionVersion_subscriptionDefinitionId' - The ID of the subscription definition.
newCreateSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Prelude.Text ->
  CreateSubscriptionDefinitionVersion
newCreateSubscriptionDefinitionVersion
  pSubscriptionDefinitionId_ =
    CreateSubscriptionDefinitionVersion'
      { amznClientToken =
          Prelude.Nothing,
        subscriptions = Prelude.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | A client token used to correlate requests and responses.
createSubscriptionDefinitionVersion_amznClientToken :: Lens.Lens' CreateSubscriptionDefinitionVersion (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersion_amznClientToken = Lens.lens (\CreateSubscriptionDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinitionVersion)

-- | A list of subscriptions.
createSubscriptionDefinitionVersion_subscriptions :: Lens.Lens' CreateSubscriptionDefinitionVersion (Prelude.Maybe [Subscription])
createSubscriptionDefinitionVersion_subscriptions = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptions} -> subscriptions) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptions = a} :: CreateSubscriptionDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subscription definition.
createSubscriptionDefinitionVersion_subscriptionDefinitionId :: Lens.Lens' CreateSubscriptionDefinitionVersion Prelude.Text
createSubscriptionDefinitionVersion_subscriptionDefinitionId = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptionDefinitionId = a} :: CreateSubscriptionDefinitionVersion)

instance
  Core.AWSRequest
    CreateSubscriptionDefinitionVersion
  where
  type
    AWSResponse CreateSubscriptionDefinitionVersion =
      CreateSubscriptionDefinitionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriptionDefinitionVersion
  where
  hashWithSalt
    _salt
    CreateSubscriptionDefinitionVersion' {..} =
      _salt
        `Prelude.hashWithSalt` amznClientToken
        `Prelude.hashWithSalt` subscriptions
        `Prelude.hashWithSalt` subscriptionDefinitionId

instance
  Prelude.NFData
    CreateSubscriptionDefinitionVersion
  where
  rnf CreateSubscriptionDefinitionVersion' {..} =
    Prelude.rnf amznClientToken `Prelude.seq`
      Prelude.rnf subscriptions `Prelude.seq`
        Prelude.rnf subscriptionDefinitionId

instance
  Data.ToHeaders
    CreateSubscriptionDefinitionVersion
  where
  toHeaders CreateSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Data.ToJSON
    CreateSubscriptionDefinitionVersion
  where
  toJSON CreateSubscriptionDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Subscriptions" Data..=)
              Prelude.<$> subscriptions
          ]
      )

instance
  Data.ToPath
    CreateSubscriptionDefinitionVersion
  where
  toPath CreateSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Data.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance
  Data.ToQuery
    CreateSubscriptionDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createSubscriptionDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'creationTimestamp', 'createSubscriptionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'id', 'createSubscriptionDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createSubscriptionDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createSubscriptionDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateSubscriptionDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriptionDefinitionVersionResponse
newCreateSubscriptionDefinitionVersionResponse
  pHttpStatus_ =
    CreateSubscriptionDefinitionVersionResponse'
      { arn =
          Prelude.Nothing,
        creationTimestamp =
          Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the version.
createSubscriptionDefinitionVersionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the version was created.
createSubscriptionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createSubscriptionDefinitionVersionResponse_id :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_id = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {id} -> id) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {id = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ID of the version.
createSubscriptionDefinitionVersionResponse_version :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_version = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {version} -> version) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {version = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The response's http status code.
createSubscriptionDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse Prelude.Int
createSubscriptionDefinitionVersionResponse_httpStatus = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateSubscriptionDefinitionVersionResponse
  where
  rnf CreateSubscriptionDefinitionVersionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf version `Prelude.seq`
            Prelude.rnf httpStatus
