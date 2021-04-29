{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a subscription definition which has already been
-- defined.
module Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
  ( -- * Creating a Request
    CreateSubscriptionDefinitionVersion (..),
    newCreateSubscriptionDefinitionVersion,

    -- * Request Lenses
    createSubscriptionDefinitionVersion_subscriptions,
    createSubscriptionDefinitionVersion_amznClientToken,
    createSubscriptionDefinitionVersion_subscriptionDefinitionId,

    -- * Destructuring the Response
    CreateSubscriptionDefinitionVersionResponse (..),
    newCreateSubscriptionDefinitionVersionResponse,

    -- * Response Lenses
    createSubscriptionDefinitionVersionResponse_creationTimestamp,
    createSubscriptionDefinitionVersionResponse_arn,
    createSubscriptionDefinitionVersionResponse_id,
    createSubscriptionDefinitionVersionResponse_version,
    createSubscriptionDefinitionVersionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { -- | A list of subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptions', 'createSubscriptionDefinitionVersion_subscriptions' - A list of subscriptions.
--
-- 'amznClientToken', 'createSubscriptionDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'subscriptionDefinitionId', 'createSubscriptionDefinitionVersion_subscriptionDefinitionId' - The ID of the subscription definition.
newCreateSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Prelude.Text ->
  CreateSubscriptionDefinitionVersion
newCreateSubscriptionDefinitionVersion
  pSubscriptionDefinitionId_ =
    CreateSubscriptionDefinitionVersion'
      { subscriptions =
          Prelude.Nothing,
        amznClientToken = Prelude.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | A list of subscriptions.
createSubscriptionDefinitionVersion_subscriptions :: Lens.Lens' CreateSubscriptionDefinitionVersion (Prelude.Maybe [Subscription])
createSubscriptionDefinitionVersion_subscriptions = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptions} -> subscriptions) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptions = a} :: CreateSubscriptionDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createSubscriptionDefinitionVersion_amznClientToken :: Lens.Lens' CreateSubscriptionDefinitionVersion (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersion_amznClientToken = Lens.lens (\CreateSubscriptionDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinitionVersion)

-- | The ID of the subscription definition.
createSubscriptionDefinitionVersion_subscriptionDefinitionId :: Lens.Lens' CreateSubscriptionDefinitionVersion Prelude.Text
createSubscriptionDefinitionVersion_subscriptionDefinitionId = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptionDefinitionId = a} :: CreateSubscriptionDefinitionVersion)

instance
  Prelude.AWSRequest
    CreateSubscriptionDefinitionVersion
  where
  type
    Rs CreateSubscriptionDefinitionVersion =
      CreateSubscriptionDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
              Prelude.<*> (x Prelude..?> "Arn")
              Prelude.<*> (x Prelude..?> "Id")
              Prelude.<*> (x Prelude..?> "Version")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriptionDefinitionVersion

instance
  Prelude.NFData
    CreateSubscriptionDefinitionVersion

instance
  Prelude.ToHeaders
    CreateSubscriptionDefinitionVersion
  where
  toHeaders CreateSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Prelude.ToJSON
    CreateSubscriptionDefinitionVersion
  where
  toJSON CreateSubscriptionDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Subscriptions" Prelude..=)
              Prelude.<$> subscriptions
          ]
      )

instance
  Prelude.ToPath
    CreateSubscriptionDefinitionVersion
  where
  toPath CreateSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Prelude.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance
  Prelude.ToQuery
    CreateSubscriptionDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createSubscriptionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createSubscriptionDefinitionVersionResponse_arn' - The ARN of the version.
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
      { creationTimestamp =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createSubscriptionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ARN of the version.
createSubscriptionDefinitionVersionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionVersionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionVersionResponse)

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
