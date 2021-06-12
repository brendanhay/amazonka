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

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Subscription],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CreateSubscriptionDefinitionVersion
newCreateSubscriptionDefinitionVersion
  pSubscriptionDefinitionId_ =
    CreateSubscriptionDefinitionVersion'
      { subscriptions =
          Core.Nothing,
        amznClientToken = Core.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | A list of subscriptions.
createSubscriptionDefinitionVersion_subscriptions :: Lens.Lens' CreateSubscriptionDefinitionVersion (Core.Maybe [Subscription])
createSubscriptionDefinitionVersion_subscriptions = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptions} -> subscriptions) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptions = a} :: CreateSubscriptionDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createSubscriptionDefinitionVersion_amznClientToken :: Lens.Lens' CreateSubscriptionDefinitionVersion (Core.Maybe Core.Text)
createSubscriptionDefinitionVersion_amznClientToken = Lens.lens (\CreateSubscriptionDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinitionVersion)

-- | The ID of the subscription definition.
createSubscriptionDefinitionVersion_subscriptionDefinitionId :: Lens.Lens' CreateSubscriptionDefinitionVersion Core.Text
createSubscriptionDefinitionVersion_subscriptionDefinitionId = Lens.lens (\CreateSubscriptionDefinitionVersion' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@CreateSubscriptionDefinitionVersion' {} a -> s {subscriptionDefinitionId = a} :: CreateSubscriptionDefinitionVersion)

instance
  Core.AWSRequest
    CreateSubscriptionDefinitionVersion
  where
  type
    AWSResponse CreateSubscriptionDefinitionVersion =
      CreateSubscriptionDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateSubscriptionDefinitionVersion

instance
  Core.NFData
    CreateSubscriptionDefinitionVersion

instance
  Core.ToHeaders
    CreateSubscriptionDefinitionVersion
  where
  toHeaders CreateSubscriptionDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance
  Core.ToJSON
    CreateSubscriptionDefinitionVersion
  where
  toJSON CreateSubscriptionDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Subscriptions" Core..=) Core.<$> subscriptions]
      )

instance
  Core.ToPath
    CreateSubscriptionDefinitionVersion
  where
  toPath CreateSubscriptionDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance
  Core.ToQuery
    CreateSubscriptionDefinitionVersion
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateSubscriptionDefinitionVersionResponse
newCreateSubscriptionDefinitionVersionResponse
  pHttpStatus_ =
    CreateSubscriptionDefinitionVersionResponse'
      { creationTimestamp =
          Core.Nothing,
        arn = Core.Nothing,
        id = Core.Nothing,
        version = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createSubscriptionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ARN of the version.
createSubscriptionDefinitionVersionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionVersionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createSubscriptionDefinitionVersionResponse_id :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionVersionResponse_id = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {id} -> id) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {id = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The ID of the version.
createSubscriptionDefinitionVersionResponse_version :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
createSubscriptionDefinitionVersionResponse_version = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {version} -> version) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {version = a} :: CreateSubscriptionDefinitionVersionResponse)

-- | The response's http status code.
createSubscriptionDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse Core.Int
createSubscriptionDefinitionVersionResponse_httpStatus = Lens.lens (\CreateSubscriptionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionDefinitionVersionResponse)

instance
  Core.NFData
    CreateSubscriptionDefinitionVersionResponse
