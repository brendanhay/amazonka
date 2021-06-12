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
-- Module      : Network.AWS.Connect.CreateRoutingProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new routing profile.
module Network.AWS.Connect.CreateRoutingProfile
  ( -- * Creating a Request
    CreateRoutingProfile (..),
    newCreateRoutingProfile,

    -- * Request Lenses
    createRoutingProfile_queueConfigs,
    createRoutingProfile_tags,
    createRoutingProfile_instanceId,
    createRoutingProfile_name,
    createRoutingProfile_description,
    createRoutingProfile_defaultOutboundQueueId,
    createRoutingProfile_mediaConcurrencies,

    -- * Destructuring the Response
    CreateRoutingProfileResponse (..),
    newCreateRoutingProfileResponse,

    -- * Response Lenses
    createRoutingProfileResponse_routingProfileId,
    createRoutingProfileResponse_routingProfileArn,
    createRoutingProfileResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { -- | The inbound queues associated with the routing profile. If no queue is
    -- added, the agent can make only outbound calls.
    queueConfigs :: Core.Maybe (Core.NonEmpty RoutingProfileQueueConfig),
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The name of the routing profile. Must not be more than 127 characters.
    name :: Core.Text,
    -- | Description of the routing profile. Must not be more than 250
    -- characters.
    description :: Core.Text,
    -- | The default outbound queue for the routing profile.
    defaultOutboundQueueId :: Core.Text,
    -- | The channels that agents can handle in the Contact Control Panel (CCP)
    -- for this routing profile.
    mediaConcurrencies :: [MediaConcurrency]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueConfigs', 'createRoutingProfile_queueConfigs' - The inbound queues associated with the routing profile. If no queue is
-- added, the agent can make only outbound calls.
--
-- 'tags', 'createRoutingProfile_tags' - One or more tags.
--
-- 'instanceId', 'createRoutingProfile_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'name', 'createRoutingProfile_name' - The name of the routing profile. Must not be more than 127 characters.
--
-- 'description', 'createRoutingProfile_description' - Description of the routing profile. Must not be more than 250
-- characters.
--
-- 'defaultOutboundQueueId', 'createRoutingProfile_defaultOutboundQueueId' - The default outbound queue for the routing profile.
--
-- 'mediaConcurrencies', 'createRoutingProfile_mediaConcurrencies' - The channels that agents can handle in the Contact Control Panel (CCP)
-- for this routing profile.
newCreateRoutingProfile ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'description'
  Core.Text ->
  -- | 'defaultOutboundQueueId'
  Core.Text ->
  CreateRoutingProfile
newCreateRoutingProfile
  pInstanceId_
  pName_
  pDescription_
  pDefaultOutboundQueueId_ =
    CreateRoutingProfile'
      { queueConfigs = Core.Nothing,
        tags = Core.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        description = pDescription_,
        defaultOutboundQueueId = pDefaultOutboundQueueId_,
        mediaConcurrencies = Core.mempty
      }

-- | The inbound queues associated with the routing profile. If no queue is
-- added, the agent can make only outbound calls.
createRoutingProfile_queueConfigs :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.NonEmpty RoutingProfileQueueConfig))
createRoutingProfile_queueConfigs = Lens.lens (\CreateRoutingProfile' {queueConfigs} -> queueConfigs) (\s@CreateRoutingProfile' {} a -> s {queueConfigs = a} :: CreateRoutingProfile) Core.. Lens.mapping Lens._Coerce

-- | One or more tags.
createRoutingProfile_tags :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.HashMap Core.Text Core.Text))
createRoutingProfile_tags = Lens.lens (\CreateRoutingProfile' {tags} -> tags) (\s@CreateRoutingProfile' {} a -> s {tags = a} :: CreateRoutingProfile) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
createRoutingProfile_instanceId :: Lens.Lens' CreateRoutingProfile Core.Text
createRoutingProfile_instanceId = Lens.lens (\CreateRoutingProfile' {instanceId} -> instanceId) (\s@CreateRoutingProfile' {} a -> s {instanceId = a} :: CreateRoutingProfile)

-- | The name of the routing profile. Must not be more than 127 characters.
createRoutingProfile_name :: Lens.Lens' CreateRoutingProfile Core.Text
createRoutingProfile_name = Lens.lens (\CreateRoutingProfile' {name} -> name) (\s@CreateRoutingProfile' {} a -> s {name = a} :: CreateRoutingProfile)

-- | Description of the routing profile. Must not be more than 250
-- characters.
createRoutingProfile_description :: Lens.Lens' CreateRoutingProfile Core.Text
createRoutingProfile_description = Lens.lens (\CreateRoutingProfile' {description} -> description) (\s@CreateRoutingProfile' {} a -> s {description = a} :: CreateRoutingProfile)

-- | The default outbound queue for the routing profile.
createRoutingProfile_defaultOutboundQueueId :: Lens.Lens' CreateRoutingProfile Core.Text
createRoutingProfile_defaultOutboundQueueId = Lens.lens (\CreateRoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@CreateRoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: CreateRoutingProfile)

-- | The channels that agents can handle in the Contact Control Panel (CCP)
-- for this routing profile.
createRoutingProfile_mediaConcurrencies :: Lens.Lens' CreateRoutingProfile [MediaConcurrency]
createRoutingProfile_mediaConcurrencies = Lens.lens (\CreateRoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@CreateRoutingProfile' {} a -> s {mediaConcurrencies = a} :: CreateRoutingProfile) Core.. Lens._Coerce

instance Core.AWSRequest CreateRoutingProfile where
  type
    AWSResponse CreateRoutingProfile =
      CreateRoutingProfileResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoutingProfileResponse'
            Core.<$> (x Core..?> "RoutingProfileId")
            Core.<*> (x Core..?> "RoutingProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRoutingProfile

instance Core.NFData CreateRoutingProfile

instance Core.ToHeaders CreateRoutingProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRoutingProfile where
  toJSON CreateRoutingProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QueueConfigs" Core..=) Core.<$> queueConfigs,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Name" Core..= name),
            Core.Just ("Description" Core..= description),
            Core.Just
              ( "DefaultOutboundQueueId"
                  Core..= defaultOutboundQueueId
              ),
            Core.Just
              ("MediaConcurrencies" Core..= mediaConcurrencies)
          ]
      )

instance Core.ToPath CreateRoutingProfile where
  toPath CreateRoutingProfile' {..} =
    Core.mconcat
      ["/routing-profiles/", Core.toBS instanceId]

instance Core.ToQuery CreateRoutingProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { -- | The identifier of the routing profile.
    routingProfileId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRoutingProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingProfileId', 'createRoutingProfileResponse_routingProfileId' - The identifier of the routing profile.
--
-- 'routingProfileArn', 'createRoutingProfileResponse_routingProfileArn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'httpStatus', 'createRoutingProfileResponse_httpStatus' - The response's http status code.
newCreateRoutingProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRoutingProfileResponse
newCreateRoutingProfileResponse pHttpStatus_ =
  CreateRoutingProfileResponse'
    { routingProfileId =
        Core.Nothing,
      routingProfileArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the routing profile.
createRoutingProfileResponse_routingProfileId :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Core.Text)
createRoutingProfileResponse_routingProfileId = Lens.lens (\CreateRoutingProfileResponse' {routingProfileId} -> routingProfileId) (\s@CreateRoutingProfileResponse' {} a -> s {routingProfileId = a} :: CreateRoutingProfileResponse)

-- | The Amazon Resource Name (ARN) of the routing profile.
createRoutingProfileResponse_routingProfileArn :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Core.Text)
createRoutingProfileResponse_routingProfileArn = Lens.lens (\CreateRoutingProfileResponse' {routingProfileArn} -> routingProfileArn) (\s@CreateRoutingProfileResponse' {} a -> s {routingProfileArn = a} :: CreateRoutingProfileResponse)

-- | The response's http status code.
createRoutingProfileResponse_httpStatus :: Lens.Lens' CreateRoutingProfileResponse Core.Int
createRoutingProfileResponse_httpStatus = Lens.lens (\CreateRoutingProfileResponse' {httpStatus} -> httpStatus) (\s@CreateRoutingProfileResponse' {} a -> s {httpStatus = a} :: CreateRoutingProfileResponse)

instance Core.NFData CreateRoutingProfileResponse
