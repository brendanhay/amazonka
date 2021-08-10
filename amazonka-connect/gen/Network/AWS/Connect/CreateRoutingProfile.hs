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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { -- | The inbound queues associated with the routing profile. If no queue is
    -- added, the agent can make only outbound calls.
    queueConfigs :: Prelude.Maybe (Prelude.NonEmpty RoutingProfileQueueConfig),
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The name of the routing profile. Must not be more than 127 characters.
    name :: Prelude.Text,
    -- | Description of the routing profile. Must not be more than 250
    -- characters.
    description :: Prelude.Text,
    -- | The default outbound queue for the routing profile.
    defaultOutboundQueueId :: Prelude.Text,
    -- | The channels that agents can handle in the Contact Control Panel (CCP)
    -- for this routing profile.
    mediaConcurrencies :: [MediaConcurrency]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'defaultOutboundQueueId'
  Prelude.Text ->
  CreateRoutingProfile
newCreateRoutingProfile
  pInstanceId_
  pName_
  pDescription_
  pDefaultOutboundQueueId_ =
    CreateRoutingProfile'
      { queueConfigs =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        description = pDescription_,
        defaultOutboundQueueId = pDefaultOutboundQueueId_,
        mediaConcurrencies = Prelude.mempty
      }

-- | The inbound queues associated with the routing profile. If no queue is
-- added, the agent can make only outbound calls.
createRoutingProfile_queueConfigs :: Lens.Lens' CreateRoutingProfile (Prelude.Maybe (Prelude.NonEmpty RoutingProfileQueueConfig))
createRoutingProfile_queueConfigs = Lens.lens (\CreateRoutingProfile' {queueConfigs} -> queueConfigs) (\s@CreateRoutingProfile' {} a -> s {queueConfigs = a} :: CreateRoutingProfile) Prelude.. Lens.mapping Lens._Coerce

-- | One or more tags.
createRoutingProfile_tags :: Lens.Lens' CreateRoutingProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRoutingProfile_tags = Lens.lens (\CreateRoutingProfile' {tags} -> tags) (\s@CreateRoutingProfile' {} a -> s {tags = a} :: CreateRoutingProfile) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
createRoutingProfile_instanceId :: Lens.Lens' CreateRoutingProfile Prelude.Text
createRoutingProfile_instanceId = Lens.lens (\CreateRoutingProfile' {instanceId} -> instanceId) (\s@CreateRoutingProfile' {} a -> s {instanceId = a} :: CreateRoutingProfile)

-- | The name of the routing profile. Must not be more than 127 characters.
createRoutingProfile_name :: Lens.Lens' CreateRoutingProfile Prelude.Text
createRoutingProfile_name = Lens.lens (\CreateRoutingProfile' {name} -> name) (\s@CreateRoutingProfile' {} a -> s {name = a} :: CreateRoutingProfile)

-- | Description of the routing profile. Must not be more than 250
-- characters.
createRoutingProfile_description :: Lens.Lens' CreateRoutingProfile Prelude.Text
createRoutingProfile_description = Lens.lens (\CreateRoutingProfile' {description} -> description) (\s@CreateRoutingProfile' {} a -> s {description = a} :: CreateRoutingProfile)

-- | The default outbound queue for the routing profile.
createRoutingProfile_defaultOutboundQueueId :: Lens.Lens' CreateRoutingProfile Prelude.Text
createRoutingProfile_defaultOutboundQueueId = Lens.lens (\CreateRoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@CreateRoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: CreateRoutingProfile)

-- | The channels that agents can handle in the Contact Control Panel (CCP)
-- for this routing profile.
createRoutingProfile_mediaConcurrencies :: Lens.Lens' CreateRoutingProfile [MediaConcurrency]
createRoutingProfile_mediaConcurrencies = Lens.lens (\CreateRoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@CreateRoutingProfile' {} a -> s {mediaConcurrencies = a} :: CreateRoutingProfile) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateRoutingProfile where
  type
    AWSResponse CreateRoutingProfile =
      CreateRoutingProfileResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoutingProfileResponse'
            Prelude.<$> (x Core..?> "RoutingProfileId")
            Prelude.<*> (x Core..?> "RoutingProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoutingProfile

instance Prelude.NFData CreateRoutingProfile

instance Core.ToHeaders CreateRoutingProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRoutingProfile where
  toJSON CreateRoutingProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QueueConfigs" Core..=) Prelude.<$> queueConfigs,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Description" Core..= description),
            Prelude.Just
              ( "DefaultOutboundQueueId"
                  Core..= defaultOutboundQueueId
              ),
            Prelude.Just
              ("MediaConcurrencies" Core..= mediaConcurrencies)
          ]
      )

instance Core.ToPath CreateRoutingProfile where
  toPath CreateRoutingProfile' {..} =
    Prelude.mconcat
      ["/routing-profiles/", Core.toBS instanceId]

instance Core.ToQuery CreateRoutingProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateRoutingProfileResponse
newCreateRoutingProfileResponse pHttpStatus_ =
  CreateRoutingProfileResponse'
    { routingProfileId =
        Prelude.Nothing,
      routingProfileArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the routing profile.
createRoutingProfileResponse_routingProfileId :: Lens.Lens' CreateRoutingProfileResponse (Prelude.Maybe Prelude.Text)
createRoutingProfileResponse_routingProfileId = Lens.lens (\CreateRoutingProfileResponse' {routingProfileId} -> routingProfileId) (\s@CreateRoutingProfileResponse' {} a -> s {routingProfileId = a} :: CreateRoutingProfileResponse)

-- | The Amazon Resource Name (ARN) of the routing profile.
createRoutingProfileResponse_routingProfileArn :: Lens.Lens' CreateRoutingProfileResponse (Prelude.Maybe Prelude.Text)
createRoutingProfileResponse_routingProfileArn = Lens.lens (\CreateRoutingProfileResponse' {routingProfileArn} -> routingProfileArn) (\s@CreateRoutingProfileResponse' {} a -> s {routingProfileArn = a} :: CreateRoutingProfileResponse)

-- | The response's http status code.
createRoutingProfileResponse_httpStatus :: Lens.Lens' CreateRoutingProfileResponse Prelude.Int
createRoutingProfileResponse_httpStatus = Lens.lens (\CreateRoutingProfileResponse' {httpStatus} -> httpStatus) (\s@CreateRoutingProfileResponse' {} a -> s {httpStatus = a} :: CreateRoutingProfileResponse)

instance Prelude.NFData CreateRoutingProfileResponse
