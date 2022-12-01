{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfile where

import Amazonka.Connect.Types.MediaConcurrency
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a routing profile.
--
-- /See:/ 'newRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the routing profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of associated users in routing profile.
    numberOfAssociatedUsers :: Prelude.Maybe Prelude.Integer,
    -- | The description of the routing profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    mediaConcurrencies :: Prelude.Maybe [MediaConcurrency],
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The number of associated queues in routing profile.
    numberOfAssociatedQueues :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the default outbound queue for this routing profile.
    defaultOutboundQueueId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'routingProfile_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'routingProfile_name' - The name of the routing profile.
--
-- 'numberOfAssociatedUsers', 'routingProfile_numberOfAssociatedUsers' - The number of associated users in routing profile.
--
-- 'description', 'routingProfile_description' - The description of the routing profile.
--
-- 'mediaConcurrencies', 'routingProfile_mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
--
-- 'routingProfileArn', 'routingProfile_routingProfileArn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'instanceId', 'routingProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'numberOfAssociatedQueues', 'routingProfile_numberOfAssociatedQueues' - The number of associated queues in routing profile.
--
-- 'defaultOutboundQueueId', 'routingProfile_defaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
--
-- 'routingProfileId', 'routingProfile_routingProfileId' - The identifier of the routing profile.
newRoutingProfile ::
  RoutingProfile
newRoutingProfile =
  RoutingProfile'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfAssociatedUsers = Prelude.Nothing,
      description = Prelude.Nothing,
      mediaConcurrencies = Prelude.Nothing,
      routingProfileArn = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      numberOfAssociatedQueues = Prelude.Nothing,
      defaultOutboundQueueId = Prelude.Nothing,
      routingProfileId = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
routingProfile_tags :: Lens.Lens' RoutingProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routingProfile_tags = Lens.lens (\RoutingProfile' {tags} -> tags) (\s@RoutingProfile' {} a -> s {tags = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the routing profile.
routingProfile_name :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_name = Lens.lens (\RoutingProfile' {name} -> name) (\s@RoutingProfile' {} a -> s {name = a} :: RoutingProfile)

-- | The number of associated users in routing profile.
routingProfile_numberOfAssociatedUsers :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Integer)
routingProfile_numberOfAssociatedUsers = Lens.lens (\RoutingProfile' {numberOfAssociatedUsers} -> numberOfAssociatedUsers) (\s@RoutingProfile' {} a -> s {numberOfAssociatedUsers = a} :: RoutingProfile)

-- | The description of the routing profile.
routingProfile_description :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_description = Lens.lens (\RoutingProfile' {description} -> description) (\s@RoutingProfile' {} a -> s {description = a} :: RoutingProfile)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfile_mediaConcurrencies :: Lens.Lens' RoutingProfile (Prelude.Maybe [MediaConcurrency])
routingProfile_mediaConcurrencies = Lens.lens (\RoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@RoutingProfile' {} a -> s {mediaConcurrencies = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfile_routingProfileArn :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileArn = Lens.lens (\RoutingProfile' {routingProfileArn} -> routingProfileArn) (\s@RoutingProfile' {} a -> s {routingProfileArn = a} :: RoutingProfile)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
routingProfile_instanceId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_instanceId = Lens.lens (\RoutingProfile' {instanceId} -> instanceId) (\s@RoutingProfile' {} a -> s {instanceId = a} :: RoutingProfile)

-- | The number of associated queues in routing profile.
routingProfile_numberOfAssociatedQueues :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Integer)
routingProfile_numberOfAssociatedQueues = Lens.lens (\RoutingProfile' {numberOfAssociatedQueues} -> numberOfAssociatedQueues) (\s@RoutingProfile' {} a -> s {numberOfAssociatedQueues = a} :: RoutingProfile)

-- | The identifier of the default outbound queue for this routing profile.
routingProfile_defaultOutboundQueueId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_defaultOutboundQueueId = Lens.lens (\RoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@RoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: RoutingProfile)

-- | The identifier of the routing profile.
routingProfile_routingProfileId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileId = Lens.lens (\RoutingProfile' {routingProfileId} -> routingProfileId) (\s@RoutingProfile' {} a -> s {routingProfileId = a} :: RoutingProfile)

instance Core.FromJSON RoutingProfile where
  parseJSON =
    Core.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "NumberOfAssociatedUsers")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "MediaConcurrencies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RoutingProfileArn")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "NumberOfAssociatedQueues")
            Prelude.<*> (x Core..:? "DefaultOutboundQueueId")
            Prelude.<*> (x Core..:? "RoutingProfileId")
      )

instance Prelude.Hashable RoutingProfile where
  hashWithSalt _salt RoutingProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfAssociatedUsers
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` mediaConcurrencies
      `Prelude.hashWithSalt` routingProfileArn
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` numberOfAssociatedQueues
      `Prelude.hashWithSalt` defaultOutboundQueueId
      `Prelude.hashWithSalt` routingProfileId

instance Prelude.NFData RoutingProfile where
  rnf RoutingProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfAssociatedUsers
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf mediaConcurrencies
      `Prelude.seq` Prelude.rnf routingProfileArn
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf numberOfAssociatedQueues
      `Prelude.seq` Prelude.rnf defaultOutboundQueueId
      `Prelude.seq` Prelude.rnf routingProfileId
