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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a routing profile.
--
-- /See:/ 'newRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { -- | The identifier of the default outbound queue for this routing profile.
    defaultOutboundQueueId :: Prelude.Maybe Prelude.Text,
    -- | The description of the routing profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    mediaConcurrencies :: Prelude.Maybe [MediaConcurrency],
    -- | The name of the routing profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of associated queues in routing profile.
    numberOfAssociatedQueues :: Prelude.Maybe Prelude.Integer,
    -- | The number of associated users in routing profile.
    numberOfAssociatedUsers :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'defaultOutboundQueueId', 'routingProfile_defaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
--
-- 'description', 'routingProfile_description' - The description of the routing profile.
--
-- 'instanceId', 'routingProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'mediaConcurrencies', 'routingProfile_mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
--
-- 'name', 'routingProfile_name' - The name of the routing profile.
--
-- 'numberOfAssociatedQueues', 'routingProfile_numberOfAssociatedQueues' - The number of associated queues in routing profile.
--
-- 'numberOfAssociatedUsers', 'routingProfile_numberOfAssociatedUsers' - The number of associated users in routing profile.
--
-- 'routingProfileArn', 'routingProfile_routingProfileArn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'routingProfileId', 'routingProfile_routingProfileId' - The identifier of the routing profile.
--
-- 'tags', 'routingProfile_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newRoutingProfile ::
  RoutingProfile
newRoutingProfile =
  RoutingProfile'
    { defaultOutboundQueueId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      mediaConcurrencies = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfAssociatedQueues = Prelude.Nothing,
      numberOfAssociatedUsers = Prelude.Nothing,
      routingProfileArn = Prelude.Nothing,
      routingProfileId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The identifier of the default outbound queue for this routing profile.
routingProfile_defaultOutboundQueueId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_defaultOutboundQueueId = Lens.lens (\RoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@RoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: RoutingProfile)

-- | The description of the routing profile.
routingProfile_description :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_description = Lens.lens (\RoutingProfile' {description} -> description) (\s@RoutingProfile' {} a -> s {description = a} :: RoutingProfile)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
routingProfile_instanceId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_instanceId = Lens.lens (\RoutingProfile' {instanceId} -> instanceId) (\s@RoutingProfile' {} a -> s {instanceId = a} :: RoutingProfile)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfile_mediaConcurrencies :: Lens.Lens' RoutingProfile (Prelude.Maybe [MediaConcurrency])
routingProfile_mediaConcurrencies = Lens.lens (\RoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@RoutingProfile' {} a -> s {mediaConcurrencies = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the routing profile.
routingProfile_name :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_name = Lens.lens (\RoutingProfile' {name} -> name) (\s@RoutingProfile' {} a -> s {name = a} :: RoutingProfile)

-- | The number of associated queues in routing profile.
routingProfile_numberOfAssociatedQueues :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Integer)
routingProfile_numberOfAssociatedQueues = Lens.lens (\RoutingProfile' {numberOfAssociatedQueues} -> numberOfAssociatedQueues) (\s@RoutingProfile' {} a -> s {numberOfAssociatedQueues = a} :: RoutingProfile)

-- | The number of associated users in routing profile.
routingProfile_numberOfAssociatedUsers :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Integer)
routingProfile_numberOfAssociatedUsers = Lens.lens (\RoutingProfile' {numberOfAssociatedUsers} -> numberOfAssociatedUsers) (\s@RoutingProfile' {} a -> s {numberOfAssociatedUsers = a} :: RoutingProfile)

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfile_routingProfileArn :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileArn = Lens.lens (\RoutingProfile' {routingProfileArn} -> routingProfileArn) (\s@RoutingProfile' {} a -> s {routingProfileArn = a} :: RoutingProfile)

-- | The identifier of the routing profile.
routingProfile_routingProfileId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileId = Lens.lens (\RoutingProfile' {routingProfileId} -> routingProfileId) (\s@RoutingProfile' {} a -> s {routingProfileId = a} :: RoutingProfile)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
routingProfile_tags :: Lens.Lens' RoutingProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routingProfile_tags = Lens.lens (\RoutingProfile' {tags} -> tags) (\s@RoutingProfile' {} a -> s {tags = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RoutingProfile where
  parseJSON =
    Data.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Prelude.<$> (x Data..:? "DefaultOutboundQueueId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> ( x Data..:? "MediaConcurrencies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NumberOfAssociatedQueues")
            Prelude.<*> (x Data..:? "NumberOfAssociatedUsers")
            Prelude.<*> (x Data..:? "RoutingProfileArn")
            Prelude.<*> (x Data..:? "RoutingProfileId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RoutingProfile where
  hashWithSalt _salt RoutingProfile' {..} =
    _salt `Prelude.hashWithSalt` defaultOutboundQueueId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` mediaConcurrencies
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfAssociatedQueues
      `Prelude.hashWithSalt` numberOfAssociatedUsers
      `Prelude.hashWithSalt` routingProfileArn
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RoutingProfile where
  rnf RoutingProfile' {..} =
    Prelude.rnf defaultOutboundQueueId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf mediaConcurrencies
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfAssociatedQueues
      `Prelude.seq` Prelude.rnf numberOfAssociatedUsers
      `Prelude.seq` Prelude.rnf routingProfileArn
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf tags
