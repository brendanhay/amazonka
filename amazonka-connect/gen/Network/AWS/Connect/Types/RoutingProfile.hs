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
-- Module      : Network.AWS.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfile where

import Network.AWS.Connect.Types.MediaConcurrency
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a routing profile.
--
-- /See:/ 'newRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The identifier of the default outbound queue for this routing profile.
    defaultOutboundQueueId :: Core.Maybe Core.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Core.Maybe Core.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    mediaConcurrencies :: Core.Maybe [MediaConcurrency],
    -- | The name of the routing profile.
    name :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the routing profile.
    description :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'routingProfile_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'defaultOutboundQueueId', 'routingProfile_defaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
--
-- 'routingProfileId', 'routingProfile_routingProfileId' - The identifier of the routing profile.
--
-- 'mediaConcurrencies', 'routingProfile_mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
--
-- 'name', 'routingProfile_name' - The name of the routing profile.
--
-- 'tags', 'routingProfile_tags' - One or more tags.
--
-- 'description', 'routingProfile_description' - The description of the routing profile.
--
-- 'routingProfileArn', 'routingProfile_routingProfileArn' - The Amazon Resource Name (ARN) of the routing profile.
newRoutingProfile ::
  RoutingProfile
newRoutingProfile =
  RoutingProfile'
    { instanceId = Core.Nothing,
      defaultOutboundQueueId = Core.Nothing,
      routingProfileId = Core.Nothing,
      mediaConcurrencies = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      routingProfileArn = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
routingProfile_instanceId :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_instanceId = Lens.lens (\RoutingProfile' {instanceId} -> instanceId) (\s@RoutingProfile' {} a -> s {instanceId = a} :: RoutingProfile)

-- | The identifier of the default outbound queue for this routing profile.
routingProfile_defaultOutboundQueueId :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_defaultOutboundQueueId = Lens.lens (\RoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@RoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: RoutingProfile)

-- | The identifier of the routing profile.
routingProfile_routingProfileId :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_routingProfileId = Lens.lens (\RoutingProfile' {routingProfileId} -> routingProfileId) (\s@RoutingProfile' {} a -> s {routingProfileId = a} :: RoutingProfile)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfile_mediaConcurrencies :: Lens.Lens' RoutingProfile (Core.Maybe [MediaConcurrency])
routingProfile_mediaConcurrencies = Lens.lens (\RoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@RoutingProfile' {} a -> s {mediaConcurrencies = a} :: RoutingProfile) Core.. Lens.mapping Lens._Coerce

-- | The name of the routing profile.
routingProfile_name :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_name = Lens.lens (\RoutingProfile' {name} -> name) (\s@RoutingProfile' {} a -> s {name = a} :: RoutingProfile)

-- | One or more tags.
routingProfile_tags :: Lens.Lens' RoutingProfile (Core.Maybe (Core.HashMap Core.Text Core.Text))
routingProfile_tags = Lens.lens (\RoutingProfile' {tags} -> tags) (\s@RoutingProfile' {} a -> s {tags = a} :: RoutingProfile) Core.. Lens.mapping Lens._Coerce

-- | The description of the routing profile.
routingProfile_description :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_description = Lens.lens (\RoutingProfile' {description} -> description) (\s@RoutingProfile' {} a -> s {description = a} :: RoutingProfile)

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfile_routingProfileArn :: Lens.Lens' RoutingProfile (Core.Maybe Core.Text)
routingProfile_routingProfileArn = Lens.lens (\RoutingProfile' {routingProfileArn} -> routingProfileArn) (\s@RoutingProfile' {} a -> s {routingProfileArn = a} :: RoutingProfile)

instance Core.FromJSON RoutingProfile where
  parseJSON =
    Core.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "DefaultOutboundQueueId")
            Core.<*> (x Core..:? "RoutingProfileId")
            Core.<*> ( x Core..:? "MediaConcurrencies"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "RoutingProfileArn")
      )

instance Core.Hashable RoutingProfile

instance Core.NFData RoutingProfile
