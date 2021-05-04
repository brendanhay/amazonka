{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a routing profile.
--
-- /See:/ 'newRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the default outbound queue for this routing profile.
    defaultOutboundQueueId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    mediaConcurrencies :: Prelude.Maybe [MediaConcurrency],
    -- | The name of the routing profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the routing profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      defaultOutboundQueueId = Prelude.Nothing,
      routingProfileId = Prelude.Nothing,
      mediaConcurrencies = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      routingProfileArn = Prelude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
routingProfile_instanceId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_instanceId = Lens.lens (\RoutingProfile' {instanceId} -> instanceId) (\s@RoutingProfile' {} a -> s {instanceId = a} :: RoutingProfile)

-- | The identifier of the default outbound queue for this routing profile.
routingProfile_defaultOutboundQueueId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_defaultOutboundQueueId = Lens.lens (\RoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@RoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: RoutingProfile)

-- | The identifier of the routing profile.
routingProfile_routingProfileId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileId = Lens.lens (\RoutingProfile' {routingProfileId} -> routingProfileId) (\s@RoutingProfile' {} a -> s {routingProfileId = a} :: RoutingProfile)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfile_mediaConcurrencies :: Lens.Lens' RoutingProfile (Prelude.Maybe [MediaConcurrency])
routingProfile_mediaConcurrencies = Lens.lens (\RoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@RoutingProfile' {} a -> s {mediaConcurrencies = a} :: RoutingProfile) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the routing profile.
routingProfile_name :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_name = Lens.lens (\RoutingProfile' {name} -> name) (\s@RoutingProfile' {} a -> s {name = a} :: RoutingProfile)

-- | One or more tags.
routingProfile_tags :: Lens.Lens' RoutingProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routingProfile_tags = Lens.lens (\RoutingProfile' {tags} -> tags) (\s@RoutingProfile' {} a -> s {tags = a} :: RoutingProfile) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the routing profile.
routingProfile_description :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_description = Lens.lens (\RoutingProfile' {description} -> description) (\s@RoutingProfile' {} a -> s {description = a} :: RoutingProfile)

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfile_routingProfileArn :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileArn = Lens.lens (\RoutingProfile' {routingProfileArn} -> routingProfileArn) (\s@RoutingProfile' {} a -> s {routingProfileArn = a} :: RoutingProfile)

instance Prelude.FromJSON RoutingProfile where
  parseJSON =
    Prelude.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "DefaultOutboundQueueId")
            Prelude.<*> (x Prelude..:? "RoutingProfileId")
            Prelude.<*> ( x Prelude..:? "MediaConcurrencies"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "RoutingProfileArn")
      )

instance Prelude.Hashable RoutingProfile

instance Prelude.NFData RoutingProfile
