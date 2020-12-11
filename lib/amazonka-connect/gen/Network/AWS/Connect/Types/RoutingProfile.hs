-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfile
  ( RoutingProfile (..),

    -- * Smart constructor
    mkRoutingProfile,

    -- * Lenses
    rpInstanceId,
    rpRoutingProfileARN,
    rpRoutingProfileId,
    rpDefaultOutboundQueueId,
    rpName,
    rpMediaConcurrencies,
    rpDescription,
    rpTags,
  )
where

import Network.AWS.Connect.Types.MediaConcurrency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a routing profile.
--
-- /See:/ 'mkRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { instanceId ::
      Lude.Maybe Lude.Text,
    routingProfileARN :: Lude.Maybe Lude.Text,
    routingProfileId :: Lude.Maybe Lude.Text,
    defaultOutboundQueueId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    mediaConcurrencies :: Lude.Maybe [MediaConcurrency],
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingProfile' with the minimum fields required to make a request.
--
-- * 'defaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
-- * 'description' - The description of the routing profile.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
-- * 'name' - The name of the routing profile.
-- * 'routingProfileARN' - The Amazon Resource Name (ARN) of the routing profile.
-- * 'routingProfileId' - The identifier of the routing profile.
-- * 'tags' - One or more tags.
mkRoutingProfile ::
  RoutingProfile
mkRoutingProfile =
  RoutingProfile'
    { instanceId = Lude.Nothing,
      routingProfileARN = Lude.Nothing,
      routingProfileId = Lude.Nothing,
      defaultOutboundQueueId = Lude.Nothing,
      name = Lude.Nothing,
      mediaConcurrencies = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpInstanceId :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpInstanceId = Lens.lens (instanceId :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: RoutingProfile)
{-# DEPRECATED rpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'routingProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRoutingProfileARN :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpRoutingProfileARN = Lens.lens (routingProfileARN :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {routingProfileARN = a} :: RoutingProfile)
{-# DEPRECATED rpRoutingProfileARN "Use generic-lens or generic-optics with 'routingProfileARN' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRoutingProfileId :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpRoutingProfileId = Lens.lens (routingProfileId :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {routingProfileId = a} :: RoutingProfile)
{-# DEPRECATED rpRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the default outbound queue for this routing profile.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDefaultOutboundQueueId :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpDefaultOutboundQueueId = Lens.lens (defaultOutboundQueueId :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {defaultOutboundQueueId = a} :: RoutingProfile)
{-# DEPRECATED rpDefaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead." #-}

-- | The name of the routing profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpName :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpName = Lens.lens (name :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RoutingProfile)
{-# DEPRECATED rpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMediaConcurrencies :: Lens.Lens' RoutingProfile (Lude.Maybe [MediaConcurrency])
rpMediaConcurrencies = Lens.lens (mediaConcurrencies :: RoutingProfile -> Lude.Maybe [MediaConcurrency]) (\s a -> s {mediaConcurrencies = a} :: RoutingProfile)
{-# DEPRECATED rpMediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead." #-}

-- | The description of the routing profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDescription :: Lens.Lens' RoutingProfile (Lude.Maybe Lude.Text)
rpDescription = Lens.lens (description :: RoutingProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RoutingProfile)
{-# DEPRECATED rpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpTags :: Lens.Lens' RoutingProfile (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rpTags = Lens.lens (tags :: RoutingProfile -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: RoutingProfile)
{-# DEPRECATED rpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON RoutingProfile where
  parseJSON =
    Lude.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "RoutingProfileArn")
            Lude.<*> (x Lude..:? "RoutingProfileId")
            Lude.<*> (x Lude..:? "DefaultOutboundQueueId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "MediaConcurrencies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
