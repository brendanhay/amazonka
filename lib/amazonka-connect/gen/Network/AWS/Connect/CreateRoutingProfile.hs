{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new routing profile.
module Network.AWS.Connect.CreateRoutingProfile
  ( -- * Creating a request
    CreateRoutingProfile (..),
    mkCreateRoutingProfile,

    -- ** Request lenses
    crpInstanceId,
    crpQueueConfigs,
    crpDefaultOutboundQueueId,
    crpName,
    crpMediaConcurrencies,
    crpDescription,
    crpTags,

    -- * Destructuring the response
    CreateRoutingProfileResponse (..),
    mkCreateRoutingProfileResponse,

    -- ** Response lenses
    crprsRoutingProfileARN,
    crprsRoutingProfileId,
    crprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
    queueConfigs :: Lude.Maybe (Lude.NonEmpty RoutingProfileQueueConfig),
    -- | The default outbound queue for the routing profile.
    defaultOutboundQueueId :: Lude.Text,
    -- | The name of the routing profile. Must not be more than 127 characters.
    name :: Lude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
    mediaConcurrencies :: [MediaConcurrency],
    -- | Description of the routing profile. Must not be more than 250 characters.
    description :: Lude.Text,
    -- | One or more tags.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoutingProfile' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'queueConfigs' - The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
-- * 'defaultOutboundQueueId' - The default outbound queue for the routing profile.
-- * 'name' - The name of the routing profile. Must not be more than 127 characters.
-- * 'mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
-- * 'description' - Description of the routing profile. Must not be more than 250 characters.
-- * 'tags' - One or more tags.
mkCreateRoutingProfile ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'defaultOutboundQueueId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateRoutingProfile
mkCreateRoutingProfile
  pInstanceId_
  pDefaultOutboundQueueId_
  pName_
  pDescription_ =
    CreateRoutingProfile'
      { instanceId = pInstanceId_,
        queueConfigs = Lude.Nothing,
        defaultOutboundQueueId = pDefaultOutboundQueueId_,
        name = pName_,
        mediaConcurrencies = Lude.mempty,
        description = pDescription_,
        tags = Lude.Nothing
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpInstanceId :: Lens.Lens' CreateRoutingProfile Lude.Text
crpInstanceId = Lens.lens (instanceId :: CreateRoutingProfile -> Lude.Text) (\s a -> s {instanceId = a} :: CreateRoutingProfile)
{-# DEPRECATED crpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpQueueConfigs :: Lens.Lens' CreateRoutingProfile (Lude.Maybe (Lude.NonEmpty RoutingProfileQueueConfig))
crpQueueConfigs = Lens.lens (queueConfigs :: CreateRoutingProfile -> Lude.Maybe (Lude.NonEmpty RoutingProfileQueueConfig)) (\s a -> s {queueConfigs = a} :: CreateRoutingProfile)
{-# DEPRECATED crpQueueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead." #-}

-- | The default outbound queue for the routing profile.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDefaultOutboundQueueId :: Lens.Lens' CreateRoutingProfile Lude.Text
crpDefaultOutboundQueueId = Lens.lens (defaultOutboundQueueId :: CreateRoutingProfile -> Lude.Text) (\s a -> s {defaultOutboundQueueId = a} :: CreateRoutingProfile)
{-# DEPRECATED crpDefaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead." #-}

-- | The name of the routing profile. Must not be more than 127 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpName :: Lens.Lens' CreateRoutingProfile Lude.Text
crpName = Lens.lens (name :: CreateRoutingProfile -> Lude.Text) (\s a -> s {name = a} :: CreateRoutingProfile)
{-# DEPRECATED crpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpMediaConcurrencies :: Lens.Lens' CreateRoutingProfile [MediaConcurrency]
crpMediaConcurrencies = Lens.lens (mediaConcurrencies :: CreateRoutingProfile -> [MediaConcurrency]) (\s a -> s {mediaConcurrencies = a} :: CreateRoutingProfile)
{-# DEPRECATED crpMediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead." #-}

-- | Description of the routing profile. Must not be more than 250 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDescription :: Lens.Lens' CreateRoutingProfile Lude.Text
crpDescription = Lens.lens (description :: CreateRoutingProfile -> Lude.Text) (\s a -> s {description = a} :: CreateRoutingProfile)
{-# DEPRECATED crpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpTags :: Lens.Lens' CreateRoutingProfile (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crpTags = Lens.lens (tags :: CreateRoutingProfile -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateRoutingProfile)
{-# DEPRECATED crpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRoutingProfile where
  type Rs CreateRoutingProfile = CreateRoutingProfileResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRoutingProfileResponse'
            Lude.<$> (x Lude..?> "RoutingProfileArn")
            Lude.<*> (x Lude..?> "RoutingProfileId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRoutingProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRoutingProfile where
  toJSON CreateRoutingProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QueueConfigs" Lude..=) Lude.<$> queueConfigs,
            Lude.Just
              ("DefaultOutboundQueueId" Lude..= defaultOutboundQueueId),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MediaConcurrencies" Lude..= mediaConcurrencies),
            Lude.Just ("Description" Lude..= description),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRoutingProfile where
  toPath CreateRoutingProfile' {..} =
    Lude.mconcat ["/routing-profiles/", Lude.toBS instanceId]

instance Lude.ToQuery CreateRoutingProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileARN :: Lude.Maybe Lude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoutingProfileResponse' with the minimum fields required to make a request.
--
-- * 'routingProfileARN' - The Amazon Resource Name (ARN) of the routing profile.
-- * 'routingProfileId' - The identifier of the routing profile.
-- * 'responseStatus' - The response status code.
mkCreateRoutingProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRoutingProfileResponse
mkCreateRoutingProfileResponse pResponseStatus_ =
  CreateRoutingProfileResponse'
    { routingProfileARN = Lude.Nothing,
      routingProfileId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'routingProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprsRoutingProfileARN :: Lens.Lens' CreateRoutingProfileResponse (Lude.Maybe Lude.Text)
crprsRoutingProfileARN = Lens.lens (routingProfileARN :: CreateRoutingProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {routingProfileARN = a} :: CreateRoutingProfileResponse)
{-# DEPRECATED crprsRoutingProfileARN "Use generic-lens or generic-optics with 'routingProfileARN' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprsRoutingProfileId :: Lens.Lens' CreateRoutingProfileResponse (Lude.Maybe Lude.Text)
crprsRoutingProfileId = Lens.lens (routingProfileId :: CreateRoutingProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {routingProfileId = a} :: CreateRoutingProfileResponse)
{-# DEPRECATED crprsRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprsResponseStatus :: Lens.Lens' CreateRoutingProfileResponse Lude.Int
crprsResponseStatus = Lens.lens (responseStatus :: CreateRoutingProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRoutingProfileResponse)
{-# DEPRECATED crprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
