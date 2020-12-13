{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition.
module Network.AWS.Greengrass.GetSubscriptionDefinition
  ( -- * Creating a request
    GetSubscriptionDefinition (..),
    mkGetSubscriptionDefinition,

    -- ** Request lenses
    gsdSubscriptionDefinitionId,

    -- * Destructuring the response
    GetSubscriptionDefinitionResponse (..),
    mkGetSubscriptionDefinitionResponse,

    -- ** Response lenses
    gsdrsLatestVersionARN,
    gsdrsARN,
    gsdrsName,
    gsdrsCreationTimestamp,
    gsdrsId,
    gsdrsLatestVersion,
    gsdrsLastUpdatedTimestamp,
    gsdrsTags,
    gsdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSubscriptionDefinition' smart constructor.
newtype GetSubscriptionDefinition = GetSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionDefinition' with the minimum fields required to make a request.
--
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
mkGetSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  GetSubscriptionDefinition
mkGetSubscriptionDefinition pSubscriptionDefinitionId_ =
  GetSubscriptionDefinition'
    { subscriptionDefinitionId =
        pSubscriptionDefinitionId_
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdSubscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinition Lude.Text
gsdSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: GetSubscriptionDefinition -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: GetSubscriptionDefinition)
{-# DEPRECATED gsdSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

instance Lude.AWSRequest GetSubscriptionDefinition where
  type
    Rs GetSubscriptionDefinition =
      GetSubscriptionDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSubscriptionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSubscriptionDefinition where
  toPath GetSubscriptionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId
      ]

instance Lude.ToQuery GetSubscriptionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSubscriptionDefinitionResponse' smart constructor.
data GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse'
  { -- | The ARN of the latest version associated with the definition.
    latestVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the definition.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the definition.
    name :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the definition.
    id :: Lude.Maybe Lude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Lude.Maybe Lude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'tags' - Tag(s) attached to the resource arn.
-- * 'responseStatus' - The response status code.
mkGetSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSubscriptionDefinitionResponse
mkGetSubscriptionDefinitionResponse pResponseStatus_ =
  GetSubscriptionDefinitionResponse'
    { latestVersionARN =
        Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsLatestVersionARN :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsLatestVersionARN = Lens.lens (latestVersionARN :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsARN :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsARN = Lens.lens (arn :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsName :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsName = Lens.lens (name :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsCreationTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsCreationTimestamp = Lens.lens (creationTimestamp :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsId :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsId = Lens.lens (id :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsLatestVersion :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsLatestVersion = Lens.lens (latestVersion :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsLastUpdatedTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
gsdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsTags :: Lens.Lens' GetSubscriptionDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gsdrsTags = Lens.lens (tags :: GetSubscriptionDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsResponseStatus :: Lens.Lens' GetSubscriptionDefinitionResponse Lude.Int
gsdrsResponseStatus = Lens.lens (responseStatus :: GetSubscriptionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSubscriptionDefinitionResponse)
{-# DEPRECATED gsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
