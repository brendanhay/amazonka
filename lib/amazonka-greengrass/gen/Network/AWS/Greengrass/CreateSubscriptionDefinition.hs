{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version of the subscription definition now or use ''CreateSubscriptionDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateSubscriptionDefinition
  ( -- * Creating a request
    CreateSubscriptionDefinition (..),
    mkCreateSubscriptionDefinition,

    -- ** Request lenses
    csdAmznClientToken,
    csdInitialVersion,
    csdName,
    csdTags,

    -- * Destructuring the response
    CreateSubscriptionDefinitionResponse (..),
    mkCreateSubscriptionDefinitionResponse,

    -- ** Response lenses
    csdrsLatestVersionARN,
    csdrsARN,
    csdrsName,
    csdrsCreationTimestamp,
    csdrsId,
    csdrsLatestVersion,
    csdrsLastUpdatedTimestamp,
    csdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the subscription definition.
    initialVersion :: Lude.Maybe SubscriptionDefinitionVersion,
    -- | The name of the subscription definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriptionDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the subscription definition.
-- * 'name' - The name of the subscription definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateSubscriptionDefinition ::
  CreateSubscriptionDefinition
mkCreateSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdAmznClientToken :: Lens.Lens' CreateSubscriptionDefinition (Lude.Maybe Lude.Text)
csdAmznClientToken = Lens.lens (amznClientToken :: CreateSubscriptionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateSubscriptionDefinition)
{-# DEPRECATED csdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the subscription definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdInitialVersion :: Lens.Lens' CreateSubscriptionDefinition (Lude.Maybe SubscriptionDefinitionVersion)
csdInitialVersion = Lens.lens (initialVersion :: CreateSubscriptionDefinition -> Lude.Maybe SubscriptionDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateSubscriptionDefinition)
{-# DEPRECATED csdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the subscription definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdName :: Lens.Lens' CreateSubscriptionDefinition (Lude.Maybe Lude.Text)
csdName = Lens.lens (name :: CreateSubscriptionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateSubscriptionDefinition)
{-# DEPRECATED csdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdTags :: Lens.Lens' CreateSubscriptionDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csdTags = Lens.lens (tags :: CreateSubscriptionDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateSubscriptionDefinition)
{-# DEPRECATED csdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateSubscriptionDefinition where
  type
    Rs CreateSubscriptionDefinition =
      CreateSubscriptionDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubscriptionDefinition where
  toHeaders CreateSubscriptionDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateSubscriptionDefinition where
  toJSON CreateSubscriptionDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateSubscriptionDefinition where
  toPath = Lude.const "/greengrass/definition/subscriptions"

instance Lude.ToQuery CreateSubscriptionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
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
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubscriptionDefinitionResponse
mkCreateSubscriptionDefinitionResponse pResponseStatus_ =
  CreateSubscriptionDefinitionResponse'
    { latestVersionARN =
        Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsLatestVersionARN :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsARN :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsARN = Lens.lens (arn :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsName :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsName = Lens.lens (name :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsCreationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsId :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsId = Lens.lens (id :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsLatestVersion :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsLatestVersion = Lens.lens (latestVersion :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsLastUpdatedTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Lude.Maybe Lude.Text)
csdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateSubscriptionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsResponseStatus :: Lens.Lens' CreateSubscriptionDefinitionResponse Lude.Int
csdrsResponseStatus = Lens.lens (responseStatus :: CreateSubscriptionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubscriptionDefinitionResponse)
{-# DEPRECATED csdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
