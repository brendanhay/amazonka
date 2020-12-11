{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a subscription definition which has already been defined.
module Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
  ( -- * Creating a request
    CreateSubscriptionDefinitionVersion (..),
    mkCreateSubscriptionDefinitionVersion,

    -- ** Request lenses
    csdvAmznClientToken,
    csdvSubscriptions,
    csdvSubscriptionDefinitionId,

    -- * Destructuring the response
    CreateSubscriptionDefinitionVersionResponse (..),
    mkCreateSubscriptionDefinitionVersionResponse,

    -- ** Response lenses
    csdvrsARN,
    csdvrsCreationTimestamp,
    csdvrsVersion,
    csdvrsId,
    csdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { amznClientToken ::
      Lude.Maybe
        Lude.Text,
    subscriptions ::
      Lude.Maybe
        [Subscription],
    subscriptionDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
-- * 'subscriptions' - A list of subscriptions.
mkCreateSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  CreateSubscriptionDefinitionVersion
mkCreateSubscriptionDefinitionVersion pSubscriptionDefinitionId_ =
  CreateSubscriptionDefinitionVersion'
    { amznClientToken =
        Lude.Nothing,
      subscriptions = Lude.Nothing,
      subscriptionDefinitionId = pSubscriptionDefinitionId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvAmznClientToken :: Lens.Lens' CreateSubscriptionDefinitionVersion (Lude.Maybe Lude.Text)
csdvAmznClientToken = Lens.lens (amznClientToken :: CreateSubscriptionDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateSubscriptionDefinitionVersion)
{-# DEPRECATED csdvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvSubscriptions :: Lens.Lens' CreateSubscriptionDefinitionVersion (Lude.Maybe [Subscription])
csdvSubscriptions = Lens.lens (subscriptions :: CreateSubscriptionDefinitionVersion -> Lude.Maybe [Subscription]) (\s a -> s {subscriptions = a} :: CreateSubscriptionDefinitionVersion)
{-# DEPRECATED csdvSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvSubscriptionDefinitionId :: Lens.Lens' CreateSubscriptionDefinitionVersion Lude.Text
csdvSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: CreateSubscriptionDefinitionVersion -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: CreateSubscriptionDefinitionVersion)
{-# DEPRECATED csdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

instance Lude.AWSRequest CreateSubscriptionDefinitionVersion where
  type
    Rs CreateSubscriptionDefinitionVersion =
      CreateSubscriptionDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubscriptionDefinitionVersion where
  toHeaders CreateSubscriptionDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateSubscriptionDefinitionVersion where
  toJSON CreateSubscriptionDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Subscriptions" Lude..=) Lude.<$> subscriptions])

instance Lude.ToPath CreateSubscriptionDefinitionVersion where
  toPath CreateSubscriptionDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateSubscriptionDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
  { arn ::
      Lude.Maybe
        Lude.Text,
    creationTimestamp ::
      Lude.Maybe
        Lude.Text,
    version ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriptionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
-- * 'version' - The ID of the version.
mkCreateSubscriptionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubscriptionDefinitionVersionResponse
mkCreateSubscriptionDefinitionVersionResponse pResponseStatus_ =
  CreateSubscriptionDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrsARN :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
csdvrsARN = Lens.lens (arn :: CreateSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateSubscriptionDefinitionVersionResponse)
{-# DEPRECATED csdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrsCreationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
csdvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionVersionResponse)
{-# DEPRECATED csdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrsVersion :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
csdvrsVersion = Lens.lens (version :: CreateSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateSubscriptionDefinitionVersionResponse)
{-# DEPRECATED csdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrsId :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
csdvrsId = Lens.lens (id :: CreateSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateSubscriptionDefinitionVersionResponse)
{-# DEPRECATED csdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrsResponseStatus :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse Lude.Int
csdvrsResponseStatus = Lens.lens (responseStatus :: CreateSubscriptionDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubscriptionDefinitionVersionResponse)
{-# DEPRECATED csdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
