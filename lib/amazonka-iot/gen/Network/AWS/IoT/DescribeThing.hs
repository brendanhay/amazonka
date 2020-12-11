{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing.
module Network.AWS.IoT.DescribeThing
  ( -- * Creating a request
    DescribeThing (..),
    mkDescribeThing,

    -- ** Request lenses
    desThingName,

    -- * Destructuring the response
    DescribeThingResponse (..),
    mkDescribeThingResponse,

    -- ** Response lenses
    dtrsDefaultClientId,
    dtrsThingTypeName,
    dtrsThingARN,
    dtrsAttributes,
    dtrsVersion,
    dtrsThingName,
    dtrsBillingGroupName,
    dtrsThingId,
    dtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DescribeThing operation.
--
-- /See:/ 'mkDescribeThing' smart constructor.
newtype DescribeThing = DescribeThing' {thingName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThing' with the minimum fields required to make a request.
--
-- * 'thingName' - The name of the thing.
mkDescribeThing ::
  -- | 'thingName'
  Lude.Text ->
  DescribeThing
mkDescribeThing pThingName_ =
  DescribeThing' {thingName = pThingName_}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desThingName :: Lens.Lens' DescribeThing Lude.Text
desThingName = Lens.lens (thingName :: DescribeThing -> Lude.Text) (\s a -> s {thingName = a} :: DescribeThing)
{-# DEPRECATED desThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DescribeThing where
  type Rs DescribeThing = DescribeThingResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeThingResponse'
            Lude.<$> (x Lude..?> "defaultClientId")
            Lude.<*> (x Lude..?> "thingTypeName")
            Lude.<*> (x Lude..?> "thingArn")
            Lude.<*> (x Lude..?> "attributes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "thingName")
            Lude.<*> (x Lude..?> "billingGroupName")
            Lude.<*> (x Lude..?> "thingId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeThing where
  toPath DescribeThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName]

instance Lude.ToQuery DescribeThing where
  toQuery = Lude.const Lude.mempty

-- | The output from the DescribeThing operation.
--
-- /See:/ 'mkDescribeThingResponse' smart constructor.
data DescribeThingResponse = DescribeThingResponse'
  { defaultClientId ::
      Lude.Maybe Lude.Text,
    thingTypeName :: Lude.Maybe Lude.Text,
    thingARN :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    version :: Lude.Maybe Lude.Integer,
    thingName :: Lude.Maybe Lude.Text,
    billingGroupName :: Lude.Maybe Lude.Text,
    thingId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The thing attributes.
-- * 'billingGroupName' - The name of the billing group the thing belongs to.
-- * 'defaultClientId' - The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service.
--
-- This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
-- * 'responseStatus' - The response status code.
-- * 'thingARN' - The ARN of the thing to describe.
-- * 'thingId' - The ID of the thing to describe.
-- * 'thingName' - The name of the thing.
-- * 'thingTypeName' - The thing type name.
-- * 'version' - The current version of the thing record in the registry.
mkDescribeThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeThingResponse
mkDescribeThingResponse pResponseStatus_ =
  DescribeThingResponse'
    { defaultClientId = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      thingARN = Lude.Nothing,
      attributes = Lude.Nothing,
      version = Lude.Nothing,
      thingName = Lude.Nothing,
      billingGroupName = Lude.Nothing,
      thingId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service.
--
-- This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
--
-- /Note:/ Consider using 'defaultClientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsDefaultClientId :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsDefaultClientId = Lens.lens (defaultClientId :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultClientId = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsDefaultClientId "Use generic-lens or generic-optics with 'defaultClientId' instead." #-}

-- | The thing type name.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsThingTypeName :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsThingTypeName = Lens.lens (thingTypeName :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The ARN of the thing to describe.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsThingARN :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsThingARN = Lens.lens (thingARN :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The thing attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsAttributes :: Lens.Lens' DescribeThingResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dtrsAttributes = Lens.lens (attributes :: DescribeThingResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The current version of the thing record in the registry.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsVersion :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Integer)
dtrsVersion = Lens.lens (version :: DescribeThingResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsThingName :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsThingName = Lens.lens (thingName :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the billing group the thing belongs to.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsBillingGroupName :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsBillingGroupName = Lens.lens (billingGroupName :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ID of the thing to describe.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsThingId :: Lens.Lens' DescribeThingResponse (Lude.Maybe Lude.Text)
dtrsThingId = Lens.lens (thingId :: DescribeThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingId = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsThingId "Use generic-lens or generic-optics with 'thingId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeThingResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeThingResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
