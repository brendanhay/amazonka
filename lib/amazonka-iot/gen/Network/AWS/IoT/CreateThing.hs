{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thing record in the registry. If this call is made multiple times using the same thing name and configuration, the call will succeed. If this call is made with the same thing name but different configuration a @ResourceAlreadyExistsException@ is thrown.
module Network.AWS.IoT.CreateThing
  ( -- * Creating a request
    CreateThing (..),
    mkCreateThing,

    -- ** Request lenses
    ctThingTypeName,
    ctAttributePayload,
    ctBillingGroupName,
    ctThingName,

    -- * Destructuring the response
    CreateThingResponse (..),
    mkCreateThingResponse,

    -- ** Response lenses
    ctrsThingARN,
    ctrsThingName,
    ctrsThingId,
    ctrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreateThing operation.
--
-- /See:/ 'mkCreateThing' smart constructor.
data CreateThing = CreateThing'
  { thingTypeName ::
      Lude.Maybe Lude.Text,
    attributePayload :: Lude.Maybe AttributePayload,
    billingGroupName :: Lude.Maybe Lude.Text,
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateThing' with the minimum fields required to make a request.
--
-- * 'attributePayload' - The attribute payload, which consists of up to three name/value pairs in a JSON document. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@
-- * 'billingGroupName' - The name of the billing group the thing will be added to.
-- * 'thingName' - The name of the thing to create.
--
-- You can't change a thing's name after you create it. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
-- * 'thingTypeName' - The name of the thing type associated with the new thing.
mkCreateThing ::
  -- | 'thingName'
  Lude.Text ->
  CreateThing
mkCreateThing pThingName_ =
  CreateThing'
    { thingTypeName = Lude.Nothing,
      attributePayload = Lude.Nothing,
      billingGroupName = Lude.Nothing,
      thingName = pThingName_
    }

-- | The name of the thing type associated with the new thing.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctThingTypeName :: Lens.Lens' CreateThing (Lude.Maybe Lude.Text)
ctThingTypeName = Lens.lens (thingTypeName :: CreateThing -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: CreateThing)
{-# DEPRECATED ctThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The attribute payload, which consists of up to three name/value pairs in a JSON document. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctAttributePayload :: Lens.Lens' CreateThing (Lude.Maybe AttributePayload)
ctAttributePayload = Lens.lens (attributePayload :: CreateThing -> Lude.Maybe AttributePayload) (\s a -> s {attributePayload = a} :: CreateThing)
{-# DEPRECATED ctAttributePayload "Use generic-lens or generic-optics with 'attributePayload' instead." #-}

-- | The name of the billing group the thing will be added to.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctBillingGroupName :: Lens.Lens' CreateThing (Lude.Maybe Lude.Text)
ctBillingGroupName = Lens.lens (billingGroupName :: CreateThing -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: CreateThing)
{-# DEPRECATED ctBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The name of the thing to create.
--
-- You can't change a thing's name after you create it. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctThingName :: Lens.Lens' CreateThing Lude.Text
ctThingName = Lens.lens (thingName :: CreateThing -> Lude.Text) (\s a -> s {thingName = a} :: CreateThing)
{-# DEPRECATED ctThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest CreateThing where
  type Rs CreateThing = CreateThingResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateThingResponse'
            Lude.<$> (x Lude..?> "thingArn")
            Lude.<*> (x Lude..?> "thingName")
            Lude.<*> (x Lude..?> "thingId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateThing where
  toJSON CreateThing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingTypeName" Lude..=) Lude.<$> thingTypeName,
            ("attributePayload" Lude..=) Lude.<$> attributePayload,
            ("billingGroupName" Lude..=) Lude.<$> billingGroupName
          ]
      )

instance Lude.ToPath CreateThing where
  toPath CreateThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName]

instance Lude.ToQuery CreateThing where
  toQuery = Lude.const Lude.mempty

-- | The output of the CreateThing operation.
--
-- /See:/ 'mkCreateThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
  { thingARN ::
      Lude.Maybe Lude.Text,
    thingName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateThingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'thingARN' - The ARN of the new thing.
-- * 'thingId' - The thing ID.
-- * 'thingName' - The name of the new thing.
mkCreateThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateThingResponse
mkCreateThingResponse pResponseStatus_ =
  CreateThingResponse'
    { thingARN = Lude.Nothing,
      thingName = Lude.Nothing,
      thingId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the new thing.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsThingARN :: Lens.Lens' CreateThingResponse (Lude.Maybe Lude.Text)
ctrsThingARN = Lens.lens (thingARN :: CreateThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: CreateThingResponse)
{-# DEPRECATED ctrsThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The name of the new thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsThingName :: Lens.Lens' CreateThingResponse (Lude.Maybe Lude.Text)
ctrsThingName = Lens.lens (thingName :: CreateThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: CreateThingResponse)
{-# DEPRECATED ctrsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The thing ID.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsThingId :: Lens.Lens' CreateThingResponse (Lude.Maybe Lude.Text)
ctrsThingId = Lens.lens (thingId :: CreateThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingId = a} :: CreateThingResponse)
{-# DEPRECATED ctrsThingId "Use generic-lens or generic-optics with 'thingId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateThingResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateThingResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
