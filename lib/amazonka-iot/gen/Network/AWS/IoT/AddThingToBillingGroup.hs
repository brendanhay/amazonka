{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AddThingToBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a billing group.
module Network.AWS.IoT.AddThingToBillingGroup
  ( -- * Creating a request
    AddThingToBillingGroup (..),
    mkAddThingToBillingGroup,

    -- ** Request lenses
    attbgThingARN,
    attbgBillingGroupARN,
    attbgThingName,
    attbgBillingGroupName,

    -- * Destructuring the response
    AddThingToBillingGroupResponse (..),
    mkAddThingToBillingGroupResponse,

    -- ** Response lenses
    attbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The ARN of the thing to be added to the billing group.
    thingARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the billing group.
    billingGroupARN :: Lude.Maybe Lude.Text,
    -- | The name of the thing to be added to the billing group.
    thingName :: Lude.Maybe Lude.Text,
    -- | The name of the billing group.
    billingGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddThingToBillingGroup' with the minimum fields required to make a request.
--
-- * 'thingARN' - The ARN of the thing to be added to the billing group.
-- * 'billingGroupARN' - The ARN of the billing group.
-- * 'thingName' - The name of the thing to be added to the billing group.
-- * 'billingGroupName' - The name of the billing group.
mkAddThingToBillingGroup ::
  AddThingToBillingGroup
mkAddThingToBillingGroup =
  AddThingToBillingGroup'
    { thingARN = Lude.Nothing,
      billingGroupARN = Lude.Nothing,
      thingName = Lude.Nothing,
      billingGroupName = Lude.Nothing
    }

-- | The ARN of the thing to be added to the billing group.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgThingARN :: Lens.Lens' AddThingToBillingGroup (Lude.Maybe Lude.Text)
attbgThingARN = Lens.lens (thingARN :: AddThingToBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: AddThingToBillingGroup)
{-# DEPRECATED attbgThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgBillingGroupARN :: Lens.Lens' AddThingToBillingGroup (Lude.Maybe Lude.Text)
attbgBillingGroupARN = Lens.lens (billingGroupARN :: AddThingToBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupARN = a} :: AddThingToBillingGroup)
{-# DEPRECATED attbgBillingGroupARN "Use generic-lens or generic-optics with 'billingGroupARN' instead." #-}

-- | The name of the thing to be added to the billing group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgThingName :: Lens.Lens' AddThingToBillingGroup (Lude.Maybe Lude.Text)
attbgThingName = Lens.lens (thingName :: AddThingToBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: AddThingToBillingGroup)
{-# DEPRECATED attbgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgBillingGroupName :: Lens.Lens' AddThingToBillingGroup (Lude.Maybe Lude.Text)
attbgBillingGroupName = Lens.lens (billingGroupName :: AddThingToBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: AddThingToBillingGroup)
{-# DEPRECATED attbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Lude.AWSRequest AddThingToBillingGroup where
  type Rs AddThingToBillingGroup = AddThingToBillingGroupResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddThingToBillingGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddThingToBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingArn" Lude..=) Lude.<$> thingARN,
            ("billingGroupArn" Lude..=) Lude.<$> billingGroupARN,
            ("thingName" Lude..=) Lude.<$> thingName,
            ("billingGroupName" Lude..=) Lude.<$> billingGroupName
          ]
      )

instance Lude.ToPath AddThingToBillingGroup where
  toPath = Lude.const "/billing-groups/addThingToBillingGroup"

instance Lude.ToQuery AddThingToBillingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddThingToBillingGroupResponse' smart constructor.
newtype AddThingToBillingGroupResponse = AddThingToBillingGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddThingToBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddThingToBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddThingToBillingGroupResponse
mkAddThingToBillingGroupResponse pResponseStatus_ =
  AddThingToBillingGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgrsResponseStatus :: Lens.Lens' AddThingToBillingGroupResponse Lude.Int
attbgrsResponseStatus = Lens.lens (responseStatus :: AddThingToBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddThingToBillingGroupResponse)
{-# DEPRECATED attbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
