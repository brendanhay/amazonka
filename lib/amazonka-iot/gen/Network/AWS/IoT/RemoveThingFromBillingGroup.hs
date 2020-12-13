{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RemoveThingFromBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given thing from the billing group.
module Network.AWS.IoT.RemoveThingFromBillingGroup
  ( -- * Creating a request
    RemoveThingFromBillingGroup (..),
    mkRemoveThingFromBillingGroup,

    -- ** Request lenses
    rtfbgThingARN,
    rtfbgBillingGroupARN,
    rtfbgThingName,
    rtfbgBillingGroupName,

    -- * Destructuring the response
    RemoveThingFromBillingGroupResponse (..),
    mkRemoveThingFromBillingGroupResponse,

    -- ** Response lenses
    rtfbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveThingFromBillingGroup' smart constructor.
data RemoveThingFromBillingGroup = RemoveThingFromBillingGroup'
  { -- | The ARN of the thing to be removed from the billing group.
    thingARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the billing group.
    billingGroupARN :: Lude.Maybe Lude.Text,
    -- | The name of the thing to be removed from the billing group.
    thingName :: Lude.Maybe Lude.Text,
    -- | The name of the billing group.
    billingGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveThingFromBillingGroup' with the minimum fields required to make a request.
--
-- * 'thingARN' - The ARN of the thing to be removed from the billing group.
-- * 'billingGroupARN' - The ARN of the billing group.
-- * 'thingName' - The name of the thing to be removed from the billing group.
-- * 'billingGroupName' - The name of the billing group.
mkRemoveThingFromBillingGroup ::
  RemoveThingFromBillingGroup
mkRemoveThingFromBillingGroup =
  RemoveThingFromBillingGroup'
    { thingARN = Lude.Nothing,
      billingGroupARN = Lude.Nothing,
      thingName = Lude.Nothing,
      billingGroupName = Lude.Nothing
    }

-- | The ARN of the thing to be removed from the billing group.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgThingARN :: Lens.Lens' RemoveThingFromBillingGroup (Lude.Maybe Lude.Text)
rtfbgThingARN = Lens.lens (thingARN :: RemoveThingFromBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: RemoveThingFromBillingGroup)
{-# DEPRECATED rtfbgThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgBillingGroupARN :: Lens.Lens' RemoveThingFromBillingGroup (Lude.Maybe Lude.Text)
rtfbgBillingGroupARN = Lens.lens (billingGroupARN :: RemoveThingFromBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupARN = a} :: RemoveThingFromBillingGroup)
{-# DEPRECATED rtfbgBillingGroupARN "Use generic-lens or generic-optics with 'billingGroupARN' instead." #-}

-- | The name of the thing to be removed from the billing group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgThingName :: Lens.Lens' RemoveThingFromBillingGroup (Lude.Maybe Lude.Text)
rtfbgThingName = Lens.lens (thingName :: RemoveThingFromBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: RemoveThingFromBillingGroup)
{-# DEPRECATED rtfbgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgBillingGroupName :: Lens.Lens' RemoveThingFromBillingGroup (Lude.Maybe Lude.Text)
rtfbgBillingGroupName = Lens.lens (billingGroupName :: RemoveThingFromBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: RemoveThingFromBillingGroup)
{-# DEPRECATED rtfbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Lude.AWSRequest RemoveThingFromBillingGroup where
  type
    Rs RemoveThingFromBillingGroup =
      RemoveThingFromBillingGroupResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveThingFromBillingGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveThingFromBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RemoveThingFromBillingGroup where
  toJSON RemoveThingFromBillingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingArn" Lude..=) Lude.<$> thingARN,
            ("billingGroupArn" Lude..=) Lude.<$> billingGroupARN,
            ("thingName" Lude..=) Lude.<$> thingName,
            ("billingGroupName" Lude..=) Lude.<$> billingGroupName
          ]
      )

instance Lude.ToPath RemoveThingFromBillingGroup where
  toPath = Lude.const "/billing-groups/removeThingFromBillingGroup"

instance Lude.ToQuery RemoveThingFromBillingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveThingFromBillingGroupResponse' smart constructor.
newtype RemoveThingFromBillingGroupResponse = RemoveThingFromBillingGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveThingFromBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveThingFromBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveThingFromBillingGroupResponse
mkRemoveThingFromBillingGroupResponse pResponseStatus_ =
  RemoveThingFromBillingGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgrsResponseStatus :: Lens.Lens' RemoveThingFromBillingGroupResponse Lude.Int
rtfbgrsResponseStatus = Lens.lens (responseStatus :: RemoveThingFromBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveThingFromBillingGroupResponse)
{-# DEPRECATED rtfbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
